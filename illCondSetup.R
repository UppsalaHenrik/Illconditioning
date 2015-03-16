


illCondSetup <- function(covCsv, magnitude, replic){
  
  orgCovMatrix <- read.table(covCsv, skip=1, header=TRUE)
  orgCovMatrix[,1] <- NULL 
  
  # Pick out the row numbers for fixed params
  fixRows <- which(apply(orgCovMatrix, 1, sum) == 0)
  
  # Pick out the theta rows
  thetaRows <- grep("THETA", names(orgCovMatrix))
  

  
  # Delete all but the non-fixed theta rows and their corresponding columns
  if(length(fixRows)==0){
    noFixCovMatrix <- orgCovMatrix
  }else{
    noFixCovMatrix <- orgCovMatrix[-c(fixRows), 
                                   -c(fixRows)]
  }
  
  # and pick out the sigma and omega rows for non-fixed params
  nonFixNonThetaRows <- c(1:length(noFixCovMatrix))[-c(grep("THETA", names(noFixCovMatrix)))]
  
  
  if(length(nonFixNonThetaRows)==0){
    covMatrix <- noFixCovMatrix
  }else{
    
    covMatrix <- noFixCovMatrix[-c(nonFixNonThetaRows),
                                -c(nonFixNonThetaRows)]
  }
  
  
  # Form a random matrix with the right number of elements
  randMat <- matrix(runif(nrow(covMatrix)*ncol(covMatrix), 
                          min=-1, max=1), 
                    nrow=nrow(covMatrix), ncol=ncol(covMatrix))
  
  # Do eigenvalue decomposition so that we can make the matrix positive definite
  eigenDecomp <- eigen(randMat, symmetric=TRUE)
  
  # New eigen value vector with all positive values
  newEigenVals <- abs(eigenDecomp$values)
  
  # Create the new positive definite matrix
  posDefRandMat <- eigenDecomp$vectors %*% diag(newEigenVals) %*% t(eigenDecomp$vectors)

  # Make sure the matrix is symmetric
  posDefSymRandMat <- (posDefRandMat+t(posDefRandMat))/2
  
  # Test some alpha stuff
  alpha <- (magnitude*log(10)) / (log(kappa(posDefSymRandMat, exact=TRUE)))
  
  
  newMat <- eigenDecomp$vectors %*% diag(newEigenVals^alpha) %*% t(eigenDecomp$vectors)
  
  
  # I should really get the condition number of the inverse theta matrix plus the original 
  # sigma/omega bits and use that. This is my attempt.
  
  condNumMatRows <- matrix(0, nrow=nrow(noFixCovMatrix), ncol=nrow(newMat))

  
  # Insert the inverted, random theta rows into the right places
  # This would be much better if I could invert the newMat in a better way...
  # 
  condNumMatRows[-nonFixNonThetaRows,] <- eigenDecomp$vectors %*% diag(newEigenVals^-alpha) %*% t(eigenDecomp$vectors)
  
  # Insert the non theta rows
  
  condNumMat <- cbind(condNumMatRows, noFixCovMatrix[,nonFixNonThetaRows])
  
  condNumMat[nonFixNonThetaRows,] <- noFixCovMatrix[nonFixNonThetaRows,]
  
  # Calculate the theoretical condition number
  theorCondNum <- kappa(condNumMat, exact=TRUE)
  
  # Write out condition number
  print(paste("Theoretical condition number for replic ", 
              replic, "is", format(theorCondNum, scientific=TRUE)))
  
  
  # Make sure the newMat is symmetric
  newSymMat <- (newMat+t(newMat))/2
  
  # Put back sigma and omega elements
  newSymMatRows <- matrix(0, nrow=nrow(noFixCovMatrix), ncol=nrow(newSymMat))
  
  
  # Insert the inverted, random theta rows into the right places
  # This would be much better if I could invert the newMat in a better way...
  # 
  newSymMatRows[-nonFixNonThetaRows,] <- newSymMat
  
  # Insert the non theta rows
  
  newSymFullMat <- cbind(newSymMatRows, noFixCovMatrix[,nonFixNonThetaRows])
  
  newSymFullMat[nonFixNonThetaRows,] <- noFixCovMatrix[nonFixNonThetaRows,]
  
  # If there were any fixed params, put back the zeroes
  
  if(length(fixRows)==0){
    
    rCondFull <- newSymFullMat
    
  } else {
    # Create a 0 matrix with the right number of rows and the rCond number of columns
    zeroMatRows <- matrix(0, nrow=max(nrow(newSymFullMat), fixRows), 
                          ncol=ncol(newSymFullMat))
    
    # Insert the non-zero values into the correct rows
    zeroMatRows[-c(fixRows), ] <- as.matrix(newSymFullMat)
    
    # Create a 0 matrix with the right number of rows and columns
    zeroMatCols <- matrix(0, nrow=nrow(zeroMatRows), ncol=max(ncol(newSymFullMat), fixRows))
    
    # Insert the values from the previous step into the correct columns 
    zeroMatCols[,-fixRows] <- zeroMatRows
    
    # Name it something better than zeroMatCols
    rCondFull <- zeroMatCols

  }
  
  # I'm having issues with the precond script saying the matrix isn't symmetric.
  # I'll fix that here.
  rCondFull <- (rCondFull+t(rCondFull))/2
  
  # Write out the csv with the magnitude in the name, formatted to 3 leading digits
  csvFileName <- paste0("./illCondFiles/illcond_rep_", 
                        formatC(replic, digits=0, width=5, format="d", flag="0"), 
                        ".csv")
  
  write.table(rCondFull, csvFileName, row.names=FALSE, col.names=FALSE, sep=",")
  
  seed <- as.integer(runif(1, min=1000, max=1000000))
  
  # return the generated csv file name
  return(c(replic, csvFileName, theorCondNum, seed))
}
