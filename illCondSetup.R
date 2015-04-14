


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
    thetaMatrix <- noFixCovMatrix
  }else{
    
    thetaMatrix <- noFixCovMatrix[-c(nonFixNonThetaRows),
                                -c(nonFixNonThetaRows)]
  }
  
  
  # Form a random matrix with the right number of elements
  randThetaMat <- matrix(runif(nrow(thetaMatrix)*ncol(thetaMatrix), 
                          min=-1, max=1), 
                    nrow=nrow(thetaMatrix), ncol=ncol(thetaMatrix))
  
  # Do eigenvalue decomposition so that we can make the matrix positive definite
  eigenDecomp <- eigen(randThetaMat, symmetric=TRUE)
  
  # New eigen value vector with all positive values
  newEigenVals <- abs(eigenDecomp$values)
  
  # Create the new positive definite matrix
  posDefRandThetaMat <- eigenDecomp$vectors %*% 
                        diag(newEigenVals) %*% 
                        t(eigenDecomp$vectors)

  # Make sure the matrix is symmetric
  # posDefSymRandThetaMat <- (posDefRandThetaMat+t(posDefRandThetaMat))/2
  
  # Test some alpha stuff
  alpha <- (magnitude*log(10)) / (log(kappa(posDefSymRandThetaMat, exact=TRUE)))
  
  
  newThetaMat <- eigenDecomp$vectors %*% 
                 diag(newEigenVals^alpha) %*% 
                 t(eigenDecomp$vectors)
  
  
  # I should really get the condition number of the inverse theta matrix plus the original 
  # sigma/omega bits and use that. This is my attempt.
  
  condNumMatRows <- matrix(0, nrow=nrow(noFixCovMatrix), ncol=nrow(newThetaMat))

  if(length(nonFixNonThetaRows)==0){
    condNumMat <- eigenDecomp$vectors %*% 
                  diag(newEigenVals^-alpha) %*% 
                  t(eigenDecomp$vectors)
  }else{
    # Insert the inverted, random theta rows into the right places
    condNumMatRows[-nonFixNonThetaRows,] <- eigenDecomp$vectors %*% 
                                            diag(newEigenVals^-alpha) %*% 
                                            t(eigenDecomp$vectors)
    
    # Insert the non theta rows
    
    condNumMat <- cbind(condNumMatRows, noFixCovMatrix[,nonFixNonThetaRows])
    
    condNumMat[nonFixNonThetaRows,] <- noFixCovMatrix[nonFixNonThetaRows,]
  }
  
  # Calculate the theoretical condition number
  theorCondNum <- kappa(condNumMat, exact=TRUE)
  
  # Write out condition number
  print(paste("Theoretical condition number for replicate ", 
              replic, "is", format(theorCondNum, scientific=TRUE)))
  
  
  # Make sure the newThetaMat is symmetric
  #newSymThetaMat <- (newThetaMat+t(newThetaMat))/2
  
  # Put back sigma and omega elements
  newSymThetaMatRows <- matrix(0, nrow=nrow(noFixCovMatrix), 
                               ncol=nrow(newSymThetaMat))
  
  
  if(length(nonFixNonThetaRows)==0){
    newSymFullMat <- newSymThetaMat
  }else{
    # Insert the random theta rows into the right places
    newSymThetaMatRows[-nonFixNonThetaRows,] <- newSymThetaMat
    
    # Insert the non theta rows
    
    newSymFullMat <- cbind(newSymThetaMatRows, 
                           noFixCovMatrix[,nonFixNonThetaRows])
    
    newSymFullMat[nonFixNonThetaRows,] <- noFixCovMatrix[nonFixNonThetaRows,]
  }
  
  
  # If there were any fixed params, put back the zeroes
  
  if(length(fixRows)==0){
    
    newMat <- newSymFullMat
    
  } else {
    # Create a 0 matrix with the right number of rows and the rCond number of columns
    zeroMatRows <- matrix(0, nrow=nrow(orgCovMatrix), 
                          ncol=ncol(newSymFullMat))
    
    # Insert the non-zero values into the correct rows
    zeroMatRows[-c(fixRows), ] <- as.matrix(newSymFullMat)
    
    # Create a 0 matrix with the right number of rows and columns
    zeroMatCols <- matrix(0, nrow=nrow(zeroMatRows), 
                          ncol=ncol(orgCovMatrix))
    
    # Insert the values from the previous step into the correct columns 
    zeroMatCols[,-fixRows] <- zeroMatRows
    
    # Name it something better than zeroMatCols
    newMat <- zeroMatCols

  }
  
  # I'm having issues with the precond script saying the matrix isn't symmetric.
  # I'll fix that here.
  newMat <- (newMat+t(newMat))/2
  
  # Write out the csv with the magnitude in the name, formatted to 3 leading digits
  csvFileName <- paste0("./illCondFiles/illcond_rep_", 
                        formatC(replic, digits=0, width=5, format="d", flag="0"), 
                        ".csv")
  
  write.table(newMat, csvFileName, row.names=FALSE, col.names=FALSE, sep=",")
  
  seed <- as.integer(runif(1, min=1000, max=1000000))
  
  # return the generated csv file name
  return(c(replic, csvFileName, magnitude, theorCondNum, seed))
}
