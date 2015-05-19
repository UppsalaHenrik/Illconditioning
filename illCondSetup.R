


illCondSetup <- function(covCsv, magnitude, replic){
  
  orgCovMatrix <- read.table(covCsv, skip=1, header=TRUE)
  orgCovMatrix[,1] <- NULL 
  
  # Remove fixed param rows
  stripZeroRowsColsList <- stripZeroRowsCols(orgCovMatrix)
  noFixCovMatrix <- stripZeroRowsColsList[[1]]
  fixRows <- stripZeroRowsColsList[[2]]
  
  stripNonThetaRowsColsList <- stripNonThetaRowsCols(noFixCovMatrix)
  thetaMatrix <- stripNonThetaRowsColsList[[1]]
  nonFixNonThetaRows <- stripNonThetaRowsColsList[[2]]

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
  # Determine alpha that will produce a condition number of the requested magnitude
  alpha <- (magnitude*log(10)) / (log(kappa(posDefRandThetaMat, exact=TRUE)))


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
  
  # Put back sigma and omega elements
  newThetaMatRows <- matrix(0, nrow=nrow(noFixCovMatrix), 
                               ncol=nrow(newThetaMat))
  
  
  if(length(nonFixNonThetaRows)==0){
    newFullMat <- newThetaMat
  }else{
    # Insert the random theta rows into the right places
    newThetaMatRows[-nonFixNonThetaRows,] <- newThetaMat
    
    # Insert the non theta rows
    
    newFullMat <- cbind(newThetaMatRows, 
                           noFixCovMatrix[,nonFixNonThetaRows])
    
    newFullMat[nonFixNonThetaRows,] <- noFixCovMatrix[nonFixNonThetaRows,]
  }


  # If there were any fixed params, put back the zeroes
  newMat <- putBackZeroRows(newFullMat, fixRows)

  # I'm having issues with the precond script saying the matrix isn't symmetric.
  # I'll fix that here.
  newSymMat <- (newMat+t(newMat))/2
    
  
  # Write out condition number
  print(paste("Theoretical condition number for replicate ", 
              replic, "is", format(theorCondNum, scientific=TRUE)))
  
  # Write out the csv with the magnitude in the name, formatted to 3 leading digits
  csvFileName <- paste0("./illCondFiles/illcond_rep_", 
                        formatC(replic, digits=0, width=5, format="d", flag="0"), 
                        ".csv")
  
  write.table(newSymMat, csvFileName, row.names=FALSE, col.names=FALSE, sep=",")
  
  seed <- as.integer(runif(1, min=1000, max=1000000))
  
  # return the generated csv file name
  return(data.frame(reps = replic, illCondFiles = csvFileName, 
                    magnitudes = magnitude, theorCondNums = theorCondNum, 
                    seeds = seed))
}
