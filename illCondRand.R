


illCondRand <- function(covCsv, magnitude){
  
  orgCovMatrix <- read.table(covCsv, skip=1, header=TRUE)
  orgCovMatrix[,1] <- NULL 
  
  # Pick out the row numbers for fixed params
  fixRows <- which(apply(orgCovMatrix, 1, sum) == 0)
  
  # Delete those rows and their corresponding columns
  if(length(fixRows)==0){
    covMatrix <- orgCovMatrix
  }else{
    covMatrix <- orgCovMatrix[-c(fixRows),-c(fixRows)]
  }                     
  
  
  randMat <- matrix(runif(nrow(covMatrix)*ncol(covMatrix), 
                          min=-1, max=1)*10^(runif(nrow(covMatrix)*ncol(covMatrix), 
                                                   min=-magnitude, max=magnitude)), 
                    nrow=nrow(covMatrix), ncol=ncol(covMatrix))
  
  # Do eigenvalue decomposition so that we can make the matrix positive definite
  eigenDecomp <- eigen(randMat, symmetric=TRUE)
  
  # New eigen value vector with all positive values
  newEigenVals <- abs(eigenDecomp$values)
  
  # Create the new positive definite matrix
  posDefRandMat <- eigenDecomp$vectors %*% diag(newEigenVals) %*% solve(eigenDecomp$vectors)

  # Make sure the matrix is symmetric
  posDefSymRandMat <- (posDefRandMat+t(posDefRandMat))/2
  
  
  # Write out condition number
  
  condNum <- kappa(posDefSymRandMat, exact=TRUE)
  print(paste("Condition number for magnitude", magnitude, "is", condNum))
  
  # If there were any fixed params, put back the zeroes
  
  if(length(fixRows)==0){
    
    rCondFull <- posDefSymRandMat
    
  } else {
    # Create a 0 matrix with the right number of rows and the rCond number of columns
    zeroMatRows <- matrix(0, nrow=max(nrow(posDefSymRandMat), fixRows), ncol=ncol(posDefSymRandMat))
    
    # Insert the non-zero values into the correct rows
    zeroMatRows[-fixRows, ] <- posDefSymRandMat
    
    # Create a 0 matrix with the right number of rows and columns
    zeroMatCols <- matrix(0, nrow=nrow(zeroMatRows), ncol=max(ncol(posDefSymRandMat), fixRows))
    
    # Insert the values from the previous step into the correct columns 
    zeroMatCols[,-fixRows] <- zeroMatRows
    
    # Name it something better than zeroMatCols
    rCondFull <- zeroMatCols

  }

  # Write out the csv with the magnitude in the name, formatted to 3 leading digits
  csvFileName <- paste0("illcond_mag_", 
                        formatC(magnitude, digits=1, 
                                width=5, format="f", 
                                flag="0"), ".csv")
  
  write.table(rCondFull, csvFileName, row.names=FALSE, col.names=FALSE, sep=",")
  
  # return the generated csv file name
  return(csvFileName)
}
