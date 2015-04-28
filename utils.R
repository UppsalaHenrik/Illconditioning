# I have chosen to separate these functions into a utils file.
# These are smaller and quite general functions.

# Required packages

require("ggplot2")


parseExtFile <- function(extFilePath){
  
  # Parsing the .ext using readLines because of different types of 
  # pesky whitespace. Also remove the first row and separate header
  rawExtFile <- readLines(extFilePath)
  header <- unlist(strsplit(gsub("^\\s+", "", rawExtFile[2]), "\\s+"))
  extFile <- rawExtFile[3:length(rawExtFile)]
  
  # Remove leading whitespace
  extFile <- gsub("^\\s+", "", extFile)
  
  # split the lines up and pack them into a df
  extFileCharDF <- data.frame(do.call("rbind", strsplit(extFile, "\\s+")), 
                          stringsAsFactors = FALSE)
  names(extFileCharDF) <- header
  
  # I need to convert the characters to numerics
  extFileDF <- as.data.frame(sapply(extFileCharDF, as.numeric))
  
  # Return the ext file contents as a dataframe
  return(extFileDF)
}


calcNMCondNum <- function(extFileDF){
  
  # Find the row that contains the eigenvalues
  eigenRowNum <- grep("-1000000002", extFileDF[,1])
  if(length(eigenRowNum) == 1){
    # Pick out that row without the iteration column
    eigenRow <- extFileDF[eigenRowNum,2:length(extFileDF)]
  
    # Remove the zeros et voila, you have a vector of eigenvalues
    eigenvals <- eigenRow[which(eigenRow!=0)]
  
    # Calculate and return the condition number
    NMCondNum <- max(eigenvals)/min(eigenvals)
    return(NMCondNum)
  }else{
    return("NA")

  }
}

getInitOFV <- function(extFileDF){
  
  # Get the last value in the first line of the ext 
  initOFV <- extFileDF[1,length(extFileDF)][1]
  
  return(initOFV)
  
}


parseRMat <- function(rMatFilePath){
  
  rawRMatFile <- readLines(rMatFilePath)
  header <- unlist(strsplit(gsub("^\\s+", "", rawRMatFile[2]), "\\s+"))
  rMatFile <- rawRMatFile[3:length(rawRMatFile)]
  
  # Remove leading whitespace
  rMatFile <- gsub("^\\s+", "", rMatFile)
  
  # split the lines up and pack them into a df
  rMatFileCharDF <- data.frame(do.call("rbind", strsplit(rMatFile, "\\s+")), 
                              stringsAsFactors = FALSE)[,-1]
  names(rMatFileCharDF) <- header[-1]
  
  # I need to convert the characters to numerics
  rMatFileDF <- as.data.frame(sapply(rMatFileCharDF, as.numeric))
  
  # Return the ext file contents as a dataframe
  return(rMatFileDF)
  
}

stripZeroRowsCols <- function(inputMatrix){
  
  # Make sure the matrix is symmetric
  if(ncol(inputMatrix) != nrow(inputMatrix)){
    print("Matrix sent to stripZeroRows is not symmetric")
    return(NULL)
  }
  
  # Pick out the row numbers for any zero rows
  zeroRows <- which(apply(inputMatrix, 1, sum) == 0)
  
  # Check if any zero rows were found and remove them, if not return the input matrix
  if(length(zeroRows)==0){
    print("No zero rows to strip out")
    return(inputMatrix)
  }else{
    noZeroRowsColsMatrix <- inputMatrix[-c(zeroRows),
                                        -c(zeroRows)]
    return(list(noZeroRowsColsMatrix, zeroRows))
  }  
}

putBackZeroRows <- function(noZeroRowsColsMatrix, zeroRows){
  
  # Make sure the matrix is symmetric
  if(ncol(noZeroRowsColsMatrix) != nrow(noZeroRowsColsMatrix)){
    print("Matrix sent to putBackZeroRows is not symmetric")
    return(NULL)
  }
  
  if(length(zeroRows)==0){
    print("No zero rows to put back")
    return(noZeroRowsColsMatrix)
  }else{
    
    zeroMatRows <- matrix(0, nrow=(nrow(noZeroRowsColsMatrix)+length(zeroRows)), 
                          ncol=ncol(noZeroRowsColsMatrix))
    
    # Insert the non-zero values into the correct rows
    zeroMatRows[-c(zeroRows), ] <- as.matrix(noZeroRowsColsMatrix)
    
    # Create a 0 matrix with the right number of rows and columns
    zeroMatCols <- matrix(0, nrow=nrow(zeroMatRows), 
                          ncol=nrow(zeroMatRows))
    
    # Insert the values from the previous step into the correct columns 
    zeroMatCols[,-zeroRows] <- zeroMatRows
    
    # Name it something better than zeroMatCols
    zerosIncludedMatrix <- zeroMatCols
    
    return(zerosIncludedMatrix)
    
  }  
}

stripNonThetaRowsCols <- function(inputMatrix){
  
  # Pick out the non-theta rows
  nonThetaRows <- c(1:length(inputMatrix))[-c(grep("THETA", names(inputMatrix)))]
  
  if(length(nonThetaRows)==0){
    thetaMatrix <- inputMatrix
  }else{
    
    thetaMatrix <- inputMatrix[-c(nonThetaRows),
                               -c(nonThetaRows)]
  }
  
  return(list(thetaMatrix, nonThetaRows))
}


getUserSlurmQ <- function(){
  
  # Get user name
  userName <- system('echo "$USER"', intern=TRUE)
  
  # Get the slurm queue for the active user
  slurmQ <- system(paste("squeue", "-u", userName), intern=TRUE)
  
  return(slurmQ)
}


# A function that waits for the SLURM queue to be smaller than a 
# target number of runs. Used either for 

waitForSlurmQ <- function(targetLength=0, secsToWait=30, maxWaits=20){
  
  keepWaiting <- TRUE
  
  i <- 1
  
  while(keepWaiting & i <= maxWaits){
    
    # Get the slurm queue and check its length
    slurmQ <- getUserSlurmQ()
    qLength <- length(slurmQ)
    
    # if it has no jobs in it end the while loop
    if(qLength - 1 <= targetLength){
      keepWaiting <- FALSE
      qTargetMessage <- paste0("Queue has ", qLength - 1, " jobs (<=", targetLength, ") - ending wait.")
      print(qTargetMessage)
      break
    }
    
    # Print a message about the wait
    qWaitMessage <- paste("Waiting for", secsToWait, "seconds (nr", 
                          i, "out of max", maxWaits, "waits) - ", 
                          qLength-1, "jobs in queue")
    print(qWaitMessage)
    
    # Wait for specified number of seconds 
    Sys.sleep(secsToWait)
    
    # Grow i 
    i <- i + 1
    
    if(i > maxWaits){
      qMaxWaitsMessage <- paste("Wait limit reached. Waited for", secsToWait*i, "seconds" )
      print(qMaxWaitsMessage)
    }
  }  
  
}

