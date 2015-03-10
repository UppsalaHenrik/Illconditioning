


prepRunsToRun <- function(runsPerMag, illCondFiles){
  
  # Create a dataframe or replicate numbers
  runNums <- c(1:runsPerMag)
  
  # Create a dataframe with rows for each combination of replicate number and 
  # illconditioning matrix file
  runsGrid <- expand.grid(runNums, illCondFiles, stringsAsFactors=FALSE)
  names(runsGrid) <- c("runNums", "illCondFiles")
  
  # Get the condition numbers
  condNumRows <- lapply(illCondFiles, function(x){
    
    matrix <- read.csv(x, header=FALSE)
    
    # Pick out the row numbers for fixed params
    fixRows <- which(apply(matrix, 1, sum) == 0)
    
    # Delete those rows and their corresponding columns
    if(length(fixRows)==0){
      matrix <- matrix
    }else{
      matrix <- matrix[-c(fixRows),-c(fixRows)]
    }
    
    condNum <- kappa(matrix, exact=TRUE)
    
    c(x, condNum)
  })
  
  # Bind together all of the rows from the list 
  condNums <- as.data.frame(do.call("rbind", condNumRows))
  
  condNumsDF <- as.data.frame(lapply(condNums, function(x) unname(unlist(x))))
  runsGridDF <- as.data.frame(lapply(runsGrid, function(x) unname(unlist(x))))
  
  names(condNumsDF) <- c("illCondFiles", "condNums")
  names(runsGridDF) <- c("runNums", "illCondFiles")
  
  runsCondNumGrid <- merge(runsGridDF, condNumsDF, by=c("illCondFiles"))
  
  # I want to wait for the last run to finish. Adding a column to the runs dataframe
  # for wait option true or false.
  waits <- vector(mode="logical", length=length(runsGrid[[1]]))
  
  # Wait for every 100th run, and the last run to finish. I set 
  # the last and every 100th wait to TRUE.
  waits[seq.int(0, length(waits), 100)] <- "TRUE"
  waits[length(waits)] <- "TRUE"
  
  # Generate seeds for the initial estimate perturbation
  seeds <- rep(as.integer(runif(length(runNums), min=1000, max=1000000)), 
               length(illCondFiles))
  
  
  # cbind them all together
  runs <- cbind(runsGrid, waits, seeds)
  
  
  return(runs)
  
}