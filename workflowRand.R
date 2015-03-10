
runMassPrecond <- function(modelFileName, magnitudes=1, runsPerMag=10, pertSize=0.2, 
                           precondScriptPath = "C:/Users/hnyberg/Dropbox/Doktorandsaker/PrecondProject/_hackedPsN3/PsN4_3/bin/precond_numStab"){
  
  # Get the working directory to set it back later
  userWD <- getwd()
  
  # Create a unique folder, copy all the relevant files there and set wd.
  modelFileNameNoExt <- doSetup(modelFileName)

  
  # Do the illconditioning for each magnitude
  illCondFiles <- lapply(magnitudes, function(x){
    illCondRand(paste0(modelFileNameNoExt, ".cov"), x)
  })
  
  # Create a data frame with all the runs' illCondFiles and run numbers
  runs <- prepRunsToRun(runsPerMag, illCondFiles)
  
  # Run all the runs!
  allRunDirs <- apply(runs, 1, function(x){
    # The way I point to the right element below is not great! This might fall over :(
    runPrecond(modelFileName, modelFileNameNoExt, as.numeric(x[2]), as.numeric(x[1]), 
               pertSize, precondScriptPath, as.logical(x[3]), as.integer(x[4]))
  })
  
  # Parse the rawres output from PsN precond
  allRows <- lapply(allRunDirs, function(x){
    
    # Put together what the file should be called
    rawresFileName <- paste0(x, "/modelfit_dir1/raw_results.csv")
    
    # Check if the rawres file exist, and if so, parse it
    
    if(file.exists(rawresFileName)==TRUE){
      rawres <- read.csv(rawresFileName)
      row <- c(x, rawres)
      return(row)
    }
    return(NULL)
  })
  
  # Bind together all of the rows from the list 
  allData <- do.call("rbind", allRows)
  
  # Bind in the run dataframe as well. Is there a risk for row mixups? 
  # Would be cool to get the condNum in there too
  rawResults <- cbind(runs, allData)
  
  # Write our the results to a raw results file
  write.csv(rawResults, file="raw_results.csv", row.names=FALSE)
  
  # List all the NM_run directories
  modelfitDirs <- list.dirs(recursive=TRUE)[grep("modelfit_dir[0-9]+$", 
                                                 list.dirs(recursive=TRUE))]
  
  # Copy out the raw_results files to the parent directory so that we 
  # don't delete them during cleanup. Not the prettiest ever but the 
  # directory runs the risk of racking up space.
  copyRes <- sapply(modelfitDirs, function(x){
    file.copy(list.files(x, full.names=TRUE)[grep("^raw_results.csv", 
                                                  list.files(x))], 
              gsub(basename(x), "raw_results.csv", x))
  })
  
  # Delete the NM_run directories
  unlink(modelfitDirs, recursive=TRUE)
  
  # Set the working directory back
  setwd(userWD)
  
  # Return the full raw results data frame
  return(allData)
}
