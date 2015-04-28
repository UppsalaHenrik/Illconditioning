
runMassPrecond <- function(modelFileName, maxMag=15, reps=1000, pertSize=0.2, 
                           precondScriptPath = "C:/Users/hnyberg/Dropbox/Doktorandsaker/PrecondProject/_hackedPsN3/PsN4_3/bin/precond_numStab"){
  
  # Get the working directory to set it back later
  userWD <- getwd()
  
  # Create a unique folder, copy all the relevant files there and set wd.
  modelFileNameNoExt <- fileSysSetup(modelFileName)
  modelFileName <- paste0(modelFileNameNoExt, ".mod")
  
  # Generate a magnitude for each rep
  magnitudes <- cbind(1:reps, sort(runif(reps, min=0, max=maxMag)))
    
  # Do the illconditioning for each magnitude
  # Create a data frame with all the runs' illCondFiles and run numbers and seeds
  runRows <- lapply(1:nrow(magnitudes), function(i){
    illCondSetup(paste0(modelFileNameNoExt, ".cov"), magnitudes[i,2], magnitudes[i,1])
  })
  
  # Bind them together
  runs <- as.data.frame(do.call("rbind", runRows))
  names(runs) <- c("reps", "illCondFiles", "magnitudes", "theorCondNums", "seeds")
  
  # Run all the runs!
  runDirs <- apply(runs, 1, function(x){
    # The way I point to the right element below is not great! This might fall over :(
    runPrecond(modelFileName, modelFileNameNoExt, pertSize, precondScriptPath, 
               as.numeric(x[1]), as.character(x[2]), as.integer(x[5]))
  })
  
  # Wait for the user's queue to be empty (slurm only) 
  waitForSlurmQ()
  
  # Bind them
  runs <- cbind(runs, runDirs)
  
  # Parse the rawres and .ext output from PsN precond
  allRows <- lapply(runDirs, function(x){
    parseIllcond(x, modelFileNameNoExt)
  })

  # Bind together all of the rows from the list 
  allData <- do.call("rbind", allRows)
  
  # Merge with the runs df and remove any pesky list issues
  rawResultsMerge <- merge(runs, allData, by="runDirs")
  rawResults <- as.data.frame(lapply(rawResultsMerge, unlist))
  
  # Bind in the run dataframe as well. Is there a risk for row mixups? 
  
  # Save the rData. Just a safety feature because of issues with csv
  save(rawResults, file = "rawres.Rdata")
  
  # Write our the results to a raw results file
  write.csv(rawResults, file=paste0("raw_results_", 
                                    basename(getwd()), 
                                    ".csv"),
            row.names=FALSE)
  
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
