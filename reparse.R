
  # List the runDirs
  runDirs <- list.dirs(recursive=FALSE)
  
  # Parse the rawres output from PsN precond
  allRows <- lapply(runDirs, function(x){
    
    # Put together what the file should be called
    rawresFileName <- paste0(x, "/modelfit_dir1/raw_results.csv")
    
    # Check if the rawres file exist, and if so, parse it
    if(file.exists(rawresFileName)==TRUE){
      rawres <- read.csv(rawresFileName)
      row <- c(x, rawres)
      names(row) <- c("runDirs", names(rawres))
      print(paste("Parsing", rawresFileName))
      return(row)
    }
    return(NULL)
  })
  
  # Bind together all of the rows from the list 
  allData <- do.call("rbind", allRows)
  
  rawResults <- allData

  
  # Write our the results to a raw results file
  write.csv(rawResults, file="raw_results_reparsed.csv", row.names=FALSE)
