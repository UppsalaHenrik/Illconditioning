
doSetup <- function(modelFileName){
  
  # Get the model file name without extension for various uses
  modelFileNameNoExt <- sub("\\.[[:alnum:]]+$", "", basename(as.character(modelFileName)))
  
  covFileName <- paste0(modelFileNameNoExt, ".cov")
  
  # Create a directory to do everything in
  dirName <- paste0("massPrecond", "_", format(Sys.time(), "%y%m%d_%H%M%S"))

  dir.create(dirName)

  # Parse the data file name from the model file
  modelFile <- readLines(modelFileName)

  # Find the data row
  dataRowNum <- grep('^\\$DATA', modelFile)

  # Pick out that line and remove the $DATA and any space after it
  dataRow <- modelFile[dataRowNum]
  cutDataRow <- gsub("^\\$DATA+\\s+", "", dataRow)

  # This is maybe a bit dangerous... picks the first 
  # word (after $DATA is removed above) as the file name
  dataFileName <- strsplit(cutDataRow, " ")[[1]][1]

  # Puts together a vector of files to copy
  filesToCopy <- list.files(pattern=eval(modelFileNameNoExt))
  filesToCopy <- c(filesToCopy, dataFileName)

  #Copy all the relevant files to the new dir
  file.copy(filesToCopy, dirName)
  
  # Set working directory to the new folder
  setwd(dirName)
  
  # If the model isn't already a _repara model, run the model in precond
  if((length(grep("_repara", modelFileNameNoExt)) == 0)){
    
    # We have all we need to build the command and run it
    # I'm using the unhacked precond version of the precond hacked PsN version :)
    
    cmd <- paste0("perl C:/Users/hnyberg/Documents/PsN4/PsN-Source/bin/precond ", 
                  modelFileName, " -pre=", covFileName," -cholesky -dir=org_mod_precond")
    
    system(cmd, wait=TRUE)
      
    # copy back the files
    reparaFiles <- list.files("./org_mod_precond/m1", full.names=TRUE)
    file.copy(reparaFiles, "./")
    
    # Edit the data line
    reparaModelFileName <- list.files()[grep("repara.mod", list.files())]
    reparaModelFile <- readLines(reparaModelFileName)
    
    # Replace $DATA statement with the original one that points to correct data location
    reparaModelFile[grep('^\\$DATA', reparaModelFile)] <- dataRow
    
    # Write out the file
    fileConn <- file(reparaModelFileName)
    writeLines(reparaModelFile, fileConn)
    close(fileConn)

    # Get the model name without extension
    reparaModelFileNameNoExt <- sub("\\.[[:alnum:]]+$", "", 
                                    basename(as.character(reparaModelFileName)))
  }
  else{
    reparaModelFileNameNoExt <- modelFileNameNoExt
  }
  
  return(reparaModelFileNameNoExt)
}
