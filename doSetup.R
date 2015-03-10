
doSetup <- function(modelFileName){
  
  # Get the model file name without extension for various uses
  modelFileNameNoExt <- sub("\\.[[:alnum:]]+$", "", basename(as.character(modelFileName)))
  
  # Create a directory to do everything in
  dirName <- paste0("massPrecond", "_", format(Sys.time(), "%y%m%d_%H%M%S"))

  dir.create(dirName)

  # Parse the data file name from the model file
  modelFile <- readLines(modelFileName)

  # Find the data row
  dataRowNum <- grep('^\\$DATA', modelFile)

  # Pick out that line and remove the $DATA and any space after it
  dataRow <- modelFile[dataRowNum]
  dataRow <- gsub("^\\$DATA+\\s+", "", dataRow)

  # This is maybe a bit dangerous... picks the first 
  # word (after $DATA is removed above) as the file name
  dataFileName <- strsplit(dataRow, " ")[[1]][1]

  # Puts together a vector of files to copy
  filesToCopy <- list.files(pattern=eval(modelFileNameNoExt))
  filesToCopy <- c(filesToCopy, dataFileName)

  #Copy all the relevant files to the new dir
  file.copy(filesToCopy, dirName)
  
  # Set working directory to the new folder
  setwd(dirName)
  
  return(modelFileNameNoExt)
}
