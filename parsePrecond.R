
parsePrecond <- function(precondDir, modelFileNameNoExt){
  
  # Paste together the expected file paths
  rawresFilePath <- paste0(precondDir, "/modelfit_dir1/raw_results.csv")
  extFilePath <- paste0(precondDir, "/m1/", modelFileNameNoExt, 
                        "_repara.ext")
  
  # Check wether the relevant folder and files exist
  dirExists <- file.exists(precondDir)
  rawresExists <- file.exists(rawresFilePath)
  extExists <- file.exists(extFilePath)
  
  # For the case where all exist
  if(dirExists & rawresExists & extExists){
    
    # Parse rawres
    rawres <- read.csv(rawresFilePath)
    
    # Parse initial OFV from .ext file
    extFile <- read.table(extFilePath, header=FALSE, sep=" ", skip=2, nrows=1)
    initOFV <- as.numeric(extFile[length(extFile)])
    
    # Put them together and return
    row <- c(precondDir, initOFV, rawres)
    names(row) <- c("runDirs", "initOFV", names(rawres))
    print(paste("Parsing", precondDir))
    return(row)
    
    
  }
  
  if(dirExists & rawresExists){
    
    # Parse rawres
    rawres <- read.csv(rawresFilePath)
    
    # Put them together and return
    row <- c(precondDir, "NA", rawres)
    names(row) <- c("runDirs", "initOFV", names(rawres))
    print(paste("Parsing", precondDir))
    return(row)
  }
  
  # If neither of those cases are true I just return NULL
  print(paste("Parsing", precondDir, "failed as the rawres file wasn't found"))
  return(NULL)
}