
parsePrecond <- function(precondDir, modelFileNameNoExt){
  
  print(paste("Parsing", precondDir))
  
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
    
    # Parse .ext file and get initial OFV and condition number of corr matrix
    extFileDF <- parseExtFile(extFilePath)
    initOFV <- getInitOFV(extFileDF)
    NMCondNum <- calcNMCondNum(extFileDF)
    
    # Put them together and return
    row <- c(precondDir, NMCondNum, initOFV, rawres)
    names(row) <- c("runDirs", "NMCondNums", "initOFVs", names(rawres))
    
    return(row)
  }
  
  if(dirExists & rawresExists){
    
    # Parse rawres
    rawres <- read.csv(rawresFilePath)
    
    # Put them together and return with NAs for the ext file information
    row <- c(precondDir, "NA", "NA", rawres)
    names(row) <- c("runDirs", "NMCondNums", "initOFVs", names(rawres))
    
    return(row)
  }
  
  # If neither of those cases are true I just return NULL
  print(paste("Parsing", precondDir, "failed as the raw results file wasn't found"))
  return(NULL)
}