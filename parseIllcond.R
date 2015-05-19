
parseIllcond <- function(precondDir, modelFileNameNoExt){
  
  print(paste("Parsing", precondDir))
  
  # Paste together the expected file paths
  rawresFilePath <- paste0(precondDir, "/modelfit_dir1/raw_results.csv")
  extFilePath <- paste0(precondDir, "/modelfit_dir1/NM_run1/psn.ext")
  corrFilePath <- paste0(precondDir, "/modelfit_dir1/NM_run1/psn.cor")
  rMatFilePath <- paste0(precondDir, "/modelfit_dir1/NM_run1/psn.rmt")
  
  # Check wether the relevant folder and files exist
  dirExists <- file.exists(precondDir)
  rawresExists <- file.exists(rawresFilePath)
  extExists <- file.exists(extFilePath)
  corrExists <- file.exists(corrFilePath)
  rMatExists <- file.exists(rMatFilePath)
  
  # If the directory cannot be found, return NULL
  if(dirExists == FALSE){
    dirMessage <- paste("Could not find directory", precondDir)
    print(dirMessage)
    return(NULL)
  }
  
  # If the raw results file cannot be found, return NULL
  if(rawresExists == FALSE){
    rawresMessage <- paste("Could not find raw results file", precondDir)
    print(rawresMessage)
    return(NULL)
  }
  
  # Parse rawres
  rawres <- read.csv(rawresFilePath)
  
  # If the ext file exists, parse the condnum from the eigen values NONMEM reports
  if(extExists){
    
    # Parse .ext file and get initial OFV and condition number of corr matrix
    extFileDF <- parseExtFile(extFilePath)
    initOFV <- getInitOFV(extFileDF)
    NMCondNum <- calcNMCondNum(extFileDF)
  
  }else{
    
    initOFV <- NA
    NMCondNum <- NA
    print("No ext file found")
  }

  # If available, parse the correlation matrix and calculate its condition number
  if(corrExists){
    
    # Parse the correlation matrix and get the condition number
    corrMat <- parseNonmemMat(corrFilePath)
    corrMatNoZero <- stripZeroRowsCols(corrMat)[[1]]
    
    corrMatCondNum <- kappa(corrMatNoZero, exact=TRUE)
    
  }else{
    
    # If the correlation matrix was not found we just display it as NA
    corrMatCondNum <- NA
    print("No cor file found")
  }
  
  # If available, parse the R matrix and calculate its condition number  
  # For a succesfully completed run with thetas only, this should be
  # the same as the theoretical condition number of the solution
  if(rMatExists){
    
    # Parse the R matrix and get the condition number
    rMat <- parseNonmemMat(rMatFilePath)
    rMatNoZero <- stripZeroRowsCols(rMat)[[1]]
    
    rMatCondNum <- kappa(rMatNoZero, exact=TRUE)
    
  }else{
    
    # If the R matrix was not found we just display it as NA
    rMatCondNum <- NA
  }
   
  # Put them together and return
  row <- data.frame(runDirs = precondDir, NMCondNums = NMCondNum, 
                    corrMatCondNums = corrMatCondNum, rMatCondNums = rMatCondNum, 
                    initOFVs = initOFV, rawres, stringAsFactors = FALSE)
  
  return(row)
}