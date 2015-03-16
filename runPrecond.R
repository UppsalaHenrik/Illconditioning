


runPrecond <- function(modelFileName, modelFileNameNoExt, pertSize, precondScriptPath, 
                       runNum, illCondFileName, pertSeed, wait){
  
  # Create a dir name to use
  dirName <- paste0(gsub(".csv$", "", basename(illCondFileName)))
  
  # Create the command
  cmd <- paste0("perl ", precondScriptPath, " ", modelFileName, " -dir=", dirName,
                " -pre=", illCondFileName, " -cholesky -pertSize=", pertSize,
                " -clean=2 -seedForPert=", pertSeed)
  
  print(cmd)
  
  # Run the command
  system(cmd, intern=wait, wait=wait)
  
  return(dirName)
}






