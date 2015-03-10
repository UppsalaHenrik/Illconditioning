


runPrecond <- function(modelFileName,modelFileNameNoExt, illCondFileName, 
                       runNum, pertSize, precondScriptPath, wait=FALSE, pertSeed){
  
  # Create a dir name to use
  dirNameString <- paste0(gsub(".csv$", "", illCondFileName), "_run_")
  dirName <- paste0(dirNameString, formatC(runNum, digits=0, width=3, format="f", flag="0"))
  
  # Create the command
  cmd <- paste0("srun perl ", precondScriptPath, " ", modelFileName, " -dir=", dirName,
                " -pre=", illCondFileName, " -cholesky -pertSize=", pertSize,
                " -clean=2 -seedForPert=", pertSeed)
  
  print(cmd)
  
  # Run the command
  system(cmd, intern=wait, wait=wait)
  
  return(dirName)
}






