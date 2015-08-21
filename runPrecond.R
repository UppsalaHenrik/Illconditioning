#' Run PsN script precond
#'
#' Needs to be rewritten...
#'
#' @param modelFileName The model file name to be run.
#' @param modelFileNameNoExt The model name without extension... should be refactored
#' @param pertSize A direct mapping of the pertSize option in PsN
#' @param precondScriptPath The path where the precond PsN script is found.
#' @param pertSeed A seed for the pertubation.
#'
#'
#' @author Henrik Bjugård Nyberg - henrik.b.nyberg@@farmbio.uu.se


runPrecond <- function(modelFileName, modelFileNameNoExt, pertSize, precondScriptPath,
                       runNum, illCondFileName, pertSeed){

  # Wait for the SLURM queue to have less than 100 runs in it
  waitForSlurmQ(targetLength=100, secsToWait=5, maxWaits=12)

  # Create a dir name to use
  dirName <- paste0("./illCondRuns/", gsub(".csv$", "", basename(illCondFileName)))

  # Create the command
  cmd <- paste0("srun perl ", precondScriptPath, " ", modelFileName, " -dir=", dirName,
                " -pre=", illCondFileName, " -cholesky -pertSize=", pertSize,
                " -clean=2 -seedForPert=", pertSeed)

  print(cmd)

  # Run the command
  system(cmd, intern=FALSE, wait=FALSE)

  return(dirName)
}






