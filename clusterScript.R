
########## Source the functions

# Setup function
source("./../Rfunctions/doSetup.R", echo=FALSE)

# Random var-covar matrix generation function
source("./../Rfunctions/illCondRand.R", echo=FALSE)

# Random var-covar matrix generation function
source("./../Rfunctions/prepRunsToRun.R", echo=FALSE)

# Precond run function
source("./../Rfunctions/runPrecond.R", echo=FALSE)

# Workflow wrapper function
source("./../Rfunctions/workflowRand.R", echo=FALSE)


########## Set off commands

rawres <- runMassPrecond("Warf2comp1_repara.mod", c(1,2,4,6,8,10,12,14,16,18), runsPerMag=100, pertSize=0.2, 
                         precondScriptPath="./../../_hackedPsN/PsN4_3/bin/precond_numStab")

