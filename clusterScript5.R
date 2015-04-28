
########## Source the functions

# File system setup function
source("./../Rfunctions/fileSysSetup.R", echo=FALSE)

# illconditioning setup function
source("./../Rfunctions/illCondSetup.R", echo=FALSE)

# Precond run function
source("./../Rfunctions/runPrecond.R", echo=FALSE)

# Workflow wrapper function
source("./../Rfunctions/workflowRand.R", echo=FALSE)

# Results parsing function
source("./../Rfunctions/parseIllcond.R", echo=FALSE)

# Reporting function
source("./../Rfunctions/reportIllcond.R", echo=FALSE)

# Util functions
source("./../Rfunctions/utils.R", echo=FALSE)

########## Set off commands

rawres <- runMassPrecond("run1.mod", maxMag=15, reps=10, pertSize=0, 
                         precondScriptPath="./../../_hackedPsN/PsN4_3/bin/precond_numStab")
