
########## Source the functions

# File system setup function
source("./../Rfunctions/fileSysSetup.R", echo=FALSE)

# illconditioning setup function
source("./../Rfunctions/illCondSetup.R", echo=FALSE)

# Precond run function
source("./../Rfunctions/runPrecond.R", echo=FALSE)

# Workflow wrapper function
source("./../Rfunctions/workflowRand.R", echo=FALSE)

# Workflow wrapper function
source("./../Rfunctions/parsePrecond.R", echo=FALSE)


########## Set off commands

rawres <- runMassPrecond("run1.mod", maxMag=20, reps=10, pertSize=0.2, 
                         precondScriptPath="./../../_hackedPsN/PsN4_3/bin/precond_numStab",
                         30)
