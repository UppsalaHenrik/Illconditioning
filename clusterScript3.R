
########## Source the functions

# File system setup function
source("./../Rfunctions/fileSysSetup.R", echo=FALSE)

# illconditioning setup function
source("./../Rfunctions/illCondSetup.R", echo=FALSE)

# Precond run function
source("./../Rfunctions/runPrecond.R", echo=FALSE)

# Workflow wrapper function
source("./../Rfunctions/workflowRand.R", echo=FALSE)


########## Set off commands

rawres <- runMassPrecond(modelFileName="Warf2comp1_repara.mod", 
                         maxMag=15, reps=10, pertSize=0.2)
