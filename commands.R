




rawres <- runMassPrecond("Warf2comp1_repara.mod", c(1,5,100), 10)

rawres <- runMassPrecond("Warf2comp1_repara.mod", c(1e+80), 10)




####### OLD STUFF ####### 





illCond("C:/Users/hnyberg/Dropbox/Doktorandsaker/PrecondProject/CONTROL4Test/CONTROL4.cov", 1)

runPrecond("CONTROL4", "illcond_power_1.csv")




setwd("C:/Users/hnyberg/Dropbox/Doktorandsaker/PrecondProject/150205_baseModel/illCondition/illCondModel")
 
illcondFile <- illCond("C:/Users/hnyberg/Dropbox/Doktorandsaker/PrecondProject/150205_baseModel/Warf2comp1.cov", 2.1)

runPrecond(modelFile="Warf2comp1_repara.mod", illcondFile)
