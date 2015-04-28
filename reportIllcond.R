
reportIllcond <- function(rawResults){
  
  # Setting up folders  
  dir.create("./Plots/")
  
  # Removing NA OFV values so that they don't interfere with plots
  rawResultsNoNA <- subset(rawResults, ofv != 'NA')
  
  # Defining groups
  rawResultsNoNA$group <- ifelse(rawResultsNoNA$minimization_successful == 1 & rawResultsNoNA$covariance_step_successful == 1, 1,
                             ifelse(rawResultsNoNA$minimization_successful == 1 & rawResultsNoNA$covariance_step_successful == 0, 2,
                                    ifelse(rawResultsNoNA$minimization_successful == 0 & rawResultsNoNA$covariance_step_successful == 1, 3,
                                           ifelse(rawResultsNoNA$minimization_successful == 0 & rawResultsNoNA$covariance_step_successful == 0, 4,
                                                  5))))
  
  
  # Counting runs for each group
  
  n1 <- nrow(subset(rawResultsNoNA, group==1))
  n2 <- nrow(subset(rawResultsNoNA, group==2))
  n3 <- nrow(subset(rawResultsNoNA, group==3))
  n4 <- nrow(subset(rawResultsNoNA, group==4))
  n5 <- nrow(subset(rawResultsNoNA, group==5))
  
  # Some stats
  
#  meanOFV1 <- mean(subset(rawResultsNoNA, group==1)$ofv)
#  meanOFV1 <- mean(subset(rawResultsNoNA, group==2)$ofv)
#  meanOFV1 <- mean(subset(rawResultsNoNA, group==3)$ofv)
#  meanOFV1 <- mean(subset(rawResultsNoNA, group==4)$ofv)
#  meanOFV1 <- mean(subset(rawResultsNoNA, group==5)$ofv)
  
#  meanInitOFV1 <- mean(subset(rawResultsNoNA, group==1)$initOFV)
#  meanInitOFV1 <- mean(subset(rawResultsNoNA, group==2)$initOFV)
#  meanInitOFV1 <- mean(subset(rawResultsNoNA, group==3)$initOFV)
#  meanInitOFV1 <- mean(subset(rawResultsNoNA, group==4)$initOFV)
#  meanInitOFV1 <- mean(subset(rawResultsNoNA, group==5)$initOFV)
  
#  meanTheorCondNum1 <- mean(subset(rawResultsNoNA, group==1)$initOFV)
  
  # OFV vs theorCondNum scatter
  
  outcome <- as.factor(subset(rawResultsNoNA, group!=2)$group)
  
  png(paste0('./Plots/', "ScatterOFVvsTheorCondNum" ,".png"),
      height=600, width=1200)
  
  ggplot(subset(rawResultsNoNA, group!=2), aes(x=teorCondNums, y=ofv,
                                               colour=outcome))+
    geom_point(size=3.5)+
    geom_point(data=subset(rawResultsNoNA, group==2), 
               colour="red", shape=4, size=5)+
    scale_x_log10(name="Theoretical Condition Number of R Matrix")+
    scale_y_continuous(name="Final OFV")+
    scale_colour_brewer(palette="Set3", 
                        name="Outcome",
                        breaks=c("1", "2", "3", "4"),
                        labels=c(paste0("Min+Cov\nN=", n1),
                                 paste0("Min Only\nN=", n2), 
                                 paste0("Cov Only\nN=", n3), 
                                 paste0("Neither\nN=", n4)))
  
  dev.off()
  
  
  # OFV boxplot

  png(paste0('./Plots/', "OFVBoxplot" ,".png"),
      height=600, width=1200)
  
  ggplot(rawResultsNoNA, aes(x=group, y=ofv, fill=as.character(rawResultsNoNA$group)))+
    geom_boxplot()+
    scale_fill_discrete(name="Outcome",
                        breaks=c("1", "2", "3", "4"),
                        labels=c(paste0("Min+Cov\nN=", n1),
                                 paste0("Min Only\nN=", n2), 
                                 paste0("Cov Only\nN=", n3), 
                                 paste0("Neither\nN=", n4)))
    
  dev.off()
  
  # initOFV boxplot
  
  ylim1 = boxplot.stats(rawResultsNoNA$initOFV)$stats[c(1, 5)]
  
  png(paste0('./Plots/', "initOFVBoxplot" ,".png"),
      height=600, width=1200)
  
  ggplot(rawResultsNoNA, aes(x=group, y=initOFV, fill=as.character(rawResultsNoNA$group)))+
    geom_boxplot()+
    scale_fill_discrete(name="Outcome",
                        breaks=c("1", "2", "3", "4"),
                        labels=c(paste0("Min+Cov\nN=", n1),
                                 paste0("Min Only\nN=", n2), 
                                 paste0("Cov Only\nN=", n3), 
                                 paste0("Neither\nN=", n4)))
  
  dev.off()
  
  # theorCondNum boxplot
  
  png(paste0('./Plots/', "theorCondNumBoxplot" ,".png"),
      height=600, width=1200)
  
  ggplot(rawResultsNoNA, aes(x=group, y=teorCondNums, fill=as.character(rawResultsNoNA$group)))+
    geom_boxplot()+
    scale_fill_discrete(name="Outcome",
                        breaks=c("1", "2", "3", "4"),
                        labels=c(paste0("Min+Cov\nN=", n1),
                                 paste0("Min Only\nN=", n2), 
                                 paste0("Cov Only\nN=", n3), 
                                 paste0("Neither\nN=", n4)))
  
  dev.off()
  
  
#  # Cool 3D plot
#  library(scatterplot3d) 
#  scatterplot3d(wt,disp,mpg, pch=16, highlight.3d=TRUE,
#                type="h", main="3D Scatterplot")
  
  
}