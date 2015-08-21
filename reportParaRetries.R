
reportIllcond <- function(rawResults){
  
  
  theme_set(theme_bw(base_size = 45))
  
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
  
  meanOFV1 <- mean(subset(rawResultsNoNA, group==1)$ofv)
  meanOFV2 <- mean(subset(rawResultsNoNA, group==2)$ofv)
  meanOFV3 <- mean(subset(rawResultsNoNA, group==3)$ofv)
  meanOFV4 <- mean(subset(rawResultsNoNA, group==4)$ofv)
  meanOFV5 <- mean(subset(rawResultsNoNA, group==5)$ofv)

  meanInitOFV1 <- mean(subset(rawResultsNoNA, group==1)$initOFV)
  meanInitOFV2 <- mean(subset(rawResultsNoNA, group==2)$initOFV)
  meanInitOFV3 <- mean(subset(rawResultsNoNA, group==3)$initOFV)
  meanInitOFV4 <- mean(subset(rawResultsNoNA, group==4)$initOFV)
  meanInitOFV5 <- mean(subset(rawResultsNoNA, group==5)$initOFV)
  
  message1 <- paste0("Group 1: N=", n1, ", mean OFV=", meanOFV1, ", mean initial OFV=", meanInitOFV1)
  message2 <- paste0("Group 2: N=", n2, ", mean OFV=", meanOFV2, ", mean initial OFV=", meanInitOFV2)
  message3 <- paste0("Group 3: N=", n3, ", mean OFV=", meanOFV3, ", mean initial OFV=", meanInitOFV3)
  message4 <- paste0("Group 4: N=", n4, ", mean OFV=", meanOFV4, ", mean initial OFV=", meanInitOFV4)
  message5 <- paste0("Group 5: N=", n5, ", mean OFV=", meanOFV5, ", mean initial OFV=", meanInitOFV5)
  
  print(message1)
  print(message2)
  print(message3)
  print(message4)
  print(message5)
  
  
  
  rawResultsNoNA$outcome <- 
    factor(rawResultsNoNA$group, 
           levels = 1:4, labels=(c(paste0("Minimization & Covariance Step Success\nN=", n1),
                                   paste0("Minimization Success Only\nN=", n2), 
                                   paste0("Covariance Step Success Only\nN=", n3), 
                                   paste0("No Minimization Nor Covariance Success\nN=", n4))))
  
  
  
  # Some basic definitions
  dataForBackground <- rawResultsNoNA[, c('model', 'ofv')]
  
 
  # Store the plots in objects and then print them. It seems I have to use 
  # print() on them when running on the cluster.
  
  # Final OFV vs Theoretical Condition Number
  OFVVsModNum2 <- ggplot()+
    geom_point(aes(x=model, y=ofv), size = 5.5, alpha = 0.2, 
               col = 'darkgrey', data = dataForBackground)+
    geom_point(aes(x=model, y=ofv), size=5.5, alpha = 0.15, 
               col = 'red', data = rawResultsNoNA)+
    #facet_wrap(~ outcome)+
    facet_grid(~ outcome) +
    geom_abline(intercept = minOFV, slope = 0, data = rawResultsNoNA)+
    coord_cartesian(ylim = ofvLims)+
    scale_x_continuous(name="Model number")+
    scale_y_continuous(name="Final OFV")
  
  
  OFVVsInitOFV <- ggplot()+
    geom_point(aes(x=initOFVs, y=ofv), size = 5.5, alpha = 0.2, 
               col = 'darkgrey', data = dataForBackground)+
    geom_point(aes(x=initOFVs, y=ofv), size=5.5, alpha = 0.15, 
               col = 'red', data = rawResultsNoNA)+
    facet_grid(~ outcome) +
    #facet_wrap(~ outcome) +
    geom_abline(intercept = minOFV, slope = 0, data = rawResultsNoNA)+
    coord_cartesian(xlim = initOfvLims, ylim = ofvLims)+
    scale_x_continuous(name="Initial OFV")+
    #scale_x_log10(name="Initial OFV")+
    scale_y_continuous(name="Final OFV")
  
  # Initial OFV Boxplot
  initOFVBoxplot <- ggplot(rawResultsNoNA, aes(x=group, y=initOFVs, fill=as.character(rawResultsNoNA$group)))+
    geom_boxplot()+
    coord_cartesian(ylim = c(100, 1e6))+
    scale_y_log10(name = "Initial OFV")+
    scale_fill_discrete(name="Outcome",
                        breaks=c("1", "2", "3", "4"),
                        labels=c(paste0("Min+Cov\nN=", n1),
                                 paste0("Min Only\nN=", n2), 
                                 paste0("Cov Only\nN=", n3), 
                                 paste0("Neither\nN=", n4)))
  
  # I put them all in a list that I can output later
  graphList <- list(OFVVsTheorCondNums, OFVVsInitOFV, rMatCondNumsVsTheorCondNums, 
                    ofvBoxplot, theorCondNumsBoxplot, nmCondNumsBoxplot,
                    initOFVBoxplot, NMCorCondNumVsTheorCondNums)
  
  graphListNames <- c("OFVVsTheorCondNums", "OFVVsInitOFV", "rMatCondNumsVsTheorCondNums", 
                      "ofvBoxplot", "theorCondNumsBoxplot", "nmCondNumsBoxplot",
                      "initOFVBoxplot", "NMCorCondNumVsTheorCondNums")
  
  sapply(seq_along(graphList), function(x){
    
    png(paste0('./Plots/', graphListNames[x],"_big_text2.png"),
        height=800, width=3000)
    
    print(graphList[x])
      
    dev.off()
        
  })
  
}