
reportIllcond <- function(rawResults){
  
  
  theme_set(theme_bw(base_size = 30))
  
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
           levels = 1:4, labels=(c(paste0("Minimization and Covariance Step Successful\nN=", n1),
                                   paste0("Minimization Successful Only\nN=", n2), 
                                   paste0("Covariance Step Successful Only\nN=", n3), 
                                   paste0("Neither Minimization Nor Covariance Step Successful\nN=", n4))))
  
  
  
  # Some basic definitions
  dataForBackground <- rawResultsNoNA[, c('theorCondNums','rMatCondNums', 'ofv', 'initOFVs', 'NMCondNums')]
  
  # Determine OFV axis limits and minimum for graphs
  ofvWhiskerLims <- boxplot.stats(rawResultsNoNA$ofv)$stats[c(1, 5)]
  lowLimOFV <- floor(ofvWhiskerLims[1]/100)*100-50
  highLimOFV <- ceiling(ofvWhiskerLims[2]/100)*200
  ofvLims <- c(lowLimOFV, highLimOFV)
  minOFV <- min(rawResultsNoNA$ofv)
  
  # Determine theorCondNums axis limits for graphs
  highLimTheorCondNums <- quantile(rawResultsNoNA$theorCondNums, 
                                   probs = 0.99, na.rm = TRUE)*100
  theorCondNumLims <- c(1e-3, highLimTheorCondNums)
  theorCondNumBoxplotLims <- c(0.001, 100000*boxplot.stats(rawResultsNoNA$theorCondNums)$stats[5])
  
  # Determine rMatCondNums axis limits for graphs  
  highLimRMatCondNums <- quantile(rawResultsNoNA$rMatCondNums, 
                                  probs = 0.99, na.rm = TRUE)*10000
  rMatCondNumLims <- c(0.1, highLimRMatCondNums)
  rMatCondNumBoxplotLims <- c(0.001, 100000*boxplot.stats(rawResultsNoNA$theorCondNums)$stats[5])
  
  
  # Determine breaks for condition numbers. 10^5 is nice
  condNumsBreaks <- 10^seq(-10, 100, by = 5)
  
  # Store the plots in objects and then print them. It seems I have to use 
  # print() on them when running on the cluster.
  
  # Final OFV vs Theoretical Condition Number
  OFVVsTheorCondNums <- ggplot()+
    geom_point(aes(x=theorCondNums, y=ofv), size = 5.5, alpha = 0.2, 
               col = 'darkgrey', data = dataForBackground)+
    geom_point(aes(x=theorCondNums, y=ofv), size=5.5, alpha = 0.15, 
               col = 'red', data = rawResultsNoNA)+
    facet_wrap(~ outcome)+
    geom_abline(intercept = minOFV, slope = 0, data = rawResultsNoNA)+
    coord_cartesian(xlim = theorCondNumLims, ylim = ofvLims)+
    scale_x_log10(name="Theoretical Condition Number of R Matrix", 
                  breaks = condNumsBreaks)+
    scale_y_continuous(name="Final OFV")
  
  # NM corr matrix condition number vs Theoretical Condition Number
  NMCorCondNumVsTheorCondNums <- ggplot()+
    geom_point(aes(x=theorCondNums, y=NMCondNums), size = 5.5, alpha = 0.2, 
               col = 'darkgrey', data = dataForBackground)+
    geom_point(aes(x=theorCondNums, y=NMCondNums), size=5.5, alpha = 0.15, 
               col = 'red', data = rawResultsNoNA)+
    facet_wrap(~ outcome)+
    coord_cartesian(xlim = theorCondNumLims, ylim = c(-100,2500))+
    scale_x_log10(name="Theoretical Condition Number of R Matrix", 
                  breaks = condNumsBreaks)+
    scale_y_continuous(name="Final OFV")
  
  # Condition number of R matrix from NONMEM vs theoretical R matrix condition number 
  rMatCondNumsVsTheorCondNums <- ggplot()+
    geom_point(aes(x=theorCondNums, y=rMatCondNums), size=5.5, alpha = 0.2, 
               col = 'darkgrey', data = dataForBackground)+
    geom_point(aes(x=theorCondNums, y=rMatCondNums), size=5.5, alpha = 0.15, 
               col = 'red', data = rawResultsNoNA)+
    facet_wrap(~ outcome)+
    coord_cartesian(xlim = theorCondNumLims, ylim = rMatCondNumLims)+
    scale_x_log10(name="Theoretical Condition Number of R Matrix", 
                  breaks = condNumsBreaks)+
    scale_y_log10(name="Condition Number of NONMEM R Matrix", 
                  breaks = condNumsBreaks)+
    geom_abline(data = rawResultsNoNA, stat = "abline", position = "identity")
  
  # OFV boxplot
  ofvBoxplot <- ggplot(rawResultsNoNA, aes(x=group, y=ofv, fill=as.character(rawResultsNoNA$group)))+
    geom_boxplot()+
    coord_cartesian(ylim = ofvLims)+
    geom_abline(intercept = minOFV, slope = 0, size = 0.2, data = rawResultsNoNA)+
    scale_fill_discrete(name="Outcome",
                        breaks=c("1", "2", "3", "4"),
                        labels=c(paste0("Min+Cov\nN=", n1),
                                 paste0("Min Only\nN=", n2), 
                                 paste0("Cov Only\nN=", n3), 
                                 paste0("Neither\nN=", n4)))
  
  # theorCondNums boxplot
  theorCondNumsBoxplot <- ggplot(rawResultsNoNA, aes(x=group, y=theorCondNums, fill=as.character(rawResultsNoNA$group)))+
    geom_boxplot()+
    coord_cartesian(ylim = theorCondNumBoxplotLims)+
    scale_y_log10(name="Theoretical Condition Number of R Matrix", 
                  breaks = condNumsBreaks)+
    scale_fill_discrete(name="Outcome",
                        breaks=c("1", "2", "3", "4"),
                        labels=c(paste0("Min+Cov\nN=", n1),
                                 paste0("Min Only\nN=", n2), 
                                 paste0("Cov Only\nN=", n3), 
                                 paste0("Neither\nN=", n4)))
  
  # nmCondNums boxplot
#  nmCondNumsBoxplot <- ggplot(rawResultsNoNA, aes(x=group, y=NMCondNums, fill=as.character(rawResultsNoNA$group)))+
#    geom_boxplot()+
#    coord_cartesian(ylim = theorCondNumBoxplotLims)+
#    scale_y_log10(name="NONMEM Condition Number of Correlation Matrix", 
#                  breaks = condNumsBreaks)+
#    scale_fill_discrete(name="Outcome",
#                        breaks=c("1", "2", "3", "4"),
#                        labels=c(paste0("Min+Cov\nN=", n1),
#                                 paste0("Min Only\nN=", n2), 
#                                 paste0("Cov Only\nN=", n3), 
#                                 paste0("Neither\nN=", n4)))
  
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
  graphList <- list(OFVVsTheorCondNums, rMatCondNumsVsTheorCondNums, 
                    ofvBoxplot, theorCondNumsBoxplot, nmCondNumsBoxplot,
                    initOFVBoxplot)
  graphListNames <- c("OFVVsTheorCondNums", "rMatCondNumsVsTheorCondNums", 
                      "ofvBoxplot", "theorCondNumsBoxplot", "nmCondNumsBoxplot",
                      "initOFVBoxplot")
  
  sapply(seq_along(graphList), function(x){
    
    png(paste0('./Plots/', graphListNames[x],"_highres.png"),
        height=1500, width=1500)
    
    print(graphList[x])
      
    dev.off()
        
  })
  
}