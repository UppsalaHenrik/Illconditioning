

rawres <- read.csv("rawres_subset.csv", stringsAsFactors=FALSE, )

head(rawres)

rawresNoNA <- subset(rawres, ofv != 'NA')

plot(rawresNoNA$teorCondNums, rawresNoNA$ofv, log="x")


rawresNoNA$group <- ifelse(rawresNoNA$minimization_successful == 1 & rawresNoNA$covariance_step_successful == 1, 1,
                       ifelse(rawresNoNA$minimization_successful == 1 & rawresNoNA$covariance_step_successful == 0, 2,
                              ifelse(rawresNoNA$minimization_successful == 0 & rawresNoNA$covariance_step_successful == 1, 3,
                                     ifelse(rawresNoNA$minimization_successful == 0 & rawresNoNA$covariance_step_successful == 0, 4,
                                            5))))

rawresNoNA$group2 <- ifelse(rawresNoNA$minimization_successful == 1 & rawresNoNA$covariance_step_successful == 0, TRUE, FALSE)

nrow(subset(rawresNoNA, group==1))
nrow(subset(rawresNoNA, group==2))
nrow(subset(rawresNoNA, group==3))
nrow(subset(rawresNoNA, group==4))
nrow(subset(rawresNoNA, group==5))

mean1 <- mean(rawresNoNA[minimization_successful==1&covariance_step_successful==1, "ofv"])
mean2 <- mean(rawresNoNA[minimization_successful==1&covariance_step_successful==0, "ofv"])
mean3 <- mean(rawresNoNA[minimization_successful==0&covariance_step_successful==1, "ofv"])
mean4 <- mean(rawresNoNA[minimization_successful==0&covariance_step_successful==0, "ofv"])

sd1 <- sd(rawresNoNA[minimization_successful==1&covariance_step_successful==1, "ofv"])
sd2 <- sd(rawresNoNA[minimization_successful==1&covariance_step_successful==0, "ofv"])
sd3 <- sd(rawresNoNA[minimization_successful==0&covariance_step_successful==1, "ofv"])
sd4 <- sd(rawresNoNA[minimization_successful==0&covariance_step_successful==0, "ofv"])

attach(rawresNoNA)

aovObj <- aov(ofv~minimization_successful*covariance_step_successful*teorCondNums)
summary(aovObj)


t.test(rawresNoNA[rawresNoNA$group==1,"ofv"], rawresNoNA[rawresNoNA$group==2,"ofv"])




require(ggplot2)

ggplot(rawresNoNA, aes(x=group, y=ofv, fill=as.character(rawresNoNA$group)))+
  geom_boxplot()+
  scale_fill_discrete(name="Outcome",
                      breaks=c("1", "2", "3", "4"),
                      labels=c("Min+Cov\nN=7997", "Min Only\nN=116", "Cov Only\nN=1167", "Neither\nN=369"))

ggplot(rawresNoNA, aes(x=group, y=teorCondNums, fill=as.character(rawresNoNA$group)))+
  geom_boxplot()+
  scale_y_log10()+
  scale_fill_discrete(name="Outcome",
                      breaks=c("1", "2", "3", "4"),
                      labels=c("Min+Cov\nN=7997", "Min Only\nN=116", "Cov Only\nN=1167", "Neither\nN=369"))


ggplot(rawresNoNA, aes(x=teorCondNums, y=ofv))+
  geom_point(shape=group, size=3, colour="black", fill=as.character(group))+
  scale_x_log10()

