library(ggplot2)
library(plyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(cowplot)
library(ggeffects)
library(patchwork)

#house keeping
rm(list=ls())

st_error<-function(x){sd(x)/sqrt(length(x))}

ApplyPercentiles20labels <- function(x) {
  breaks <- quantile(x, probs = seq(0, 1, by = 0.01), names = FALSE)
  labels <- breaks[-1]
  cut(x, breaks = breaks, labels = labels, include.lowest = TRUE)
}

frame <- readRDS(file="guessinggame.Rda")

#use this for different version of data cleaning
#frame <- readRDS(file="guessinggame_excluding_few_guesses_or_machines.Rda")

small_frame <- ddply(frame, c("participant", "machines"), summarize, engagement = rounds[1], variance = variance[1], variance_mapped = variance_mapped[1])

numberTrials <- ddply(small_frame, .(participant), summarize, numbertrials=sum(engagement))
NumberOfTrials <- numberTrials$numbertrials
hist(NumberOfTrials)
mean(NumberOfTrials)
sd(NumberOfTrials)

numberMachines <-ddply(small_frame, .(participant), summarize, numbermachines=max(machines))
NumberOfMachines <- numberMachines$numbermachines
hist(NumberOfMachines)
mean(NumberOfMachines)
sd(NumberOfMachines)

AvgTrialsPerMachine <- ddply(small_frame, .(participant), summarize, avgtrialspermachine=mean(engagement))
avgtrialspermachine <- AvgTrialsPerMachine$avgtrialspermachine
hist(avgtrialspermachine)
mean(avgtrialspermachine)
sd(avgtrialspermachine)

#summarize data for plotting
meansperpart <- ddply(small_frame, .(participant, variance), summarize, eng=mean(engagement))
totalmeans <-ddply(meansperpart, ~variance, summarize, engagement=mean(eng), se=st_error(eng))

# Define custom breaks and labels for the x-axis
custom_breaks <- c(0.1, 1, 10, 100, 1000)
custom_labels <- c("0.1", "1", "10", "100", "1000")

options(scipen = 10)
engagement_plot<- ggplot(totalmeans, aes(x=variance, y=engagement)) + 
  #error bars
  geom_errorbar(aes(ymin=engagement-se, ymax=engagement+se), width=.01, col="#007268", size = 1)+
  #a line
  geom_line(col="#007268", size = 1) +
  #points
  geom_point(col="black", size = 2)+
  #classic theme 
  theme_classic() +
  scale_x_continuous(
    breaks = custom_breaks,  # Specify custom breaks
    labels = custom_labels,  # Specify custom labels
    trans = "log10"          # Use log10 scale
  )+
  #axes and title
  labs(x="True Variance", y = "Number of Guesses")+
  #change fonts and their size
  theme(text = element_text(size=18,  family="sans"))

engagement_plot

#svg("Plot_Behavior.svg", width=4.5, height=3.5)
#plot_grid(engagement_plot, nrow = 1)
#dev.off()

regression_data <- small_frame

mean(regression_data$engagement)
var(regression_data$engagement)
regression_data$variance_mapped_scaled <- scale(regression_data$variance_mapped)

#main
engagementregression <- glmer.nb(engagement ~ poly(variance_mapped, degree=2, raw=TRUE) + (1|participant), data=regression_data)
summary(engagementregression)

#main with random slope
engagementregression2 <- glmer.nb(engagement ~ poly(variance_mapped, degree=2, raw=TRUE) + (1+poly(variance_mapped, degree=2, raw=TRUE)|participant), data=regression_data)
summary(engagementregression2)

#scaled
engagementregression3 <- glmer.nb(engagement ~ poly(variance_mapped_scaled, degree=2, raw=TRUE) + (1|participant), data=regression_data)
summary(engagementregression3)

#scaled with random slope - fails to converge
engagementregression4 <- glmer.nb(engagement ~ poly(variance_mapped_scaled, degree=2, raw=TRUE) + (1+poly(variance_mapped_scaled, degree=2, raw=TRUE)|participant), data=regression_data)


engreg<-data.frame(muengreg=as.numeric(summary(engagementregression)$coef[2:3,1]),
                   seengreg=as.numeric(1.96*summary(engagementregression)$coef[2:3,2]),
                   estimate=c("True Variance", "True Variance\nsquared"))
engreg$estimate<-factor(engreg$estimate, levels = c("True Variance", "True Variance\nsquared"))


engregestimates <- ggplot(engreg, aes(y=muengreg, x=estimate)) +
  #summary plots
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", fill="#007268", width=0.8) + 
  #points
  geom_point()+
  #error bars
  geom_errorbar(aes(ymin=muengreg-seengreg, ymax=muengreg+seengreg),color='black', width = .4, position=position_dodge((width=0.9))) +
  #axes labels
  xlab("Estimate")+
  ylab(expression(beta))+ 
  #theme
  theme_classic()+
  #y=0 line
  geom_hline(yintercept = 0)+
  #changed colors
  scale_fill_manual(values=col)+
  #font size
  theme(text = element_text(size=18,  family="sans"))

engregestimates

#svg("Plot_Regression.svg", width=4.5, height=3.5)
#plot_grid(engregestimates, nrow = 1)
#dev.off()

#analysis of individual participants

indiv_coefs_engagement <- coef(engagementregression2)$participant
nrow(indiv_coefs_engagement) #(98)
colnames(indiv_coefs_engagement) <- c("intercept","vari1","vari2")
#How many show an inverted U-shape?
length(which(indiv_coefs_engagement$vari2<0)) #(76)
#How many have these show a peak in the range of variances we look at?
indiv_coefs_engagement$peak <- - (indiv_coefs_engagement$vari1 / (2* indiv_coefs_engagement$vari2))
length(which(indiv_coefs_engagement$peak >-1 & indiv_coefs_engagement$peak<3 & indiv_coefs_engagement$vari2<0)) #(64)


#guess update analysis

frame_gue <- frame

#add column, with difference in guesses: 
for (j in 1:nrow(frame_gue)) {
  if(j == 1){
    frame_gue$guessupdate[j] <- NA
  }else{
    if(frame_gue$trials[j]-frame_gue$trials[j-1] == 1){
      frame_gue$guessupdate[j] <- frame_gue$guess[j] - frame_gue$guess[j-1]
    }else{
      frame_gue$guessupdate[j] <- NA
    }
  }
}

#prepare for regression
frame_gue <- frame_gue[!is.na(frame_gue$guessupdate), ]
frame_gue$guessupdate_abs <- abs(frame_gue$guessupdate)
frame_gue$guessupdate_abs_scaled <- scale(frame_gue$guessupdate_abs)
frame_gue$t_scaled<-as.numeric(scale(frame_gue$trials))

updateregression <- lmer(guessupdate_abs_scaled ~ t_scaled+ (1+t_scaled|participant), data = frame_gue)
summary(updateregression)



#priors analysis

numberPart <- max(frame$participant)

# estimating the variance & prior variance (either weighted by number of guesses or not) from sampled values. 
frame2 <- frame
frame2$varest <- -1
frame2$priorest <- -1
frame2$priorest_weighted <- -1
for (p in 1:numberPart){
  currentrows <- frame2[frame2$participant == p, ]
  participant_vars <- vector()
  participant_trials <- vector()
  if(nrow(currentrows)!=0){
    numberMachines <- max(currentrows$machines)
    for(J in 1: (numberMachines)){
      currentmach <- frame2[frame2$participant == p & frame2$machines == J, ]
      maxtrials <- max(currentmach$trials)
      for(t in 1:maxtrials){
        currentlist <- vector()
        for (t2 in 1:t){
          currentlist <- c(currentlist, currentmach$value[currentmach$trials == t2])
        }
        frame2[frame2$participant == p & frame2$machines == J & frame2$trials == t,]$varest <- var(currentlist)
      }
      frame2[frame2$participant == p & frame2$machines == J,]$priorest <- mean(participant_vars)
      if(length(participant_vars!=0)){
        frame2[frame2$participant == p & frame2$machines == J,]$priorest_weighted <- sum(participant_vars*participant_trials)/sum(participant_trials)
      }
      participant_vars <- c(participant_vars, var(currentlist))
      participant_trials <- c(participant_trials, maxtrials)
      
    }
    
  }
}


#prepare data for regression
frame2_small <- frame2 %>%
  group_by(participant, machines) %>%
  filter(trials == max(trials))

regression_data2 <- frame2_small
regression_data2 <- regression_data2[!is.na(regression_data2$priorest), ]

regression_data2$varest[regression_data2$varest == 0] <- 0.01
regression_data2$priorest[regression_data2$priorest == 0] <- 0.01
regression_data2$priorest_weighted[regression_data2$priorest_weighted == 0] <- 0.01

regression_data2$varest_mapped <- log10(regression_data2$varest)
regression_data2$priorest_mapped <- log10(regression_data2$priorest)
regression_data2$priorest_weighted_mapped <- log10(regression_data2$priorest_weighted)

#calculate differences
regression_data2$variance_difference_est <- regression_data2$varest_mapped - regression_data2$priorest_mapped
regression_data2$variance_difference_est_weighted <- regression_data2$varest_mapped - regression_data2$priorest_weighted_mapped
regression_data2$variance_difference_est_scaled <- scale(regression_data2$variance_difference_est)
regression_data2$variance_difference_est_weighted_scaled <- scale(regression_data2$variance_difference_est_weighted)

regression_data2$machines_scaled <- scale(regression_data2$machines)

#main regression, using the scaled difference between the estimated underlying variance and the estimated expected variance weighted by number of guesses
differenceregression <- glmer.nb(trials ~ poly(variance_difference_est_weighted_scaled, 2, raw=TRUE) + machines_scaled + (1|participant), data=regression_data2)
summary(differenceregression)

#without scaling the difference
differenceregression2 <- glmer.nb(trials ~ poly(variance_difference_est_weighted, 2, raw=TRUE) + machines_scaled + (1|participant), data=regression_data2)
summary(differenceregression2)

#without weighting the estimated expected variance by the number of guesses per machine
differenceregression3 <- glmer.nb(trials ~ poly(variance_difference_est_scaled, 2, raw=TRUE) + machines_scaled + (1|participant), data=regression_data2)
summary(differenceregression3)


#Seperate version: taking the base-10 logarithm before averaging to calculate the expected variance

frame3 <- frame
frame3$varest <- -1
frame3$priorest <- -1
frame3$priorest_weighted <- -1
for (p in 1:numberPart){
  currentrows <- frame3[frame3$participant == p, ]
  participant_vars <- vector()
  participant_trials <- vector()
  if(nrow(currentrows)!=0){
    numberMachines <- max(currentrows$machines)
    for(J in 1: (numberMachines)){
      currentmach <- frame3[frame3$participant == p & frame3$machines == J, ]
      maxtrials <- max(currentmach$trials)
      for(t in 1:maxtrials){
        currentlist <- vector()
        for (t2 in 1:t){
          currentlist <- c(currentlist, currentmach$value[currentmach$trials == t2])
        }
        if(!is.na(var(currentlist))){
          if(var(currentlist)==0){
            currentvar <- log10(0.01)
          }
          else{
            currentvar <- log10(var(currentlist))
          }
        }
        else{
          currentvar <- NA
        }
        frame3[frame3$participant == p & frame3$machines == J & frame3$trials == t,]$varest <- currentvar
      }
      frame3[frame3$participant == p & frame3$machines == J,]$priorest <- mean(participant_vars)
      if(length(participant_vars!=0)){
        frame3[frame3$participant == p & frame3$machines == J,]$priorest_weighted <- sum(participant_vars*participant_trials)/sum(participant_trials)
      }
      participant_vars <- c(participant_vars, currentvar)
      participant_trials <- c(participant_trials, maxtrials)
      
    }
    
  }
}


frame3_small <- frame3 %>%
  group_by(participant, machines) %>%
  filter(trials == max(trials))

regression_data3 <- frame3_small
regression_data3 <- regression_data3[!is.na(regression_data3$priorest), ]

regression_data3$variance_difference_est <- regression_data3$varest - regression_data3$priorest
regression_data3$variance_difference_est_weighted <- regression_data3$varest - regression_data3$priorest_weighted
regression_data3$variance_difference_est_scaled <- scale(regression_data3$variance_difference_est)
regression_data3$variance_difference_est_weighted_scaled <- scale(regression_data3$variance_difference_est_weighted)

regression_data3$machines_scaled <- scale(regression_data3$machines)

differenceregression4 <- glmer.nb(trials ~ poly(variance_difference_est_weighted_scaled, 2, raw=TRUE) + machines_scaled + (1|participant), data=regression_data3)
summary(differenceregression4)


#preparing data for plotting

predictionsdifference <- ggpredict(differenceregression2, terms= c("variance_difference_est_weighted[all]"))

regression_data2$percentiles <- ApplyPercentiles20labels(regression_data2$variance_difference_est_weighted)
regression_data2$percentiles <- as.numeric(levels(regression_data2$percentiles))[regression_data2$percentiles]

summary(regression_data2$percentiles)

h1<-hist(regression_data2$percentiles)
h1

plotdata<-ddply(regression_data2, ~variance_difference_est_weighted, summarize,  m=mean(trials), se=st_error(trials))

#create inset plot
pred_plot_small <- ggplot(predictionsdifference)+
  geom_point(data = plotdata, aes(x=variance_difference_est_weighted, y=m), alpha = 0.3, stroke = 0, shape=16, size=2) + 
  geom_line(aes(x=x, y=predicted), col="#02e0ce", size = 1)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +  # error band
  ylim(2,20) +
  labs(x = "", y = "") + 
  theme_classic()+
  theme(text = element_text(size=15,  family="sans"), axis.title.x=element_blank(), axis.title.y=element_blank())
pred_plot_small

#create full plot
pred_plot <- ggplot(predictionsdifference)+
  geom_point(data = plotdata, aes(x=variance_difference_est_weighted, y=m), alpha = 0.3, stroke = 0, shape=16, size=2) + 
  geom_line(aes(x=x, y=predicted), col="#02e0ce", size = 1)+
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +  # error band
  labs(x = "Est. Variance - Est. Exp. Variance", y = "Number of Guesses") + 
  theme_classic()+
  theme(text = element_text(size=18,  family="sans")) +
  inset_element(pred_plot_small, 0.5, 0.5, 1, 1)
pred_plot

#svg("Plot_Priors.svg", width=4.5, height=3.5)
#plot_grid(pred_plot, nrow = 1)
#dev.off()
