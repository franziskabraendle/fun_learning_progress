library(ggplot2)
library(plyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(cowplot)
library(ggeffects)

trace(grDevices::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)

#house keeping
rm(list=ls())

st_error<-function(x){sd(x)/sqrt(length(x))}


frame <- readRDS(file="guessinggame.Rda")

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
#regression_data$variance_mapped_scaled <- scale(regression_data$variance_mapped)

mean(regression_data$engagement)
var(regression_data$engagement)

#####DECIDE FOR MAPPING / SCALING
engagementregression <- glmer(engagement ~ poly(variance_mapped, degree=2, raw=TRUE) + (1|participant), data=regression_data)
summary(engagementregression)

engreg<-data.frame(muengreg=as.numeric(summary(engagementregression)$coef[2:3,1]),
                   seengreg=as.numeric(1.96*summary(engagementregression)$coef[2:3,2]),
                   estimate=c("True\nVariance", "True\nVariance\nsquared"))
engreg$estimate<-factor(engreg$estimate, levels = c("True\nVariance", "True\nVariance\nsquared"))

engregestimates <- ggplot(engreg, aes(y=muengreg, x=estimate)) +
  #summary plots
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", fill="#007268", width=0.8) + 
  #points
  geom_point()+
  #error bars
  geom_errorbar(aes(ymin=muengreg-seengreg, ymax=muengreg+seengreg),color='black', width = .4, position=position_dodge((width=0.9))) +
  #axes labels
  xlab("Estimate")+
  ylab(expression(hat(beta)))+ 
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

frame_gue <- frame_gue[!is.na(frame_gue$guessupdate), ]

frame_gue$guessupdate_abs <- abs(frame_gue$guessupdate)

frame_gue$t_scaled<-as.numeric(scale(frame_gue$trials))

updateregression <- lmer(guessupdate_abs ~ t_scaled+ (1+t_scaled|participant), data = frame_gue)
summary(updateregression)



#priors analysis

numberPart <- max(frame$participant)

# estimating the variance from values. 
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
        frame2[frame2$participant == p & frame2$machines == J & frame2$trials == t,]$varest <- currentvar
      }
      frame2[frame2$participant == p & frame2$machines == J,]$priorest <- mean(participant_vars)
      if(length(participant_vars!=0)){
        frame2[frame2$participant == p & frame2$machines == J,]$priorest_weighted <- sum(participant_vars*participant_trials)/sum(participant_trials)
      }
      participant_vars <- c(participant_vars, currentvar)
      participant_trials <- c(participant_trials, maxtrials)
      
    }
    
  }
}



frame2_small <- frame2 %>%
  group_by(participant, machines) %>%
  filter(trials == max(trials))

regression_data2 <- frame2_small
regression_data2 <- regression_data2[!is.na(regression_data2$priorest), ]

#regression_data2$varest[regression_data2$varest == 0] <- 0.01
#regression_data2$priorest[regression_data2$priorest == 0] <- 0.01
#regression_data2$priorest_weighted[regression_data2$priorest_weighted == 0] <- 0.01

#regression_data2$varest_mapped <- log10(regression_data2$varest)
#regression_data2$priorest_mapped <- log10(regression_data2$priorest)
#regression_data2$priorest_weighted_mapped <- log10(regression_data2$priorest_weighted)

#calculate differences
#regression_data2$variance_difference_est <- regression_data2$varest_mapped - regression_data2$priorest_mapped
#regression_data2$variance_difference_est_weighted <- regression_data2$varest_mapped - regression_data2$priorest_weighted_mapped
#regression_data2$variance_difference_est_scaled <- scale(regression_data2$variance_difference_est)
#regression_data2$variance_difference_est_weighted_scaled <- scale(regression_data2$variance_difference_est_weighted)


#calculate differences
regression_data2$variance_difference_est <- regression_data2$varest - regression_data2$priorest
regression_data2$variance_difference_est_weighted <- regression_data2$varest - regression_data2$priorest_weighted
regression_data2$variance_difference_est_scaled <- scale(regression_data2$variance_difference_est)
regression_data2$variance_difference_est_weighted_scaled <- scale(regression_data2$variance_difference_est_weighted)

regression_data2$machines_scaled <- scale(regression_data2$machines)

#works with scaled or not scaled (and lmer or glmer.nb, and weighted and not weighted)
differenceregression <- glmer.nb(trials ~ poly(variance_difference_est_weighted_scaled, 2, raw=TRUE) + (1|participant), data=regression_data2) 

#(here, does not converge when using not weighted)
differenceregression <- glmer.nb(trials ~ poly(variance_difference_est_weighted_scaled, 2, raw=TRUE) + machines_scaled + (1|participant), data=regression_data2)

summary(differenceregression)

predictionsdifference <- ggpredict(differenceregression, terms= c("variance_difference_est_weighted[all]"))

plotdata<-ddply(regression_data2, ~variance_difference_est_weighted, summarize,  m=mean(trials), se=st_error(trials))

pred_plot <- ggplot(predictionsdifference)+
  geom_line(aes(x=x, y=predicted), col="#007268", size = 1)+
  #geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
  #fill = "#02e0ce", alpha = 0.5) +  # error band
  geom_point(data = plotdata,                      # adding the raw data (scaled values)
             aes(x=variance_difference_est_weighted, y=m, stroke =0), alpha = 0.5) + 
  labs(x = "Est. Variance - Est. Prior Variance", y = "Number of Guesses") + 
  scale_y_continuous(
    trans = "log2"          # Use log10 scale
  )+
  theme_classic()+
  theme(text = element_text(size=18,  family="sans"))

pred_plot

#svg("Plot_Priors.svg", width=4.5, height=3.5)
#plot_grid(pred_plot, nrow = 1)
#dev.off()