#house keeping
rm(list=ls())

#libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(jsonlite)
library(lmerTest)
library(ggeffects)
library(cowplot)

#standard error
se<-function(x){sd(x)/sqrt(length(x))}

d<-read.csv("funsearch_data.csv")

n_distinct(d$id)

#number of clicks over all grids
numberClicks <- ddply(d, .(id), summarize, numberclicks=sum(clicks))
mean(numberClicks$numberclicks)
sd(numberClicks$numberclicks)
hist(numberClicks$numberclicks)

#number of clicks per grid
avgclickspergrid <- ddply(d, .(id), summarize, avgclicks=mean(clicks))
mean(avgclickspergrid$avgclicks)
sd(avgclickspergrid$avgclicks)
hist(avgclickspergrid$avgclicks)

#number of grids
numberGrids <-ddply(d, .(id), summarize, numbergrids=max(grid))
mean(numberGrids$numbergrids)
sd(numberGrids$numbergrids)
hist(numberGrids$numbergrids)

#bin mu
d$mu5<-round(d$mu/5)*5
#summarize data
dd_mag<-ddply(d, ~mu5, summarize,  m=mean(clicks), se=se(clicks))

#plotting
p1<-ggplot(dd_mag, aes(x=mu5, y=m, group=1)) +
  #theme
  theme_classic()+
  #error bar
  geom_errorbar(width=0, aes(ymin=m-se, ymax=m+se), col="#1d6b67", size=1) +
  #lines
  geom_line(col="#1d6b67", size=1) +
  #points
  geom_point(col="black", size = 2)+
  #axes labels
  xlab("Maximum point values")+ylab("Number of samples")+
  #font size
  theme(text = element_text(size=18,  family="sans"))
p1

svg("Plot_Behavior_Points.svg", width=4.5, height=3.5)
plot_grid(p1, nrow = 1)
dev.off()

#summarize data
dd_lam<-ddply(d, ~lambda, summarize,  m=mean(clicks), se=se(clicks))

#plotting
p2<-ggplot(dd_lam, aes(x=lambda, y=m, group=1)) +
  #theme
  theme_classic()+
  #error bar
  geom_errorbar(width=0, aes(ymin=m-se, ymax=m+se), col="#1d6b67", size=1) +
  #line
  geom_line(col="#1d6b67", size=1) +
  #points
  geom_point(col="black", size = 2)+
  #axes labels
  labs(x = expression(paste("Smoothness (", lambda, ")")), y = "Number of samples")+
  #font size
  theme(text = element_text(size=18,  family="sans"))
p2

svg("Plot_Behavior_Smoothness.svg", width=4.5, height=3.5)
plot_grid(p2, nrow = 1)
dev.off()


#Generate variables for regression analysis
d$mag_scaled<-as.numeric(scale(d$mu))
d$lambda_scaled<-as.numeric(scale(d$lambda))

mean(d$clicks)
var(d$clicks)

#regression analysis
#scaled
regression_combined <-glmer.nb(clicks~poly(lambda_scaled, degree=2, raw=TRUE)+mag_scaled+(1|id), data=d)
summary(regression_combined)

#with random slopes -> singular fit
regression_combined2 <-glmer.nb(clicks~poly(lambda_scaled, degree=2, raw=TRUE)+mag_scaled+(1+poly(lambda_scaled, degree=2, raw=TRUE)+mag_scaled|id), data=d)
summary(regression_combined3)

#with interaction
regression_combined3 <-glmer.nb(clicks~poly(lambda_scaled, degree=2, raw=TRUE)+mag_scaled+poly(lambda_scaled, degree=2, raw=TRUE)*mag_scaled+(1|id), data=d)
summary(regression_combined4)

anova(regression_combined, regression_combined3)

#get coefficients into data frame
dd2<-data.frame(mu=as.numeric(summary(regression_combined)$coef[2:4,1]),
               se=as.numeric(1.96*summary(regression_combined)$coef[2:4,2]),
               estimate=c("Smoothness", "Smoothness\nsquared", "Magnitude"))

#change orders of factors
dd2$estimate<-factor(dd2$estimate, levels = c("Smoothness", "Smoothness\nsquared", "Magnitude"))

#plot coefficients
p3 <- ggplot(dd2, aes(y=mu, x=estimate)) +
  #summary plots
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", fill="#1d6b67", width=0.8) +
  #points
  geom_point()+
  #error bars
  geom_errorbar(aes(ymin=mu-se, ymax=mu+se),color='black', width = .4, position=position_dodge((width=0.9))) +
  #axes lavewls
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
  #title
#show it
p3

svg("Plot_Regression.svg", width=4.6, height=3.5)
plot_grid(p3, nrow = 1)
dev.off()

#individual data

indiv_coefs_engagement <- coef(regression_combined2)$id
nrow(indiv_coefs_engagement) #(44)
colnames(indiv_coefs_engagement) <- c("intercept","lambda1","lambda2", "magnitude")
length(which(indiv_coefs_engagement$lambda2<0)) #(44)
length(which(indiv_coefs_engagement$magnitude>0)) #(37)
indiv_coefs_engagement$peak <- - (indiv_coefs_engagement$lambda1 / (2* indiv_coefs_engagement$lambda2))
list1 <- which(indiv_coefs_engagement$peak >-0.8186560 & indiv_coefs_engagement$peak<2.1162237 & indiv_coefs_engagement$lambda2<0) #values taken from dataframe d - it's 0.25 and 16 scaled.
length(list1)
