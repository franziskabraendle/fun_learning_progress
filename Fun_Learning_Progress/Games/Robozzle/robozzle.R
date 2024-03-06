
#house keeping
rm(list=ls())

#required libraries
library(readr)
library(plyr)
library(ggplot2)
library(lme4)
library(binr)
library(MASS)
library(cowplot)
library(ggExtra)

#confidence interval function
ci<-function(x){1.96*sd(x)/sqrt(length(x))}


#Clean Data set
d<-read.csv("robozzle_data.csv", head=TRUE, sep=",")
length(d$Id)
d$Solved.<-NULL
d<-na.omit(d)
length(d$Id)
d<-subset(d, Difficulty!="?")
length(d$Id)
d$DifficultyNumeric<-as.numeric(d$Difficulty)

d$RatingsCount<-d$Liked+(d$Disliked*-1)
sum(d$RatingsCount)
d<-subset(d, RatingsCount!=0)
length(d$Id)
d$ProbLiked<-d$Liked/d$RatingsCount

hist(d$ProbLiked)
summary(d$RatingsCount)
sum(is.na(d$Liked))

d$DifficultyNumeric_scaled <- scale(d$DifficultyNumeric)

regression <- lm(ProbLiked ~ poly(DifficultyNumeric_scaled,2, raw=TRUE), data=d)
summary(regression)

#function to break into 20 Percentiles
ApplyPercentiles20labels <- function(x) {
  breaks <- quantile(x, probs = seq(0, 1, by = 0.05), na.rm = TRUE)
  labels <- as.character(round(breaks[-length(breaks)],1))
  cut(x, breaks = breaks, labels = labels, include.lowest = TRUE)
}

#Add the Percentiles to the dataframe
d$Percentiles <- ApplyPercentiles20labels(d$DifficultyNumeric)

h1<-hist(d$Like.Dislike)
box() 
h1


dd2<-ddply(d, ~Percentiles, summarize,  m=mean(ProbLiked,na.rm=T), ci=ci(ProbLiked))

#plotting it
p1<-ggplot(dd2, aes(x=Percentiles, y=m, group=1)) +
  #minimal theme
  theme_classic()+
  #error bars with MPI colors
  geom_errorbar(width=0, aes(ymin=m-ci, ymax=m+ci), col="#007268", size=1) +
  #line with MPI colors
  geom_line(col="#007268", size=1) +
  #black points
  geom_point(size=2, fill="black", col="black") +
  #axes labels
  xlab("Difficulty (Percentiles)")+ylab("Average like rate")+
  scale_x_discrete(breaks = c(1, 1.6, 2, 2.6, 3, 3.4, 4))+
  theme(text = element_text(size=18,  family="sans"))

p1

#svg("plot_Robozzle.svg", width=4.5, height=3.5)
#plot_grid(p1, nrow = 1)
#dev.off()


#plot raw data for SI
p2<- ggplot(d, aes(x=DifficultyNumeric, y=ProbLiked)) +
  theme_classic()+
  geom_point(alpha = 0.5, stroke = 0, shape=16, size=2) +
  theme(legend.position="none") + 
  theme(text = element_text(size=18,  family="sans")) +
  xlab("Difficulty")+ylab("Average like rate")
p2

p3 <- ggMarginal(p2, type="histogram", size=3, fill = "#007268")
p3

#svg("plot_Robozzle_raw.svg", width=4.5, height=3.5)
#plot_grid(p3, nrow = 1)
#dev.off()

#plot difficulty histogram
p4<- ggplot(d, aes(x=DifficultyNumeric)) +
  geom_histogram(col="black", fill="#007268", binwidth=0.5)+
  theme_classic()+
  theme(text = element_text(size=18,  family="sans")) +
  xlab("Difficulty")+ylab("Count")
p4

#svg("plot_Robozzle_hist.svg", width=4.5, height=3.5)
#plot_grid(p4, nrow = 1)
#dev.off()
