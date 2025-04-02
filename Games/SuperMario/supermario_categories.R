#Analysis of Super Mario Data using categories. 
#Data origin: https://www.kaggle.com/datasets/leomauro/smmnet/data  by Leonardo Mauro 

#house keeping
rm(list=ls())

#required libraries
library(readr)
library(plyr)
library(ggplot2)
library(lme4)
library(cowplot)


#standard error function
se<-function(x){sd(x)/sqrt(length(x))}

ApplyPercentiles20labels <- function(x) {
  breaks <- quantile(x, probs = seq(0, 1, by = 0.05), names = FALSE)
  labels <- breaks[-1]
  cut(x, breaks = breaks, labels = labels, include.lowest = TRUE)
}

d <- readRDS(file="merged_supermario_metafile.Rda")

#as a player can only give one star per level, the likerate is calculated this way and not stars/attempts
d$likerate<-d$stars/d$players

#difficulty: calculate clears /nr. of players who attempted the level
d$difficulty <-round((1-(d$clears/d$players)),2)

#split dataset into difficulty categories
indiv_d<-split(d, d$priordifficulty)
d_easy <- indiv_d[[1]]
d_normal <- indiv_d[[2]]
d_expert <- indiv_d[[3]]
d_superExpert <- indiv_d[[4]]


#get mean true difficulty for each category
priordifficulties <- aggregate(d$difficulty, list(d$priordifficulty), FUN=mean) 

#replace category labels with mean true difficulty per category
d$priordifficulty <- mapvalues(d$priordifficulty, 
                                from=c("easy","normal","expert","superExpert"), 
                                to=priordifficulties$x)

d$difficulty_difference <- d$difficulty - as.numeric(d$priordifficulty)
d$difficulty_difference_scale <- scale(d$difficulty_difference)

regression <- lm(likerate ~ poly(difficulty_difference,2, raw=TRUE), data=d)
summary(regression)


#calculate percentiles of difficulty
d$percentiles <- ApplyPercentiles20labels(d$difficulty_difference)
summary(d$percentiles)

#per clear rate/difficulty/percentiles we are calculating the mean like rate and SEs
dd<-ddply(d, ~percentiles, summarize,  m=mean(likerate), se=se(likerate))
dd$percentiles <- as.numeric(levels(dd$percentiles))       ##### Problem, does not give me the right labels. 
summary(dd$percentiles)
dd$percentiles <- round(dd$percentiles, 2)

p1<-ggplot(dd, aes(x=percentiles, y=m, group=1)) +
  #minimal theme
  theme_classic()+
  #error bars
  geom_errorbar(width=0, aes(ymin=m-se, ymax=m+se), col="#FF7F00", size=1) +
  #line
  #geom_line(col="#007268", size=1) +
  geom_line(col="#FF7F00", size=1) +
  #black points
  geom_point(size=2, fill="black", col="black") +
  #axes labels
  xlab("Difficulty - Category (Percentiles)")+ylab("Average like rate")+
  #title
  #ggtitle(expression("Super Mario Maker"))+
  #font size
  theme(text = element_text(size=18,  family="sans"))

#show it!
p1

#svg("Plot_SuperMario_Difficulty_Difference.svg", width=4.5, height=3.5)
#plot_grid(p1, nrow = 1)
#dev.off()

nrow(d_easy)
dd_easy<-ddply(d_easy, ~difficulty, summarize,  m=mean(likerate), se=se(likerate))

nrow(d_normal)
dd_normal<-ddply(d_normal, ~difficulty, summarize,  m=mean(likerate), se=se(likerate))

nrow(d_expert)
dd_expert<-ddply(d_expert, ~difficulty, summarize,  m=mean(likerate), se=se(likerate))

nrow(d_superExpert)
dd_superExpert<-ddply(d_superExpert, ~difficulty, summarize,  m=mean(likerate), se=se(likerate))


#p1<-ggplot(dd_superExpert, aes(x=percentiles, y=m, group=1)) +
p2<-ggplot(dd_easy, aes(x=difficulty, y=m, group=1)) +
  #minimal theme
  theme_classic()+
  #error bars with MPI colors
  geom_errorbar(width=0, aes(ymin=m-se, ymax=m+se), col="#007268", size=1) +
  #line with MPI colors
  #geom_line(col="#007268", size=1) +
  #black points
  geom_point(size=2, fill="black", col="black") +
  xlim(0, 1)+
  #ylim(-0.25,1)+
  #axes labels
  xlab("Difficulty")+ylab("Average like rate")+
  #xlab("Difficulty (Percentiles)")+ylab("Average like rate")+
  #title
  ggtitle(expression("a: Super Mario Maker - Easy"))+
  #font size
  theme(text = element_text(size=18,  family="sans"))

#show it!
p2

p3<-ggplot(dd_normal, aes(x=difficulty, y=m, group=1)) +
  #minimal theme
  theme_classic()+
  #error bars with MPI colors
  geom_errorbar(width=0, aes(ymin=m-se, ymax=m+se), col="#007268", size=1) +
  #line with MPI colors
  #geom_line(col="#007268", size=1) +
  #black points
  geom_point(size=2, fill="black", col="black") +
  xlim(0, 1)+
  #ylim(-0.25,1)+
  #axes labels
  xlab("Difficulty")+ylab("Average like rate")+
  #xlab("Difficulty (Percentiles)")+ylab("Average like rate")+
  #title
  ggtitle(expression("b: Super Mario Maker - Normal"))+
  #font size
  theme(text = element_text(size=18,  family="sans"))

#show it!
p3

p4<-ggplot(dd_expert, aes(x=difficulty, y=m, group=1)) +
  #minimal theme
  theme_classic()+
  #error bars with MPI colors
  geom_errorbar(width=0, aes(ymin=m-se, ymax=m+se), col="#007268", size=1) +
  #line with MPI colors
  #geom_line(col="#007268", size=1) +
  #black points
  geom_point(size=2, fill="black", col="black") +
  xlim(0, 1)+
  #ylim(-0.25,1)+
  #axes labels
  xlab("Difficulty")+ylab("Average like rate")+
  #xlab("Difficulty (Percentiles)")+ylab("Average like rate")+
  #title
  ggtitle(expression("c: Super Mario Maker - Expert"))+
  #font size
  theme(text = element_text(size=18,  family="sans"))

#show it!
p4

p5<-ggplot(dd_superExpert, aes(x=difficulty, y=m, group=1)) +
  #minimal theme
  theme_classic()+
  #error bars with MPI colors
  geom_errorbar(width=0, aes(ymin=m-se, ymax=m+se), col="#007268", size=1) +
  #line with MPI colors
  #geom_line(col="#007268", size=1) +
  #black points
  geom_point(size=2, fill="black", col="black") +
  xlim(0, 1)+
  #ylim(-0.25,1)+
  #axes labels
  xlab("Difficulty")+ylab("Average like rate")+
  #xlab("Difficulty (Percentiles)")+ylab("Average like rate")+
  #title
  ggtitle(expression("d: Super Mario Maker - Super Expert"))+
  #font size
  theme(text = element_text(size=18,  family="sans"))

#show it!
p5

#svg("Plot_SuperMario_Categories.svg", width=18, height=7.5) #4.5
#plot_grid(p2,p3,p4,p5, nrow = 2)
#dev.off()
