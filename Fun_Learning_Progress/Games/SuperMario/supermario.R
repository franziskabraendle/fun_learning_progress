

#house keeping
rm(list=ls())

#required libraries
library(readr)
library(plyr)
library(ggplot2)
library(lme4)

#confidence interval function
ci<-function(x){1.96*sd(x)/sqrt(length(x))}

ApplyPercentiles20labels <- function(x) {
  breaks <- quantile(x, probs = seq(0, 1, by = 0.05), names = FALSE)
  labels <- breaks[-1]
  cut(x, breaks = breaks, labels = labels, include.lowest = TRUE)
}

#code to create merged file to save time when loading it

# #read in course meta data
# d<-read_tsv("course-meta.csv")
# dc<-read_tsv("courses.csv")
# dc_clean<-data.frame(id = dc$id, priordifficulty = dc$difficulty)
# d_total<-merge(d,dc_clean, by="id")
# d<-d_total

# #delete the double ones and only take the version with the highest attempts
# g<-ddply(d, "id", summarise, attempts = max(attempts))
# d<-merge(g, d, by=c("id", "attempts"))
# rm(g)
# 
# # have deleted the double attempts but not double stars, so do that now: 
# h<-ddply(d, "id", summarise, stars = max(stars))
# d<-merge(h, d, by=c("id", "stars"))
# rm(h)
# #rest can be removed, just duplicates of firstClear tag and tweets
# d<-d[!duplicated(d$id),]

#saveRDS(d,file="merged_supermario_metafile.Rda")

#read in the merged file
d <- readRDS(file="merged_supermario_metafile.Rda")


#how many attempts?
sum(d$attempts)
sum(d$stars)

#how many levels? Should be the same as length(unique(d$id))
length(d$id)
length(unique(d$id))

#as a player can only give one star per level, the likerate is calculated this way and not stars/attempts
d$likerate<-d$stars/d$players

#difficulty: calculate clears /nr. of players who attempted the level
d$difficulty <-round((1-(d$clears/d$players)),2)
d$difficulty_scale <- scale(d$difficulty)

#calculate percentiles of difficulty
d$percentiles <- ApplyPercentiles20labels(d$difficulty)
summary(d$percentiles)

regression <- lm(likerate ~ poly(difficulty_scale,2, raw=TRUE), data=d)
summary(regression)

h1<-hist(d$difficulty)
box() 
h1


#per percentiles we are calculating the mean like rate and CIs
dd<-ddply(d, ~percentiles, summarize,  m=mean(likerate), ci=ci(likerate))

#plotting it
p1<-ggplot(dd, aes(x=percentiles, y=m, group=1)) +
  #minimal theme
  theme_classic()+
  #error bars with MPI colors
  geom_errorbar(width=0, aes(ymin=m-ci, ymax=m+ci), col="#007268", size=1) +
  #line with MPI colors
  geom_line(col="#007268", size=1) +
  #black points
  geom_point(size=2, fill="black", col="black") +
  #axes labels
  #xlab("Difficulty (Percentiles)")+ylab("Average like rate ± 95% CI")+
  xlab("Difficulty (Percentiles)")+ylab("Average like rate")+
  scale_x_discrete(breaks = c(0.03, 0.23, 0.5, 0.75,  1))+
  #title
  #ggtitle(expression("Super Mario Maker"))+
  #font size
  theme(text = element_text(size=18,  family="sans"))

#show it!
p1

#svg("Plot_SuperMario.svg", width=4.5, height=3.5)
#plot_grid(p1, nrow = 1)
#dev.off()

#color the part that is not predicted by learning progress
dd$percentiles_number <- as.numeric(levels(dd$percentiles))
dd$segment <- ifelse(dd$percentiles_number < 0.33, "segment1", "segment2")

#plotting it
p2<-ggplot(dd, aes(x=percentiles, y=m, group=1)) +
  #minimal theme
  theme_classic()+
  #error bars with MPI colors
  geom_errorbar(width=0, aes(ymin=m-ci, ymax=m+ci, col = segment), size=1, show.legend = FALSE) +
  #line with MPI colors
  geom_line(aes(color=segment), size=1, show.legend = FALSE) +
  #black points
  geom_point(size=2, fill="black", col="black") +
  #axes labels
  #xlab("Difficulty (Percentiles)")+ylab("Average like rate ± 95% CI")+
  xlab("Difficulty (Percentiles)")+ylab("Average like rate")+
  scale_x_discrete(breaks = c(0.03, 0.23, 0.5, 0.75,  1))+
  #title
  #ggtitle(expression("Super Mario Maker"))+
  #font size
  scale_color_manual(values = c("segment1" = "#FF7F00", "segment2" = "#007268")) +
  theme(text = element_text(size=18,  family="sans"))

#show it!
p2

#svg("Plot_SuperMario_segments.svg", width=4.5, height=3.5)
#plot_grid(p2, nrow = 1)
#dev.off()


#plot raw data for SI
p3<- ggplot(d, aes(x=difficulty, y=likerate)) +
  theme_classic()+
  geom_point(alpha = 0.5, stroke = 0, shape=16, size=2) +
  theme(legend.position="none") + 
  theme(text = element_text(size=18,  family="sans")) +
  xlab("Difficulty")+ylab("Average like rate")
p3

p4 <- ggMarginal(p3, type="histogram", size=3, fill = "#007268")
p4

#svg("plot_SuperMario_raw.svg", width=4.5, height=3.5)
#plot_grid(p4, nrow = 1)
#dev.off()

#plot difficulty histogram
p5<- ggplot(d, aes(x=difficulty)) +
  geom_histogram(col="black", fill="#007268", binwidth = 0.125)+
  theme_classic()+
  theme(text = element_text(size=18,  family="sans")) +
  xlab("Difficulty")+ylab("Count")+
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1))
p5

#svg("plot_SuperMario_hist.svg", width=4.5, height=3.5)
#plot_grid(p5, nrow = 1)
#dev.off()
