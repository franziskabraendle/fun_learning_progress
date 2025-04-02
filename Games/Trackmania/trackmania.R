#Analysis of Trackmania Data. 
#Data origin: https://tmnf.exchange/

#house keeping
rm(list=ls())

#required libraries
library(readr)
library(ggplot2)
library(lme4)
library(binr)

#standard error function
se<-function(x){sd(x)/sqrt(length(x))}

#Clean Data set
d<-read.csv("Trackmania.csv", head=TRUE, sep=";", fileEncoding="latin1")
length(d$Track.Name)
d$Diff.<-round(d$Diff.)
hist(d$Diff.)
hist(d$Aw.)
sum(d$Aw.)
d$Diff_scaled <- scale(d$Diff.)
regression <- lm(Aw. ~ poly(Diff_scaled,2, raw=TRUE), data=d)
summary(regression)

dd<-ddply(d, ~Diff., summarize,  m=mean(Aw.), se=se(Aw.))

#plotting it
p1<-ggplot(dd, aes(x=Diff., y=m, group=1)) +
  #minimal theme
  theme_classic()+
  #error bars with MPI colors
  geom_errorbar(width=0, aes(ymin=m-se, ymax=m+se), col="#007268", size=1) +
  #line with MPI colors
  geom_line(col="#007268", size=1) +
  #black points
  geom_point(fill="black", col="black", size=2) +
  #axes labels
  xlab("Difficulty")+ylab("Average Awards")+
  theme(text = element_text(size=18,  family="sans"))

#show it!
p1

#svg("Plot_Trackmania.svg", width=4.5, height=3.5)
#plot_grid(p1, nrow = 1)
#dev.off()


#plot raw data for SI
p2<- ggplot(d, aes(x=Diff., y=Aw.)) +
  theme_classic()+
  geom_point(alpha = 0.5, stroke = 0, shape=16, size=2) +
  theme(legend.position="none") + 
  theme(text = element_text(size=18,  family="sans")) +
  xlab("Difficulty")+ylab("Average Awards") #+
p2

p3 <- ggMarginal(p2, type="histogram", size=3, fill = "#007268")
p3

#svg("plot_Trackmania_raw.svg", width=4.5, height=3.5)
#plot_grid(p3, nrow = 1)
#dev.off()


#plot difficulty histogram
p4<- ggplot(d, aes(x=Diff.)) +
  geom_histogram(col="black", fill="#007268", binwidth = 1)+
  theme_classic()+
  theme(text = element_text(size=18,  family="sans")) +
  xlab("Difficulty")+ylab("Count")
p4

#svg("plot_Trackmania_hist.svg", width=4.5, height=3.5)
#plot_grid(p4, nrow = 1)
#dev.off()

#### "scree" plot
max_degree <- 10
mse_results <- data.frame(Degree = integer(), MSE = numeric())

for (deg in 1:max_degree) {
  model <- lm(Aw. ~ poly(Diff_scaled,deg, raw=TRUE), data=d)  
  mse <- mean(residuals(model)^2)  # Calculate MSE
  mse_results <- rbind(mse_results, data.frame(Degree = deg, MSE = mse))
}

# Plot the MSE vs. Model Degree
p_mse <- ggplot(mse_results, aes(x = Degree, y = MSE, group = 1)) +
  theme_classic() +
  geom_line(col = "#007268", size = 1) +
  scale_x_continuous(breaks = 1:max_degree) +
  geom_point(fill = "black", col = "black", size = 2) + 
  xlab("Polynomial Degree") + ylab("Mean Squared Error") +
  theme(text = element_text(size = 18, family = "sans"))

# Show the plot
p_mse
