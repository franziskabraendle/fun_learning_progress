#house keeping
rm(list=ls())

#libraries
library(ggplot2)
library(plyr)
library(lmerTest)
library(cowplot)
library(viridis)

st_error<-function(x){sd(x)/sqrt(length(x))}

#threshold when simulation should stop interacting with current grid
threshold <- 1 #set to 0.5 for main analysis, but flexible. Used 0.1, 0.25, 0.5, 0.75 and 1 for the SI.

lambdas <- c(0.25, 0.5, 1, 2, 4, 8, 16)

for (lambda in lambdas){
  #read simulated sampling data
  location <- paste(c("Sampling_Simulation/Lambda=",lambda,"Difference.txt"), collapse = "")
  d<-read.delim(location, header = FALSE, sep = " ")
  stoppingpoints <- list()
  d_t <-t(d)
  ncol(d_t)
  nrow(d_t)
  #check if updates lie below threshold and save stopping points
  for (i in 1:ncol(d_t)){
    nexttrial = FALSE
    for (j in 1:nrow(d_t)){
      if(abs(d_t[j,i]) < threshold & j>5){
        nexttrial = TRUE
      }
      else{
        nexttrial = FALSE
      }
      if(nexttrial == TRUE){
        stoppingpoints <- append(stoppingpoints, j)
        break
      }
    }
    assign(paste("stoppingpoints", lambda, sep = ""), stoppingpoints)
  }
}

#prepare data frames
mean_values <- c(mean(as.numeric(stoppingpoints0.25)), mean(as.numeric(stoppingpoints0.5)), mean(as.numeric(stoppingpoints1)), mean(as.numeric(stoppingpoints2)), mean(as.numeric(stoppingpoints4)), mean(as.numeric(stoppingpoints8)), mean(as.numeric(stoppingpoints16)))
sterror_values <- c(st_error(as.numeric(stoppingpoints0.25)), st_error(as.numeric(stoppingpoints0.5)), st_error(as.numeric(stoppingpoints1)), st_error(as.numeric(stoppingpoints2)), st_error(as.numeric(stoppingpoints4)), st_error(as.numeric(stoppingpoints8)), st_error(as.numeric(stoppingpoints16)))
summary <- data.frame(lambda, mean_values, sterror_values)

p1<-ggplot(summary, aes(x=lambdas, y=mean_values)) +
  #theme
  theme_classic()+
  #error bar
  geom_errorbar(width=0, aes(ymin=mean_values-sterror_values, ymax=mean_values+sterror_values), col="#007268", size=1) +
  #line
  geom_line(col="#007268", size=1) +
  #points
  geom_point(col="black", size = 2)+
  #axes labels
  labs(x = expression(paste("Smoothness (", lambda, ")")), y = "Number of samples")+
  #font size
  theme(text = element_text(size=18,  family="sans"))
p1
 
#filename <- paste(c("Plot_Simulation_Engagement", threshold, ".svg"), collapse = "")
#svg(filename, width=4.5, height=3.5)
#plot_grid(p1, nrow = 1)
#dev.off()

#read in mean squared error data (could also transform the one from above, but already saved it in right format during simulation)
for (lambda in lambdas){
  location_mse <- paste(c("Sampling_Simulation/Lambda=",lambda,"Mse.txt"), collapse = "")
  mse_data<-read.delim(location_mse, header = FALSE, sep = " ")
  assign(paste("Mse", lambda, sep = ""), colMeans(mse_data))
}
#prepare data frame
MseSummary <- data.frame(seq.int(150), Mse0.25, Mse0.5, Mse1, Mse2, Mse4, Mse8, Mse16)

p2<-ggplot(MseSummary, aes(x=seq.int.150.)) +
  theme_classic()+
  geom_line(y=Mse0.25, aes(colour = " 0.25"), size = 1) +
  geom_line(y=Mse0.5, aes(colour = " 0.5"), size = 1) +
  geom_line(y=Mse1, aes(colour = " 1"), size = 1) +
  geom_line(y=Mse2, aes(colour = " 2"), size = 1) +
  geom_line(y=Mse4, aes(colour = " 4"), size = 1) +
  geom_line(y=Mse8, aes(colour = " 8"), size = 1) +
  geom_line(y=Mse16, aes(colour = "16"), size = 1) +
  labs(color = paste("Smoothness\n(\u03bb)")) +
  xlab("Sample step")+
  ylab("Error")+
  xlim(0,150)+
  ylim(0,70)+
  scale_color_viridis_d() +
  theme(text = element_text(size=18,  family="sans"))

p2
 
svg("Plot_Simulation_Error.svg", width=4.5, height=3.5)
plot_grid(p2, nrow = 1)
dev.off()

