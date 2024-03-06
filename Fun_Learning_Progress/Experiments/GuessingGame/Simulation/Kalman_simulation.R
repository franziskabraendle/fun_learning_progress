
#house keeping
rm(list=ls())

#libraries
library(MASS)
library(plyr)
library(ggplot2)
library(cowplot)
library(plyr)

st_error<-function(x){sd(x)/sqrt(length(x))}

#Bayesian mean tracker updates the previous posterior based on single observations
bayesmean <- function(x, vare=1, varprior=1, prevPost=NULL){
  
  #priors
  mu0 <- 50 #prior mean
  var0 <- varprior #prior variance
  
  if (is.null(prevPost)){#if no posterior prior -> first observation
    predictions <- data.frame(mu=mu0, sig2=var0)
    
  }else{#if previous posterior -> update
    predictions <- prevPost
  }
  
  #Kalman gain
  kGain <- predictions$sig2/ (predictions$sig2 +vare)
  
  #update mean
  predictions$mu <- predictions$mu + (kGain * (x-predictions$mu))
  
  #update variance
  predictions$sig2 <- predictions$sig2 * (1 - kGain)
  
  #return output
  return(predictions)
  
}

errvarvec<-c(0.1, 1, 10, 100, 1000)
threshold <- 0.5 #0.5 for main analysis, but flexible, e.g. 0.1, 0.25, 0.75 and 1 used for SI

for (j in seq_along(errvarvec)){

  #error variance vector over which to collect trials
  priorvariance <- errvarvec[j]
  
  #collect frame
  dcollect<-data.frame(nruns=numeric(), errvar=numeric())
  
  #loop over error variance
  for (i in seq_along(errvarvec)){
    #get error variance
    errvar<-errvarvec[i]
    
    #do 1000 repeats per error variance
    for (ntimes in 1:1000){
      
      #sample mutrue uniformly between 20 and 80, similar to experiment
      mutrue<-sample(20:80, 1)
      
      #sample an xi from the true distribution
      xi<-rnorm(1, mutrue, sd=sqrt(errvar))
      
      samples <- c(xi)
      
      #initialize delta
      fun<-100
      
      #initialize mean tracker
      posteriorold<-bayesmean(xi, vare=errvar, varprior=priorvariance)
      
      #initialize runs
      nruns<-0
      
      #as long as agent has fun, they should continue to play
      #otherwise they stop (with a minimum of three observations)
      
      
      while (abs(fun)>threshold | nruns < 3){
        #sample a new observation
        xi<-rnorm(1, mutrue, sd=sqrt(errvar))
        #resample if it's outside the range, similar to experiment
        while(xi<1 | xi >100){
          xi<-rnorm(1, mutrue, sd=sqrt(errvar))
        }
        
        samples <- c(samples,xi)
        
        #update the tracked posterior
        posteriornew<-bayesmean(xi, vare=errvar, prevPost=posteriorold)
        
        #calculate fun
        fun<-posteriorold$mu-posteriornew$mu
        
        #new posterior becomes old posterior
        posteriorold<-posteriornew
        
        #increment runs
        nruns<-nruns+1
        
      }
      
      #collect everything into a data frame
      dcollect<-rbind(dcollect, data.frame(nruns, errvar))
      
    }
    
  }
  
  #get mean per errorvariance
  print(j)
  if (j != 1){
    dd_old <- dd
  }
  dd <- ddply(dcollect, ~errvar, summarize, m=mean(nruns), se=st_error(nruns))
  dd$dataset <-  as.character(priorvariance)
  if (j != 1){
    combined <- rbind(dd_old,dd)
  }
  else{
    combined <- dd
  }
  dd <- combined
  
  options(scipen = 10)
  
}

custom_breaks <- c(0.1, 1, 10, 100, 1000)
custom_labels <- c("0.1", "1", "10", "100", "1000")

p <- ggplot(dd, aes(x = errvar, y = m, color = dataset, group = dataset)) +
  geom_line(size = 1)+
  geom_errorbar(aes(ymin=m-se, ymax=m+se), width=.01, size = 1)+
  geom_point(col="black", size = 2)+
  scale_x_continuous(
    breaks = custom_breaks,  # Specify custom breaks
    labels = custom_labels,  # Specify custom labels
    trans = "log10"          # Use log10 scale
  )+
  theme_classic() +
  labs(x="True Variance", y = "Number of Guesses", color = "Prior \nVariance")+
  scale_color_viridis_d() +
  theme(text = element_text(size=18,  family="sans"), legend.position = "none")
p

filename <- paste(c("Plot_Simulation_", threshold, ".svg"), collapse = "")
svg(filename, width=4.5, height=3.5)
plot_grid(p, nrow = 1)
dev.off()


