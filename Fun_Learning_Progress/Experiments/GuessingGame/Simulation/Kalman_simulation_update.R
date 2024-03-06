
#house keeping
rm(list=ls())

#libraries
library(MASS)
library(plyr)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(ggbreak)

st_error<-function(x){sd(x)/sqrt(length(x))}

#Bayesian mean tracker updates the previous posterior based on single observations
bayesmean <- function(x, vare=1, varprior=1, meanprior = 50, prevPost=NULL){
  
  #agent's priors
  mu0 <- meanprior #prior mean
  var0 <- varprior #prior variance
  
  if (is.null(prevPost)){#if no posterior prior, assume it is the first observation
    predictions <- data.frame(mu=mu0, sig2=var0)
    
  }else{#if previous posterior is provided, update
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

priorvariance <- 10 #randomly set, other prior variances work as well.
priormean <- 50

#error variance vector over which to collect trials
errvarvec<-c(0.1, 1, 10, 100, 1000)

#collect frames
mean_updates<-data.frame()

#loop over error variance
for (i in seq_along(errvarvec)){
  #get error variance
  errvar<-errvarvec[i]
  
  #do 1000 repeats per error variance
  for (ntimes in 1:1000){
    
    #sample Mu_true uniformly between 1 and 100
    mutrue<-sample(1:100, 1)
    
    #sample an x_i from the true distribution
    xi<-rnorm(1, mutrue, sd=sqrt(errvar))
    
    samples <- c(xi)
    
    #initialize delta
    fun<-100
    
    #initialize mean tracker
    posteriorold<-bayesmean(xi, vare=errvar, varprior=priorvariance, meanprior=priormean)
    
    #initialize runs
    nruns<-1

    for (j in 0:10){
      #sample a new observation
      if ( j == 0){
        fun<- priormean - posteriorold$mu 
      }
      
      else{
      xi<-rnorm(1, mutrue, sd=sqrt(errvar))
      samples <- c(samples,xi)
      
      #update the tracked posterior
      posteriornew<-bayesmean(xi, vare=errvar, meanprior = priormean, prevPost=posteriorold)
      
      #calculate fun
      fun<-posteriorold$mu-posteriornew$mu
      
      #new posterior becomes old posterior
      posteriorold<-posteriornew
      }
      #increment runs
      nruns<-nruns+1
      
      #save absolute update
      absfun <- abs(fun)
      mean_updates<-rbind(mean_updates, data.frame(errvar, j, absfun))
    
    }
  }
}

updatesummary<-ddply(mean_updates, .(errvar,j), summarize, m=mean(absfun), ferror=st_error(absfun))

update_plot <- ggplot(updatesummary, aes(x=factor(j), y=m, color = factor(errvar), group = factor(errvar))) +
  geom_line(size = 1)+
  geom_errorbar(aes(ymin=m-ferror, ymax=m+ferror), width=.01, size = 1)+
  scale_y_break(c(4.7, 6), scale = 0.5) +
  theme_classic() +
  scale_color_viridis_d() +
  theme(text = element_text(size=18,  family="sans"), plot.margin = unit(c(3, 3, 3, -8), "pt")) +
  labs(x="Steps", y = "Update of Mean", colour = "True \nVariance")
update_plot

svg("Plot_Simulation_Update.svg", width=4.5, height=3.5)
plot_grid(update_plot, nrow = 1)
dev.off()


