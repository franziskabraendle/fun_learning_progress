library(jsonlite)
library(dplyr)

#house keeping
rm(list=ls())

UseCompCheckFilteredData <- TRUE   #filters out participants who had to do the comprehension check more than 9 times (here 4) 
#(participant 44 not included anyway, as they only played with 1 machine)

UseMinVariancesAsFilter <- FALSE #remove players who played with less than three different variances (9)
UseMoreThan3ClicksFilter <- FALSE #remove players who never spent more than three clicks per machine (7)

if(UseCompCheckFilteredData){
  compcheckdata<-fromJSON("participants_highcompcheck")
}else{
  compcheckdata<-as.numeric()
}

temp = list.files(path= "Data/", pattern="*.csv")
myfiles = lapply(paste("Data/",temp,sep=""), read.delim, sep=",")

frame <- data.frame(matrix(ncol = 12, nrow = 0))
titles <- c("X", "participant", "totalmachines", "machines", "rounds", "trials", "variance", "mean", "value", "guess", "difference", "funrating")
colnames(frame) <- titles

for (i in 1:length(myfiles)) {
  if(!(is.element(temp[i],compcheckdata))){
    tempdf <-data.frame(myfiles[i])
    tempdf <- tempdf[tempdf$totalmachines != tempdf$machines, ]
    tempdf$participant <- rep(i, nrow(tempdf))
    frame <- bind_rows(frame,tempdf)
  }
}

participants_to_filter <- list()

if(UseMinVariancesAsFilter){
  varianceperpart <- ddply(frame, .(participant, variance), summarize)
  numbervariances <- ddply(varianceperpart, .(participant), nrow)
  for (i in 1:nrow(numbervariances)){
    if(numbervariances$V1[i]<3){
      participants_to_filter <- append(participants_to_filter, numbervariances$participant[i])
    }
  }
}

if(UseMoreThan3ClicksFilter){
  MaxTrialsPerMachine <- ddply(frame, .(participant), summarize, maxtrialspermachine=max(rounds))
  for (i in 1:nrow(MaxTrialsPerMachine)){
    if(MaxTrialsPerMachine$maxtrialspermachine[i]==3){
      participants_to_filter <- append(participants_to_filter,MaxTrialsPerMachine$participant[i])
    }
  }
}

participants_to_filter_unique <- unique(participants_to_filter)

frame <- frame[!(frame$participant %in% participants_to_filter_unique), ]

names(frame)[names(frame) == 'X'] <- 'OverallTrial'

frame$variance_mapped <- log10(frame$variance)

n_distinct(frame$participant)

saveRDS(frame,file="guessinggame.Rda")








