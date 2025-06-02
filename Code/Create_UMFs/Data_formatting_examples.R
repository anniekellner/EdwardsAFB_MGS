###############################################################################
###         DATA FORMATTING: CROSSBILL DATASET                           ###
###############################################################################

library(tidyverse)
library(unmarked)


##  ----  LOAD DATA   ----  ##

wt <- read.csv(system.file("csv","widewt.csv", package="unmarked"))

val <- allspecies.zerofilled
head(val)

species <- val[,"Jackrabbit"]
dat <- allspecies.zerofilled[,c("RelativePath","Date","days.since.rebait")]
species <- cbind.data.frame(dat,species)
species <- species[order(species$Date,species$RelativePath),]
colnames(species)[4] <- "thespp"
species$julian <- julian(species$Date)


species01 <- species %>% select(-Date,-days.since.rebait) %>% group_by(RelativePath) %>% 
  pivot_wider(names_from = julian,values_from = thespp)

#FUNCTION
umf1spp <- function(allspecies.zerofilled, spp){
  species <- allspecies.zerofilled[,spp]
  dat <- allspecies.zerofilled[,c("RelativePath","Date","days.since.rebait")]
  species <- cbind.data.frame(dat,species)
  species <- species[order(species$Date,species$RelativePath),]
  colnames(species)[4] <- "thespp"
  species$julian <- julian(species$Date)
  dsr <- species %>% select(-Date,-thespp) %>% group_by(RelativePath) %>% 
    pivot_wider(names_from = julian,values_from = days.since.rebait) %>% ungroup() %>% select(-RelativePath) #%>%
  # pivot_longer(cols = everything())
  
  #species detection history in wide format
  species01 <- species %>% select(-Date,-days.since.rebait) %>% group_by(RelativePath) %>% 
    pivot_wider(names_from = julian,values_from = thespp) #%>% ungroup() %>% select(-RelativePath)
  print("check columns are ordered correctly")
  print(as.numeric(colnames(species01)[2:ncol(species01)]))
  dsr <- species %>% select(-Date,-thespp) %>% group_by(RelativePath) %>% 
    pivot_wider(names_from = julian,values_from = days.since.rebait) %>% ungroup() %>% select(-RelativePath) #%>%
  # pivot_longer(cols = everything())
  julian <- species %>% select(-Date,-thespp,-days.since.rebait) %>% group_by(RelativePath) %>% 
    pivot_wider(names_from = julian,values_from = julian) %>% ungroup() %>% select(-RelativePath) 
  
  julian <- species %>% select(-Date,-thespp,-days.since.rebait) %>% group_by(RelativePath) %>% 
    pivot_wider(names_from = julian,values_from = julian) %>% ungroup() %>% select(-RelativePath) #%>%
  
  ##obsCovs
  obsCovs <- list(day =julian,days.since.rebait = dsr)
  
  ###siteCovs:
  #camera
  camera <- unique(species$RelativePath)
  #lure:
  lure <- substr(camera,start=(nchar(camera)-1),stop=nchar(camera))
  siteCovs <- cbind.data.frame(camera=camera,lure=lure)
  #landcover
  siteCovs <- merge(siteCovs,landcover,by.x="camera",by.y="Camera Name",all.x=T,all.y=F)
  
  ###y
  y <- species01[,2:ncol(species01)]
  
  umf <- unmarkedFrameOccu(y=y,siteCovs = siteCovs,obsCovs=obsCovs)
  #umf@obsCovs$day <- scale(umf@obsCovs$day)
  
  return(umf)
}