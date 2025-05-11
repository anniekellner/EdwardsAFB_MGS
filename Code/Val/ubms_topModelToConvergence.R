###DESCRIPTION (1) running the top model to convergence; 7000 iters?


##DATE 7/7/2023

library(MuMIn)
library(ubms)
library(tidyr)
library(dplyr)
library(readxl)
library(lubridate)
rm(list = ls())

load("~/EAFB/Analysis_camera/input/landcover.nlcd.roads.RData")
landcover[which(landcover$`Camera Name` %in% "A_156a_CL"),"Camera Name"] <- "A_156_CL"

load("output/allspecies.zerofilled.rebait.06132023.A2.RData")
a2.dat <- as.data.frame(allspecies.zerofilled);rm(allspecies.zerofilled)
load("output/allspecies.zerofilled.rebait.06152023.B1.RData")
b1.dat <- as.data.frame(allspecies.zerofilled);rm(allspecies.zerofilled)
load("output/allspecies.zerofilled.rebait.06162023.B2.RData")
b2.dat <- as.data.frame(allspecies.zerofilled);rm(allspecies.zerofilled)
load("output/allspecies.zerofilled.rebait.20231306.A1.RData")
a1.dat <- as.data.frame(allspecies.zerofilled);rm(allspecies.zerofilled)

#pick 90 days each
summary(a1.dat$Date) #most cameras working 18909-19016
a1.dat <- a1.dat[which(a1.dat$Date < as.Date("2022-01-08") & a1.dat$Date > as.Date("2021-10-09")),]
min(a1.dat$Date); max(a1.dat$Date)
max(a1.dat$Date) -min(a1.dat$Date)

summary(a2.dat$Date); max(a2.dat$Date) -min(a2.dat$Date)
min(a2.dat$Date);max(a2.dat$Date)
summary(b2.dat$Date); max(b2.dat$Date) -min(b2.dat$Date)
b2.dat$censored
b2.dat <- b2.dat[which(b2.dat$Date <= as.Date("2022-11-05")),]
min(b2.dat$Date); max(b2.dat$Date)
b1.dat <- b1.dat[which(b1.dat$Date <= as.Date("2022-05-22")),]
summary(b1.dat$Date); max(b1.dat$Date) -min(b1.dat$Date)
min(b1.dat$Date); max(b1.dat$Date)

summary(a1.dat)
cbind.data.frame(a1.dat$Date,julian(a1.dat$Date))

a1.dat$survey <- "a1"
a2.dat$survey <- "a2"
b1.dat$survey <- "b1"
b2.dat$survey <- "b2"

#FUNCTION
umf1spp <- function(allspecies.zerofilled, spp){
  species <- allspecies.zerofilled[,spp]
  dat <- allspecies.zerofilled[,c("RelativePath","Date","days.since.rebait")]
  species <- cbind.data.frame(dat,species)
  species <- species[order(species$Date,species$RelativePath),]
  colnames(species)[4] <- "thespp"
  species$julian <- julian(species$Date)
  
  #species detection history in wide format
  species01 <- species %>% select(-Date,-days.since.rebait) %>% group_by(RelativePath) %>% 
    pivot_wider(names_from = julian,values_from = thespp) #%>% ungroup() %>% select(-RelativePath)
  print("check columns are ordered correctly")
  print(as.numeric(colnames(species01)[2:ncol(species01)]))
  dsr <- species %>% select(-Date,-thespp) %>% group_by(RelativePath) %>% 
    pivot_wider(names_from = julian,values_from = days.since.rebait) %>% ungroup() %>% select(-RelativePath) #%>%
  # pivot_longer(cols = everything())
  
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



###kit fox###
load("~/EAFB/Analysis_camera/output/dredge.result.Kit Fox.RData")
  umfa1 <- umf1spp(a1.dat,spp="Kit Fox")
  umfa2 <- umf1spp(a2.dat,spp="Kit Fox")
  umfb1 <- umf1spp(b1.dat,spp="Kit Fox")
  umfb2 <- umf1spp(b2.dat,spp="Kit Fox")
  
  umfa1@siteCovs <- cbind(umfa1@siteCovs,survey="a1")
  umfa2@siteCovs <- cbind(umfa2@siteCovs,survey="a2")
  umfb1@siteCovs <- cbind(umfb1@siteCovs,survey="b1")
  umfb2@siteCovs <- cbind(umfb2@siteCovs,survey="b2")
  
  umf.all <- umfa1
  umf.all@y <- rbind(umf.all@y,umfa2@y,umfb1@y,umfb2@y)
  umf.all@obsCovs <- rbind(umf.all@obsCovs,umfa2@obsCovs,umfb1@obsCovs,umfb2@obsCovs)
  umf.all@siteCovs <- rbind(umf.all@siteCovs,umfa2@siteCovs,umfb1@siteCovs,umfb2@siteCovs)
  
  umf <- umf.all
  umf@siteCovs$nlcd.dev.800 <- scale(rowSums(umf@siteCovs[,c("dev.open.800","dev.low.800","dev.med.800","dev.high.800")]))
  umf@siteCovs$nlcd.roads.800 <- scale(rowSums(umf@siteCovs[,c("primaryroad.800","secondaryroad.800","tertiaryroad.800","thinnedroad.800")]))
  umf@siteCovs$nlcd.water.800 <- scale(rowSums(umf@siteCovs[,c("herbwl.800","open.800")]))
  umf@siteCovs$shrubscrub.800 <- scale(umf@siteCovs$shrubscrub.800)
  umf@siteCovs$barren.800 <- scale(umf@siteCovs$barren.800)
  umf@siteCovs$survey <- as.factor(umf@siteCovs$survey)
  
  umf@obsCovs$day <- scale(umf@obsCovs$day)
  umf@obsCovs$days.since.rebait <- scale(umf@obsCovs$days.since.rebait)
  
  #this model was based on a couple of additional comparisons that showed no interaction and no water improved elpd.
  top.model.kitfox <- stan_occu(~ day + h0.habtype + lure + days.since.rebait + (1|camera)
                            ~  survey + nlcd.dev.800 + (1|camera), umf,iter=15000)#+
  save(top.model.kitfox,file="output/allsurveys.topModeltoRhat1.1.kitfox.RData")
 
  
  ##Coyote## Fixed: day and lure
  umfa1 <- umf1spp(a1.dat,spp="Coyote")
  umfa2 <- umf1spp(a2.dat,spp="Coyote")
  umfb1 <- umf1spp(b1.dat,spp="Coyote")
  umfb2 <- umf1spp(b2.dat,spp="Coyote")
  
  umfa1@siteCovs <- cbind(umfa1@siteCovs,survey="a1")
  umfa2@siteCovs <- cbind(umfa2@siteCovs,survey="a2")
  umfb1@siteCovs <- cbind(umfb1@siteCovs,survey="b1")
  umfb2@siteCovs <- cbind(umfb2@siteCovs,survey="b2")
  
  umf.all <- umfa1
  umf.all@y <- rbind(umf.all@y,umfa2@y,umfb1@y,umfb2@y)
  umf.all@obsCovs <- rbind(umf.all@obsCovs,umfa2@obsCovs,umfb1@obsCovs,umfb2@obsCovs)
  umf.all@siteCovs <- rbind(umf.all@siteCovs,umfa2@siteCovs,umfb1@siteCovs,umfb2@siteCovs)
  
  umf <- umf.all
  umf@siteCovs$nlcd.dev.800 <- scale(rowSums(umf@siteCovs[,c("dev.open.800","dev.low.800","dev.med.800","dev.high.800")]))
  umf@siteCovs$nlcd.roads.800 <- scale(rowSums(umf@siteCovs[,c("primaryroad.800","secondaryroad.800","tertiaryroad.800","thinnedroad.800")]))
  umf@siteCovs$nlcd.water.800 <- scale(rowSums(umf@siteCovs[,c("herbwl.800","open.800")]))
  umf@siteCovs$shrubscrub.800 <- scale(umf@siteCovs$shrubscrub.800)
  umf@siteCovs$barren.800 <- scale(umf@siteCovs$barren.800)
  umf@siteCovs$survey <- as.factor(umf@siteCovs$survey)
  
  umf@obsCovs$day <- scale(umf@obsCovs$day)
  umf@obsCovs$days.since.rebait <- scale(umf@obsCovs$days.since.rebait)

  top.mod.coyote <-stan_occu(~ day + h0.habtype + lure + days.since.rebait + (1|camera)
                       ~ survey +  nlcd.water.800 + nlcd.roads.800 + shrubscrub.800 + barren.800 + (1|camera), umf,
                       iter=32000)#+  
  save(top.mod.coyote,file="output/allsurveys.topModeltoRhat1.1.coyote.RData")
  

  ##Bobcat## Fixed = survey
  
  umfa1 <- umf1spp(a1.dat,spp="Bobcat")
  umfa2 <- umf1spp(a2.dat,spp="Bobcat")
  umfb1 <- umf1spp(b1.dat,spp="Bobcat")
  umfb2 <- umf1spp(b2.dat,spp="Bobcat")
  
  umfa1@siteCovs <- cbind(umfa1@siteCovs,survey="a1")
  umfa2@siteCovs <- cbind(umfa2@siteCovs,survey="a2")
  umfb1@siteCovs <- cbind(umfb1@siteCovs,survey="b1")
  umfb2@siteCovs <- cbind(umfb2@siteCovs,survey="b2")
  
  umf.all <- umfa1
  umf.all@y <- rbind(umf.all@y,umfa2@y,umfb1@y,umfb2@y)
  umf.all@obsCovs <- rbind(umf.all@obsCovs,umfa2@obsCovs,umfb1@obsCovs,umfb2@obsCovs)
  umf.all@siteCovs <- rbind(umf.all@siteCovs,umfa2@siteCovs,umfb1@siteCovs,umfb2@siteCovs)
  
  umf <- umf.all
  umf@siteCovs$nlcd.dev.800 <- scale(rowSums(umf@siteCovs[,c("dev.open.800","dev.low.800","dev.med.800","dev.high.800")]))
  umf@siteCovs$nlcd.roads.800 <- scale(rowSums(umf@siteCovs[,c("primaryroad.800","secondaryroad.800","tertiaryroad.800","thinnedroad.800")]))
  umf@siteCovs$nlcd.water.800 <- scale(rowSums(umf@siteCovs[,c("herbwl.800","open.800")]))
  umf@siteCovs$shrubscrub.800 <- scale(umf@siteCovs$shrubscrub.800)
  umf@siteCovs$barren.800 <- scale(umf@siteCovs$barren.800)
  umf@siteCovs$survey <- as.factor(umf@siteCovs$survey)
  
  umf@obsCovs$day <- scale(umf@obsCovs$day)
  umf@obsCovs$days.since.rebait <- scale(umf@obsCovs$days.since.rebait)

  top.mod.bobcat <-stan_occu(~ day + h0.habtype + lure + (1|camera)
                                ~  survey + nlcd.dev.800 + nlcd.water.800 + (1|camera), umf,iter=7000)#+  
  save(top.mod.bobcat,file="output/allsurveys.topModeltoRhat1.1.bobcat.RData")
  
  
  ##Badger## Fixed = survey and day
   rm(dredge.result.delta2)
   load("~/EAFB/Analysis_camera/output/dredge.result.Badger.RData")
  umfa1 <- umf1spp(a1.dat,spp="Badger")
  umfa2 <- umf1spp(a2.dat,spp="Badger")
  umfb1 <- umf1spp(b1.dat,spp="Badger")
  umfb2 <- umf1spp(b2.dat,spp="Badger")

  umfa1@siteCovs <- cbind(umfa1@siteCovs,survey="a1")
  umfa2@siteCovs <- cbind(umfa2@siteCovs,survey="a2")
  umfb1@siteCovs <- cbind(umfb1@siteCovs,survey="b1")
  umfb2@siteCovs <- cbind(umfb2@siteCovs,survey="b2")

  umf.all <- umfa1
  umf.all@y <- rbind(umf.all@y,umfa2@y,umfb1@y,umfb2@y)
  umf.all@obsCovs <- rbind(umf.all@obsCovs,umfa2@obsCovs,umfb1@obsCovs,umfb2@obsCovs)
  umf.all@siteCovs <- rbind(umf.all@siteCovs,umfa2@siteCovs,umfb1@siteCovs,umfb2@siteCovs)

  umf <- umf.all
  umf@siteCovs$nlcd.dev.800 <- scale(rowSums(umf@siteCovs[,c("dev.open.800","dev.low.800","dev.med.800","dev.high.800")]))
  umf@siteCovs$nlcd.roads.800 <- scale(rowSums(umf@siteCovs[,c("primaryroad.800","secondaryroad.800","tertiaryroad.800","thinnedroad.800")]))
  umf@siteCovs$nlcd.water.800 <- scale(rowSums(umf@siteCovs[,c("herbwl.800","open.800")]))
  umf@siteCovs$shrubscrub.800 <- scale(umf@siteCovs$shrubscrub.800)
  umf@siteCovs$barren.800 <- scale(umf@siteCovs$barren.800)
  umf@siteCovs$survey <- as.factor(umf@siteCovs$survey)

  umf@obsCovs$day <- scale(umf@obsCovs$day)
  umf@obsCovs$days.since.rebait <- scale(umf@obsCovs$days.since.rebait)

  summary(umf@obsCovs$day)
  dredge.result.delta2#Fixed: day and lure
  mod.badger3 <-stan_occu(~ lure 
                    ~  survey  + shrubscrub.800 + barren.800 + (1|camera), umf,iter=7000)#+
  top.mod.badger <- mod.badger3
  
   save(top.mod.badger,file="output/allsurveys.topModeltoRhat1.1.bad.RData")
  # 
  ##Kangaroo Rat## Fixed = survey ?
  umfa1 <- umf1spp(a1.dat,spp="Kangaroo Rat")
  umfa2 <- umf1spp(a2.dat,spp="Kangaroo Rat")
  umfb1 <- umf1spp(b1.dat,spp="Kangaroo Rat")
  umfb2 <- umf1spp(b2.dat,spp="Kangaroo Rat")
  
  umfa1@siteCovs <- cbind(umfa1@siteCovs,survey="a1")
  umfa2@siteCovs <- cbind(umfa2@siteCovs,survey="a2")
  umfb1@siteCovs <- cbind(umfb1@siteCovs,survey="b1")
  umfb2@siteCovs <- cbind(umfb2@siteCovs,survey="b2")
  
  umf.all <- umfa1
  umf.all@y <- rbind(umf.all@y,umfa2@y,umfb1@y,umfb2@y)
  umf.all@obsCovs <- rbind(umf.all@obsCovs,umfa2@obsCovs,umfb1@obsCovs,umfb2@obsCovs)
  umf.all@siteCovs <- rbind(umf.all@siteCovs,umfa2@siteCovs,umfb1@siteCovs,umfb2@siteCovs)
  
  umf <- umf.all
  umf@siteCovs$nlcd.dev.800 <- scale(rowSums(umf@siteCovs[,c("dev.open.800","dev.low.800","dev.med.800","dev.high.800")]))
  umf@siteCovs$nlcd.roads.800 <- scale(rowSums(umf@siteCovs[,c("primaryroad.800","secondaryroad.800","tertiaryroad.800","thinnedroad.800")]))
  umf@siteCovs$nlcd.water.800 <- scale(rowSums(umf@siteCovs[,c("herbwl.800","open.800")]))
  umf@siteCovs$shrubscrub.800 <- scale(umf@siteCovs$shrubscrub.800)
  umf@siteCovs$barren.800 <- scale(umf@siteCovs$barren.800)
  umf@siteCovs$survey <- as.factor(umf@siteCovs$survey)
  
  umf@obsCovs$day <- scale(umf@obsCovs$day)
  umf@obsCovs$days.since.rebait <- scale(umf@obsCovs$days.since.rebait)
  
  top.mod.krat <-stan_occu(~ day + h0.habtype + days.since.rebait + (1|camera)
                              ~  survey + (1|camera), umf,iter=7000)#+

  save(top.mod.krat,file="output/allsurveys.topModeltoRhat1.1.krat.RData")
  
  ##Jackrabbit## Not sure = just survey?
  umfa1 <- umf1spp(a1.dat,spp="Jackrabbit")
  umfa2 <- umf1spp(a2.dat,spp="Jackrabbit")
  umfb1 <- umf1spp(b1.dat,spp="Jackrabbit")
  umfb2 <- umf1spp(b2.dat,spp="Jackrabbit")
  
  umfa1@siteCovs <- cbind(umfa1@siteCovs,survey="a1")
  umfa2@siteCovs <- cbind(umfa2@siteCovs,survey="a2")
  umfb1@siteCovs <- cbind(umfb1@siteCovs,survey="b1")
  umfb2@siteCovs <- cbind(umfb2@siteCovs,survey="b2")
  
  umf.all <- umfa1
  umf.all@y <- rbind(umf.all@y,umfa2@y,umfb1@y,umfb2@y)
  umf.all@obsCovs <- rbind(umf.all@obsCovs,umfa2@obsCovs,umfb1@obsCovs,umfb2@obsCovs)
  umf.all@siteCovs <- rbind(umf.all@siteCovs,umfa2@siteCovs,umfb1@siteCovs,umfb2@siteCovs)
  
  umf <- umf.all
  umf@siteCovs$nlcd.dev.800 <- scale(rowSums(umf@siteCovs[,c("dev.open.800","dev.low.800","dev.med.800","dev.high.800")]))
  umf@siteCovs$nlcd.roads.800 <- scale(rowSums(umf@siteCovs[,c("primaryroad.800","secondaryroad.800","tertiaryroad.800","thinnedroad.800")]))
  umf@siteCovs$nlcd.water.800 <- scale(rowSums(umf@siteCovs[,c("herbwl.800","open.800")]))
  umf@siteCovs$shrubscrub.800 <- scale(umf@siteCovs$shrubscrub.800)
  umf@siteCovs$barren.800 <- scale(umf@siteCovs$barren.800)
  umf@siteCovs$survey <- as.factor(umf@siteCovs$survey)
  
  umf@obsCovs$day <- scale(umf@obsCovs$day)
  umf@obsCovs$days.since.rebait <- scale(umf@obsCovs$days.since.rebait)

  top.mod.jrab <-stan_occu(~ day + h0.habtype + lure*days.since.rebait + (1|camera)
                          ~  survey + shrubscrub.800 + barren.800 + (1|camera), umf,iter=7000)#rhat = 1.4, n_eff=13; I would run at least 6000 iters

  save(top.mod.jrab,file="output/allsurveys.topModeltoRhat1.1.jrab.RData")
  
  ##Cottontail Rabbit## Fixed = survey
  umfa1 <- umf1spp(a1.dat,spp="Cottontail Rabbit")
  umfa2 <- umf1spp(a2.dat,spp="Cottontail Rabbit")
  umfb1 <- umf1spp(b1.dat,spp="Cottontail Rabbit")
  umfb2 <- umf1spp(b2.dat,spp="Cottontail Rabbit")
  umfa1@siteCovs <- cbind(umfa1@siteCovs,survey="a1")
  umfa2@siteCovs <- cbind(umfa2@siteCovs,survey="a2")
  umfb1@siteCovs <- cbind(umfb1@siteCovs,survey="b1")
  umfb2@siteCovs <- cbind(umfb2@siteCovs,survey="b2")
  umf.all <- umfa1
  umf.all@y <- rbind(umf.all@y,umfa2@y,umfb1@y,umfb2@y)
  umf.all@obsCovs <- rbind(umf.all@obsCovs,umfa2@obsCovs,umfb1@obsCovs,umfb2@obsCovs)
  umf.all@siteCovs <- rbind(umf.all@siteCovs,umfa2@siteCovs,umfb1@siteCovs,umfb2@siteCovs)
  umf <- umf.all
  umf@siteCovs$nlcd.dev.800 <- scale(rowSums(umf@siteCovs[,c("dev.open.800","dev.low.800","dev.med.800","dev.high.800")]))
  umf@siteCovs$nlcd.roads.800 <- scale(rowSums(umf@siteCovs[,c("primaryroad.800","secondaryroad.800","tertiaryroad.800","thinnedroad.800")]))
  umf@siteCovs$nlcd.water.800 <- scale(rowSums(umf@siteCovs[,c("herbwl.800","open.800")]))
  umf@siteCovs$shrubscrub.800 <- scale(umf@siteCovs$shrubscrub.800)
  umf@siteCovs$barren.800 <- scale(umf@siteCovs$barren.800)
  umf@siteCovs$survey <- as.factor(umf@siteCovs$survey)
  umf@obsCovs$day <- scale(umf@obsCovs$day)
  umf@obsCovs$days.since.rebait <- scale(umf@obsCovs$days.since.rebait)
  
 top.mod.crab <-stan_occu(~ day + h0.habtype + lure + days.since.rebait + (1|camera)
                    ~  survey + nlcd.dev.800  + shrubscrub.800  +(1|camera), umf,iter=7000)#+
  
  save(top.mod.crab,file="output/allsurveys.topModeltoRhat1.1.crab.RData")
  
  top.models.6spp <- list(top.mod.coyote,top.model.kitfox,top.mod.bobcat,top.mod.crab,top.mod.jrab,top.mod.krat)
  save(top.models.6spp,file="output/top.models.6spp")
  
  ##White-tailed Antelope Ground Squirrel## Not sure = just survey?
  # umfa1 <- umf1spp(a1.dat,spp="White-tailed Antelope Ground Squirrel")
  # umfa2 <- umf1spp(a2.dat,spp="White-tailed Antelope Ground Squirrel")
  # umfb1 <- umf1spp(b1.dat,spp="White-tailed Antelope Ground Squirrel")
  # umfb2 <- umf1spp(b2.dat,spp="White-tailed Antelope Ground Squirrel")
  # umfa1@siteCovs <- cbind(umfa1@siteCovs,survey="a1")
  # umfa2@siteCovs <- cbind(umfa2@siteCovs,survey="a2")
  # umfb1@siteCovs <- cbind(umfb1@siteCovs,survey="b1")
  # umfb2@siteCovs <- cbind(umfb2@siteCovs,survey="b2")
  # umf.all <- umfa1
  # umf.all@y <- rbind(umf.all@y,umfa2@y,umfb1@y,umfb2@y)
  # umf.all@obsCovs <- rbind(umf.all@obsCovs,umfa2@obsCovs,umfb1@obsCovs,umfb2@obsCovs)
  # umf.all@siteCovs <- rbind(umf.all@siteCovs,umfa2@siteCovs,umfb1@siteCovs,umfb2@siteCovs)
  # umf <- umf.all
  # umf@siteCovs$nlcd.dev.800 <- scale(rowSums(umf@siteCovs[,c("dev.open.800","dev.low.800","dev.med.800","dev.high.800")]))
  # umf@siteCovs$nlcd.roads.800 <- scale(rowSums(umf@siteCovs[,c("primaryroad.800","secondaryroad.800","tertiaryroad.800","thinnedroad.800")]))
  # umf@siteCovs$nlcd.water.800 <- scale(rowSums(umf@siteCovs[,c("herbwl.800","open.800")]))
  # umf@siteCovs$shrubscrub.800 <- scale(umf@siteCovs$shrubscrub.800)
  # umf@siteCovs$barren.800 <- scale(umf@siteCovs$barren.800)
  # umf@siteCovs$survey <- as.factor(umf@siteCovs$survey)
  # 
  # umf@obsCovs$day <- scale(umf@obsCovs$day)
  # umf@obsCovs$days.since.rebait <- scale(umf@obsCovs$days.since.rebait)
  # 
  # summary(umf@obsCovs$day)
  # top.mod.wtags <-stan_occu(~ day + h0.habtype + lure*days.since.rebait + (1|camera)
  #                            ~  survey + nlcd.roads.800 + (1|camera), umf,iter=7000)#+
  # 
  # save(top.mod.wtags,file="output/allsurveys.topModeltoRhat1.1.wtags.RData")
  # 
  
  #Raven## Not sure = just survey?
  umfa1 <- umf1spp(a1.dat,spp="Raven")
  umfa2 <- umf1spp(a2.dat,spp="Raven")
  umfb1 <- umf1spp(b1.dat,spp="Raven")
  umfb2 <- umf1spp(b2.dat,spp="Raven")

  umfa1@siteCovs <- cbind(umfa1@siteCovs,survey="a1")
  umfa2@siteCovs <- cbind(umfa2@siteCovs,survey="a2")
  umfb1@siteCovs <- cbind(umfb1@siteCovs,survey="b1")
  umfb2@siteCovs <- cbind(umfb2@siteCovs,survey="b2")

  umf.all <- umfa1
  umf.all@y <- rbind(umf.all@y,umfa2@y,umfb1@y,umfb2@y)
  umf.all@obsCovs <- rbind(umf.all@obsCovs,umfa2@obsCovs,umfb1@obsCovs,umfb2@obsCovs)
  umf.all@siteCovs <- rbind(umf.all@siteCovs,umfa2@siteCovs,umfb1@siteCovs,umfb2@siteCovs)

  umf <- umf.all
  umf@siteCovs$nlcd.dev.800 <- scale(rowSums(umf@siteCovs[,c("dev.open.800","dev.low.800","dev.med.800","dev.high.800")]))
  umf@siteCovs$nlcd.roads.800 <- scale(rowSums(umf@siteCovs[,c("primaryroad.800","secondaryroad.800","tertiaryroad.800","thinnedroad.800")]))
  umf@siteCovs$nlcd.water.800 <- scale(rowSums(umf@siteCovs[,c("herbwl.800","open.800")]))
  umf@siteCovs$shrubscrub.800 <- scale(umf@siteCovs$shrubscrub.800)
  umf@siteCovs$barren.800 <- scale(umf@siteCovs$barren.800)
  umf@siteCovs$survey <- as.factor(umf@siteCovs$survey)

  umf@obsCovs$day <- scale(umf@obsCovs$day)
  umf@obsCovs$days.since.rebait <- scale(umf@obsCovs$days.since.rebait)

  ##run to convergence
  top.mod.raven <-stan_occu(~ day + h0.habtype + lure + days.since.rebait  + (1|camera)
                             ~  survey + barren.800 + (1|camera), umf,iter=5000)#+
  save(top.mod.raven,file="output/allsurveys.topModeltoRhat1.1.raven.RData")

