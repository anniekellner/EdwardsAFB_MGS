###DESCRIPTION 

##TO DO, I THINK THIS IS TAKEN CARE OF; landcover has Cameras A_156a_CL and b, however, the species data has A_156_CL and the rebait file has only A_156_CL. So, which is it?
#the a and b camera locations file just has A_156a_CL, so I'll go with that (I'm not sure where I got b, obvly an a exists so it makes sense a b exists/existed)

##DATE 6/28/2023




library(MuMIn)
library(unmarked)
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
max(a1.dat$Date) -min(a1.dat$Date)

summary(a2.dat$Date); max(a2.dat$Date) -min(a2.dat$Date)

summary(b2.dat$Date); max(b2.dat$Date) -min(b2.dat$Date)
b2.dat <- b2.dat[which(b2.dat$Date <= as.Date("2022-11-05")),]

summary(b1.dat$Date); max(b1.dat$Date) -min(b1.dat$Date)
b1.dat <- b1.dat[which(b1.dat$Date <= as.Date("2022-05-22")),]

summary(a1.dat)
cbind.data.frame(a1.dat$Date,julian(a1.dat$Date))
min(a1.dat$Date);max(a1.dat$Date)
min(b1.dat$Date);max(b1.dat$Date)
min(a2.dat$Date);max(a2.dat$Date)
min(b2.dat$Date);max(b2.dat$Date)
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


spp.c <- c("Kit Fox","Coyote","Bobcat","Kangaroo Rat","Jackrabbit", "Cottontail Rabbit","Badger","White-tailed Antelope Ground Squirrel","Raven")

for(i in 8){ #(length(spp.c)-1)
  rm(global); rm(dredge.result); rm(dredge.model.avg); rm(dredge.result.delta2)
  umfa1 <- umf1spp(a1.dat,spp=spp.c[i])
  umfa2 <- umf1spp(a2.dat,spp=spp.c[i])
  umfb1 <- umf1spp(b1.dat,spp=spp.c[i])
  umfb2 <- umf1spp(b2.dat,spp=spp.c[i])
  
  umfa1@siteCovs <- cbind(umfa1@siteCovs,survey="a1")
  umfa2@siteCovs <- cbind(umfa2@siteCovs,survey="a2")
  umfb1@siteCovs <- cbind(umfb1@siteCovs,survey="b1")
  umfb2@siteCovs <- cbind(umfb2@siteCovs,survey="b2")

  umf.all <- umfa1
  umf.all@y <- rbind(umf.all@y,umfa2@y,umfb1@y,umfb2@y)
  umf.all@obsCovs <- rbind(umf.all@obsCovs,umfa2@obsCovs,umfb1@obsCovs,umfb2@obsCovs)
  umf.all@siteCovs <- rbind(umf.all@siteCovs,umfa2@siteCovs,umfb1@siteCovs,umfb2@siteCovs)
  
  umf <- umf.all
  umf@siteCovs$nlcd.dev.800 <- rowSums(umf@siteCovs[,c("dev.open.800","dev.low.800","dev.med.800","dev.high.800")])
  umf@siteCovs$nlcd.roads.800 <- rowSums(umf@siteCovs[,c("primaryroad.800","secondaryroad.800","tertiaryroad.800","thinnedroad.800")])
  umf@siteCovs$nlcd.water.800 <- rowSums(umf@siteCovs[,c("herbwl.800","open.800")])
  umf@siteCovs$nlcd.dev.800 <- scale(umf@siteCovs$nlcd.dev.800)
  umf@siteCovs$nlcd.roads.800 <- scale(umf@siteCovs$nlcd.roads.800)
  umf@siteCovs$nlcd.water.800 <- scale(umf@siteCovs$nlcd.water.800)
  umf@siteCovs$shrubscrub.800 <- scale(umf@siteCovs$shrubscrub.800)
  umf@siteCovs$barren.800 <- scale(umf@siteCovs$barren.800)
  umf@siteCovs$survey <- as.factor(umf@siteCovs$survey)
  
  umf@obsCovs$day <- scale(umf@obsCovs$day)
  umf@obsCovs$days.since.rebait <- scale(umf@obsCovs$days.since.rebait)
  
  summary(umf@obsCovs$day)
  global <-occu(~ day + h0.habtype +lure*days.since.rebait 
                ~  survey+ nlcd.dev.800  + nlcd.roads.800 + nlcd.water.800 + shrubscrub.800, umf)#+  + barren.800
  global@TMB$par #starts should be in order of state (intercept + n covars) followed by detection(intercept + n covars)
  # imodel <-occu(~ 1
  #               ~ 1, umf)
  # 
 # dredge.result <- dredge(global,evaluate=T,rank="AIC",fixed = c("p(day)","psi(shrubscrub.800)"),
        # trace=T)#worked for kitfox
#  global <-occu(~ day + h0.habtype +lure*days.since.rebait 
 #               ~  barren.800+ nlcd.dev.800  + nlcd.roads.800, umf)#+ nlcd.water.800 + shrubscrub.800
  
  
  # dredge.result <- dredge(global,evaluate=T,rank="AIC",fixed = c("p(day)","psi(shrubscrub.800)"),
  #                          trace=T)
  #                         
  # dredge.result <- dredge(global,evaluate=T,rank="AIC",fixed = c("p(day)","p(lure)"),
  #                         trace=T)#coyote 
  
 
  dredge.result <- dredge(global,evaluate=T,rank="AIC",fixed = c("psi(survey)"),#
                          trace=T)
  
  dredge.result.delta2<- subset(dredge.result, delta < 2)
  dredge.model.avg <- model.avg(dredge.result, subset = cumsum(weight) <= .95)# get averaged coefficients

  
  save(dredge.result.delta2,file=paste0("output/","dredge.result.survey.fixed.",spp.c[i],".RData"))
  save(dredge.model.avg,file=paste0("output/","dredge.model.avg.survey.fixed.",spp.c[i],".RData"))
   print(i)
}

 plot_effects(ufittext, "det")
 plot_effects(ufittext, "state")
ufittext
model.results.l <- list()
for(i in c(1:5,8,9)){
  sum_mod1 <- round(summary(ufit.l1[[i]],"state"),2)[,c(1,3)]
  rownames(sum_mod1) <- paste0("psi_",rownames(sum_mod1))
  sum_mod1.det <- round(summary(ufit.l1[[i]],"det"),2)[,c(1,3)]
  rownames(sum_mod1.det) <- paste0("det_",rownames(sum_mod1.det))
  mod1 <- rbind(sum_mod1,sum_mod1.det)
  looic.l1 <- round(ufit.l1[[i]]@loo$estimates[3],2)#looic
  gof.l1 <- gof(ufit.l1[[i]],draws=100, quiet=TRUE)
  gof.l1.est <- gof.l1@estimate
  gof.l1.pvalu <-gof.l1@post_pred_p
  fits<- round(c(looic.l1,gof.l1.est,gof.l1.pvalu),2)
  fits<- cbind(fits,c(NA,NA,NA))
  rownames(fits) <- c("wfit.looic","wfit.gof.est","wfit.gof.p")
  colnames(fits) <- c("mean.mod1","sd.mod1")
  colnames(mod1) <- c("mean.mod1","sd.mod1")
  mod1 <- rbind.data.frame(mod1,fits)
  
  sum_mod2 <- round(summary(ufit.l2[[i]],"state"),2)[,c(1,3)]
  rownames(sum_mod2) <- paste0("psi_",rownames(sum_mod2))
  sum_mod2.det <- round(summary(ufit.l2[[i]],"det"),2)[,c(1,3)]
  rownames(sum_mod2.det) <- paste0("det_",rownames(sum_mod2.det))
  mod2 <- rbind(sum_mod2,sum_mod2.det)
  looic.l2 <- round(ufit.l2[[i]]@loo$estimates[3],2)#looic
  gof.l2 <- gof(ufit.l2[[i]],draws=100, quiet=TRUE)
  gof.l2.est <- gof.l2@estimate
  gof.l2.pvalu <-gof.l2@post_pred_p
  fits<- round(c(looic.l2,gof.l2.est,gof.l2.pvalu),2)
  fits<- cbind(fits,c(NA,NA,NA))
  rownames(fits) <- c("wfit.looic","wfit.gof.est","wfit.gof.p")
  colnames(fits) <- c("mean.mod2","sd.mod2")
  colnames(mod2) <- c("mean.mod2","sd.mod2")
  mod2 <- rbind.data.frame(mod2,fits)
  
  sum_mod3 <- round(summary(ufit.l3[[i]],"state"),2)[,c(1,3)]
  rownames(sum_mod3) <- paste0("psi_",rownames(sum_mod3))
  sum_mod3.det <- round(summary(ufit.l3[[i]],"det"),2)[,c(1,3)]
  rownames(sum_mod3.det) <- paste0("det_",rownames(sum_mod3.det))
  mod3 <- rbind(sum_mod3,sum_mod3.det)
  looic.l3 <- round(ufit.l3[[i]]@loo$estimates[3],2)#looic
  gof.l3 <- gof(ufit.l3[[i]],draws=100, quiet=TRUE)
  gof.l3.est <- gof.l3@estimate
  gof.l3.pvalu <-gof.l3@post_pred_p
  fits<- round(c(looic.l3,gof.l3.est,gof.l3.pvalu),2)
  fits<- cbind(fits,c(NA,NA,NA))
  rownames(fits) <- c("wfit.looic","wfit.gof.est","wfit.gof.p")
  colnames(fits) <- c("mean.mod3","sd.mod3")
  colnames(mod3) <- c("mean.mod3","sd.mod3")
  mod3 <- rbind.data.frame(mod3,fits)
  
  
  mod.all <-  merge(mod1,mod2,by="row.names",all.x=T,all.y=T)
  mod.all <-  merge(mod.all,mod3,by.x="Row.names",by.y="row.names",all.x=T,all.y=T)
  
  
  model.results.l[[i]] <- mod.all
  names(model.results.l)[i] <- names(ufit.l1)[i]
}
save(model.results.l, file="output/modelResults_rebait.ModelComparison.surveyA2.RData")
write.csv(model.results.l[c(1:5,8,9)],file="output/modelResults_rebait.ModelComparison.surveyA2.csv")

ufit.l$`Kit Fox`
(sum_kf <- round(summary(ufit.l$`Kit Fox`,"state"),2)[,c(1,3)])
(sum_kf.det <- round(summary(ufit.l$`Kit Fox`,"det"),2)[,c(1,3)])
for(i in 1:length(spp.daysDetected.min20)){
  print(spp.daysDetected.min20[i])
  print(sum_kf <- round(summary(ufit.l[[i]],"state"),2)[,c(1,3)])
  print(sum_kf.det <- round(summary(ufit.l[[i]],"det"),2)[,c(1,3)])
}

for(i in 1:length(spp.daysDetected.min20)){
  print(spp.daysDetected.min20[i])
  print(ufit.l[[i]])
}
ufit.l$Coyote
ufit.l$Jackrabbit
ufit.l$`Kangaroo Rat`
#Random effects
ran <- ranef(fit_stack, submodel="state")
head(ran$site[[1]])

ran <- ranef(fit_stack, submodel="state", summary=TRUE)
head(ran$site[[1]])

###############################
####DIAGNOSTICS AND MODEL FIT##

#TRACE PLOTS
traceplot(ufit.l$`Kit Fox`)


#RESIDUAL PLOTS
plot_residuals(ufit.l$`Kit Fox`,"det")
plot_residuals(ufit.l$`Kit Fox`, submodel="state")
plot_residuals(ufit1, submodel="state", covariate="day")

plot(ufit.l[[3]])



#############################
###INFERENCE###

#MARGINAL EFFECTS PLOTS
#MARGINAL EFFECTS PLOTS
library(grid)
library(gridExtra)
plot.det <- plot_effects(ufit15, "det")
plot.det <- plot_effects(ufit.l$`Kit Fox`, "det")
plot.state <- plot_effects(ufit.l$`Kit Fox`, "state")
grid.arrange(plot.det,plot.state,top=textGrob("kit fox A1"))

plot.det <- plot_effects(ufit.l$Coyote, "det")
plot.state <- plot_effects(ufit.l$Coyote, "state")
grid.arrange(plot.det,plot.state,top=textGrob("Coyote A1"))

plot.det <-plot_effects(ufit.l$`White-tailed Antelope Ground Squirrel`, "det")
plot.state <-plot_effects(ufit.l$`White-tailed Antelope Ground Squirrel`, "state")
grid.arrange(plot.det,plot.state,top=textGrob("White-tailed Antelope Ground Squirrel A1"))

plot.det <-plot_effects(ufit.l$`Kangaroo Rat`, "det")
plot.state <-plot_effects(ufit.l$`Kangaroo Rat`, "state")
grid.arrange(plot.det,plot.state,top=textGrob("Kangaroo Rat A1"))

plot.det <-plot_effects(ufit.l$`Cottontail Rabbit`, "det")
plot.state <-plot_effects(ufit.l$`Cottontail Rabbit`, "state")
grid.arrange(plot.det,plot.state,top=textGrob("Cottontail Rabbit A1"))

plot.det <-plot_effects(ufit.l$`Jackrabbit`, "det")
plot.state <-plot_effects(ufit.l$`Jackrabbit`, "state")
grid.arrange(plot.det,plot.state,top=textGrob("Jackrabbit A1"))

plot.det <-plot_effects(ufit.l$Bobcat, "det")
plot.state <-plot_effects(ufit.l$Bobcat, "state")
grid.arrange(plot.det,plot.state,top=textGrob("Bobcat A1"))

plot_effects(ufit.l$Badger, "det")
plot_effects(ufit.l$Badger, "state")

plot_effects(ufit.l$Raven, "det")
plot_effects(ufit.l$Raven, "state")

plot_effects(ufit.l$Human, "det")
plot_effects(ufit.l$Human, "state")

#Compare models
modSel()

#Predictions; estimate parameters at provided covariate values, e.g., predictive mapping
predict()

library(ggplot2)
library(sf)

##map of bay
edwards.bdry <- sf::st_read("C:\\Users\\vasteen\\Documents\\EAFB\\Analysis_camera\\GIS\\EdwardsBdry.shp", quiet = TRUE)

siteCovs <- umf@siteCovs
predict.mat.cameras <- siteCovs[!duplicated(siteCovs$camera),]
head(predict.mat.cameras)
predict.mat.cameras$day = 0

mean(allspecies.zerofilled$Date)
mean.date <- "Mid-November 2021"

############
pdf(file = "graphics/SurveyA1.rebaitModel.EstimatedOccupancyAtSites.pdf")
##KIT FOX
k.fox.pred <- round(predict(object = ufit.l1$`Kit Fox`, submodel ='state',transform = T,newdata= predict.mat.cameras),2)
head(k.fox.pred)
k.fox.pred <- cbind(k.fox.pred,siteCovs[!duplicated(siteCovs$camera),c("camera","Latitude","Longitude")])
head(k.fox.pred)
colnames(k.fox.pred)[1] <- "Estimated"

ggplot(edwards.bdry) +
  geom_sf()+
  theme_bw()+
  geom_point(data=k.fox.pred,aes(x=Longitude,y=Latitude,color=Estimated,size=SD))+
  scale_colour_steps2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0.5,
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "colour"
  )+
  ggtitle(paste0("Kit Fox, Survey A1, ", mean.date, "; Mean Occupancy = ",round(plogis(summary(ufit.l1$`Kit Fox`, "state")$mean[1]),2)))+
theme(plot.title = element_text(size = 12))



##COYOTE
coyote.pred <- round(predict(object = ufit.l1$`Coyote`, submodel ='state',transform = T,newdata= predict.mat.cameras),2)
head(coyote.pred)
coyote.pred <- cbind(coyote.pred,siteCovs[!duplicated(siteCovs$camera),c("camera","Latitude","Longitude")])
head(coyote.pred)
colnames(coyote.pred)[1] <- "Estimated"

ggplot(edwards.bdry) +
  geom_sf()+
  theme_bw()+
  geom_point(data=coyote.pred,aes(x=Longitude,y=Latitude,color=Estimated,size=SD))+
  scale_colour_steps2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0.5,
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "colour"
  )+
  ggtitle(paste0("Coyote, Survey A1, ", mean.date, "; Mean Occupancy = ",round(plogis(summary(ufit.l1$Coyote, "state")$mean[1]),2)))+
theme(plot.title = element_text(size = 12))

##BOBCAT
bobcat.pred <- round(predict(object = ufit.l1$Bobcat, submodel ='state',transform = T,newdata= predict.mat.cameras),2)
head(bobcat.pred)
bobcat.pred <- cbind(bobcat.pred,siteCovs[!duplicated(siteCovs$camera),c("camera","Latitude","Longitude")])
head(bobcat.pred)
colnames(bobcat.pred)[1] <- "Estimated"

ggplot(edwards.bdry) +
  geom_sf()+
  theme_bw()+
  geom_point(data=bobcat.pred,aes(x=Longitude,y=Latitude,color=Estimated,size=SD))+
  scale_colour_steps2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0.5,
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "colour"
  )+
  ggtitle(paste0("Bobcat, Survey A1, ", mean.date, "; Mean Occupancy = ",round(plogis(summary(ufit.l1$Bobcat, "state")$mean[1]),2)))+
theme(plot.title = element_text(size = 12))


##JACKRABBIT
Jackrabbit.pred <- round(predict(object = ufit.l1$`Jackrabbit`, submodel ='state',transform = T,newdata= predict.mat.cameras),2)
head(Jackrabbit.pred)
Jackrabbit.pred <- cbind(Jackrabbit.pred,siteCovs[!duplicated(siteCovs$camera),c("camera","Latitude","Longitude")])
head(Jackrabbit.pred)
colnames(Jackrabbit.pred)[1] <- "Estimated"

ggplot(edwards.bdry) +
  geom_sf()+
  theme_bw()+
  geom_point(data=Jackrabbit.pred,aes(x=Longitude,y=Latitude,color=Estimated,size=SD))+
  scale_colour_steps2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0.5,
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "colour"
  )+
  ggtitle(paste0("Jackrabbit, Survey A1, ", mean.date, "; Mean Occupancy = ",round(plogis(summary(ufit.l1$Jackrabbit, "state")$mean[1]),2)))+
theme(plot.title = element_text(size = 12))

##White-tailed Antelope Ground Squirrel
wtags.pred <- round(predict(object = ufit.l1$`White-tailed Antelope Ground Squirrel`, submodel ='state',transform = T,newdata= predict.mat.cameras),2)
head(wtags.pred)
wtags.pred <- cbind(wtags.pred,siteCovs[!duplicated(siteCovs$camera),c("camera","Latitude","Longitude")])
head(wtags.pred)
colnames(wtags.pred)[1] <- "Estimated"

ggplot(edwards.bdry) +
  geom_sf()+
  theme_bw()+
  geom_point(data=wtags.pred,aes(x=Longitude,y=Latitude,color=Estimated,size=SD))+
  scale_colour_steps2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0.5,
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "colour"
  )+
  ggtitle(paste0("White-tailed Antelope Ground Squirrel, Survey A1, ", mean.date, "; Mean Occupancy = ",round(plogis(summary(ufit.l1$`White-tailed Antelope Ground Squirrel`, "state")$mean[1]),2)))+
theme(plot.title = element_text(size = 10))

##Kangaroo Rat
krat.pred <- round(predict(object = ufit.l1$`Kangaroo Rat`, submodel ='state',transform = T,newdata= predict.mat.cameras),2)
head(krat.pred)
krat.pred <- cbind(krat.pred,siteCovs[!duplicated(siteCovs$camera),c("camera","Latitude","Longitude")])
head(krat.pred)
colnames(krat.pred)[1] <- "Estimated"

ggplot(edwards.bdry) +
  geom_sf()+
  theme_bw()+
  geom_point(data=krat.pred,aes(x=Longitude,y=Latitude,color=Estimated,size=SD))+
  scale_colour_steps2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0.5,
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "colour"
  )+
  ggtitle(paste0("Kangaroo Rat, Survey A1, ", mean.date, "; Mean Occupancy = ",round(plogis(summary(ufit.l1$`Kangaroo Rat`, "state")$mean[1]),2)))+
theme(plot.title = element_text(size = 12))

dev.off()

#############end
###################
#Posterior predictions
kitfox <- posterior_predict(ufit.l1[["Kit Fox"]])
kitfox.mean <- apply(kitfox,2,function(x) mean(x)) # mean across mcmc reps
kitfox.mat <- matrix(kitfox.mean,ncol=7,byrow = T)
kitfox.mat <- round(kitfox.mat,2)
head(kitfox.mat)
kitfox.mat2 <- cbind(siteCovs[,c(1,4,5)],kitfox.mat)
