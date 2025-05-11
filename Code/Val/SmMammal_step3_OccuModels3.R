###DESCRIPTION

###DATE 1/27/2025



#libraries

library(lubridate)
library(dplyr)
library(unmarked)
library(tidyr)
library(ubms)
rm(list=ls())

load("input/sm.mammal.data.RData")
dat$mth <- month(ymd(dat$date ))
dat <- dat %>% filter(!mth == 9)

dat$julian <- yday(dat$date)

head(dat)
table(dat$`Common shrew_NA`)
table(dat$`Common shrew_Pitfall`)
table(dat$`Common shrew_pitfall`)
#most to least common: common shrew (1823),northern red-backed vole (1217), shrew spp (239), montane shrew (110), tundra shrew (10)


#NORTHERN RED-BACKED VOLE
dat.nrbv <- select(dat,plotID,year,night, station, azimuth, 'Northern red-backed vole_sherman','Northern red-backed vole_pitfall',
                   'Northern red-backed vole_snap',julian,viereck.3,maxTempCelsius,minTempCelsius,precipitationStandardized)

dat.nrbv$loc <- paste(dat.nrbv$plotID,dat.nrbv$azimuth,dat.nrbv$station,sep=".")
dat.nrbv$count <- rowSums(dat.nrbv[,6:8],na.rm=T)
dat.nrbv$count <- ifelse(dat.nrbv$count > 1,1,dat.nrbv$count)
dat.nrbv[is.na(dat.nrbv$count),"count"] <-0
head(dat.nrbv)

dat.nrbv.w <- dat.nrbv %>% select(night,loc,count) %>%  group_by(loc) %>% pivot_wider(names_from =night,values_from = count)
det.covars <- select(dat.nrbv,loc,night,julian,maxTempCelsius,minTempCelsius,precipitationStandardized) %>% pivot_wider(names_from = night,values_from = c(julian,maxTempCelsius,minTempCelsius,precipitationStandardized))
det.covars$maxTempCelsius_1[is.na(det.covars$maxTempCelsius_1)] <- median(det.covars$maxTempCelsius_1,na.rm=T)
det.covars$maxTempCelsius_2[is.na(det.covars$maxTempCelsius_2)] <- median(det.covars$maxTempCelsius_2,na.rm=T)
det.covars$maxTempCelsius_3[is.na(det.covars$maxTempCelsius_3)] <- median(det.covars$maxTempCelsius_3,na.rm=T)
det.covars$minTempCelsius_1[is.na(det.covars$maxTempCelsius_1)] <- median(det.covars$minTempCelsius_1,na.rm=T)
det.covars$minTempCelsius_2[is.na(det.covars$maxTempCelsius_2)] <- median(det.covars$minTempCelsius_2,na.rm=T)
det.covars$minTempCelsius_3[is.na(det.covars$maxTempCelsius_3)] <- median(det.covars$minTempCelsius_3,na.rm=T)
table(det.covars$precipitationStandardized_1)
table(det.covars$precipitationStandardized_2)
table(det.covars$precipitationStandardized_3)

site.covars <- dat.nrbv[!duplicated(dat.nrbv$loc),]
site.covars <- select(site.covars,loc,plotID,year,viereck.3)

dat.occu <- left_join(dat.nrbv.w,det.covars,by="loc")
dat.occu <- left_join(dat.occu,site.covars,by="loc")
det.2 <- dat.occu %>%ungroup() %>% select("julian_1":"precipitationStandardized_3")
det.2.l <- list(julian = det.2[,1:3],max.temp = det.2[,4:6],precip = det.2[,7:9])

#create unmarked frame
umf.nrbv <- unmarkedFrameOccu(y=dat.occu[,2:4], siteCovs=dat.occu[,c("plotID","year","viereck.3")], 
                  obsCovs=det.2.l)   
str(umf.nrbv@siteCovs)
umf.nrbv@siteCovs$year <- as.factor(umf.nrbv@siteCovs$year)

#model in stan: ~ detection ~ occupancy
fm.yr.stan.nrbv.z <- stan_occu(~ scale(julian) + scale(max.temp) ~ (1|plotID)  +viereck.3 + year
                              , umf.nrbv, chains = 3, cores=3)
fm.yr.stan.nrbv.z
fm.null.stan.nrbv.z <- stan_occu(~ 1 ~ 1
                               , umf.nrbv, chains = 3, cores=3)
fm.null.stan.nrbv.z
ran <- ranef(fm.yr.stan.nrbv.z, submodel="state",summary=T)
ran
plot_effects(fm.yr.stan.nrbv.z, "state")
plot_effects(fm.yr.stan.nrbv.z, "det")


#WESTERN MEADOW VOLE
dat.wmv <- select(dat,plotID,year,night, station, azimuth, 'Western meadow vole_sherman','Western meadow vole_pitfall',
                   'Western meadow vole_snap',julian,viereck.3,maxTempCelsius,minTempCelsius,precipitationStandardized)

dat.wmv$loc <- paste(dat.wmv$plotID,dat.wmv$azimuth,dat.wmv$station,sep=".")
dat.wmv$count <- rowSums(dat.wmv[,6:8])
dat.wmv$count <- ifelse(dat.wmv$count > 1,1,dat.wmv$count)
dat.wmv[is.na(dat.wmv$count),"count"] <-0
head(dat.wmv)
table(dat.wmv$count)
dat.wmv.w <- dat.wmv %>% select(night,loc,count) %>%  group_by(loc) %>% pivot_wider(names_from =night,values_from = count)
det.covars <- select(dat.wmv,loc,night,julian,maxTempCelsius,minTempCelsius,precipitationStandardized) %>% pivot_wider(names_from = night,values_from = c(julian,maxTempCelsius,minTempCelsius,precipitationStandardized))
det.covars$maxTempCelsius_1[is.na(det.covars$maxTempCelsius_1)] <- median(det.covars$maxTempCelsius_1,na.rm=T)
det.covars$maxTempCelsius_2[is.na(det.covars$maxTempCelsius_2)] <- median(det.covars$maxTempCelsius_2,na.rm=T)
det.covars$maxTempCelsius_3[is.na(det.covars$maxTempCelsius_3)] <- median(det.covars$maxTempCelsius_3,na.rm=T)
det.covars$minTempCelsius_1[is.na(det.covars$maxTempCelsius_1)] <- median(det.covars$minTempCelsius_1,na.rm=T)
det.covars$minTempCelsius_2[is.na(det.covars$maxTempCelsius_2)] <- median(det.covars$minTempCelsius_2,na.rm=T)
det.covars$minTempCelsius_3[is.na(det.covars$maxTempCelsius_3)] <- median(det.covars$minTempCelsius_3,na.rm=T)
table(det.covars$precipitationStandardized_1)
table(det.covars$precipitationStandardized_2)
table(det.covars$precipitationStandardized_3)

site.covars <- dat.wmv[!duplicated(dat.wmv$loc),]
site.covars <- select(site.covars,loc,plotID,year,viereck.3)

dat.occu <- left_join(dat.wmv.w,det.covars,by="loc")
dat.occu <- left_join(dat.occu,site.covars,by="loc")
det.2 <- dat.occu %>%ungroup() %>% select("julian_1":"precipitationStandardized_3")
det.2.l <- list(julian = det.2[,1:3],max.temp = det.2[,4:6],precip = det.2[,7:9])

#create unmarked frame
umf.wmv <- unmarkedFrameOccu(y=dat.occu[,2:4], siteCovs=dat.occu[,c("plotID","year","viereck.3")], 
                         obsCovs=det.2.l)   # 
str(umf.wmv@siteCovs)
umf.wmv@siteCovs$year <- as.factor(umf.wmv@siteCovs$year)
umf.wmv@siteCovs$viereck.3 <- relevel(umf.wmv@siteCovs$viereck.3,"IC2")

#stan: ~ detection ~ occupancy
fm.yr.stan.wmv.z <- stan_occu(~ scale(julian) + scale(max.temp) ~ (1|plotID)  +viereck.3 + year
                           , umf.wmv, chains = 3, cores=3)

fm.null.stan.wmv.z <- stan_occu(~ 1 ~ 1
                              , umf.wmv, chains = 3, cores=3)

ran <- ranef(fm.yr.stan.wmv.z, submodel="state",summary=T)
ran
plot_effects(fm.yr.stan.wmv.z, "state")
plot_effects(fm.yr.stan.wmv.z, "det")


#COMMON SHREW; 2014 -2020
dat.14.20 <- dat[which(dat$year < 2021),]
dat.cshrew <- select(dat.14.20,plotID,year,night, station, azimuth, 'Common shrew_sherman','Common shrew_pitfall',
                     'Common shrew_snap',julian,viereck.3,maxTempCelsius,minTempCelsius,precipitationStandardized)

dat.cshrew$loc <- paste(dat.cshrew$plotID,dat.cshrew$azimuth,dat.cshrew$station,sep=".")
dat.cshrew$count <- rowSums(dat.cshrew[,6:8])
dat.cshrew$count <- ifelse(dat.cshrew$count > 1,1,dat.cshrew$count)
dat.cshrew[is.na(dat.cshrew$count),"count"] <-0
head(dat.cshrew)

dat.cshrew.w <- dat.cshrew %>% select(night,loc,count) %>%  group_by(loc) %>% pivot_wider(names_from =night,values_from = count)
det.covars <- select(dat.cshrew,loc,night,julian,maxTempCelsius,minTempCelsius,precipitationStandardized) %>% pivot_wider(names_from = night,values_from = c(julian,maxTempCelsius,minTempCelsius,precipitationStandardized))
det.covars$maxTempCelsius_1[is.na(det.covars$maxTempCelsius_1)] <- median(det.covars$maxTempCelsius_1,na.rm=T)
det.covars$maxTempCelsius_2[is.na(det.covars$maxTempCelsius_2)] <- median(det.covars$maxTempCelsius_2,na.rm=T)
det.covars$maxTempCelsius_3[is.na(det.covars$maxTempCelsius_3)] <- median(det.covars$maxTempCelsius_3,na.rm=T)
det.covars$minTempCelsius_1[is.na(det.covars$maxTempCelsius_1)] <- median(det.covars$minTempCelsius_1,na.rm=T)
det.covars$minTempCelsius_2[is.na(det.covars$maxTempCelsius_2)] <- median(det.covars$minTempCelsius_2,na.rm=T)
det.covars$minTempCelsius_3[is.na(det.covars$maxTempCelsius_3)] <- median(det.covars$minTempCelsius_3,na.rm=T)
table(det.covars$precipitationStandardized_1)
table(det.covars$precipitationStandardized_2)
table(det.covars$precipitationStandardized_3)

site.covars <- dat.cshrew[!duplicated(dat.cshrew$loc),]
site.covars <- select(site.covars,loc,plotID,year,viereck.3)

dat.occu <- left_join(dat.cshrew.w,det.covars,by="loc")
dat.occu <- left_join(dat.occu,site.covars,by="loc")
det.2 <- dat.occu %>%ungroup() %>% select("julian_1":"precipitationStandardized_3")
det.2.l <- list(julian = det.2[,1:3],max.temp = det.2[,4:6],precip = det.2[,7:9])

#create unmarked frame
umf.cshrew <- unmarkedFrameOccu(y=dat.occu[,2:4], siteCovs=dat.occu[,c("plotID","year","viereck.3")], 
                         obsCovs=det.2.l)   # 
str(umf.cshrew@siteCovs)
umf.cshrew@siteCovs$year <- as.factor(umf.cshrew@siteCovs$year)#factor
umf.cshrew@siteCovs$viereck.3 <- relevel(umf.cshrew@siteCovs$viereck.3,"IC2")


#stan: ~ detection ~ occupancy
fm.stan.yr.cshrew.z <- stan_occu(~ scale(julian) + scale(max.temp) ~ (1|plotID) +year +viereck.3
                       , umf.cshrew, chains = 3, cores=3)

fm.stan.null.cshrew.z <- stan_occu(~ 1 ~ 1
                                 , umf.cshrew, chains = 3, cores=3)

ran <- ranef(fm.stan.yr.cshrew.z, submodel="state",summary=T)
ran
plot_effects(fm.stan.yr.cshrew.z, "state")
plot_effects(fm.stan.yr.cshrew.z, "det")


#MONTANE SHREW
dat.mshrew <- select(dat.14.20,plotID,year,night, station, azimuth, 'Montane shrew_sherman','Montane shrew_pitfall',
                  'Montane shrew_snap',julian,viereck.3,maxTempCelsius,minTempCelsius,precipitationStandardized)

dat.mshrew$loc <- paste(dat.mshrew$plotID,dat.mshrew$azimuth,dat.mshrew$station,sep=".")
dat.mshrew$count <- rowSums(dat.mshrew[,6:8])
dat.mshrew$count <- ifelse(dat.mshrew$count > 1,1,dat.mshrew$count)
dat.mshrew[is.na(dat.mshrew$count),"count"] <-0
head(dat.mshrew)
table(dat.mshrew$count)
dat.mshrew.w <- dat.mshrew %>% select(night,loc,count) %>%  group_by(loc) %>% pivot_wider(names_from =night,values_from = count)
det.covars <- select(dat.mshrew,loc,night,julian,maxTempCelsius,minTempCelsius,precipitationStandardized) %>% pivot_wider(names_from = night,values_from = c(julian,maxTempCelsius,minTempCelsius,precipitationStandardized))
det.covars$maxTempCelsius_1[is.na(det.covars$maxTempCelsius_1)] <- median(det.covars$maxTempCelsius_1,na.rm=T)
det.covars$maxTempCelsius_2[is.na(det.covars$maxTempCelsius_2)] <- median(det.covars$maxTempCelsius_2,na.rm=T)
det.covars$maxTempCelsius_3[is.na(det.covars$maxTempCelsius_3)] <- median(det.covars$maxTempCelsius_3,na.rm=T)
det.covars$minTempCelsius_1[is.na(det.covars$maxTempCelsius_1)] <- median(det.covars$minTempCelsius_1,na.rm=T)
det.covars$minTempCelsius_2[is.na(det.covars$maxTempCelsius_2)] <- median(det.covars$minTempCelsius_2,na.rm=T)
det.covars$minTempCelsius_3[is.na(det.covars$maxTempCelsius_3)] <- median(det.covars$minTempCelsius_3,na.rm=T)
table(det.covars$precipitationStandardized_1)
table(det.covars$precipitationStandardized_2)
table(det.covars$precipitationStandardized_3)

site.covars <- dat.mshrew[!duplicated(dat.mshrew$loc),]
site.covars <- select(site.covars,loc,plotID,year,viereck.3)

dat.occu <- left_join(dat.mshrew.w,det.covars,by="loc")
dat.occu <- left_join(dat.occu,site.covars,by="loc")
det.2 <- dat.occu %>%ungroup() %>% select("julian_1":"precipitationStandardized_3")
det.2.l <- list(julian = det.2[,1:3],max.temp = det.2[,4:6],precip = det.2[,7:9])

umf.mshrew <- unmarkedFrameOccu(y=dat.occu[,2:4], siteCovs=dat.occu[,c("plotID","year","viereck.3")], 
                         obsCovs=det.2.l)   # 
str(umf.mshrew@siteCovs)
umf.mshrew@siteCovs$year <- as.factor(umf.mshrew@siteCovs$year)
umf.mshrew@siteCovs$viereck.3 <- relevel(umf.mshrew@siteCovs$viereck.3,"IC2")

#fm <- occu(~julian + max.temp ~plotID , umf) 

#stan: ~ detection ~ occupancy
fm.yr.stan.mshrew.z <- stan_occu(~ scale(julian) + scale(max.temp) ~ (1|plotID)  +viereck.3+ year
                              , umf.mshrew, chains = 3, cores=3)
fm.null.stan.mshrew.z <- stan_occu(~ 1 ~ 1, umf.mshrew, chains = 3, cores=3)
fm.null.stan.mshrew.z
fm.yr.stan.mshrew.z
ran <- ranef(fm.yr.stan.mshrew.z, submodel="state",summary=T)
ran
plot_effects(fm.yr.stan.mshrew.z, "state")
plot_effects(fm.yr.stan.mshrew.z, "det")


