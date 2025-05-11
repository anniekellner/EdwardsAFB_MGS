###DESCRIPTION Summarizes detection rates (mean, sd, max, min) across 
#cameras; calculates naive occupancy by survey; plots detection rates 
#by camera by survey


###DATE 5/23/23
###UPDATE 5/30/23 filtered A1 data to 9/21/21 onward
###UPDATED 6/16/2023 = FINALIZED DATASETS
####UPDATE 6/20/2023 GRAY FOX = KIT FOX FOR THE SURVEYS WITH GRAY FOX which is just b1
###UPDATE 6/22 Now mean detection rate across cameras (average of the daily detection rate across cameras) and 
#SD of camera mean detection rate (min, max doesn't really make sense so this is removed)
###UPDATE 8/8/2023 Restricted to the 90 days analyzed in occupancy models
library(tidyr)
library(dplyr)
library(ggplot2)

rm(list=ls())


load("output/allspecies.zerofilled.rebait.20231306.A1.RData")
a1.dat <- allspecies.zerofilled;rm(allspecies.zerofilled)
#a1.dat <- a1.dat[which(a1.dat$Date > "2021-09-20"),]

load("output/allspecies.zerofilled.rebait.06152023.B1.RData")
b1.dat <- allspecies.zerofilled;rm(allspecies.zerofilled)
b1.dat$`Gray Fox`
table(ifelse(b1.dat$'Gray Fox' > b1.dat$`Kit Fox`,b1.dat$`Gray Fox`,b1.dat$`Kit Fox`))
table(b1.dat$`Kit Fox`)
b1.dat$`Kit Fox` <- ifelse(b1.dat$'Gray Fox' > b1.dat$`Kit Fox`,b1.dat$`Gray Fox`,b1.dat$`Kit Fox`)

load("output/allspecies.zerofilled.rebait.06162023.B2.RData")
b2.dat <- allspecies.zerofilled;rm(allspecies.zerofilled)

load("output/allspecies.zerofilled.rebait.06132023.A2.RData")
a2.dat <- allspecies.zerofilled;rm(allspecies.zerofilled)

#pick 90 days each
summary(a1.dat$Date) #most cameras working 18909-19016
a1.dat <- a1.dat[which(a1.dat$Date < as.Date("2022-01-08") & a1.dat$Date > as.Date("2021-10-09")),]
max(a1.dat$Date) -min(a1.dat$Date)

summary(a2.dat$Date); max(a2.dat$Date) -min(a2.dat$Date)

summary(b2.dat$Date); max(b2.dat$Date) -min(b2.dat$Date)
b2.dat$censored
b2.dat <- b2.dat[which(b2.dat$Date <= as.Date("2022-11-05")),]

summary(b1.dat$Date); max(b1.dat$Date) -min(b1.dat$Date)
b1.dat <- b1.dat[which(b1.dat$Date <= as.Date("2022-05-22")),]
summary(b1.dat$Date); max(b1.dat$Date) -min(b1.dat$Date)

#total trap days
sum(table(a2.dat$`White-tailed Antelope Ground Squirrel`))
sum(table(a1.dat$`White-tailed Antelope Ground Squirrel`))
sum(table(b2.dat$`White-tailed Antelope Ground Squirrel`))
sum(table(b1.dat$`White-tailed Antelope Ground Squirrel`))


a1.dat$survey <- "a1"

a2.dat$survey <- "a2"
b1.dat$survey <- "b1"
b2.dat$survey <- "b2"


spp.c <- c("Kit Fox","Coyote","Bobcat","Kangaroo Rat","Jackrabbit","White-tailed Antelope Ground Squirrel","Cottontail Rabbit","Badger","Raven")

detectionDaysA1 <- cbind.data.frame(detectionDays=colSums(a1.dat[,spp.c],na.rm=T),survey="A1")
detectionDaysA2 <- cbind.data.frame(detectionDays=colSums(a2.dat[,spp.c],na.rm=T),survey="A2")
detectionDaysB1 <- cbind.data.frame(detectionDays=colSums(b1.dat[,spp.c],na.rm=T),survey="B1")
detectionDaysB2 <- cbind.data.frame(detectionDays=colSums(b2.dat[,spp.c],na.rm=T),survey="B2")
detectionDaysA1$species <- rownames(detectionDaysA1)
detectionDaysA2$species <- rownames(detectionDaysA2)
detectionDaysB1$species <- rownames(detectionDaysB1)
detectionDaysB2$species <- rownames(detectionDaysB2)

detectionDays.08082023 <- rbind(detectionDaysA1,detectionDaysA2,detectionDaysB1,detectionDaysB2)
detectionDays.08082023 <- pivot_wider(detectionDays.08082023,names_from = survey,values_from = detectionDays)
write.csv(detectionDays.08082023,file="output/detectionDays.08082023.csv")
survey.c <- c("a1","a2","b1","b2")
dat.all <- bind_rows(a1.dat,a2.dat,b1.dat,b2.dat) %>% select(RelativePath, Date,week, survey,all_of(spp.c))# %>% group_by(as.factor(survey)) %>% summarise(prop = mean('Kit Fox'))

spp.daysDetected <- apply(dat.all[,5:13],2,function(x) table(x))[2,]
spp.daysDetected <- spp.daysDetected[rev(order(spp.daysDetected))]
spp.daysDetected


det.rate.summary <- NULL
occ.mean <- NULL
  for(i in 1:length(spp.c)){ #
  occ.sum <- dat.all %>% group_by(survey,RelativePath) %>% summarise(totals = sum(get(spp.c[i]),na.rm=T)) %>% mutate(occupied = ifelse(totals>0,1,0)) #%>% group_by(survey) %>% mutate(naive.occ = sum(occupied)/n())
  occ.mean.tmp <- occ.sum %>% group_by(survey) %>% summarise(naive.occ = mean(occupied,na.rm=T))
  date.tab <- dat.all %>% group_by(survey) %>% summarise(total.days =max(Date) - min(Date))
   #count_na = sum(is.na(x)))
  occ.mean.tmp <- left_join(occ.mean.tmp,date.tab)
  occ.mean.tmp$species <- spp.c[i]
  occ.mean <- rbind(occ.mean,occ.mean.tmp)


for(j in 1:length(survey.c)){ #
  
det.rate.tmp <- dat.all %>% group_by(survey,RelativePath,week) %>% summarise(det.rate = sum(get(spp.c[i]),na.rm=T)/7,mid.date = mean(Date,na.rm=T)) 
if(survey.c[j] %in% "a1"){
det.rate <- det.rate.tmp[which(det.rate.tmp$survey %in% survey.c[j]),] %>% mutate(week = ifelse(week < 6,week+52,week))
} else{
 det.rate <- det.rate.tmp[which(det.rate.tmp$survey %in% survey.c[j]),]
}

png(paste("graphics/detection.rate",spp.c[i],survey.c[j],"06.20.2023.png",sep="."),width = 11,height=8,units = "in",res = 600)
if(survey.c[j] %in% "a1"){
ggp <- ggplot(det.rate,aes(x=week,y=det.rate)) +
  geom_bar(stat='identity')+
  ylab("weekly det rate")+
  facet_wrap(~RelativePath)+
  ylim(0,1)+
  ggtitle(paste(spp.c[i],"Detection Rate Per Week Based on Daily Detections (note: Jan weeks are 53-56)",sep= " "))
} else{
 ggp<- ggplot(det.rate,aes(x=week,y=det.rate)) +
    geom_bar(stat='identity')+
    ylab("weekly det rate")+
    facet_wrap(~RelativePath)+
   ylim(0,1)+
   ggtitle(paste(spp.c[i],"Detection Rate Per Week Based on Daily Detections",sep= " "))
}
print(ggp)
dev.off()
}
  
  det.rate.tmp.camera <- dat.all %>% group_by(survey,RelativePath) %>% summarise(det.rate = mean(get(spp.c[i]),na.rm=T)) 
  
det.rate.summary.tmp <- det.rate.tmp.camera %>% group_by(survey) %>% summarise(mean.det.rate = mean(det.rate,na.rm=T),sd.det.rate = sd(det.rate,na.rm=T))

det.rate.summary.tmp$species <- spp.c[i]
det.rate.summary <- rbind(det.rate.summary,det.rate.summary.tmp)

   }

trapdays <- cbind.data.frame(survey=c("a1","a2","b1","b2"),trapdays =c(sum(table(a1.dat$`White-tailed Antelope Ground Squirrel`)),
                 sum(table(a2.dat$`White-tailed Antelope Ground Squirrel`)),
                 sum(table(b1.dat$`White-tailed Antelope Ground Squirrel`)),
                 sum(table(b2.dat$`White-tailed Antelope Ground Squirrel`))))
det.rate.summary[,2:3] <- round(det.rate.summary[,2:3],2)
write.csv(det.rate.summary,file="output/detection.rate.summary_June20th2023.csv")
occ.mean
occ.mean <- merge(occ.mean,trapdays,by="survey",all.x=T,all.y=T)
occ.mean <- occ.mean %>% select(-total.days) %>% mutate(naive.occ = round(naive.occ,3))

occ.mean.w <- occ.mean %>% select(-trapdays) %>% pivot_wider(names_from = "survey",values_from = "naive.occ")

write.csv(occ.mean.w, file="output/naive.occupancy_June20th2023.csv")
write.csv(trapdays, file="output/trapdays_June20th2023.csv")

