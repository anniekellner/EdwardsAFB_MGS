#DESCRIPTION v4 updates the third version  by 
#using known start and end dates for each camera from Adam
#this also replaces 0 in the wide data with NAs where a camera was known to be offline


#DATE 6/6/2023
#UPDATE 6/28/2023 I Updated so that only censor dates within the start and end dates of a survey are merged (B1 - at least - had censor dates outside the start and end dates)
#TO DO
#finish updating the A2 code / 6/13/2023

#libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(readxl)
rm(list=ls())


#survey A1
load("input/A1.cam_survey.nona.06132023.RData")
#censor dates; for A cameras, and looks like A1
CameraSurveyCensorDates06052023 <- read_excel("Task 1 - Camera Trapping Data-20230427T203950Z-001/Task 1 - Camera Trapping Data/CameraSurveyCensorDates06052023.xlsx")
colnames(CameraSurveyCensorDates06052023)
colnames(CameraSurveyCensorDates06052023)[2:3] <- c("start","end")

#start end dates A cameras
CameraSurveyDates_06052023 <- read_excel("Task 1 - Camera Trapping Data-20230427T203950Z-001/Task 1 - Camera Trapping Data/CameraSurveyDates_06052023.xlsx")
colnames(CameraSurveyDates_06052023)
colnames(CameraSurveyDates_06052023)[2:3] <- c("start","end")

#DIDN'T WANT TO CODE THE FILTER BY START VS END FOR DIFFERENT SURVEYS, SO NOW JUST NEED TO MANUALLY STEP THROUGH THE FUNCTION
#allspecies.w <- zerofillfcn(survey.nona = A1.cam_survey.nona,cameraDates = CameraSurveyDates_06052023,cameraCensor = CameraSurveyCensorDates06052023)
allspecies.zerofilled <- allspecies.w
lookatcensor <- allspecies.zerofilled[which(allspecies.zerofilled$RelativePath %in% "A_120_FL"),]#i think just as an example?
lookatcensor[100:127,c(1:8,40:43)]#should show NAs where censored
head(allspecies.zerofilled)

save(allspecies.zerofilled,file="input/A1.allspecies.zerofilled.20231306.RData")
rm(allspecies.w);rm(allspecies.zerofilled)


#SURVEY A2
CameraSurveyCensorDates.a2 <- read_excel("Task 1 - Camera Trapping Data-20230427T203950Z-001/Task 1 - Camera Trapping Data/CameraSurveyCensorDates06052023.xlsx",sheet=2)
CameraSurveyDates_06052023 <- read_excel("Task 1 - Camera Trapping Data-20230427T203950Z-001/Task 1 - Camera Trapping Data/CameraSurveyDates_06052023.xlsx",sheet=2)
colnames(CameraSurveyDates_06052023)
CameraSurveyDates_06052023$start <-  date(CameraSurveyDates_06052023$`A2 Start Date`)
CameraSurveyDates_06052023$end <-  date(CameraSurveyDates_06052023$`A2 End Date`)
head(CameraSurveyDates_06052023)
CameraSurveyDates_06052023 <- CameraSurveyDates_06052023[,c(1,4,5)]
load("input/A2.cam_survey.nona.06132023.RData")
cameraCensor<- CameraSurveyCensorDates.a2;rm(CameraSurveyCensorDates.a2)
cameraDates <- CameraSurveyDates_06052023;rm(CameraSurveyDates_06052023)
print(length(table(A2.cam_survey.nona$RelativePath)))

A2.cam_survey.nona$Date <- date(A2.cam_survey.nona$DateTime)
survey.nona <- merge(A2.cam_survey.nona, cameraDates,by.x="RelativePath",by.y="Camera Name",all.x=T,all.y=F)
survey.nona$end <- date(survey.nona$`A2 End Date`)
survey.nona$start <- date(survey.nona$`A2 Start Date`)
head(survey.nona)
survey.nona <- survey.nona[which(survey.nona$Date <= survey.nona$end),]#SOMETIMES THIS FILTER WILL NEED TO BE THE END DATE, E.G., A2
survey.nona <- survey.nona[which(survey.nona$Date >= survey.nona$start),]
print(summary(survey.nona$Date))

as.data.frame(cameraCensor)
colnames(cameraCensor)[2:3] <- c("start","end")
#survey.nona,cameraDates,cameraCensor
allspecies.w <- zerofillfcn(survey.nona = A2.cam_survey.nona,cameraDates = cameraDates,cameraCensor = cameraCensor)
allspecies.w[which(allspecies.w$censored %in% 'y'),]#make sure NAs in the other columns
allspecies.zerofilled <- allspecies.w
save(allspecies.zerofilled,file="input/A2.allspecies.zerofilled.06132023.RData")
rm(allspecies.w);rm(allspecies.zerofilled)

#survey B1
load("input/B1.cam_survey.nona.06142023.RData")
CameraSurveyCensorDates.b1 <- read_excel("Task 1 - Camera Trapping Data-20230427T203950Z-001/Task 1 - Camera Trapping Data/CameraSurveyCensorDates06152023.xlsx",sheet=3)
CameraSurveyDates_b1 <- read_excel("Task 1 - Camera Trapping Data-20230427T203950Z-001/Task 1 - Camera Trapping Data/CameraSurveyDates_06152023.xlsx",sheet=3)
colnames(CameraSurveyDates_b1)
CameraSurveyDates_b1$start <-  date(CameraSurveyDates_b1$`B1 Start Date`)
CameraSurveyDates_b1$end <-  date(CameraSurveyDates_b1$`B1 End Date`)
head(CameraSurveyDates_b1)
CameraSurveyDates_b1 <- CameraSurveyDates_b1[,c(1,5,6)]

cameraCensor<- CameraSurveyCensorDates.b1;rm(CameraSurveyCensorDates.b1)
colnames(cameraCensor)[2:3] <- c("start","end")

cameraDates <- CameraSurveyDates_b1;rm(CameraSurveyDates_b1)
cameraDates <- cameraDates[!is.na(cameraDates$start),]
print(length(table(B1.cam_survey.nona$RelativePath)))

B1.cam_survey.nona$Date <- date(B1.cam_survey.nona$DateTime)
survey.nona <- merge(B1.cam_survey.nona, cameraDates,by.x="RelativePath",by.y="Camera Name",all.x=T,all.y=F)
head(survey.nona)
survey.nona <- survey.nona[which(survey.nona$Date <= survey.nona$end),]#SOMETIMES THIS FILTER WILL NEED TO BE THE END DATE, E.G., A2
survey.nona <- survey.nona[which(survey.nona$Date >= survey.nona$start),]
print(summary(survey.nona$Date))

table(B1.cam_survey.nona$RelativePath)#B_281_CL is removed
allspecies.w <- zerofillfcn(survey.nona = B1.cam_survey.nona,cameraDates = cameraDates,cameraCensor=cameraCensor)
allspecies.zerofilled <- allspecies.w
summary(allspecies.zerofilled$Date)
save(allspecies.zerofilled,file="input/B1.allspecies.zerofilled.06152023.RData")
rm(allspecies.w);rm(allspecies.zerofilled)

#survey B2
load("input/B2.cam_survey.nona.06162023.RData")
CameraSurveyCensorDates.b2 <- read_excel("Task 1 - Camera Trapping Data-20230427T203950Z-001/Task 1 - Camera Trapping Data/CameraSurveyCensorDates06162023.xlsx",sheet=4)
CameraSurveyDates_b2 <- read_excel("Task 1 - Camera Trapping Data-20230427T203950Z-001/Task 1 - Camera Trapping Data/CameraSurveyDates_06162023.xlsx",sheet=4)

colnames(CameraSurveyDates_b2)
CameraSurveyDates_b2$start <-  date(CameraSurveyDates_b2$`B2 Start Date`)
CameraSurveyDates_b2$end <-  date(CameraSurveyDates_b2$`B2 End Date`)
head(CameraSurveyDates_b2)
CameraSurveyDates_b2 <- CameraSurveyDates_b2[,c(1,4,5)]

cameraCensor<- CameraSurveyCensorDates.b2;rm(CameraSurveyCensorDates.b2)
colnames(cameraCensor)[2:3] <- c("start","end")

cameraDates <- CameraSurveyDates_b2;rm(CameraSurveyDates_b2)
cameraDates <- cameraDates[!is.na(cameraDates$start),]
print(length(table(B2.cam_survey.nona$RelativePath)))

B2.cam_survey.nona$Date <- date(B2.cam_survey.nona$DateTime)
survey.nona <- merge(B2.cam_survey.nona, cameraDates,by.x="RelativePath",by.y="Camera Name",all.x=T,all.y=F)
head(survey.nona)
survey.nona <- survey.nona[which(survey.nona$Date <= survey.nona$end),]#SOMETIMES THIS FILTER WILL NEED TO BE THE END DATE, E.G., A2
survey.nona <- survey.nona[which(survey.nona$Date >= survey.nona$start),]
print(summary(survey.nona$Date))

table(B2.cam_survey.nona$RelativePath)#B_281_CL is removed
allspecies.w <- zerofillfcn(survey.nona = B2.cam_survey.nona,cameraDates = cameraDates,cameraCensor=cameraCensor)
allspecies.zerofilled <- allspecies.w
save(allspecies.zerofilled,file="input/B2.allspecies.zerofilled.06162023.RData")
rm(allspecies.w);rm(allspecies.zerofilled)

#######
##FUNCTION
#######
zerofillfcn <- function(survey.nona,cameraDates,cameraCensor){
#number of cameras in this file, will need to crosscheck with the original file if I didn't lose any
print(length(table(survey.nona$RelativePath)))

#survey.nona$Date <- date(survey.nona$DateTime)
survey.nona <- merge(survey.nona, cameraDates,by.x="RelativePath",by.y="Camera Name",all.x=T,all.y=F)
survey.nona <- survey.nona[which(survey.nona$Date >= survey.nona$start),]
survey.nona <- survey.nona[which(survey.nona$Date <= survey.nona$end),]
print(summary(survey.nona$Date))

print(head(survey.nona))
print(head(as.data.frame(cameraDates),30))
print(tail(as.data.frame(cameraDates),70))
cameraDates <- as.data.frame(cameraDates)
#create template with all unique combos of day and camera; include column for week to mark a period of 7 days (or can change to alter the sampling 'occassion')
template.allcams <- NULL
for(i in 1:nrow(cameraDates)){#
  date.byday <- data.frame(Date=date(seq.Date(as.Date(cameraDates$start[i]),as.Date(cameraDates$end[i]),by="day")))
  cbind.data.frame(date.byday,rep(cameraDates[i,"Camera Name"],length(date.byday)))
  date.byday$RelativePath <- cameraDates[i,"Camera Name"]
  
  template.allcams <- rbind(template.allcams,date.byday)
}

template.censoredcams <- NULL
cameraCensor <- as.data.frame(cameraCensor)
for(j in 1:nrow(cameraCensor)){
  date.byday.Censor.tmp <- data.frame(Date=date(seq.Date(as.Date(cameraCensor$start[j]),as.Date(cameraCensor$end[j]),by="day")))
  date.byday.Censor.tmp$RelativePath <- rep(cameraCensor[j,1],nrow(date.byday.Censor.tmp))
  template.censoredcams <- rbind(template.censoredcams,date.byday.Censor.tmp)
}
  template.censoredcams$censored <- "y"
template.allcams <- merge(template.allcams,template.censoredcams,by=c("Date","RelativePath"),all.x=T,all.y=F)
##filter to one observation per species per day
allspecies <- survey.nona %>% group_by(RelativePath, Date, species) %>% filter(row_number()==1) %>% ungroup()
allspecies$occurred <- 1
allspecies2 <- merge(allspecies,template.allcams,by=c("RelativePath","Date"),all.x=T,all.y=T)
print(head(as.data.frame(allspecies2),50))
print("number of days where nothing was detected for a camera")
print(nrow(allspecies2[is.na(allspecies2$occurred),]))#the number of days where nothing was detected for a camera: 10,145


allspecies2[which(allspecies2$species == ""),]#how many rows have blanks in the species columns
which(allspecies2$species == "")
#allspecies2t <- allspecies2[!which(allspecies2$species == ""),]
allspecies.w <- allspecies2 %>% select(RelativePath, Date, species,occurred,censored) %>% 
  pivot_wider(names_from = "species",values_from = "occurred",values_fill = 0) %>% select(-c("NA","Unknown"))

colnames(allspecies.w)
head(allspecies.w)

print("should match or check")
#print(nrow(test));print(nrow(allspecies.w))
print(head(as.data.frame(allspecies.w),50))

allspecies.w[allspecies.w$censored %in% 'y',4:ncol(allspecies.w)] <- NA

#allspecies.w$dayofwk <- wday(allspecies.w$Date,week_start =1)#monday = 1
allspecies.w$week <- isoweek(allspecies.w$Date)
#allspecies.w %>% group_by(RelativePath) %>% arrange(Date, .by_group = T) %>% mutate(week = rep(1))

return(allspecies.w)

}

