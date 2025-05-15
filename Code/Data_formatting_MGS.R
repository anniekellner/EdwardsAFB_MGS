##########################################################################
###       DATA FORMATTING                                             ####
##########################################################################

library(unmarked)
library(tidyverse)


rm(list = ls())

##  ----  LOAD AND CHECK DATA   ----    ##

detNA <- readRDS("./Data/detection_withNAs_05112024.Rds")

cams <- Occupancy_Operational_Camera_Days_All_MGS_Data

# Remove white space

cams <- cams %>%
  select(where(~!all(is.na(.))))


detNA <- detNA %>% # data in which NA's are already subbed
  select(where(~!all(is.na(.)))) 


# save as R objects

#saveRDS(det, file = './Data/detections.Rds')
#saveRDS(cams, file = './Data/camera_operation.Rds')


rm(Occupancy_All_MGS_Data)
rm(Occupancy_Operational_Camera_Days_All_MGS_Data)

# checking to see whether camera names are identical. They are.

setdiff(cams$Camera_Name, det$Camera_Name)  
all.equal(det$Camera_Name, cams$Camera_Name) # TRUE

# If sorting needed to ensure same order
det2 <- det %>% 
  arrange(Camera_Name)

cams2 <- cams %>% 
  arrange(Camera_Name)

# To see any differences
setdiff(det2$Camera_Name, cams2$Camera_Name)  # Order is identical

rm(det2)
rm(cams2)


##  --  SUB NA FOR NON-OPERATIONAL DAYS --  ##

detNA <- det %>%
  mutate(across(2:ncol(.), function(x) {
    col_name <- cur_column() # current column
    ifelse(cams[[col_name]] == 0, NA, x)
  }))

#saveRDS(detNA, file = './Data/detection_withNAs_05112024.Rds')

# Tabulate 0's, 1's, NA's

detNA %>%
  select(-1) %>%
  pivot_longer(everything()) %>%
  count(value) %>%
  arrange(value)

###   --    COVARIATES  --  ##

# Observation Covariates (obsCovs)

names(detNA)[-1] <- paste0(names(detNA)[-1], "/2024") # add year to column names

dates <- colnames(detNA)[-1]
dates <- mdy(dates)

# Ordinal Dates
ordinal <- yday(dates)

# Quadratic Term

ordinal2 <- ordinal^2

# Scale both to eliminate collinearity

scOrdinal <- scale(ordinal)



##  --- CREATE UMF OBJECT   --- ##

## y = 0/1 observations

# First create y (observations)

y <- as.matrix(detNA[,c(2:ncol(detNA))]) # 0/1 observations


obsCovs <- list(
  scOrdinal = matrix(scOrdinal, 
                  nrow = nrow(y), 
                  ncol = ncol(y), 
                  byrow = TRUE),
  scOrdinal2 = matrix(scOrdinal^2, 
                    nrow = nrow(y),
                    ncol = ncol(y),
                    byrow = TRUE))

# Create siteCovs

siteCovs <- detNA$Camera_Name


# unmarkedFrameOccu call

umf <- unmarkedFrameOccu(
  y = y,
  siteCovs = data.frame(site = siteCovs),
  obsCovs = obsCovs
)

#saveRDS(umf, file = "./Data/umf_05152025.Rds")






