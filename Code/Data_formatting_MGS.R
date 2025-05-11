##########################################################################
###       DATA FORMATTING                                             ####
##########################################################################

library(unmarked)
library(tidyverse)


rm(list = ls())

##  ----  LOAD DATA   ----    ##

det <- Occupancy_All_MGS_Data
cams <- Occupancy_Operational_Camera_Days_All_MGS_Data

# save as R objects

saveRDS(det, file = './Data/detections.Rds')
saveRDS(cams, file = './Data/camera_operation.Rds')


rm(Occupancy_All_MGS_Data)
rm(Occupancy_Operational_Camera_Days_All_MGS_Data)

# checking to see whether camera names are identical. They are.

setdiff(cams$Camera_Name, det$Camera_Name)  

# Check if they're identical
all.equal(det$Camera_Name, cams$Camera_Name) # TRUE

# If you need to sort them to ensure same order
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

saveRDS(detNA, file = './Data/detection_withNAs.Rds')

# Tabulate 0's, 1's, NA's



