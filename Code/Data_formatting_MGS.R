##########################################################################
###       DATA FORMATTING                                             ####
##########################################################################

library(unmarked)
library(tidyverse)


##  ----  LOAD DATA   ----    ##

det <- Occupancy_All_MGS_Data; rm(Occupancy_All_MGS_Data)
cams <- Occupancy_Operational_Camera_Days_All_MGS_Data; rm(Occupancy_Operational_Camera_Days_All_MGS_Data)

# checking to see whether camera names are identical. They are.

setdiff(cams$Camera_Name, det$Camera_Name)  


##  -- CHANGE COLUMN NAMES TO DATE FORMAT -- ##

colNames_det <- names(det)
colNames_cams <- names(cams)









##  --  SUB NA FOR NON-OPERATIONAL DAYS --  ##

detNA <- det %>%
  mutate(across(everything(), ~ifelse(cams == 0, NA, .)))

det %>%
  pivot_longer(everything()) %>%
  count(value) %>%
  arrange(value)

