###############################################################################
###               TO SCALE OR NOT TO SCALE RECENTERED DATES                 ###
###############################################################################

# Annie Kellner for CEMML, Mojave Ground Squirrel Occupancy Modeling (Edwards AFB)
# 9-17-2025

library(tidyverse)
library(unmarked)


##  --  LOAD DATA   --  ##

umf <- readRDS("~/Repos/MGS/Data/UMFs/umf_recentered_scaled_dates_09172025.Rds")

finalDF <- readRDS("./Data/indCams_adults_envCovs_06022025.Rds")

# Filter out cameras with no data BEFORE any analysis
finalDF <- finalDF %>%
  filter(!(Camera_Name == "AFRL-NW-05" | 
             Camera_Name == "HQA-38-03")) # 2 cameras without data


##  --  MODELING  --  ##

noScale <- occu(~ recentered + recentered_squared + season ~ Dist_Scaled,
           data = umf,
           linkPsi = "logit")

scaled <- occu(~ scaled_recentered + scaled_recentered_squared + season ~ Dist_Scaled,
               data = umf,
               linkPsi = "logit")

summary(noScale)
summary(scaled)

## Clearly it is far better to scale the recentered dates!!!!!