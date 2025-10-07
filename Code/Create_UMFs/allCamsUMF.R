###############################################################################
###                 TOP MODEL ANALYSIS USING ALL CAMERAS                    ###
###############################################################################

# written by Annie Kellner for CEMML, Edwards AFB Mojave Ground Squirrel Project
# 10-6-2025


##    --    LOAD PACKAGES   --    ##

library(tidyverse)
library(unmarked)


##    --    LOAD DATA   --    ##

detAll <- readRDS("~/Repos/MGS/Data/Detection/Derived/detection_withNAs_05112024.Rds") # Detection
allCams <- readRDS("~/Repos/MGS/Data/Covariates/envCams_allCams_shpHabitat_20251003.Rds") # Lat/lon/habitat/distance to stream