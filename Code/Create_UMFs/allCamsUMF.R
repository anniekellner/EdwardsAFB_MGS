###############################################################################
###                 TOP MODEL ANALYSIS USING ALL CAMERAS                    ###
###############################################################################

# written by Annie Kellner for CEMML, Edwards AFB Mojave Ground Squirrel Project
# 10-6-2025


##    --    LOAD PACKAGES   --    ##

library(tidyverse)
library(unmarked)


##    --    LOAD AND PREP DATA   --    ##

detAll <- readRDS("~/Repos/MGS/Data/Detection/Derived/detection_withNAs_05112024.Rds") # Detection
allCams <- readRDS("~/Repos/MGS/Data/Covariates/envCams_allCams_shpHabitat_20251003.Rds") # Lat/lon/habitat/distance to stream
dateDF <- readRDS("~/Repos/MGS/Data/Detection/Derived/finalDateDF_20251002.Rds")

# Join

detAll <- detAll %>%
  left_join(allCams, by = "Camera_Name") 


# Rename columns to get dates

detAll <- detAll %>% 
  rename_with(~paste0(., "/2024"), 2:111)

# Scale distance to stream

scDist <- as.vector(scale(detAll$Dist_to_Stream))

detAll$DistScaled <- scDist


# Remove inoperable cameras

detAll <- detAll %>%
  filter(!(Camera_Name == "AFRL-NW-05" | Camera_Name == "HQA-38-03"))

#saveRDS(detAll, file = "./Data/allCamsDF_topModel_20251008.Rds")



##    --    CREATE FINAL UMF    --    ##


## Create y

y <- detAll[,2:111]

## Site Covs

siteCovs <- data.frame(
  Dist_Scaled = detAll$DistScaled,
  Site = detAll$Camera_Name # for ensuring camera-dist association
)

# Observation Covs

obsCovs <- list(
  scaled_recentered = matrix(dateDF$scaled_recentered,
                             nrow = nrow(y),
                             ncol = ncol(y),
                             byrow = TRUE),
  scaled_recentered_squared = matrix(dateDF$scaled_recentered_squared,
                                     nrow = nrow(y),
                                     ncol = ncol(y),
                                     byrow = TRUE),
  season = matrix(dateDF$season,
                  nrow = nrow(y),
                  ncol = ncol(y),
                  byrow = TRUE))

# UMF

umf_allCams_20251008 <- unmarkedFrameOccu(
  y = y,
  siteCovs = siteCovs,
  obsCovs = obsCovs
)

saveRDS(umf_allCams_20251008, file = "./Data/UMFs/umf_allCams_20251008.Rds")
