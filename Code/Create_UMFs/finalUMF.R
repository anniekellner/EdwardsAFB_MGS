###############################################################################
###                   ADD HABITAT TO FINAL UMF                              ###
###############################################################################

# written by Annie Kellner for CEMML (Edwards AFB Mojave Ground Squirrel Occupancy)
# 10-02-2025


library(tidyverse)
library(unmarked)

##    ----    LOAD DATA    ----    ##

## Dataframes: envCovs (all cams) & adultsOnly detection DF

envCams_all <- readRDS("~/Repos/MGS/Data/Covariates/envCams_allCams_shpHabitat_20251003.Rds")

adultsOnly <- readRDS("./Data/Detection/Derived/Detection_NAs_indCams_adultsOnly.Rds")

dateDF <- readRDS("~/Repos/MGS/Data/Detection/Derived/finalDateDF_20251002.Rds")


##    --    INDEPENDENT CAMERAS ONLY    --    ##


adults_only_ind_cams <- adultsOnly %>%
  left_join(envCams_all, by = "Camera_Name")

# Rename columns to get dates

adults_only_ind_cams <- adults_only_ind_cams %>%
  rename_with(~paste0(., "/2024"), 2:111)


##    --    RECLASSIFY HABITAT    --    ##

# using LandCover_A shapefile instead of Raster image

# Desert Scrub

adults_only_ind_cams <- adults_only_ind_cams %>%
  mutate(Desert_Scrub = ifelse(str_detect(shp_Habitat_Class, "Scrub"), 1, 0)) 

# Joshua Tree Woodland

adults_only_ind_cams <- adults_only_ind_cams %>%
  mutate(JoshTree = ifelse(shp_Habitat_Class == "joshuaTreeWoodland", 1, 0)) 


##    --    ADD SCALED DISTANCE VARIABLE    --    ##

scDist <- as.vector(scale(adults_only_ind_cams$Dist_to_Stream))

adults_only_ind_cams$DistScaled <- scDist


##    --    REMOVE INOPERABLE CAMERAS   --    ##

adults_only_ind_cams <- adults_only_ind_cams %>%
  filter(!(Camera_Name == "AFRL-NW-05" | 
             Camera_Name == "HQA-38-03"))

#saveRDS(adults_only_ind_cams, file = "./Data/finalDF_shpHabitat_camsDeleted_20251003.Rds")



##    --    CREATE FINAL UMF    --    ##


## Create y

y <- adults_only_ind_cams[,2:111]


## Site Covs

siteCovs <- data.frame(
  Dist_to_Stream = as.numeric(adults_only_ind_cams$Dist_to_Stream), # included for mapping
  Dist_Scaled = adults_only_ind_cams$DistScaled,
  Camera_Name = adults_only_ind_cams$Camera_Name, # for ensuring camera-dist association
  Habitat = adults_only_ind_cams$shp_Habitat_Class,
  JoshTree = adults_only_ind_cams$JoshTree,
  DesertScrub = adults_only_ind_cams$Desert_Scrub
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


##    --    FINAL UMF   --    ##

umf_FINAL_20251003 <- unmarkedFrameOccu(
  y = y,
  siteCovs = siteCovs,
  obsCovs = obsCovs
)

#saveRDS(umf_FINAL_20251003, file = "./Data/UMFs/umf_FINAL_20251003.Rds")
