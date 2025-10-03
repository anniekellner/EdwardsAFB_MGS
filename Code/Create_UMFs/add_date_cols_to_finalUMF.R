###############################################################################
###                   UMF: ALL VARIABLES                                     ###
###         INDEPENDENT CAMERAS, ADULTS ONLY                                 ###
################################################################################

# 06-12-2015

library(tidyverse)
library(unmarked)

##    ----    LOAD DATA    ----    ##

## Dataframes: envCovs (all cams) & adultsOnly detection DF

envCams_all <- readRDS("~/Repos/MGS/Data/Covariates/envCams_allCams_shpHabitat_20251003.Rds")

adultsOnly <- readRDS("./Data/Detection/Derived/Detection_NAs_indCams_adultsOnly.Rds")

dateDF <- readRDS("~/Repos/MGS/Data/Detection/Derived/dateDF_recentered_scaled_09172025.Rds")


##    --    INDEPENDENT CAMERAS ONLY    --    ##


adults_only_ind_cams <- adultsOnly %>%
  left_join(envCams, by = "Camera_Name")

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


##    ----    CREATE UMF    ----    ##

## Create y

y <- adults_only_ind_cams[,2:111]


## Site Covs

siteCovs <- data.frame(
  Dist_to_Stream = as.numeric(adults_only_ind_cams$Dist_to_Stream), # included for mapping
  Dist_Scaled = scDist, # scaled data used for model
  Camera_Name = adults_only_ind_cams$Camera_Name, # for ensuring camera-dist association
  Habitat = adults_only_ind_cams$Raster_Habitat_Class
)


# Observation Covs

obsCovs <- list(
  #recentered = matrix(dateDF$recentered_date, 
                     #nrow = nrow(y), 
                     #ncol = ncol(y), 
                     #byrow = TRUE),
  #recentered_squared = matrix(dateDF$recentered_date_squared, 
                      #nrow = nrow(y),
                      #ncol = ncol(y),
                      #byrow = TRUE),
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

  
  

umf_FINAL_20251002 <- unmarkedFrameOccu(
  y = y,
  siteCovs = siteCovs,
  obsCovs = obsCovs
)

#saveRDS(umf_FINAL_20251002, file = "./Data/UMFs/umf_FINAL_20251002.Rds")

