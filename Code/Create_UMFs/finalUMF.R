###############################################################################
###                   ADD HABITAT TO FINAL UMF                              ###
###############################################################################

# written by Annie Kellner for CEMML (Edwards AFB Mojave Ground Squirrel Occupancy)
# 10-02-2025


library(tidyverse)
library(unmarked)


##    ----    LOAD AND PREP DATA    ----    ##

envCams <- readRDS("./Data/Spatial/camCoords_withEnv_05262025.Rds") # dated DF has Camera_Name instead of 'Camera Trap Name'

envCams <- envCams %>%
  select(Camera_Name, 
         `Study Site`, 
         `Trap Name`, 
         Assigned_Longitude, 
         Assigned_Latitude,
         #Raster_Habitat_Class,
         shp_Habitat_Class,
         Dist_to_Stream)

#saveRDS(envCams, file = "./Data/Covariates/envCams_allCams_shpHabitat_20251003.Rds")

adultsOnly <- readRDS("./Data/Detection/Derived/Detection_NAs_indCams_adultsOnly.Rds") 

dateDF <- readRDS("./Data/Detection/Derived/finalDateDF_20251002.Rds") # save date DF with Season variable

# Join Joshua Tree Woodland and Desert Scrub

JTree <- JTree %>%
  rename(JTree = Raster_Habitat_Class) %>%
  select(Camera_Name, JTree) 

scrub <- scrub %>%
  rename(Desert_Scrub = Raster_Habitat_Class) %>%
  select(Camera_Name, Desert_Scrub)


# Join Jtree and Scrub variables with envCovs DF

envCams <- envCams %>%
  left_join(JTree) %>%
  left_join(scrub)

envCams <- envCams %>%
  mutate(JTree = ifelse(JTree == "Joshua Tree Woodland", 1, 0)) %>%
  mutate(Desert_Scrub = ifelse(Desert_Scrub == "Desert Scrub", 1, 0))


##    ---   JOIN WITH DETECTION DATA    ---   ##


adults_only_ind_cams <- adultsOnly %>%
  left_join(envCams, by = "Camera_Name")

# Rename columns to get dates

adults_only_ind_cams <- adults_only_ind_cams %>%
  rename_with(~paste0(., "/2024"), 2:111)


# Scale Distance to Stream

scDist <- as.vector(scale(adults_only_ind_cams$Dist_to_Stream))

adults_only_ind_cams$DistScaled <- scDist

# Remove inoperable cameras

adults_only_ind_cams <- adults_only_ind_cams %>%
  filter(!(Camera_Name == "AFRL-NW-05" | 
             Camera_Name == "HQA-38-03"))

#saveRDS(adults_only_ind_cams, file = "./Data/finalDF_habitat_camsDeleted_20251002.Rds")


##    --    CREATE FINAL UMF    --    ##


## Create y

y <- adults_only_ind_cams[,2:111]


## Site Covs

siteCovs <- data.frame(
  Dist_to_Stream = as.numeric(adults_only_ind_cams$Dist_to_Stream), # included for mapping
  Dist_Scaled = adults_only_ind_cams$DistScaled,
  Camera_Name = adults_only_ind_cams$Camera_Name, # for ensuring camera-dist association
  Habitat = adults_only_ind_cams$Raster_Habitat_Class,
  JoshTree = adults_only_ind_cams$JTree,
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




umf_FINAL_20251002 <- unmarkedFrameOccu(
  y = y,
  siteCovs = siteCovs,
  obsCovs = obsCovs
)

#saveRDS(umf_FINAL_20251002, file = "./Data/UMFs/umf_FINAL_20251002.Rds")
