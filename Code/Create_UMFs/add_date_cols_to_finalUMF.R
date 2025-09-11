###############################################################################
###                   UMF: ALL VARIABLES                                     ###
###         INDEPENDENT CAMERAS, ADULTS ONLY                                 ###
################################################################################

# 06-12-2015

library(tidyverse)
library(unmarked)

##    ----    LOAD AND PREP DATA    ----    ##

## Dataframes: envCovs & adultsOnly detection DF

envCams <- readRDS("./Data/Spatial/camCoords_withEnv.Rds")

envCams <- envCams %>%
  rename("Camera_Name" = `Camera Trap Name`) %>%
  select(Camera_Name, 
         `Study Site`, 
         `Trap Name`, 
         Assigned_Longitude, 
         Assigned_Latitude,
         #Raster_Habitat_Class,
         #shp_Habitat_Class,
         Dist_to_Stream)

adultsOnly <- readRDS("./Data/Detection/Derived/Detection_NAs_indCams_adultsOnly.Rds")

dateDF <- readRDS("./Data/Detection/Derived/date_df.Rds")

dateDF <- dateDF %>% # categorical classification system for parabolic detection trend
  rename("recentered_date" = "scaled_date") %>%
  rename("recentered_date_squared" = "scaled_date_squared") %>%
  mutate(season = case_when(
    ordinal_date < 128 ~ "early",
    ordinal_date > 127 & ordinal_date < 152 ~ "peak",
    ordinal_date > 151 ~ "late"
  )) %>%
  mutate(ordinal_date_squared = ordinal_date^2) %>%
  mutate(scaled_ordinal = scale(ordinal_date)) %>%
  mutate(scaled_ordinal_squared = scaled_ordinal^2) %>%
  select(original_date, 
         ordinal_date, 
         ordinal_date_squared, 
         scaled_ordinal,
         scaled_ordinal_squared,
         recentered_date, 
         recentered_date_squared,
         season)

dateDF <- dateDF %>%
  mutate(scaled_recentered = scale(recentered_date)) %>%
  mutate(scaled_recentered_squared = scale(recentered_date_squared))

# Join

adults_only_ind_cams <- adultsOnly %>%
  left_join(envCams, by = "Camera_Name")

# Rename columns to get dates

adults_only_ind_cams <- adults_only_ind_cams %>%
  rename_with(~paste0(., "/2024"), 2:111)


# Scale Distance

scDist <- as.vector(scale(adults_only_ind_cams$Dist_to_Stream))

adults_only_ind_cams$DistScaled <- scDist


##    ----    CREATE UMF    ----    ##

## Create y

y <- adults_only_ind_cams[,2:111]


## Site Covs

siteCovs <- data.frame(
  Dist_to_Stream = as.numeric(adults_only_ind_cams$Dist_to_Stream), # included for mapping
  Dist_Scaled = scDist, # scaled data used for model
  Camera_Name = adults_only_ind_cams$Camera_Name # for ensuring camera-dist association
)

#obsCovs <- list(
  #scaled_ordinal = dateDF[,"scaled_ordinal"],
  #scaled_ordinal_sq = dateDF[,"scaled_ordinal_squared"],
  #recentered_date = dateDF[,"recentered_date"],
  #recentered_date_squared = dateDF[,"recentered_date_squared"])




# Observation Covs

obsCovs <- list(
  scaled_ordinal = matrix(dateDF$scaled_ordinal, 
                     nrow = nrow(y), 
                     ncol = ncol(y), 
                     byrow = TRUE),
  scaled_ord_squared = matrix(dateDF$ordinal_date_squared, 
                      nrow = nrow(y),
                      ncol = ncol(y),
                      byrow = TRUE),
  recentered_date = matrix(dateDF$recentered_date,
                           nrow = nrow(y),
                           ncol = ncol(y),
                           byrow = TRUE),
  recentered_date_squared = matrix(dateDF$recentered_date_squared,
                                   nrow = nrow(y),
                                   ncol = ncol(y),
                                   byrow = TRUE),
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

  
  

umf_09112025 <- unmarkedFrameOccu(
  y = y,
  siteCovs = siteCovs,
  obsCovs = obsCovs
)

#saveRDS(umf_09112025, file = "./Data/UMFs/umf_09112015.Rds")

