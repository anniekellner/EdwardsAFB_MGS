################################################################################
###                 CREATE UMF: INDEPENDENT CAMS, ADULTS ONLY                ###
################################################################################

library(unmarked)
library(tidyverse)


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
         Raster_Habitat_Class,
         shp_Habitat_Class,
         Dist_to_Stream)

adultsOnly <- readRDS("./Data/Detection/Derived/Detection_NAs_indCams_adultsOnly.Rds")

# Join

adults_only_ind_cams <- adultsOnly %>%
  left_join(envCams, by = "Camera_Name")

# Rename columns to get dates

adults_only_ind_cams <- adults_only_ind_cams %>%
  rename_with(~paste0(., "/2024"), 2:111)


##    ----      CREATE UMF  ----    ##

# ObsCovs

dates <- colnames(adults_only_ind_cams)[2:111]
dates <- mdy(dates)

# Ordinal Dates
ordinal <- yday(dates)
scOrdinal <- scale(ordinal)

# Create y

y <- as.matrix(adults_only_ind_cams[,c(2:111)]) # 0/1 observations

# obsCovs

obsCovs <- list(
  scOrdinal = matrix(scOrdinal, 
                     nrow = nrow(y), 
                     ncol = ncol(y), 
                     byrow = TRUE),
  scOrdinal2 = matrix(scOrdinal^2, 
                      nrow = nrow(y),
                      ncol = ncol(y),
                      byrow = TRUE))

# Site Covs

# Scale Distance to Stream

scDist <- scale(adults_only_ind_cams$Dist_to_Stream)

siteCovs <- data.frame(
  Dist_to_Stream = adults_only_ind_cams$Dist_to_Stream, # included for mapping
  Dist_Scaled = scDist, # for modeling
  Camera_Name = adults_only_ind_cams$Camera_Name # for ensuring camera-dist association
)

umf_FINAL <- unmarkedFrameOccu(
  y = y,
  siteCovs = siteCovs,
  obsCovs = obsCovs
)

#saveRDS(umf_FINAL, file = "./Data/UMFs/umf_FINAL_06012025.Rds")
