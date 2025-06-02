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



saveRDS(adults_only_ind_cams, "./Data/indCams_adults_envCovs_ordinalDateCols_06012025.Rds")


##    ----      CREATE UMF  ----    ##

# Create y

y_AK <- adults_only_ind_cams[,2:111]  # 0/1 observations

# Site Covs

scDist <- as.vector(scale(adults_only_ind_cams$Dist_to_Stream))

adults_only_ind_cams$DistScaled <- scDist


siteCovs_AK <- data.frame(
  Dist_to_Stream = as.numeric(adults_only_ind_cams$Dist_to_Stream), # included for mapping
  Dist_Scaled = scDist, # scaled data used for model
  Camera_Name = adults_only_ind_cams$Camera_Name # for ensuring camera-dist association
)


# ObsCovs

dates <- colnames(adults_only_ind_cams)[2:111]
dates <- mdy(dates)

# Ordinal Dates
ordinal <- yday(dates)
scOrdinal <- as.vector(scale(ordinal))



# obsCovs

obsCovs_AK <- obsCovs <- list(
  scOrdinal = matrix(scOrdinal, 
                     nrow = nrow(y_AK), 
                     ncol = ncol(y_AK), 
                     byrow = TRUE),
  scOrdinal2 = matrix(scOrdinal^2, 
                      nrow = nrow(y_AK),
                      ncol = ncol(y_AK),
                      byrow = TRUE))


umf_AK <- unmarkedFrameOccu(
  y = y_AK,
  siteCovs = siteCovs_AK,
  obsCovs = obsCovs_AK
)

#saveRDS(umf_AK, file = "./Data/UMFs/umf_CORRECT_06022025.Rds")
glimpse(adults_only_ind_cams)
#saveRDS(adults_only_ind_cams, file = "./Data/indCams_adults_envCovs_06022025.Rds")
