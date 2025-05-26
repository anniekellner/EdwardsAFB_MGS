################################################################################
###               ADULTS ONLY; ALL CAMERAS - CREATE UMF                     ###
################################################################################

library(unmarked)
library(tidyverse)



##    ----    LOAD AND PREP DATA    ----    ##

## Dataframes: envCovs & adultsOnly_allCams detection DF

envCams <- readRDS("./Data/Spatial/camCoords_withEnv.Rds")
allCams_adultsOnly <- readRDS("./Data/Detection/Derived/allCams_adultsOnly.Rds")

envCams <- rename(envCams, "Camera_Name" = `Camera Trap Name`)

# saving with renamed column so don't have to keep changing it
#saveRDS(envCams, file = "./Data/Spatial/camCoords_withEnv_05262025.Rds")

# Join

adults_only_all_cams <- allCams_adultsOnly %>%
  left_join(envCams, by = "Camera_Name")

# Rename columns to get dates

adults_only_all_cams <- adults_only_all_cams %>%
  rename_with(~paste0(., "/2024"), 2:111)


##    ----      CREATE UMF  ----    ##

# ObsCovs

dates <- colnames(adults_only_all_cams)[2:111]
dates <- mdy(dates)

# Ordinal Dates
ordinal <- yday(dates)
scOrdinal <- scale(ordinal)

# Create y

y <- as.matrix(adults_only_all_cams[,c(2:111)]) # 0/1 observations

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

siteCovs <- data.frame(
  Habitat_Class = adults_only_all_cams$Raster_Habitat_Class,
  Dist_to_Stream = adults_only_all_cams$Dist_to_Stream,
  camera_name = adults_only_all_cams$Camera_Name
)

umf_adultsOnly_allCams_envCovs <- unmarkedFrameOccu(
  y = y,
  siteCovs = siteCovs,
  obsCovs = obsCovs
)

#saveRDS(umf_adultsOnly_allCams_envCovs, file = "./Data/UMFs/umf_adultsOnly_allCams_envCovs.Rds")
