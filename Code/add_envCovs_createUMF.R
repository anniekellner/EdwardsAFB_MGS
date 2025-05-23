################################################################################
###               CREATE UMF WITH ENVIRONMENTAL COVARIATES                   ###
################################################################################

library(tidyverse)
library(unmarked)


##    ----    LOAD AND PREP DATA    ----    ##

## Environmental Covs

envCams <- readRDS("./Data/Spatial/camCoords_withEnv.Rds")

envCams <- envCams %>%
  select(`Camera Trap Name`, 
         `Study Site`, 
         Raster_Habitat_Class, 
         shp_Habitat_Class,
         Dist_to_Stream) %>%
  rename("Camera_Name" = `Camera Trap Name`)

## Detection Data

detAll <- readRDS("./Data/Detection/Derived/detection_withNAs_05112024.Rds")

# Join env data by camera name

detEnv <- detAll %>%
  left_join(envCams)


##    ----    CREATE UMF    ----    ##

## Adjust column names

detEnv <- detEnv %>%
  rename_with(~paste0(., "/2024"), where(is.numeric))

names(detEnv) <- as.character(names(detEnv))

detEnv <- detEnv %>%
  rename("Dist_to_Stream" = "Dist_to_Stream/2024")

# Observation Covariates (obsCovs)

scOrdinal <- readRDS("./Data/Covariates/ordinal_vector_scaled.Rds") # scaled ordinal dates


## Create UMF

y <- as.matrix(detEnv[,c(2:111)]) # 0/1 observations

# Observation Covs

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

siteCovs(list(
  detEnv$Camera_Name,
  
))