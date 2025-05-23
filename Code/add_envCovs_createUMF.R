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


