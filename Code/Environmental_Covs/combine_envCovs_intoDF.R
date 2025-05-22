################################################################################
###                   COMBINE ENVIRONMENTAL COVS INTO DF                     ###
################################################################################

library(tidyverse)


##    ----    LOAD DATA   ----    ##

imgHab <- readRDS("./Data/Spatial/Habitat/Derived/img_habitat.Rds")

shpHab <- readRDS("./Data/Spatial/Habitat/Derived/shp_habitat.Rds")

dist <- readRDS("./Data/Spatial/Habitat/Derived/dist_to_water.Rds")

camCoords <- readRDS("./Data/Spatial/camCoords.Rds")

camCoords <- camCoords %>%
  select(`Camera Trap Name`:Assigned_Longitude)

camCoords <- camCoords %>%
  left_join(imgHab, by = "Camera Trap Name")

dups <- filter(camCoords, `Camera Trap Name` == "1994-N02A-01")