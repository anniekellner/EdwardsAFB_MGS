################################################################################
###         ENVIRONMENTAL COVARIATES: HABITAT & WATER FEATURES              ####
################################################################################

library(terra)
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)

# Habitat layer

img <- rast("./Data/Spatial/Habitat/FINAL_CWHR_EAFB_20231103.img")
plot(img)

# Quick look at water features

waterfeat <- st_read("./Data/Spatial/Habitat/Water_features/WaterFeature_L.shp")
plot(waterfeat)

wetland <- st_read("./Data/Spatial/Habitat/Water_features/Wetland_A.shp")
plot(wetland)


##  --- Camera Coord Data: Data Prep  ---   ##

camCoords <- EAFB_MGS_FinalCameraLocations_mf
camNamesXL <- unique(camCoords$`Camera Trap Name`)

allCams <- readRDS("./Data/Detection/Derived/allCams_adultsOnly.Rds")
camNamesRDS <- unique(allCams$Camera_Name)

setdiff(camNamesXL, camNamesRDS)
setdiff(camNamesRDS, camNamesXL) 

# Change names

camCoords <- camCoords %>%
  mutate('Camera Trap Name' = case_when(
    `Camera Trap Name` == "1994-N02-05" ~ "1994-N02A-01",
    `Camera Trap Name` == "HQA-43-07" ~ "HQA-43A-07",
    `Camera Trap Name` == "HQA-43-09" ~ "HQA-43A-09",
    TRUE ~ `Camera Trap Name`)) # Redid setdiff() and looks fine; names are correct

#saveRDS(camCoords, file = "./Data/Spatial/camCoords.Rds")


##  --  Add lat/long to camera DF   --  ##



  

