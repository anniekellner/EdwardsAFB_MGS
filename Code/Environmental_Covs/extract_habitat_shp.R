################################################################################
###               EXTRACT HABITAT: SHAPEFILE                                 ###
################################################################################

# Habitat data extracted from 
  # N:\RStor\CEMML\EdwardsAFB\Mohave_Ground_Squirrel\GIS_and_Monitoring_Data\2024\Analysis\Occupancy\Habitat_layer


library(tidyverse)
library(terra)
library(sf)
library(terra)

##  --  LOAD AND PREP DATA  --  ##

# Habitat layer

hab <- st_read("./Data/Spatial/Habitat/Land_Cover/LandCover_A.shp")
hab <- hab %>%
  select(geometry, VEGETATION, LONGITUDEC, LATITUDECE)

plot(st_geometry(hab))

st_crs(hab) # in UTM 11N

# Cameras

camCoords <- readRDS("./Data/Spatial/camCoords.Rds")

# Make DF into spatial object
camCoordsSF <- st_as_sf(camCoords, 
                        coords = c("Assigned_Longitude", "Assigned_Latitude"), 
                        crs = "EPSG:4326")

