################################################################################
###                 DISTANCE TO WATER SOURCE                                 ###
################################################################################

library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)

#   ----   LOAD AND PREP DATA   ----    ##

# Water Features

streams <- st_read("./Data/Spatial/Habitat/Water_features/WaterFeature_L.shp") # UTM 11N
wetlands <- st_read("./Data/Spatial/Habitat/Water_features/Wetland_A.shp") # UTM 11N

# Join water features

allWater <- st_union(streams,wetlands) 


# Camera Locations

camCoords <- readRDS("./Data/Spatial/camCoords.Rds")

camCoords <- camCoords %>%
  select(`Camera Trap Name`: Assigned_Longitude)

# Make DF into spatial object

camCoordsSF <- st_as_sf(camCoords, 
                        coords = c("Assigned_Longitude", "Assigned_Latitude"), 
                        crs = "EPSG:4326")

# Reproject camera shp to UTM 11N

camCoordsUTM <- st_transform(camCoordsSF, st_crs(streams))
st_crs(camCoordsUTM) # looks good



##    ----    CALCULATE DISTANCE    ----    ##

# Streams only

nearest_stream <- st_nearest_feature(camCoordsUTM, streams) # Find streams closest to each camera
streamDist <- st_distance(camCoordsUTM, streams[nearest_stream,], by_element = TRUE)

camCoordsUTM$Dist_to_Stream <- streamDist # Incorporate results into camCoordsUTM

# All Water

nearest_water <- st_nearest_feature(camCoordsUTM, allWater)

waterDist <- st_distance(camCoordsUTM, allWater[nearest_water,], by_element = TRUE)

all.equal(streamDist, waterDist) # there are no water features closer than the streams

# Using only streams for the analysis


##    ----    ADD TO DF AND SAVE    ----    ##

head(camCoordsUTM)
camCoordsUTM <- st_drop_geometry(camCoordsUTM)

#saveRDS(camCoordsUTM, file = "./Data/Spatial/Habitat/Derived/dist_to_water.Rds")
