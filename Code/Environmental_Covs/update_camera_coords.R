################################################################################
###           UPDATED CAMERA LOCATIONS                                        ###
################################################################################

# 5-28-2025
# Revised lat/lon coordinates for cameras

library(sf)
library(tidyverse)


##    ----    LOAD AND PREP DATA   ----    ##

## Load

streams <- st_read("./Data/Spatial/Habitat/Water_features/WaterFeature_L.shp") # UTM 11N

camsAll <- read_csv("./Data/Spatial/Final_Camera_Locations_FromGIS_AD.csv")

indCams <- readRDS("./Data/Detection/Derived/Detection_NAs_indCams_adultsOnly.Rds")


## Prep

## Use independent cameras only

indCams <- indCams$Camera_Name

camsInd <- camsAll %>%
  filter(CameraName %in% indCams)

# Make spatial object

camsSF <- st_as_sf(camsInd, coords = c("Longitude", "Latitude"), crs = "EPSG:4326")

plot(st_geometry(camsSF))


camsSF <- st_transform(camsSF, st_crs(streams)) # Project cams to UTM 11N


##    ----    CALCULATE DISTANCE TO NEAREST STREAM LINE SEGMENT   ----    ##

nearest_stream <- st_nearest_feature(camsSF, streams) # Find streams closest to each camera
streamDist <- st_distance(camsSF, streams[nearest_stream,], by_element = TRUE)

camsSF$Dist_to_Stream <- streamDist # Incorporate results into sf object

#st_write(camsSF, "./Results/indCameras_spatial.shp")

indCams_UTM <- st_drop_geometry(camsSF)

#saveRDS(indCams_UTM, file = "./Data/Spatial/Derived/independentCams_DF_dist2stream.Rds")



