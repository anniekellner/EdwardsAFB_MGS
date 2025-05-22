################################################################################
###               EXTRACT HABITAT TYPE                                       ###
################################################################################

library(tidyverse)
library(terra)
library(sf)


##  ----  LOAD AND PREP DATA   ----  ##

## Load Data

camCoords <- readRDS("./Data/Spatial/camCoords.Rds")
#det <- readRDS("./Data/Detection/Derived/allCams_adultsOnly.Rds")

camCoords <- camCoords %>%
  select(`Camera Trap Name`: Assigned_Longitude)

img <- rast("./Data/Spatial/Habitat/CWHR_EAFB_20231103/FINAL_CWHR_EAFB_20231103.img")


## Reproject camera data into UTM 11N (same as raster)

camCoordsLL <- vect(camCoords,
                  geom = c("Assigned_Longitude", "Assigned_Latitude"),
                  crs = "EPSG:4326")

crs(img) # Check raster projection: it's UTM 11N

camCoordsUTM <- project(camCoordsLL, "EPSG:32611") # Reproject camCoords to UTM


##  EXTRACT HABITAT DATA

# Find out what metadata is in the raster

names(img)
summary(img)

img_data <- terra::extract(img, 
                           camCoordsUTM, 
                           bind = TRUE) # bind = TRUE retains the metadata from the original file
table(img_data$Class_Name)



###################### CURSOR CODE FOR LATER - BIND NEW UTM COORDS TO DF ######
# Extract the transformed coordinates
coords_utm_df <- as.data.frame(coords_utm, geom = "XY")
names(coords_utm_df)[names(coords_utm_df) %in% c("x", "y")] <- c("UTM_Easting", "UTM_Northing")

# Add the UTM coordinates back to your original data
env <- bind_cols(env, coords_utm_df[c("UTM_Easting", "UTM_Northing")])
#################################################################################

##  ----    DATA EXTRACTION    ----    ##


env <- camCoords %>%
  select(`Camera Grid Type`:Assigned_Longitude)










