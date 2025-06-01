################################################################################
###                 MAP OCCUPANCY PROBABILITY ACROSS SITES                   ###
################################################################################

library(tidyverse)
library(tmap)
library(tmaptools)
library(sf)
library(viridisLite)
library(cols4all)


# Load data

streams <- st_read("./Data/Spatial/Habitat/Water_features/WaterFeature_L.shp") # river shp

indCams <- readRDS("./Data/Detection/Derived/Detection_NAs_indCams_adultsOnly.Rds")


# Remove cameras without data (site IDs 53 and 107)

indCams[53,] # AFRL-NW-05
indCams[107,] # HQA-38-03

indCams <- indCams %>%
  filter(!(Camera_Name == "AFRL-NW-05" | 
              Camera_Name == "HQA-38-03"))


camProbsSF <- bind_cols(indCamsSF, occuProbs) # occuProbs is from MGS_Final_Analysis.Rmd

occuProbs <- occuProbs %>%
  select(Camera_Name, Assigned_Latitude, Assigned_Longitude, Dist_to_Stream, Predicted)


# Make into spatial object

#occuProbsSF <- st_as_sf(occuProbs, 
                        #coords = c("Assigned_Longitude", "Assigned_Latitude"),
                        #crs = "EPSG:4326")

#occuProbsUTM <- st_transform(occuProbsSF, st_crs(streams))

tmap_mode('view')

tm_shape(streams) + 
  tm_lines(col = "#3D5B9F") +
tm_shape(camProbsSF) + 
  tm_symbols(fill = "Predicted",
             size = 0.5,
             popup.vars = "Dst_t_S",
             fill.scale = tm_scale_continuous(values = "viridis")) +
  tm_layout(legend.reverse = TRUE) 
 


