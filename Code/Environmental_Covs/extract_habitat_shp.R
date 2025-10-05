################################################################################
###               EXTRACT HABITAT: SHAPEFILE                                 ###
################################################################################

# Habitat data extracted from 
  # N:\RStor\CEMML\EdwardsAFB\Mohave_Ground_Squirrel\GIS_and_Monitoring_Data\2024\Analysis\Occupancy\Habitat_layer


library(tidyverse)
library(terra)
library(sf)
library(terra)
library(tmap)
library(tmaptools)

##  --  LOAD AND PREP DATA  --  ##

# Habitat layer

hab <- st_read("./Data/Spatial/Habitat/Land_Cover/LandCover_A.shp")
hab <- hab %>%
  select(geometry, VEGETATION, LONGITUDEC, LATITUDECE)

plot(st_geometry(hab))

st_crs(hab) # in UTM 11N

# Cameras

camCoords <- readRDS("./Data/Spatial/camCoords.Rds") %>%
  rename(Camera_Name = `Camera Trap Name`) 

camCoords <- camCoords %>%
  select(Camera_Name, Assigned_Latitude, Assigned_Longitude)

indCams <- readRDS("./Data/Detection/Derived/Detection_NAs_indCams_adultsOnly.Rds") # independent cameras only

indCams <- indCams$Camera_Name

# Filter camCoords to only include independent cameras

camCoords <- camCoords[camCoords$Camera_Name %in% indCams, ]

camCoords <- camCoords[!duplicated(camCoords$Camera_Name), ] # remove duplicates


# Make DF into spatial object

camCoordsSF <- st_as_sf(camCoords, 
                        coords = c("Assigned_Longitude", "Assigned_Latitude"), 
                        crs = "EPSG:4326")

# Reproject camera shp to UTM 11N

camCoordsUTM <- st_transform(camCoordsSF, st_crs(hab))
st_crs(camCoordsUTM) # looks good

# Plot to be sure projections align - looks good

tm_shape(hab) + 
  tm_polygons(fill = "VEGETATION") + 
  tm_shape(camCoordsUTM) + 
  tm_symbols(fill = "black")

# Create buffer around points

camCoordsBuff <- st_buffer(camCoordsUTM, dist = 150)

##  --  DATA EXTRACTION   --  ##

hab_data <- st_intersection(camCoordsBuff, hab)


## Calculate majority

majority_hab <- hab_data %>%
  group_by(Camera.Trap.Name) %>%
  summarize(cam_veg_type = names(which.max(table(VEGETATION))),
            .groups = "drop")

# Save file

#majority_hab <- st_drop_geometry(majority_hab) # save as dataframe, not spatial object
#saveRDS(majority_hab, file = "./Data/Spatial/Habitat/Derived/shp_habitat.Rds")



## Calculate proportions

hab_data$area <- st_area(hab_data)

habDF <- st_drop_geometry(hab_data) # Convert to regular data frame for calculations

# Calculate total area per camera buffer

buffer_totals <- habDF %>%
  group_by(Camera_Name) %>%
  summarise(total_area = sum(area), .groups = "drop")

habPercent <- habDF %>%
  group_by(Camera_Name, VEGETATION) %>%
  summarise(habitat_area = sum(area), .groups = "drop") %>%
  left_join(buffer_totals, by = "Camera_Name") %>%
  mutate(
    percentage = (habitat_area / total_area) * 100
  ) %>%
  select(Camera_Name, VEGETATION, percentage)

habPercent <- habPercent %>%
  filter(!VEGETATION %in% c("playaClaypan", "mesquiteWoodland")) %>%
  mutate(percentage = as.numeric(percentage))

# Pivot wider so veg types become columns

habDF <- habPercent %>%
  select(Camera_Name, VEGETATION, percentage) %>%
  pivot_wider(
    names_from = VEGETATION,
    values_from = percentage,
    values_fill = 0  # Fill missing combinations with 0
  ) 

#saveRDS(habDF, file = "./Data/Covariates/habitat_percentages.Rds")





