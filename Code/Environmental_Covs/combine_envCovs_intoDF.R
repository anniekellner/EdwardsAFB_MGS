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

## Find duplicates (renamed cameras)

dups <- camCoords %>%
  group_by(`Camera Trap Name`) %>%
  filter(n() > 1) 

dupsDelete <- dups %>% # get dataframe of duplicates
  ungroup() %>%
  filter(row_number() %in% c(4,5,6)) # remove rows with old names/data

camCoords_noDups <- anti_join(camCoords, dupsDelete) # seems to have worked


##    ----    JOIN COVARIATE DATA   ----    ##

# Join with raster habitat data

camCoordsEnv <- camCoords_noDups %>%
  left_join(imgHab)

camCoordsEnv <- camCoordsEnv %>%
  rename("Raster_Habitat_Class" = "Class_Name")


# Join shapefile data

shpHab <- shpHab %>% # rename columns so they match
  rename(`Camera Trap Name` = "Camera.Trap.Name") %>%
  rename("shp_Habitat_Class" = "cam_veg_type")

camCoordsEnv <- camCoordsEnv %>%
  left_join(shpHab)


# Join Distance Data

camCoordsEnv <- camCoordsEnv %>%
  left_join(dist)


# Save file

saveRDS(camCoordsEnv, file = "./Data/Spatial/camCoords_withEnv.Rds")


