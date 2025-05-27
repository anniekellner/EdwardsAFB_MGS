################################################################################
###                 RECLASSIFY HABITAT                                       ###
################################################################################

# Reclassify 
  # Desert Scrub/Other
  # Joshua Tree Woodland/Other


# Reclassify habitat as Desert Scrub/Not Desert Scrub

allHab <- readRDS("./Data/Spatial/camCoords_withEnv_05262025.Rds") 

hab <- allHab %>%
  mutate(Raster_Habitat_Class = ifelse(Raster_Habitat_Class == "Desert Scrub",
                                       "Desert Scrub",
                                       "Other"))

table(hab$Raster_Habitat_Class)

#saveRDS(hab, file = "./Data/Spatial/Habitat/DesertScrubOnly.Rds")


jtree <- allHab %>%
  mutate(Raster_Habitat_Class = ifelse(Raster_Habitat_Class == "Joshua Tree Woodland",
                                       "Joshua Tree Woodland",
                                       "Other"))
table(jtree$Raster_Habitat_Class)

#saveRDS(jtree, file = "./Data/Spatial/Habitat/JTreeOnly.Rds")
