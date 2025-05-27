################################################################################
###                 RECLASSIFY HABITAT                                       ###
################################################################################

# Reclassify 
  # Desert Scrub/Other
  # Joshua Tree Woodland/Other


# Reclassify habitat as Desert Scrub/Other

allHab <- readRDS("./Data/Spatial/camCoords_withEnv_05262025.Rds") 

hab <- allHab %>%
  mutate(Raster_Habitat_Class = ifelse(Raster_Habitat_Class == "Desert Scrub",
                                       "Desert Scrub",
                                       "Other"))

table(hab$Raster_Habitat_Class)

#saveRDS(hab, file = "./Data/Spatial/Habitat/DesertScrubOnly.Rds")


# Reclassify Habitat as Joshua Tree Woodland/Other

jtree <- allHab %>%
  mutate(Raster_Habitat_Class = ifelse(Raster_Habitat_Class == "Joshua Tree Woodland",
                                       "Joshua Tree Woodland",
                                       "Other"))
table(jtree$Raster_Habitat_Class)

#saveRDS(jtree, file = "./Data/Spatial/Habitat/JTreeOnly.Rds")


###

# Join with independent cams only

detHab <- det %>%
  left_join(hab)

#saveRDS(detHab, file = "./Data/Covariates/indCams_adultsOnly_DesertScrub.Rds")

detJtree <- det %>%
  left_join(jtree)

#saveRDS(detJtree, file = "./Data/Covariates/indCams_adultsOnly_JTree.Rds")
