###############################################################################
###                   FINAL UMF                                              ###
###############################################################################

# written by Annie Kellner for CEMML (Edwards AFB Mojave Ground Squirrel Occupancy)
# 10-02-2025


library(tidyverse)
library(unmarked)

##    ----    LOAD AND PREP DATA    ----    ##

## Dataframes: envCovs (all cams) & adultsOnly detection DF

#envCams_all <- readRDS("~/Repos/MGS/Data/Covariates/envCams_allCams_shpHabitat_20251003.Rds")

#adultsOnly <- readRDS("./Data/Detection/Derived/Detection_NAs_indCams_adultsOnly.Rds")

dateDF <- readRDS("~/Repos/MGS/Data/Detection/Derived/finalDateDF_20251002.Rds")

finalDF <- readRDS("~/Repos/MGS/Data/finalDF_shpHabitat_camsDeleted_20251003.Rds")

habPercentDF <- readRDS("~/Repos/MGS/Data/Covariates/habitat_percentages.Rds")

# Remove Desert Scrub and Joshua Tree; replace with percentages

finalDF <- finalDF %>%
  select(-c(Desert_Scrub, JoshTree)) %>%
  left_join(habPercentDF) %>% 
  mutate(across(c(xeroSaltbushScrub, haloSaltbushScrub, joshuaTreeWoodland, creosoteBushScrub), 
                ~ replace_na(., 0))) %>%
  filter(!(Camera_Name == "AFRL-NW-05" | 
             Camera_Name == "HQA-38-03")) # remove inoperable cameras


################################################################################
#########             NOT NECESSARY TO REPEAT                       ############
################################################################################

##    --    INDEPENDENT CAMERAS ONLY    --    ##


#adults_only_ind_cams <- adultsOnly %>%
  #left_join(envCams_all, by = "Camera_Name")

# Rename columns to get dates

#adults_only_ind_cams <- adults_only_ind_cams %>%
  #rename_with(~paste0(., "/2024"), 2:111)


##    --    ADD SCALED DISTANCE VARIABLE    --    ##

#scDist <- as.vector(scale(adults_only_ind_cams$Dist_to_Stream))

#adults_only_ind_cams$DistScaled <- scDist


##    --    REMOVE INOPERABLE CAMERAS   --    ##

#adults_only_ind_cams <- adults_only_ind_cams %>%
  #filter(!(Camera_Name == "AFRL-NW-05" | 
            # Camera_Name == "HQA-38-03"))

#saveRDS(finalDF, file = "./Data/finalDF_20251005.Rds")



##    --    CREATE FINAL UMF    --    ##


## Create y

y <- finalDF[,2:111]


## Site Covs

siteCovs <- data.frame(
  Dist_to_Stream = as.numeric(finalDF$Dist_to_Stream), # included for mapping
  Dist_Scaled = finalDF$DistScaled,
  Camera_Name = finalDF$Camera_Name, # for ensuring camera-dist association
  Habitat = finalDF$shp_Habitat_Class,
  xeroSaltbush = finalDF$xeroSaltbushScrub,
  haloSaltbush = finalDF$haloSaltbushScrub,
  joshuaTree = finalDF$joshuaTreeWoodland,
  creosote = finalDF$creosoteBushScrub
)


# Observation Covs

obsCovs <- list(
  scaled_recentered = matrix(dateDF$scaled_recentered,
                             nrow = nrow(y),
                             ncol = ncol(y),
                             byrow = TRUE),
  scaled_recentered_squared = matrix(dateDF$scaled_recentered_squared,
                                     nrow = nrow(y),
                                     ncol = ncol(y),
                                     byrow = TRUE),
  season = matrix(dateDF$season,
                  nrow = nrow(y),
                  ncol = ncol(y),
                  byrow = TRUE))


##    --    FINAL UMF   --    ##

umf_FINAL_20251005 <- unmarkedFrameOccu(
  y = y,
  siteCovs = siteCovs,
  obsCovs = obsCovs
)

#saveRDS(umf_FINAL_20251005, file = "./Data/UMFs/umf_FINAL_20251005.Rds")
