################################################################################
####                  OCCUPANCY AND DETECTION PROBABILITIES                 ####
####                  TOP MODEL - DRAFT 1                                   ####
################################################################################

# Creating csv's for Adam Dillon with occupancy and detection probabilities
  # for top model and top model without 'season' covariate


library(tidyverse)
library(unmarked)


##    --    LOAD AND PREP DATA   --    ##


umf <- readRDS("~/Repos/MGS/Data/UMFs/umf_FINAL_20251005.Rds") 

df <- readRDS("~/Repos/MGS/Data/finalDF_shpHabitat_camsDeleted_20251003.Rds")

ll <- readRDS("./Data/indCams_adults_envCovs_06022025.Rds") # has lat/lon

ll <- ll %>%
  select(Camera_Name, Assigned_Longitude, Assigned_Latitude)

df <- df %>%
  left_join(ll) %>% 
  select(-c(shp_Habitat_Class, Desert_Scrub, JoshTree, DistScaled))


##    --    RUN MODELS   --    ##

top <- occu(~scaled_recentered + scaled_recentered_squared + season ~Dist_Scaled,
           data = umf,
           linkPsi = "logit")

noSeason <- occu(~scaled_recentered + scaled_recentered_squared ~Dist_Scaled,
                 data = umf,
                 linkPsi = "logit")

# Get estimates

occuProbs_top <- predict(top, type = "state", na.rm = TRUE)
detProbs_top <- predict(top, type = "det")

occuProbs_noSeason <- predict(noSeason, type = "state", na.rm = TRUE)
detProbs_noSeason <- predict(noSeason, type = "det")


##    --    OCCUPANCY ESTIMATES    --    ##

## TOP MODEL

site_info <- top@data@siteCovs

site_info <- site_info %>%
  select(Camera_Name, Dist_to_Stream)

site_info <- left_join(site_info, ll)


occuProbs_topDF <- data.frame(
  site_info,
  OccuProb_Estimate = occuProbs_top$Predicted,
  SE = occuProbs_top$SE,
  CI_Lower = occuProbs_top$lower,
  CI_Upper = occuProbs_top$upper
)

occuProbs_topDF <- occuProbs_topDF %>%
  select(Camera_Name, Assigned_Longitude, Assigned_Latitude, OccuProb_Estimate,
         SE, CI_Lower, CI_Upper) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

#write.csv(occuProbs_topDF, 
          #file = "C:/Users/akellner/OneDrive - Colostate/Documents/Edwards/Mojave Ground Squirrel/Outputs/Occupancy_Results_topModel_20251010.csv", row.names = FALSE)


## NO SEASON

site_infoNS <- noSeason@data@siteCovs

site_infoNS <- site_info %>%
  select(Camera_Name, Dist_to_Stream)

site_infoNS <- left_join(site_info, ll)


occuProbsNS_DF <- data.frame(
  site_infoNS,
  OccuProb_Estimate = occuProbs_noSeason$Predicted,
  SE = occuProbs_noSeason$SE,
  CI_Lower = occuProbs_noSeason$lower,
  CI_Upper = occuProbs_noSeason$upper
)

occuProbsNS_DF <- occuProbsNS_DF %>%
  select(Camera_Name, Assigned_Longitude, Assigned_Latitude, OccuProb_Estimate,
         SE, CI_Lower, CI_Upper) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

#write.csv(occuProbsNS_DF, 
  #file = "C:/Users/akellner/OneDrive - Colostate/Documents/Edwards/Mojave Ground Squirrel/Outputs/Occupancy_Results_noSeason_20251010.csv", 
  #row.names = FALSE)

# Differences

occuProbsNS_DF <- occuProbsNS_DF %>%
  rename(OccuProb_EstimateNS = OccuProb_Estimate,
         SE_NS = SE,
         CI_Lower_NS = CI_Lower,
         CI_Upper_NS = CI_Upper)

all <- left_join(occuProbs_topDF, occuProbsNS_DF) 

all <- all %>%
  mutate(Diff_Estimate = OccuProb_Estimate - OccuProb_EstimateNS) %>%
  mutate(Diff_SE = SE - SE_NS) 

#write.csv(all, file = "C:/Users/akellner/OneDrive - Colostate/Documents/Edwards/Mojave Ground Squirrel/Outputs/Diffs_topModel_noSeason_20251010.csv", 
            #row.names = FALSE)
  






detProbDF <- data.frame(
  "Date" = colnames(finalDF[,2:111]),
  "Detection_Prob" = detProbs$Predicted
)

detProbDF$Date <- mdy(detProbDF$Date)




