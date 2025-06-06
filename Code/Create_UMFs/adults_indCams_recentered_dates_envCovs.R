################################################################################
###             CREATE UMF WITH DATES SCALED AROUND MAY 22                  ###
################################################################################


library(unmarked)
library(tidyverse)


##    ----    LOAD DATA   ----    ##

datesDF <- readRDS("./Data/Detection/datesDF.Rds")

mgsDF <- readRDS("./Data/indCams_adults_envCovs_06022025.Rds")



##    ----      CREATE UMF  ----    ##

## PREP

# Create y

y <- mgsDF[,2:111] 


# Site Covs (Occupancy)

scDist <- as.vector(scale(mgsDF$Dist_to_Stream))


siteCovs <- data.frame(
  Dist_to_Stream = as.numeric(mgsDF$Dist_to_Stream), # included for mapping
  Dist_Scaled = scDist, # scaled data used for model
  Camera_Name = mgsDF$Camera_Name # for ensuring camera-dist association
)

# Obs Covs (Detection)

dates_recentered_scaled <- datesDF$scaled_date


obsCovs <- obsCovs <- list(
  scOrdinal = matrix(dates_recentered_scaled, 
                     nrow = nrow(y), 
                     ncol = ncol(y), 
                     byrow = TRUE),
  scOrdinal2 = matrix(dates_recentered_scaled^2, 
                      nrow = nrow(y),
                      ncol = ncol(y),
                      byrow = TRUE))

umf <- unmarkedFrameOccu(
  y = y,
  siteCovs = siteCovs,
  obsCovs = obsCovs
)

#saveRDS(umf, file = "./Data/UMFs/recentered_dates_06062025.Rds")
