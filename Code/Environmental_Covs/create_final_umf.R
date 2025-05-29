###############################################################################
###                 FINAL UMF: DATE TERMS + DISTANCE TO STREAM              ###
################################################################################

library(tidyverse)
library(unmarked)

##    ----    LOAD AND PREP DATA    ----    ##

# Load

camCoords <- readRDS("./Data/Spatial/Derived/independentCams_DF_dist2stream.Rds")
det <- readRDS("./Data/Detection/Derived/Detection_NAs_indCams_adultsOnly.Rds")

# Prep

camCoords <- camCoords %>% # 
  rename("Camera_Name" = "CameraName") 


##    ----    JOIN COORDS & STREAM DATA WITH DETECTION DATA   ----    ##

detEnv <- det %>%
  left_join(camCoords)


## Adjust column names to dates

detEnv <- detEnv %>%
  rename_with(.fn = function(x) paste0(x, "/2024"),
              .cols = 2:111)


##    ----    CREATE UMF    ----    ##

# Get dates from column names

dates <- colnames(detEnv)[2:111]
dates <- mdy(dates)

# Ordinal Dates
ordinal <- yday(dates)
scOrdinal <- scale(ordinal)

## Create UMF

y <- as.matrix(detEnv[,c(2:111)]) # 0/1 observations


# Observation Covs

obsCovs <- list(
  scOrdinal = matrix(scOrdinal, 
                     nrow = nrow(y), 
                     ncol = ncol(y), 
                     byrow = TRUE),
  scOrdinal2 = matrix(scOrdinal^2, 
                      nrow = nrow(y),
                      ncol = ncol(y),
                      byrow = TRUE))
# Site Covs

siteCovs <- data.frame(
  Dist_to_Stream = detEnv$Dist_to_Stream
)

# UMF

finalUMF <- unmarkedFrameOccu(
  y = y,
  siteCovs = siteCovs,
  obsCovs = obsCovs
)

saveRDS(finalUMF, file = "./Data/UMFs/finalUMF_05292025.Rds")

  