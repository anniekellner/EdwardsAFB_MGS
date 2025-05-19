################################################################################
###         ENVIRONMENTAL COVARIATES: HABITAT                               ####
################################################################################

library(terra)
library(tidyverse)


img <- rast("./Data/Spatial/Habitat/FINAL_CWHR_EAFB_20231103.img")
plot(img)
