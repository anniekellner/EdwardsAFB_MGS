############################################################################
###                 CAMERA DATA QA/QC                                   ####
############################################################################

# 5/28/2025
# Compare original camera input data with updated version from Cole


library(tidyverse)


##    ----    LOAD DATA   ----    ##

old <- readRDS("./Data/Camera_Operation/Derived/camera_operation.Rds")
new <- read_csv(file = "./Data/Camera_Operation/Raw/Camera_Locations_GIS_MGS_v4_alteredAK.csv",
                col_names = TRUE,
                col_select = c("Camera_Name", "Longitude", "Latitude"),
                trim_ws = TRUE) # Cole spreadsheet

newAdam <- read_csv("./Data/Camera_Operation/Raw/Final_Camera_Locations_FromGIS_AD.csv")


##    ----    CHECK CAMERA NAMES    ----    ##

# Cole

setdiff(old$Camera_Name, new$Camera_Name)
setdiff(new$Camera_Name, old$Camera_Name)

# Two extra cameras are those without data that were eliminated from the analysis
# Cole had "old name/new name" listed for camera names
# Otherwise looks good as far as camera names

new_nameChanges <- new %>%
  mutate(Camera_Name = case_when(
    Camera_Name == "1994-N02-05/1994-N02A-01" ~ "1994-N02A-01",
    Camera_Name == "HQA-43-07/HQA-43A-07" ~ "HQA-43A-07",
    Camera_Name == "HQA-43-09/HQA-43A-09" ~ "HQA-43A-09",
    TRUE ~ Camera_Name
  ))


# Adam

newAdam <- newAdam %>%
  filter(!CameraName %in% c("1994-N02-05", "HQA-43-07", "HQA-43-09"))

#saveRDS(newAdam, file = "./Data/Camera_Operation/Derived/Final_Camera_Locations_FromGIS_AD_AK_05282025.Rds")

