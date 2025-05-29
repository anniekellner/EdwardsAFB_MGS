################################################################################
###             QAQC: CHECK CAMERA OPERATIONAL DAYS AGAINST ORIGINAL        ####
################################################################################

# 5-28-2025

# Check camera operational days from new document against old
  # new: ~./Raw/Operational_Camera_Days_FINAL_AD.csv
  # old: ~./Derived/camera_operation.Rds


library(tidyverse)


##    ----    LOAD AND PREP DATA   ----    ##

new <- read_csv("./Data/Camera_Operation/Raw/Operational_Camera_Days_FINAL_AD.csv")
old <- readRDS("./Data/Camera_Operation/Derived/camera_operation.Rds")

all.equal(new$Camera_Name, old$Camera_Name) # names are identical

new <- new[,-c(112:116)] # remove last four columns bc irrelevent


##    ----    CHECK THAT 0'S AND 1'S ARE IDENTICAL    ----    ##

# Check totals

check_new <- new %>%
  rowwise() %>%
  mutate(row_sum = sum(c_across(2:111)))

check_old <- old %>%
  rowwise() %>%
  mutate(row_sum = sum(c_across(2:111)))

setdiff(check_new$row_sum, check_old$row_sum)

checkDF <- data.frame(new = check_new$row_sum,
                      old = check_old$row_sum)

# Check individual values

all.equal(new, old) # TRUE




