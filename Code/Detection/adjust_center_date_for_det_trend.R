###############################################################################
###       ADJUST ORDINAL DATE TO MATCH PARABOLIC DETECTION TREND            ###
###############################################################################

# Because the study start and end dates are somewhat arbitrary, we are adjusting
  # the center of the date spread to capture the parabolic detection trend. 
  # We know a priori (and based on visual inspection of our data) that 
  # a springtime waxing and waning presence is part of MGS life history


library(tidyverse)


##    ----    LOAD DATA   ----    ##

df <- readRDS("~/Repos/MGS/Data/indCams_adults_envCovs_06022025.Rds")

# Get dates from column names

dates <- colnames(df)[2:111]
dates <- mdy(dates)
ord <- yday(dates)

# Center dates around May 22 (ordinal date 143)
target_center <- 143 
scaled_dates <- ord - target_center

# Create a data frame with the original and scaled dates
date_df <- data.frame(
  original_date = dates,
  ordinal_date = ord,
  scaled_date = scaled_dates
)

# Save the scaled dates for later use
#saveRDS(date_df, "~/Repos/MGS/Data/Detection/datesDF.Rds")

