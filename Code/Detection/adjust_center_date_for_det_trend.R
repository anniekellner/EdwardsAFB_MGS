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


##    ----    APPROACH 1: NO STANDARD DEVIATION   ----    ##

# Center and standardize dates around May 23 (ordinal date 144)
target_center <- 144 
recentered_dates <- (ord - target_center) 
recentered_dates_squared <- recentered_dates^2

# Create a data frame with the original and scaled dates
date_df <- data.frame(
  original_date = dates,
  ordinal_date = ord,
  recentered_date = recentered_dates,
  recentered_date_squared = recentered_dates_squared
)

#saveRDS(date_df, file = "./Data/Detection/Derived/date_df.Rds")

# Plot 1: Original dates vs ordinal dates
plot(dates, ord, type = "l", 
     main = "Original Dates vs Ordinal Dates",
     xlab = "Date", ylab = "Ordinal Date")
abline(v = as.Date("2025-05-07"), col = "red", lty = 2)  # May 7
abline(v = as.Date("2025-05-23"), col = "blue", lty = 2) # May 23

# Plot 2: Scaled dates
plot(dates, recentered_dates, type = "l",
     main = "Scaled Dates",
     xlab = "Date", ylab = "Scaled Date")
abline(h = 0, col = "red", lty = 2)  # Center line
abline(v = as.Date("2025-05-07"), col = "red", lty = 2)  # May 7
abline(v = as.Date("2025-05-23"), col = "blue", lty = 2) # May 23

# Plot 3: Scaled dates squared
plot(dates, recentered_dates_squared, type = "l",
     main = "Scaled Dates Squared",
     xlab = "Date", ylab = "Scaled Date Squared")
abline(v = as.Date("2025-05-07"), col = "red", lty = 2)  # May 7
abline(v = as.Date("2025-05-23"), col = "blue", lty = 2) # May 23

# Plot 4: Scaled vs squared
plot(recentered_dates, recentered_dates_squared, type = "l",
     main = "Scaled vs Squared",
     xlab = "Scaled Date", ylab = "Scaled Date Squared")
abline(h = 0, col = "red", lty = 2)



## ---- APPROACH TWO: USING STANDARD DEVIATION FOR 'ACTIVE' DATES ONLY  ---- ##

active_dates <- ord[ord >= 112] # I set May 23 as the center date with an equal
  # number of dates before and after, ranging to the end of the study. 
  # Thus, the dates used to determine the sd are April 22 - June 25


active_sd <- sd(active_dates) 

recentered_active <- (ord - target_center) / active_sd
recentered_active_squared <- recentered_active^2

# Create a data frame with the original and scaled dates
active_df <- data.frame(
  original_date = dates,
  ordinal_date = ord,
  recentered_date = recentered_active,
  recentered_active_squared = recentered_active_squared
)

# Plot

# Diagnostic plots
par(mfrow = c(2, 2))

# Plot 2: recentered dates
plot(dates, recentered_active, type = "l",
     main = "recentered Dates",
     xlab = "Date", ylab = "recentered Date")
abline(h = 0, col = "red", lty = 2)  # Center line
abline(v = as.Date("2025-05-07"), col = "red", lty = 2)  # May 7
abline(v = as.Date("2025-05-23"), col = "blue", lty = 2) # May 23

# Plot 3: recentered dates squared
plot(dates, recentered_active_squared, type = "l",
     main = "recentered Dates Squared",
     xlab = "Date", ylab = "recentered Date Squared (Using Active Date SD")
abline(v = as.Date("2025-05-07"), col = "red", lty = 2)  # May 7
abline(v = as.Date("2025-05-23"), col = "blue", lty = 2) # May 23

# Plot 4: recentered vs squared
plot(recentered_active, recentered_active_squared, type = "l",
     main = "recentered vs Squared",
     xlab = "recentered Date", ylab = "recentered Date Squared")
abline(h = 0, col = "red", lty = 2)

print("Range of recentered_dates (no standardization):")
print(range(recentered_dates))
print("\nRange of recentered_active (with active date SD):")
print(range(recentered_active))


# Create a data frame with the original and scaled dates
date_df <- data.frame(
  original_date = dates,
  ordinal_date = ord,
  recentered_date = recentered_dates,
  recentered_date_squared = recentered_dates_squared,
  scaled_recentered = recentered_active,
  scaled_recentered_squared = recentered_active_squared
)

#saveRDS(date_df, file = "./Data/Detection/Derived/dateDF_recentered_scaled_09172025.Rds")


##    ----    APPROACH 3: GAUSSIAN WEIGHTING   ----    ##

sigma <- 15

gaussian_weights <- exp(-(ord - target_center)^2 / (2 * sigma^2))

# dataframe for plotting

sigma15DF <- data.frame(date = dates, 
           weights = gaussian_weights)

ggplot(sigma15DF, aes(x = date, y = weights)) + 
  geom_line() # this looks pretty good


saveRDS(gaussian_weights, "./Data/Detection/Derived/Gaussian_weights.Rds")






