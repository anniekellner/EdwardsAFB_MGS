# Step 1: Create date sequence and standardize day of year, centered on May 5
dates <- seq(as.Date("2025-03-08"), as.Date("2025-06-25"), by = "day")
day_of_year <- as.numeric(format(dates, "%j"))
center_day <- as.numeric(format(as.Date("2025-05-02"), "%j"))  # = 122
day_std <- (day_of_year - center_day) / sd(day_of_year)
day_std2 <- day_std^2 

# Step 2: Simulate a logistic detection model with a quadratic effect
# Coefficients (adjust as needed)
alpha <- 0.2        # intercept
beta1 <- 1.5        # linear effect
beta2 <- -2         # quadratic effect (negative for hump shape)

# Compute linear predictor (logit scale)
logit_p <- alpha + beta1 * day_std + beta2 * day_std2

# Back-transform to probability
p <- 1 / (1 + exp(-logit_p))

# Step 3: Plot
plot(dates, p, type = "l", lwd = 2,
     ylab = "Detection Probability", xlab = "Date",
     main = "Quadratic Effect of Day on Detection Probability")




# Step 1: Create date sequence and standardize day of year, centered on May 5
dates2 <- seq(as.Date("2025-03-08"), as.Date("2025-06-25"), by = "day")
day_of_year2 <- as.numeric(format(dates, "%j"))
#center_day <- as.numeric(format(as.Date("2025-05-05"), "%j"))  # = 122
day_std2 <- (day_of_year2 - mean(day_of_year2)) / sd(day_of_year2) # = 111
day_std22 <- day_std^2