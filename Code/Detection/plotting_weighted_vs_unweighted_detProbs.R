################################################################################
###           WEIGHTED VS UNWEIGHTED DETECTION PROBABILITIES                ###
################################################################################

m5 <- occu(~scOrdinal ~ Dist_Scaled,
           data = umf) # From MGS_Final_Analysis.Rmd

detProbs <- predict(m5, type = "det")


##    ----    PLOTS   ----    ##


# Unweighted, logit scale

detProbDF <- data.frame(
  "Date" = colnames(finalDF[,2:111]),
  "Detection_Prob" = detProbs$Predicted
)

detProbDF$Date <- mdy(detProbDF$Date)

ggplot(detProbDF, aes(x = Date, y = Detection_Prob)) + 
  geom_point() + 
  geom_line() + 
  scale_x_date(date_breaks = "5 days",
               date_labels = "%m/%d") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Date", y = "Detection Probability")


# Weighted

ord_dates <- yday(detProbDF$Date)

center <- 144
sigma <- 15
weights <- exp(-(ord_dates - center)^2 / (2 * sigma^2))

detProbDF$weight <- weights

detProbDF$weighted_p <- detProbDF$Detection_Prob*detProbDF$weight


# Plot

ggplot(detProbDF, aes(x = Date, y = weighted_p)) + 
  geom_point() + 
  geom_line() + 
  scale_x_date(date_breaks = "5 days",
               date_labels = "%m/%d") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Date", y = "Detection Probability")



