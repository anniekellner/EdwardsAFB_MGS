################################################################################
####                  OCCUPANCY AND DETECTION PROBABILITIES                 ####
####                  TOP MODEL - DRAFT 1                                   ####
################################################################################

# Creating csv's for Adam Dillon with occupancy and detection probabilities
  # for top model and top model without 'season' covariate


library(tidyverse)
library(unmarked)


##    --    LOAD AND PREP DATA   --    ##

# Load

umf <- readRDS("~/Repos/MGS/Data/UMFs/umf_FINAL_20251005.Rds") 

df <- readRDS("~/Repos/MGS/Data/finalDF_shpHabitat_camsDeleted_20251003.Rds")

ll <- readRDS("./Data/indCams_adults_envCovs_06022025.Rds") # has lat/lon

dateDF <- readRDS("~/Repos/MGS/Data/Detection/Derived/finalDateDF_20251002.Rds")


# Prep

# Add lat/lon

ll <- ll %>%
  select(Camera_Name, Assigned_Longitude, Assigned_Latitude)

df <- df %>%
  left_join(ll) %>% 
  select(-c(shp_Habitat_Class, Desert_Scrub, JoshTree, DistScaled))

# Get season from dateDF

season <- dateDF %>%
  select(original_date, season) %>%
  rename("Date" = "original_date")


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
  select(Camera_Name, Assigned_Longitude, Assigned_Latitude, Dist_to_Stream, OccuProb_Estimate,
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
  select(Camera_Name, Assigned_Longitude, Assigned_Latitude, Dist_to_Stream, OccuProb_Estimate,
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
  

##    --    DETECTION ESTIMATES   --    ##

## TOP MODEL

top_detProb_matrix <- matrix(detProbs_top$Predicted, 
                      nrow = nrow(umf@y),
                      ncol = ncol(umf@y),
                      byrow = TRUE)

det_probs_by_date <- colMeans(top_detProb_matrix, na.rm = TRUE)

# Create dataframe

detProbDF_top <- data.frame(
  Date = colnames(df[,2:111]),
  Detection_Prob = det_probs_by_date
)


#detProbDF_top$Date <- mdy(detProbDF$Date)

detProbDF_top <- detProbDF_top %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

#write.csv(detProbDF_top,
          #file = "C:/Users/akellner/OneDrive - Colostate/Documents/Edwards/Mojave Ground Squirrel/Outputs/detection_probs_by_date_topModel_20251010.csv",
          #row.names = FALSE)


## NO SEASON

NS_detProb_matrix <- matrix(detProbs_noSeason$Predicted, 
                             nrow = nrow(umf@y),
                             ncol = ncol(umf@y),
                             byrow = TRUE)

NS_detProbs_byDate <- colMeans(NS_detProb_matrix, na.rm = TRUE)


# Create dataframe

detProbDF_NS <- data.frame(
  Date = colnames(df[,2:111]),
  Detection_Prob = NS_detProbs_byDate
)

detProbDF_NS <- detProbDF_NS %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

#write.csv(detProbDF_NS,
          #file = "C:/Users/akellner/OneDrive - Colostate/Documents/Edwards/Mojave Ground Squirrel/Outputs/detection_probs_by_date_noSeason_20251010.csv")


##    --    PLOT DETECTION PROBABILITIES BY DATE X ACTUAL COUNT   --    ##

# Top Model

detProbDF_top$Date <- mdy(detProbDF_top$Date)

detProbDF_top <- detProbDF_top %>%
  left_join(season, by = "Date") 

# Plot
ggplot(detProbDF_top, aes(x = Date, y = Detection_Prob)) + 
  geom_line(aes(color = season), size = 1) +
  geom_point(aes(color = season), size = 1.5) +
  scale_x_date(date_breaks = "5 days", date_labels = "%m/%d") +
  scale_color_manual(values = c("early" = "#2C7BB6", "peak" = "#D7191C", "late" = "#FDAE61")) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Date", y = "Detection Probability", color = "Season")

counts <- df %>%
  summarize(across(2:111, sum, na.rm = TRUE))

counts_long <- counts %>%
  pivot_longer(everything(),
               names_to = "Date",
               values_to = "Total_Detections") %>%
  mutate(Date = mdy(Date))

# Combine predicted and observed

comparison_df <- detProbDF_top %>%
  left_join(counts_long, by = "Date") %>%
  mutate(Scaled_Detections = Total_Detections / max(Total_Detections, na.rm = TRUE))  # Scale to 0-1 for comparison

# Plot both on same graph
ggplot(comparison_df, aes(x = Date)) +
  geom_line(aes(y = Detection_Prob, color = "Predicted"), size = 1) +
  geom_line(aes(y = Scaled_Detections, color = "Observed (scaled)"), size = 1, linetype = "dashed") +
  scale_x_date(date_breaks = "5 days", date_labels = "%m/%d") +
  scale_color_manual(values = c("Predicted" = "blue", "Observed (scaled)" = "red")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Date", y = "Probability / Scaled Counts", color = "Data Type",
       title = "Predicted Detection Probability vs Observed Detections")


# No Season

detProbDF_NS$Date <- mdy(detProbDF_NS$Date)


# Plot
ggplot(detProbDF_NS, aes(x = Date, y = Detection_Prob)) + 
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  scale_x_date(date_breaks = "5 days", date_labels = "%m/%d") +
  #scale_color_manual(values = c("early" = "#2C7BB6", "peak" = "#D7191C", "late" = "#FDAE61")) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Date", y = "Detection Probability")

counts <- df %>%
  summarize(across(2:111, sum, na.rm = TRUE))

counts_long <- counts %>%
  pivot_longer(everything(),
               names_to = "Date",
               values_to = "Total_Detections") %>%
  mutate(Date = mdy(Date))

# Combine predicted and observed

comparison_df <- detProbDF_NS %>%
  left_join(counts_long, by = "Date") %>%
  mutate(Scaled_Detections = Total_Detections / max(Total_Detections, na.rm = TRUE))  # Scale to 0-1 for comparison

# Plot both on same graph
ggplot(comparison_df, aes(x = Date)) +
  geom_line(aes(y = Detection_Prob, color = "Predicted"), size = 1) +
  geom_line(aes(y = Scaled_Detections, color = "Observed (scaled)"), size = 1, linetype = "dashed") +
  scale_x_date(date_breaks = "5 days", date_labels = "%m/%d") +
  scale_color_manual(values = c("Predicted" = "blue", "Observed (scaled)" = "red")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Date", y = "Probability / Scaled Counts", color = "Data Type",
       title = "Predicted Detection Probability vs Observed Detections")

## CHECK FOR QUADRATIC TREND

# Plot on the LOGIT scale to see the true quadratic shape
logit_det_probs <- qlogis(top_det_probs_by_date)

plot(1:110, logit_det_probs, type = "l", 
     main = "Detection Probability on Logit Scale",
     xlab = "Date Index", ylab = "Logit(Detection Probability)")

test_model <- occu(~ scaled_recentered + scaled_recentered_squared ~ Dist_Scaled,
                   data = umf)

# Get predictions
test_preds <- predict(test_model, type = "det")
test_matrix <- matrix(test_preds$Predicted, nrow = 143, ncol = 110, byrow = TRUE)
test_probs <- colMeans(test_matrix, na.rm = TRUE)

# Plot
plot(1:110, test_probs, type = "l", 
     main = "Detection without Season Factor",
     xlab = "Date Index", ylab = "Detection Probability")

actual_peak_date <- which.max(counts_long$Total_Detections)
cat("Peak detection occurs at date index:", actual_peak_date, "\n")
cat("That corresponds to:", counts_long$Date[actual_peak_date], "\n")


