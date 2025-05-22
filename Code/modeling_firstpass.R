#######################################################################
###           PRELIMINARY ANALYSES                                  ###
#######################################################################

library(unmarked)
library(tidyverse)

# Load data
umf <- readRDS('./Data/umf_05152025.Rds')

# Model with temporal covariates and random effects
mgs.occu <- occu(~ scOrdinal + scOrdinal2 ~ 1 + (1|site),  # Detection ~ temporal covariates, Occupancy ~ random effect
                 data = umf,
                 linkPsi = "logit")

# Model summary
summary(mgs.occu)

# Confidence intervals
confint(mgs.occu, type = "det", method = "normal")
confint(mgs.occu, type = "state", method = "normal")

# Predictions
pred_scaled <- predict(mgs.occu, type = "state")



# Save model results
saveRDS(mgs.occu, "./Results/mgs_occu_model.Rds")
