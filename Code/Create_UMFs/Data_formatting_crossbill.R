###############################################################################
###         DATA FORMATTING: CROSSBILL DATASET                           ###
###############################################################################

library(tidyverse)
library(unmarked)


##  ----  LOAD DATA   ----  ##

data(crossbill)

glimpse(crossbill)
head(crossbill)


DATE <- as.matrix(crossbill[,32:58])
y.cross <- as.matrix(crossbill[,5:31])
y.cross[is.na(DATE) != is.na(y.cross)] <- NA