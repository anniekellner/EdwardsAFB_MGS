#######################################################################
###           PRELIMINARY ANALYSES                                  ###
#######################################################################

library(unmarked)
library(tidyverse)



umf <- readRDS('./Data/umf_05142025.Rds')


mgs.occu <- occu(~ordinal + ordinal2 ~(1|site))