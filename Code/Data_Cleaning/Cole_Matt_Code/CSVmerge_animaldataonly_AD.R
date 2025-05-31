library (dplyr)
library(tidyverse)

file_path <- "C:/Users/mlegac/Documents/CEMML Files/Projects/MGS/CSV Merge/Data"
all_data <- list()
for (file in list.files(file_path, pattern = ".csv")) {
  data <- read.csv(paste(file_path, file, sep = "/"))
  all_data[[file]] <- data}

combined_data <- do.call(rbind, all_data)

combined_animal_data <- combined_data %>% 
  drop_na(species1)

write.csv(combined_animal_data, "combined_animaldata.csv", row.names = FALSE)


