library (dplyr)
library(tidyverse)

file_path <- "C:/Users/mlegac/Documents/CEMML Files/Projects/MGS/CSV Merge/Data"
all_data <- list()
for (file in list.files(file_path, pattern = ".csv")) {
  data <- read.csv(paste(file_path, file, sep = "/"))
  all_data[[file]] <- data}

combined_data <- do.call(rbind, all_data)

combined_data <- combined_data %>%
  mutate(camera = sub("(_.*)$", "", File))

combined_data <- combined_data %>%
  mutate(date = as.Date(DateTime))

date_range <- seq(as.Date("2024-03-10"), as.Date("2024-06-30"), by = "day")

compressed_data <- combined_data %>%
  group_by(camera, date) %>%
  summarise(data_collected = 1, .groups = "drop") %>%
  complete(camera, date = date_range, fill = list(data_collected = 0))

final_output <- compressed_data %>%
  pivot_wider(names_from = date, values_from = data_collected, values_fill = 0)


write.csv(final_output, "operational_camera_days.csv", row.names = FALSE)


