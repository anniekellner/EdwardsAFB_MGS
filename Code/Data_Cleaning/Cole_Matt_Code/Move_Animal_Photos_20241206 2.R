#Install Libraries 
install.packages(c("tidyverse","readxl","conflicted"))

#Loading Libraries
library(tidyverse)  
library(readxl)     
library(conflicted) 

#Load and Prepare Data
allanimalcaptures <- read_excel("N:\RStor\CEMML\EdwardsAFB\Mohave_Ground_Squirrel\GIS_and_Monitoring_Data\2024\Field Data\MGS_combined_animaldata_20241205")
head(allanimalcaptures)

#Data Preparation
all <- allanimalcaptures %>%
  dplyr::select(-species2) %>%  # Drops the column `species2` (secondary species info).
  rename(Species = species1)   # Renames the `species1` column to `Species`.

#Create Directories
mainstem <- "N:\\RStor\\CEMML\\EdwardsAFB\\Mohave_Ground_Squirrel\\Camera_Images\\Temporary_Images_renamed"
species <- unique(all$Species)

#Directories
for(i in 1:length(species)){
  to = paste0(mainstem,"\\","QAQC","\\",species[i],"\\")
  if (!dir.exists(to)){
    dir.create(to)
  } else {
    print("dir.exists")
  }
}

#Copy Images
for(i in 1:nrow(all)){
  year = all[i,]$RootFolder
  RelativePath = all[i,]$RelativePath
  picture = all[i,]$File
  iSpecies = all[i,]$Species 
  
#Construct File Paths
from = paste0(mainstem,"\\",year,"\\",RelativePath,"\\",picture)
to = paste0(mainstem,"\\","QAQC","\\",iSpecies,"\\",picture)

#Copy Files:
file.copy(from = from,
          to = to,
          overwrite = FALSE)
