
# 07_ExtractDataForShinyMap.R

# Preliminaries
library(tidyverse)  # contains dplyr, ggplot2, readr, tidyr, stringr, purrr and forcats
library(sf)
library(mapview)
library(smoothr)    # to fill_holes (in polygons)

setwd("H:/000_Coastal_Related/00_ESRC Coastal Classification/00_SpatialDefinitionofCoastal_R/01_Scripts")
getwd()


# Now we need to extract from BlueLSOAs_All the data we need for the shiny script 
# that I have already written - so we are just trying to fit into what I already have

# So what we already have are:
BSC_DistData_NoLondonNoGaps = read.csv("../ShinyMapping/Old_BSC_LSOA21_Distance_NoLondon_NoGaps.csv")
names(BSC_DistData_NoLondonNoGaps)

BGC_DistData_NoLondonNoGaps = read.csv("../ShinyMapping/Old_BGC_LSOA21_Distance_NoLondon_NoGaps.csv")
names(BGC_DistData_NoLondonNoGaps)

BlueData = read.csv("../ShinyMapping/BlueLSOACategories.csv")
names(BlueData)





# Save final collated version
BlueLSOAs_All = read.csv("../20_LSOACoastalLists/BlueGreenCategorisation.csv")
names(BlueLSOAs_All)
