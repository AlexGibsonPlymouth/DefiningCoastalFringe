
# 03_FindPoints_on_BGC_Coastline.R

# Establish which LSOA population-weighted centroids lie within 500m - 40km of the BGC Coastline

# Preliminaries

library(tidyverse)  # contains dplyr, ggplot2, readr, tidyr, stringr, purrr and forcats
library(sf)         # 'simple features' for mapping
library(mapview)    # for simple display
library(data.table) # for rbindlist

setwd("H:/000_Coastal_Related/00_ESRC Coastal Classification/00_SpatialDefinitionofCoastal_R/01_Scripts")
getwd()

# Get the data we need #########################################################
Region = st_read("../02_InputData/Regions_December_2024_Boundaries_EN_BGC_-2638963433149857560.gpkg")
RegionList = unique(Region$RGN24CD)

Lookup = read.csv("../02_InputData/OA21CD_LSOA21CD_MSOA21CD_LAD23CD_UTLA23CD_RGN23CD_England.csv")
LSOA_Region = Lookup %>% group_by(LSOA21CD,RGN23CD) %>% summarise(Count = n()) %>%
  ungroup() %>% select(LSOA21CD,RGN23CD) %>% rename(RGN24CD = RGN23CD)
LSOA.df = Lookup %>% select(LSOA21CD) %>% distinct(LSOA21CD) %>% arrange(LSOA21CD)

# (2) Read in 2021 LSOA spatial centroids file 
LSOA21_Centroids = st_read("../02_InputData/LLSOA_Dec_2021_PWC_for_England_and_Wales_2022_-7826745778656378855.gpkg")
LSOA21_Centroids = LSOA21_Centroids %>% dplyr::left_join(LSOA_Region, by = "LSOA21CD")


Coastline = st_read("../02_InputData/Countries_December_2024_Boundaries_UK_BGC_-9016689610564788328.gpkg")
mapview(Coastline)
# Get rid of NI
Coastline = Coastline %>% dplyr::filter(CTRY24NM != "Northern Ireland")

# Create cookie cutter to minimise coastline 
poly = st_polygon(list(rbind(
  c(394986,700000), c(500345,533497),c(561137,414863),c(665027,327080),c(666795,238951),c(652706,141533),
  c(586713,92446),c(163679,1421),c(75740,476),c(70318,40983),c(253797,169548),c(222004,674964),c(394986,700000))))

sf_poly <- st_sf(geometry = st_sfc(poly))
CookieCutter <- st_set_crs(sf_poly, 27700)
#mapview(CookieCutter)

# Clip the coastline
Coastline <- st_intersection(Coastline, CookieCutter)
#mapview(Coastline)

# Dissolve
Dissolved_Coastline = Coastline %>% dplyr::group_by() %>% dplyr::summarise()
#mapview(Dissolved_Coastline)



coords <- as.data.frame(st_coordinates(Dissolved_Coastline))
Loops = nrow(coords) - 1

NewCoastline = data.frame(X = coords$X[1], Y = coords$Y[1])

for (i in 2:Loops) {
  j=i+1
  print(paste0("Dealing with i = ",i," of ",Loops))
  dist = sqrt(((coords$X[i] - coords$X[j])^2)+((coords$Y[i] - coords$Y[j])^2))
  dist
  if (dist > 500 & dist < 20000){
    Addin = data.frame(X = coords$X[i], Y = coords$Y[i])
    Cuts = ceiling(dist / 500)
    Additions = Cuts - 1
    XDiff = coords$X[j] - coords$X[i]
    YDiff = coords$Y[j] - coords$Y[i]
    XPart = XDiff/Cuts
    YPart = YDiff/Cuts
    for (k in 1:Additions){
      Addon = data.frame(X = coords$X[i] + k*XPart, Y = coords$Y[i] + k*YPart)
      Addin = rbind(Addin,Addon)
    }
    NewCoastline = rbind(NewCoastline,Addin)
  } else {
    Addin = data.frame(X = coords$X[i], Y = coords$Y[i])
    MyList = list(NewCoastline,Addin)
    NewCoastline = rbindlist(MyList)
  }
}

nrow(NewCoastline)

CoastalPoints = NewCoastline |>  st_as_sf(coords = c("X", "Y"), crs = "EPSG:27700") 

#mapview(CoastalPoints) + mapview(Coastline_BGC)

st_write(CoastalPoints,"../04_CoastlinesAsPoints/BGC_Coastline_asPoints.gpkg", append = FALSE)
