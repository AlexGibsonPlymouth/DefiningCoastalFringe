
# 01_Create_BGC_and_BSC_Coastline_Buffers.R

# Create coastal buffers 50m and 500m to 40km at 500m intervals using the BGC and BSC resolution maps.


# Preliminaries
library(tidyverse)  # contains dplyr, ggplot2, readr, tidyr, stringr, purrr and forcats
library(tidytable)  # for case
library(sf)         # 'simple features' for mapping
library(ggplot2)    # for chloropleth mapping
library(mapview)    # for simple display
library(sgapi)      # a wrapper for using the Open Geography Portal & NOMIS APIs 
library(geojsonsf)  # for reading geojson files
library(viridis)    # for colour palettes 
library(nngeo)      # needed to remove holes (with st_remove_holes)
library(smoothr)    # to fill_holes (in polygons)

setwd("H:/000_Coastal_Related/00_ESRC Coastal Classification/00_SpatialDefinitionofCoastal_R/01_Scripts")
getwd()


########################  Deal with BSC resolution #################
# (1) Get the data 
Coastline = st_read("../02_InputData/Countries_December_2024_Boundaries_UK_BSC_9221418745258737265.gpkg")
Output.Filename.Prefix = "../03_Buffers/BSC_Buffers/BSC_Buffer_"
Output.Filename.Suffix = "m.gpkg"

# (2) Minimise coastline for which buffer to be produced

  # (a) Remove NI
    Coastline = Coastline %>% dplyr::filter(CTRY24NM != "Northern Ireland")
    Coastline
    #mapview(Coastline)
  
  # (b) Create cookie cutter and restrict coastline polygon to 40km around England
    poly = st_polygon(list(rbind(
      c(394986,700000), c(500345,533497),c(561137,414863),c(665027,327080),c(666795,238951),c(652706,141533),
      c(586713,92446),c(163679,1421),c(75740,476),c(70318,40983),c(253797,169548),c(222004,674964),c(394986,700000))))
    
    sf_poly <- st_sf(geometry = st_sfc(poly))
    CookieCutter <- st_set_crs(sf_poly, 27700)
  
    # Clip the coastline
    Coastline <- st_intersection(Coastline, CookieCutter)
  
  
  # (c) Dissolve countries into single unit
    Dissolved_Coastline = Coastline %>% dplyr::group_by() %>% dplyr::summarise()
    mapview(Dissolved_Coastline)
  

# (3) Create buffers for 50m, 100m, 200m, 300m and 500m - 40km at 500m intervals
    
BufferDistances = c(50,100,200,300,seq(500,40000,500))   
BufferNames = as.character(BufferDistances)
Loops = length(BufferDistances)

for (i in 1:Loops) {
  print(paste0("Dealing with i = ",i))
  InternalDistance = -1*BufferDistances[i]
  internal.buffer = st_buffer(Dissolved_Coastline, InternalDistance)
  buffer = st_difference(Dissolved_Coastline,internal.buffer) 
  st_write(buffer, paste0(Output.Filename.Prefix,BufferNames[i],Output.Filename.Suffix), append=FALSE)
}

# Extra 5m for blue/blueproximate
print(paste0("Dealing with special 5metre buffer for blue & blue proximate LSOAs"))
InternalDistance = -5
internal.buffer = st_buffer(Dissolved_Coastline, InternalDistance)
buffer = st_difference(Dissolved_Coastline,internal.buffer) 
mapview(buffer)
st_write(buffer, "../03_Buffers/BSC_Buffers/BSC_Buffer_5m.gpkg", append=FALSE)


########################  Deal with BGC resolution #################
# (1) Get the data 
Coastline = st_read("../02_InputData/Countries_December_2024_Boundaries_UK_BGC_-9016689610564788328.gpkg")
Output.Filename.Prefix = "../03_Buffers/BGC_Buffers/BGC_Buffer_"
Output.Filename.Suffix = "m.gpkg"

# (2) Minimise coastline for which buffer to be produced

# (a) Remove NI
Coastline = Coastline %>% dplyr::filter(CTRY24NM != "Northern Ireland")
Coastline
mapview(Coastline)

# (b) Create cookie cutter and restrict coastline polygon to 40km around England
poly = st_polygon(list(rbind(
  c(394986,700000), c(500345,533497),c(561137,414863),c(665027,327080),c(666795,238951),c(652706,141533),
  c(586713,92446),c(163679,1421),c(75740,476),c(70318,40983),c(253797,169548),c(222004,674964),c(394986,700000))))

sf_poly <- st_sf(geometry = st_sfc(poly))
CookieCutter <- st_set_crs(sf_poly, 27700)

# Clip the coastline
Coastline <- st_intersection(Coastline, CookieCutter)


# (c) Dissolve countries into single unit
Dissolved_Coastline_BGC = Coastline %>% dplyr::group_by() %>% dplyr::summarise()
mapview(Dissolved_Coastline_BGC)


# (3) Create buffers for 50m, 100m, 200m, 300m and 500m - 40km at 500m intervals

BufferDistances = c(50,100,200,300,seq(500,40000,500))   
BufferNames = as.character(BufferDistances)
Loops = length(BufferDistances)

for (i in 1:Loops) {
  print(paste0("Dealing with i = ",i))
  InternalDistance = -1*BufferDistances[i]
  internal.buffer = st_buffer(Dissolved_Coastline_BGC, InternalDistance)
  buffer = st_difference(Dissolved_Coastline_BGC,internal.buffer) 
  st_write(buffer, paste0(Output.Filename.Prefix,BufferNames[i],Output.Filename.Suffix), append=FALSE)
}

# Extra 5m for blue/blueproximate
print(paste0("Dealing with special 5metre buffer for blue & blue proximate LSOAs"))
InternalDistance = -5
internal.buffer = st_buffer(Dissolved_Coastline_BGC, InternalDistance)
buffer = st_difference(Dissolved_Coastline_BGC,internal.buffer) 
mapview(buffer)
st_write(buffer, "../03_Buffers/BGC_Buffers/BGC_Buffer_5m.gpkg", append=FALSE)





