
# 02_LSOADistanceFromCoastlineCategorisation.R

# Establish which LSOA population-weighted centroids lie within 500m - 20km of the 
# coastline - defined using the BFC, BGC and BSC resolution coastlines

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


setwd("H:/000_Coastal_Related/00_ESRC Coastal Classification/00_SpatialDefinitionofCoastal_R")
getwd()

# Objectives
# Download Countries_December_2024_Boundaries_UK_BSC from the Open Geography Portal
# Dissolve countries
# Create 1k - 20km buffers around coastline

# (1) Download LSOA21 data from Open Geography Portal
# (2) Save as geopackage
# (3) Download 

# I need to speed this up by creating hierarchical approach - e.g. buffers with regions



# Get the data we need #########################################################

# (1) Read in version of the countries spatial file
Coastline_BFC = st_read("Countries_December_2024_Boundaries_UK_BFC_-7025106291414208090.gpkg")
Coastline_BFC
#mapview(Coastline_BFC)

Coastline_BGC = st_read("Countries_December_2024_Boundaries_UK_BGC_-9016689610564788328.gpkg")
Coastline_BGC
#mapview(Coastline_BGC)

Coastline_BSC = st_read("Countries_December_2024_Boundaries_UK_BSC_9221418745258737265.gpkg")
Coastline_BSC
#mapview(Coastline_BSC)



# (2) Read in versions of the 2021 LSOA spatial file
LSOA21_BFC = st_read("Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BFC_V10_-672099234420024429.gpkg")
LSOA21_BFC
#mapview(LSOA21_BFC)

LSOA21_BGC = st_read("Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BGC_V5_-6970154227154374572.gpkg")
LSOA21_BGC
#mapview(Coastline_BGC) + mapview(LSOA21_BGC)

LSOA21_BSC = st_read("Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BSC_V4_6453788336260919790.gpkg")
LSOA21_BSC
#mapview(Coastline_BSC) + mapview(LSOA21_BSC) # touches precisely


# (3) Read in versions of the Region spatial file
Region_BFC = st_read("Regions_December_2024_Boundaries_EN_BFC_8318178770890567257.gpkg")
Region_BFC
#mapview(Region_BFC)

Region_BGC = st_read("Regions_December_2024_Boundaries_EN_BGC_-2638963433149857560.gpkg")
Region_BGC
#mapview(Region_BGC)

Region_BSC = st_read("Regions_December_2024_Boundaries_EN_BSC_-5107433749138478884.gpkg")
Region_BSC
#mapview(Region_BSC)


# (4) Read in 2021 LSOA > Region lookup file
Lookup = read.csv("OA21CD_LSOA21CD_MSOA21CD_LAD23CD_UTLA23CD_RGN23CD_England.csv")
LSOA_Region = Lookup %>% group_by(LSOA21CD,RGN23CD) %>% summarise(Count = n()) %>%
  ungroup() %>% select(LSOA21CD,RGN23CD)
Regions = unique(LSOA_Region$RGN23CD)
# There are 9 regions




# (6) Find LSOAs according to the BGC resolution ################################
# 
# (a) Dissolve countries into single unit
#Dissolved_Coastline_BGC = Coastline_BGC %>% dplyr::group_by() %>% dplyr::summarise()
#st_write(Dissolved_Coastline_BGC, "./Dissolved_Coastline_BGC.gpkg", append=FALSE)
Dissolved_Coastline_BGC = st_read("./Dissolved_Coastline_BGC.gpkg")

# (b) Create a narrow (50 metre) buffer on the inside of the coastline
#buffer_50 = st_buffer(Dissolved_Coastline_BGC, -50)
#buffer = st_difference(Dissolved_Coastline_BGC,buffer_50) 
#st_write(buffer, "./Definitive/BGC_CoastlineBuffer.gpkg", append=FALSE)
buffer = st_read("./Definitive/BGC_CoastlineBuffer.gpkg")
#mapview(buffer)

# (c) Deal with LSOAs within Region 1 (set up output)
print(paste0("Dealing with region 1 of 9"))

# Extract LSOAs in the region
LSOAList = LSOA_Region %>% filter(RGN23CD == Regions[1]) %>% select(LSOA21CD) %>% pull(.)
LSOASet = LSOA21_BGC %>% dplyr::filter(LSOA21CD %in% LSOAList)
#mapview(LSOASet)

# Extract the Buffer for the region
WorkingRegion = Region_BGC %>% dplyr::filter(RGN24CD == Regions[1])
WorkingBuffer = st_intersection(WorkingRegion,buffer)
#mapview(WorkingBuffer)

# Find LSOAs on the region that intersect with the region buffer
LSOAIntersectsBGC = LSOASet[WorkingBuffer, op = st_intersects] 
#mapview(LSOAIntersectsBGC)

# Dissolved the set of LSOAs into a single polygon
Dissolved_LSOAs = LSOAIntersectsBGC %>% dplyr::group_by() %>% dplyr::summarise()
#mapview(Dissolved_LSOAs)

# Fill holes in the single dissolved polygon
area_thresh <- units::set_units(500, km^2)
Filled_Dissolved_LSOAs = fill_holes(Dissolved_LSOAs, threshold = area_thresh)
#mapview(Filled_Dissolved_LSOAs, col.regions="red") + mapview(Dissolved_LSOAs) 

# Find all LSOAs within the single dissolved polygon without holes
LSOAWithinBGC = LSOASet[Filled_Dissolved_LSOAs, op = st_within] 
#mapview(LSOAWithinBGC, col.regions="red")

# Merge the original LSOAs with any that were found within the dissolved Polygon
LSOAIntersectsBGC = rbind(LSOAIntersectsBGC,LSOAWithinBGC)
FinalBGC = unique(LSOAIntersectsBGC)
#mapview(LSOAIntersectsBGC)


# (d) Loop through remaining regions and rbind to LSOAIntersectsBGC
for (i in 2:9){
  print(paste0("Dealing with region ",i," of 9"))
  # Extract LSOAs in the region
  LSOAList = LSOA_Region %>% filter(RGN23CD == Regions[i]) %>% select(LSOA21CD) %>% pull(.)
  LSOASet = LSOA21_BGC %>% dplyr::filter(LSOA21CD %in% LSOAList)
  #mapview(LSOASet)
  
  # Extract the Buffer for the region
  WorkingRegion = Region_BGC %>% dplyr::filter(RGN24CD == Regions[i])
  WorkingBuffer = st_intersection(WorkingRegion,buffer)
  #mapview(WorkingBuffer)
  
  # Find LSOAs on the region that intersect with the region buffer
  LSOAIntersectsBGC = LSOASet[WorkingBuffer, op = st_intersects] 
  #mapview(LSOAIntersectsBGC)
  
  if (nrow(LSOAIntersectsBGC) > 0) {
    # Dissolved the set of LSOAs into a single polygon
    Dissolved_LSOAs = LSOAIntersectsBGC %>% dplyr::group_by() %>% dplyr::summarise()
    #mapview(Dissolved_LSOAs)
    
    # Fill holes in the single dissolved polygon
    area_thresh <- units::set_units(500, km^2)
    Filled_Dissolved_LSOAs = fill_holes(Dissolved_LSOAs, threshold = area_thresh)
    #mapview(Filled_Dissolved_LSOAs, col.regions="red") + mapview(Dissolved_LSOAs) 
    
    # Find all LSOAs within the single dissolved polygon without holes
    LSOAWithinBGC = LSOASet[Filled_Dissolved_LSOAs, op = st_within] 
    #mapview(LSOAWithinBGC, col.regions="red")
    
    # Merge the original LSOAs with any that were found within the dissolved Polygon
    LSOAIntersectsBGC = rbind(LSOAIntersectsBGC,LSOAWithinBGC)
    LSOAIntersectsBGC = unique(LSOAIntersectsBGC)
    #mapview(LSOAIntersectsBGC)
    
    FinalBGC = rbind(FinalBGC,LSOAIntersectsBGC)
  }
}

bufferBGC = st_read("./Definitive/BGC_CoastlineBuffer.gpkg")
mapview(FinalBGC) + mapview(bufferBGC)

LSOA21_BGC_Dist0_List = FinalBGC %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
write.csv(LSOA21_BGC_Dist0_List,"./Definitive/LSOA21_BGC_Dist0_List.csv")

st_write(FinalBGC, "./Definitive/LSOA21_BGC_Dist0.gpkg", append=FALSE)


################################################################################
################################################################################
################################################################################


# (5) Find LSOAs according to the BFC resolution ###############################
# 
# (a) Dissolve countries into single unit
#Dissolved_Coastline_BFC = Coastline_BFC %>% dplyr::group_by() %>% dplyr::summarise()
#st_write(Dissolved_Coastline_BFC, "./Dissolved_Coastline_BFC.gpkg", append=FALSE)
Dissolved_Coastline_BFC = st_read("./Dissolved_Coastline_BFC.gpkg")

# (b) Create a narrow (50 metre) buffer on the inside of the coastline
#buffer_50 = st_buffer(Dissolved_Coastline_BFC, -50)
#buffer = st_difference(Dissolved_Coastline_BFC,buffer_50) 
#st_write(buffer, "./Definitive/BFC_CoastlineBuffer.gpkg", append=FALSE)
buffer = st_read("./Definitive/BFC_CoastlineBuffer.gpkg")
#mapview(buffer)

# (c) Deal with LSOAs within Region 1 (set up output)
print(paste0("Dealing with region 1 of 9"))

# Extract LSOAs in the region
LSOAList = LSOA_Region %>% filter(RGN23CD == Regions[1]) %>% select(LSOA21CD) %>% pull(.)
LSOASet = LSOA21_BFC %>% dplyr::filter(LSOA21CD %in% LSOAList)
#mapview(LSOASet)

# Extract the Buffer for the region
WorkingRegion = Region_BFC %>% dplyr::filter(RGN24CD == Regions[1])
WorkingBuffer = st_intersection(WorkingRegion,buffer)
#mapview(WorkingBuffer)

# Find LSOAs on the region that intersect with the region buffer
LSOAIntersectsBFC = LSOASet[WorkingBuffer, op = st_intersects] 
#mapview(LSOAIntersectsBFC)

# Dissolved the set of LSOAs into a single polygon
Dissolved_LSOAs = LSOAIntersectsBFC %>% dplyr::group_by() %>% dplyr::summarise()
#mapview(Dissolved_LSOAs)

# Fill holes in the single dissolved polygon
area_thresh <- units::set_units(500, km^2)
Filled_Dissolved_LSOAs = fill_holes(Dissolved_LSOAs, threshold = area_thresh)
#mapview(Filled_Dissolved_LSOAs, col.regions="red") + mapview(Dissolved_LSOAs) 

# Find all LSOAs within the single dissolved polygon without holes
LSOAWithinBFC = LSOASet[Filled_Dissolved_LSOAs, op = st_within] 
#mapview(LSOAWithinBFC, col.regions="red")

# Merge the original LSOAs with any that were found within the dissolved Polygon
LSOAIntersectsBFC = rbind(LSOAIntersectsBFC,LSOAWithinBFC)
FinalBFC = unique(LSOAIntersectsBFC)
#mapview(LSOAIntersectsBFC)


# (d) Loop through remaining regions and rbind to LSOAIntersectsBFC
for (i in 2:9){
  print(paste0("Dealing with region ",i," of 9"))
  # Extract LSOAs in the region
  LSOAList = LSOA_Region %>% filter(RGN23CD == Regions[i]) %>% select(LSOA21CD) %>% pull(.)
  LSOASet = LSOA21_BFC %>% dplyr::filter(LSOA21CD %in% LSOAList)
  #mapview(LSOASet)
  
  # Extract the Buffer for the region
  WorkingRegion = Region_BFC %>% dplyr::filter(RGN24CD == Regions[i])
  WorkingBuffer = st_intersection(WorkingRegion,buffer)
  #mapview(WorkingBuffer)
  
  # Find LSOAs on the region that intersect with the region buffer
  LSOAIntersectsBFC = LSOASet[WorkingBuffer, op = st_intersects] 
  #mapview(LSOAIntersectsBFC)
  
  if (nrow(LSOAIntersectsBFC) > 0) {
    # Dissolved the set of LSOAs into a single polygon
    Dissolved_LSOAs = LSOAIntersectsBFC %>% dplyr::group_by() %>% dplyr::summarise()
    #mapview(Dissolved_LSOAs)
    
    # Fill holes in the single dissolved polygon
    area_thresh <- units::set_units(500, km^2)
    Filled_Dissolved_LSOAs = fill_holes(Dissolved_LSOAs, threshold = area_thresh)
    #mapview(Filled_Dissolved_LSOAs, col.regions="red") + mapview(Dissolved_LSOAs) 
    
    # Find all LSOAs within the single dissolved polygon without holes
    LSOAWithinBFC = LSOASet[Filled_Dissolved_LSOAs, op = st_within] 
    #mapview(LSOAWithinBFC, col.regions="red")
    
    # Merge the original LSOAs with any that were found within the dissolved Polygon
    LSOAIntersectsBFC = rbind(LSOAIntersectsBFC,LSOAWithinBFC)
    LSOAIntersectsBFC = unique(LSOAIntersectsBFC)
    #mapview(LSOAIntersectsBFC)
    
    FinalBFC = rbind(FinalBFC,LSOAIntersectsBFC)
  }
}

bufferBFC = st_read("./Definitive/BFC_CoastlineBuffer.gpkg")
mapview(bufferBFC, col.regions = "red") + mapview(FinalBFC)

LSOA21_BFC_Dist0_List = FinalBFC %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
write.csv(LSOA21_BFC_Dist0_List,"./Definitive/LSOA21_BFC_Dist0_List.csv")

st_write(FinalBFC, "./Definitive/LSOA21_BFC_Dist0.gpkg", append=FALSE)




################################################################################
################################################################################
################################################################################



# (7) Find LSOAs according to the BSC resolution ###############################
# 
# (a) Dissolve countries into single unit
#Dissolved_Coastline_BSC = Coastline_BSC %>% dplyr::group_by() %>% dplyr::summarise()
#st_write(Dissolved_Coastline_BSC, "./Dissolved_Coastline_BSC.gpkg", append=FALSE)
#Dissolved_Coastline_BSC = st_read("./Dissolved_Coastline_BSC.gpkg")

# (b) Create a narrow (50 metre) buffer on the inside of the coastline
#buffer_50 = st_buffer(Dissolved_Coastline_BSC, -50)
#buffer = st_difference(Dissolved_Coastline_BSC,buffer_50) 
#st_write(buffer, "./Definitive/BSC_CoastlineBuffer.gpkg", append=FALSE)
buffer = st_read("./Definitive/BSC_CoastlineBuffer.gpkg")
#mapview(buffer)

# (c) Deal with LSOAs within Region 1 (set up output)
print(paste0("Dealing with region 1 of 9"))

# Extract LSOAs in the region
LSOAList = LSOA_Region %>% filter(RGN23CD == Regions[1]) %>% select(LSOA21CD) %>% pull(.)
LSOASet = LSOA21_BSC %>% dplyr::filter(LSOA21CD %in% LSOAList)
#mapview(LSOASet)

# Extract the Buffer for the region
WorkingRegion = Region_BSC %>% dplyr::filter(RGN24CD == Regions[1])
WorkingBuffer = st_intersection(WorkingRegion,buffer)
#mapview(WorkingBuffer)

# Find LSOAs on the region that intersect with the region buffer
LSOAIntersectsBSC = LSOASet[WorkingBuffer, op = st_intersects] 
#mapview(LSOAIntersectsBSC)

# Dissolved the set of LSOAs into a single polygon
Dissolved_LSOAs = LSOAIntersectsBSC %>% dplyr::group_by() %>% dplyr::summarise()
#mapview(Dissolved_LSOAs)

# Fill holes in the single dissolved polygon
area_thresh <- units::set_units(500, km^2)
Filled_Dissolved_LSOAs = fill_holes(Dissolved_LSOAs, threshold = area_thresh)
#mapview(Filled_Dissolved_LSOAs, col.regions="red") + mapview(Dissolved_LSOAs) 

# Find all LSOAs within the single dissolved polygon without holes
LSOAWithinBSC = LSOASet[Filled_Dissolved_LSOAs, op = st_within] 
#mapview(LSOAWithinBSC, col.regions="red")

# Merge the original LSOAs with any that were found within the dissolved Polygon
LSOAIntersectsBSC = rbind(LSOAIntersectsBSC,LSOAWithinBSC)
FinalBSC = unique(LSOAIntersectsBSC)
#mapview(LSOAIntersectsBSC)


# (d) Loop through remaining regions and rbind to LSOAIntersectsBSC
for (i in 2:9){
  print(paste0("Dealing with region ",i," of 9"))
  # Extract LSOAs in the region
  LSOAList = LSOA_Region %>% filter(RGN23CD == Regions[i]) %>% select(LSOA21CD) %>% pull(.)
  LSOASet = LSOA21_BSC %>% dplyr::filter(LSOA21CD %in% LSOAList)
  #mapview(LSOASet)
  
  # Extract the Buffer for the region
  WorkingRegion = Region_BSC %>% dplyr::filter(RGN24CD == Regions[i])
  WorkingBuffer = st_intersection(WorkingRegion,buffer)
  #mapview(WorkingBuffer)
  
  # Find LSOAs on the region that intersect with the region buffer
  LSOAIntersectsBSC = LSOASet[WorkingBuffer, op = st_intersects] 
  #mapview(LSOAIntersectsBSC)
  
  if (nrow(LSOAIntersectsBSC) > 0) {
    # Dissolved the set of LSOAs into a single polygon
    Dissolved_LSOAs = LSOAIntersectsBSC %>% dplyr::group_by() %>% dplyr::summarise()
    #mapview(Dissolved_LSOAs)
    
    # Fill holes in the single dissolved polygon
    area_thresh <- units::set_units(500, km^2)
    Filled_Dissolved_LSOAs = fill_holes(Dissolved_LSOAs, threshold = area_thresh)
    #mapview(Filled_Dissolved_LSOAs, col.regions="red") + mapview(Dissolved_LSOAs) 
    
    # Find all LSOAs within the single dissolved polygon without holes
    LSOAWithinBSC = LSOASet[Filled_Dissolved_LSOAs, op = st_within] 
    #mapview(LSOAWithinBSC, col.regions="red")
    
    # Merge the original LSOAs with any that were found within the dissolved Polygon
    LSOAIntersectsBSC = rbind(LSOAIntersectsBSC,LSOAWithinBSC)
    LSOAIntersectsBSC = unique(LSOAIntersectsBSC)
    #mapview(LSOAIntersectsBSC)
    
    FinalBSC = rbind(FinalBSC,LSOAIntersectsBSC)
  }
}

bufferBSC = st_read("./Definitive/BSC_CoastlineBuffer.gpkg")
mapview(FinalBSC) + mapview(bufferBSC)

LSOA21_BSC_Dist0_List = FinalBSC %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
write.csv(LSOA21_BSC_Dist0,"./Definitive/LSOA21_BSC_Dist0_List.csv")

st_write(FinalBSC, "./Definitive/LSOA21_BSC_Dist0.gpkg", append=FALSE)


#################################################################################
#################################################################################
#################################################################################


















# (e) There will be LSOAs completely surrounded by LSOAs that touch the coast
#     These will be categorised coastal - start by dissolving LSOAIntersectsBSC
Dissolved_LSOAs = LSOAIntersectsBSC %>% dplyr::group_by() %>% dplyr::summarise()
mapview(Dissolved_LSOAs)

# (f) Fill holes in the Dissolved_LSOAs
area_thresh <- units::set_units(500, km^2)
Filled_Dissolved_LSOAs = fill_holes(Dissolved_LSOAs, threshold = area_thresh)
mapview(Filled_Dissolved_LSOAs, col.regions="red") + mapview(Dissolved_LSOAs) 

# (g) Now find all LSOAs within the area covered by Filled_Dissolved_LSOAs
#     Starting with the first region 

print(paste0("Dealing with region 1 of 9"))
LSOAList = LSOA_Region %>% filter(RGN23CD == Regions[1]) %>% select(LSOA21CD) %>% pull(.)
LSOASet = LSOA21_BSC %>% dplyr::filter(LSOA21CD %in% LSOAList)

WorkingRegion = Region_BSC %>% dplyr::filter(RGN24CD == Regions[1])
WorkingFilled_Dissolved_LSOAs = st_intersection(WorkingRegion,Filled_Dissolved_LSOAs)
mapview(WorkingFilled_Dissolved_LSOAs)
        
FinalLSOAOverlapBSC = LSOASet[WorkingFilled_Dissolved_LSOAs, op = st_overlaps]
FinalLSOAWithinBSC = LSOASet[WorkingFilled_Dissolved_LSOAs, op = st_within] 
FinalLSOAIntersectBSC = LSOASet[WorkingFilled_Dissolved_LSOAs, op = st_intersects] 

FinalLSOAIntersectsBSC = rbind(FinalLSOAOverlapBSC,FinalLSOAWithinBSC)
FinalLSOAIntersectsBSC = unique(FinalLSOAIntersectsBSC)
mapview(FinalLSOAOverlapBSC)
mapview(FinalLSOAWithinBSC)
mapview(FinalLSOAIntersectBSC)
mapview(FinalLSOAIntersectsBSC)

# (h) And now looping through the remaining regions
for (i in 2:9){
  print(paste0("Dealing with region ",i," of 9"))
  LSOAList = LSOA_Region %>% filter(RGN23CD == Regions[i]) %>% select(LSOA21CD) %>% pull(.)
  LSOASet = LSOA21_BSC %>% dplyr::filter(LSOA21CD %in% LSOAList)
  
  WorkingRegion = Region_BSC %>% dplyr::filter(RGN24CD == Regions[i])
  WorkingFilled_Dissolved_LSOAs = st_intersection(WorkingRegion,Filled_Dissolved_LSOAs)
  mapview(WorkingFilled_Dissolved_LSOAs)
  
  WorkingLSOAOverlapBSC = LSOASet[WorkingFilled_Dissolved_LSOAs, op = st_overlaps] 
  WorkingLSOAWithinBSC = LSOASet[WorkingFilled_Dissolved_LSOAs, op = st_within] 
  WorkingLSOAIntersectsBSC = rbind(WorkingLSOAOverlapBSC,WorkingLSOAWithinBSC)
  WorkingLSOAIntersectsBSC = unique(WorkingLSOAIntersectsBSC)
  
  FinalLSOAIntersectsBSC = rbind(FinalLSOAIntersectsBSC,WorkingIntersectsBSC)
}
#mapview(Dissolved_Coastline_BSC) + mapview(FinalLSOAIntersectsBSC)
mapview(FinalLSOAIntersectsBSC) + mapview(LSOAIntersectsBSC)

# (i) Extract the LSOAs in the FinalLSOAIntersectsBSC and save as a list
LSOA21_BSC_Dist0 = FinalLSOAIntersectsBSC %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
write.csv(LSOA21_BSC_Dist0,"./Definitive/LSOA21_BSC_Dist0.csv")



################################################################################

# Alternative (faster?) Methodology

buffer = Coastline_BGC = st_read("./Definitive/BSC_CoastlineBuffer.gpkg")
mapview(buffer)

LondonRegion = Region_BGC %>% dplyr::filter(RGN24NM == "South East")
LondonBuffer = st_intersection(LondonRegion,buffer)
mapview(Coastline_BGC) + mapview(LondonBuffer)
  
LondonBuffer = Coastline_BGC[LondonRegion, op = st_within()]
mapview(LondonBuffer)








# (3) Rebuffer# (3) Read in 2021 LSOA centroids file
LSOA21_Centroids = st_read("LLSOA_Dec_2021_PWC_for_England_and_Wales_2022_-7826745778656378855.gpkg")
#LSOA21_Centroids
#mapview(LSOA21_Centroids)


# Process the data for the 'baseline' 20km #############################################################

# (1) Dissolve countries into single unit
Dissolved_Coastline_BSC = Coastline_BSC %>% dplyr::group_by() %>% dplyr::summarise()
#Dissolved_Coastline_BSC
#mapview(Dissolved_Coastline_BSC)
st_write(Dissolved_Coastline_BSC, "./Dissolved_Coastline_BSC.gpkg", append=FALSE)


Dissolved_Coastline_BSC = st_read("./Dissolved_Coastline_BSC.gpkg")

# (2) Create buffer
buff_40km = st_buffer(Dissolved_Coastline_BSC, -40000)
mapview(Dissolved_Coastline_BSC,col.regions = "red") + mapview(buff_40km)
buffer40km = st_difference(Dissolved_Coastline_BSC,buff_40km) 
mapview(buffer40km,col.regions = "red")
st_write(buffer40km, "./buffer40km.gpkg", append=FALSE)



# (3) Find all LSOAs with population-weighted centroids within the buffer
# This takes a very long time as it has to test each of the 33,755 LSOAs (plus those in Wales which I could have excluded)
# Can I speed up each pass by only looking at LSOAs in the previous pass?
Extract.Centroids = LSOA21_Centroids[buffer40km, op = st_within]
# Save file (in this case as GeoPackage) - will fail if file with name already exists
st_write(Extract.Centroids, "./Extract.Centroids_40kmBuffer.gpkg", append=FALSE)

Extract.Centroids = st_read("./Extract.Centroids_40kmBuffer.gpkg")


mapview(Extract.Centroids, color = "green", cex = 3)

Extract.LSOA.Codes = as.data.frame(Extract.Centroids$LSOA21CD)
names(Extract.LSOA.Codes) = "LSOA21CD"

Extract.LSOAs = LSOA21_BFC %>% dplyr::filter(LSOA21CD %in% Extract.LSOA.Codes$LSOA21CD)
mapview(Extract.LSOAs) + mapview(Extract.Centroids, color = "red", cex = 3)
st_write(Extract.LSOAs, "./Extract.LSOAs_withHoles_40kmBuffer.gpkg", append=FALSE)

Extract.LSOAs = st_read("./Extract.LSOAs_withHoles_40kmBuffer.gpkg")


#      # (4) find & remove holes 
#      Dissolve.Extract.LSOAs = Extract.LSOAs %>% dplyr::group_by() %>% dplyr::summarise()
#      mapview(Dissolve.Extract.LSOAs) 
#      
#      NoHole <- nngeo::st_remove_holes(Dissolve.Extract.LSOAs)
#      mapview(LSOA21_BFC, col.regions = "red") + mapview(Dissolve.Extract.LSOAs)
#      
#      # (5) Find all LSOAs in no holes region (st_within)
#      # I let this run for 12 hours - hadn't finished!
#      # It might have been looping blindly (there was heavy processor use) and I might have been doing the 
#      # wrong thing, but I suspect the task was just too big!  
#      # It can be done by eye relatively quickly - so have used QGIS to identify the 
#      # very few problematic LSOAs
#      
#      # Because centroid outside irregular coastline - MUST be included
#      "E01018849"
      
#      # Because surrounded by coastal
#      c("E01018946","E01018948","E01018949","E01018950","E01034848")
#      c("E01022220","E01034789","E01022219")
#      c("E01006253","E01006254","E01006260","E01006372","E01006373")
#      "E01020054"
      
#      # But (apart from "E01018848") there is logic in using the centroids - even if there are visual holes!

# Our baseline list of all E&W LSOAs (accepting holes) is:
Addon = data.frame(LSOA21CD = "E01018849")
nrow(Addon) # 1
LSOAList = Extract.Centroids %>% st_drop_geometry()%>% select(LSOA21CD)
nrow(LSOAList) # 24989
LSOA21_EW_40km_withholes = rbind(LSOAList,Addon)
nrow(LSOA21_EW_40km_withholes) # 24990
write.csv(LSOA21_EW_40km_withholes,"LSOA21_EW_40km_withholes.csv", row.names=FALSE)
st_write(Extract.LSOAs, "./Extract.LSOAs_withHoles_40kmBuffer.gpkg", append=FALSE)
mapview(Extract.LSOAs)

Extract.LSOAs = st_read("./Extract.LSOAs_withHoles_40kmBuffer.gpkg")



# Our list of all E&W LSOAs (having removed holes) is:
AddonList = c("E01018849","E01011390","E01018849")
Addon = data.frame(LSOA21CD = AddonList)
nrow(Addon) # 3
LSOAList = Extract.Centroids %>% st_drop_geometry()%>% select(LSOA21CD)
nrow(LSOAList) # 24989
LSOA21_EW_40km_noholes = rbind(LSOAList,Addon)
nrow(LSOA21_EW_40km_noholes) # 24992
write.csv(LSOA21_EW_40km_noholes,"LSOA21_EW_40km_noholes.csv", row.names=FALSE)

Extract.LSOAs.withHoles = LSOA21_BFC %>% dplyr::filter(LSOA21CD %in% LSOA21_EW_40km_withholes$LSOA21CD)
nrow(Extract.LSOAs.withHoles) # 24990
Extract.LSOAs.withHoles %>% dplyr::filter(LSOA21CD == "E01018849")
st_write(Extract.LSOAs, "./Extract.LSOAs_withHoles_40kmBuffer.gpkg", append=FALSE)


# Loop to create 'with holes' versions for 39.5km - 20.5km geographies


LSOA21_EW_40km_noholes = read.csv("./LSOA21_EW_40km_noholes.csv")
LSOA21_Centroids = st_read("LLSOA_Dec_2021_PWC_for_England_and_Wales_2022_-7826745778656378855.gpkg")
Dissolved_Coastline_BSC = st_read("./Dissolved_Coastline_BSC.gpkg")
LSOA21_BFC = st_read(dsn = "Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10)", layer = "Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10)")

London_LSOA21 = read.csv("./OA21CD_LSOA21CD_MSOA21CD_LAD23CD_UTLA23CD_RGN23CD_England.csv")
head(London_LSOA21)
London_LSOA21 = London_LSOA21 %>% filter(RGN23NM == "London")
nrow(London_LSOA21) # 26369
London_LSOA21 = London_LSOA21 %>% distinct(LSOA21CD) %>% pull(.)
length(London_LSOA21) # 4994

Wales_LSOA21 = LSOA21_BFC %>% st_drop_geometry() %>% dplyr::select(LSOA21CD) %>% 
  mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "W") %>% select(LSOA21CD) %>% pull(.)
length(Wales_LSOA21) # 1917

London_Wales = c(London_LSOA21,Wales_LSOA21)
length(London_Wales) # 6911

# (1) Extract subset of E&W LSOA21s centroids within the 40km buffer
# NB - I will use the 'no holes' list
LSOA40km_List = LSOA21_EW_40km_noholes %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
length(LSOA40km_List) # 24992
LSOA21_Centroids_40km = LSOA21_Centroids %>% dplyr::filter(LSOA21CD %in% LSOA40km_List)
LSOA21_Centroids_40km
nrow(LSOA21_Centroids_40km) # 24991

# Start Loop here
DistSeq = seq(-27500, -20500, 500)
Loops = length(DistSeq)

for (i in 2:Loops) {
    #i = 1
    Dist = DistSeq[i]
    KmDistText = as.character(-1*Dist)
    BufferOutFilename = paste0("./buffer_",KmDistText,"metre.gpkg")
    LSOA21CentroidsSpatialFilename = paste0("./Extract_LSOA21Centroids_",KmDistText,"metreBuffer.gpkg")
    LSOA21BoundariesFilename = paste0("./Extract_LSOA21_withHoles_",KmDistText,"metreBuffer.gpkg")
    LSOA21ListFilename = paste0("./Extract_LSOA21_withHoles_",KmDistText,"metreBuffer.csv")
    
    
    # (2) Create buffer
    buff_current = st_buffer(Dissolved_Coastline_BSC, Dist)
    buffer_current = st_difference(Dissolved_Coastline_BSC,buff_current) 
    #mapview(buffer_current)
    st_write(buffer_current, BufferOutFilename, append=FALSE)
    
    
    # (3) Find all LSOAs with population-weighted centroids within the buffer
    # This takes a long time as it has to test each of the LSOAs
    # I have improved things a bit by 
    # Can I speed up each pass by only looking at LSOAs in the previous pass?
    Extract.Centroids = LSOA21_Centroids_40km[buffer_current, op = st_within]
    # Save file (in this case as GeoPackage) - will fail if file with name already exists
    st_write(Extract.Centroids, LSOA21CentroidsSpatialFilename, append=FALSE)
    
    #mapview(Extract.Centroids, color = "green", cex = 3)
    
    #Extract.Centroids = st_read(LSOA21CentroidsSpatialFilename)
    Extract.LSOA.Codes = as.data.frame(Extract.Centroids$LSOA21CD)
    names(Extract.LSOA.Codes) = "LSOA21CD"
    
    Extract.LSOAs = LSOA21_BFC %>% dplyr::filter(LSOA21CD %in% Extract.LSOA.Codes$LSOA21CD)
    #nrow(Extract.LSOAs) # 18544
    # Add the missing LSOA 
    Addon = LSOA21_BFC %>% dplyr::filter(LSOA21CD == "E01018849")
    Extract.LSOAs = rbind(Extract.LSOAs,Addon)
    #nrow(Extract.LSOAs) # 18545
    #mapview(Extract.LSOAs1)
    
    #mapview(Extract.LSOAs) + mapview(Extract.Centroids, color = "red", cex = 3)
    st_write(Extract.LSOAs, LSOA21BoundariesFilename, append=FALSE)
    
    Extract.LSOA.List = Extract.LSOAs %>% st_drop_geometry() %>% select(LSOA21CD,LSOA21NM)
    TotalEWLSOAs = nrow(Extract.LSOA.List) # 18545
    write.csv(Extract.LSOA.List,LSOA21ListFilename, row.names = FALSE)
    
    # Exclude London and Wales LSOAs - need list of London & Wales Region LSOAs
    NoLondonWalesLSOA21BoundariesFilename = paste0("./England_LSOA21_withHoles_",KmDistText,"metreBuffer.gpkg")
    NoLondonWalesLSOA21ListFilename = paste0("./England_LSOA21_withHoles_",KmDistText,"metreBuffer.csv")
    
    #Extract.LSOAs = st_read(LSOA21BoundariesFilename)
    #nrow(Extract.LSOAs) # 18544
    England.Extract.LSOAs = Extract.LSOAs %>% dplyr::filter(! LSOA21CD %in% London_Wales)
    #nrow(England.Extract.LSOAs) # 11977
    #mapview(England.Extract.LSOAs)
    st_write(England.Extract.LSOAs, NoLondonWalesLSOA21BoundariesFilename, append=FALSE)
    
    #Extract.LSOA.List = read.csv(LSOA21ListFilename)
    #nrow(Extract.LSOA.List) # 18545
    England.Extract.LSOA.List = Extract.LSOA.List %>% filter(! LSOA21CD %in% London_Wales)
    TotalEnglandLSOAs = nrow(England.Extract.LSOA.List) # 11977
    #mapview(England.Extract.LSOAs)
    write.csv(England.Extract.LSOA.List, NoLondonWalesLSOA21ListFilename, row.names = FALSE)
    
    # Print report to screen
    print(paste0("Have processed i = ",i," of ",Loops," where Dist = ",KmDistText," resulting in ",TotalEWLSOAs," E&W LSOAs and ",TotalEnglandLSOAs," English LSOAs"))
    
}

# End loop


# Collate all csv files ################
head(LSOA21_Centroids)
LSOA21.List = LSOA21_Centroids %>% st_drop_geometry() %>% select(LSOA21CD)

DistSeq = seq(20500, 40000, 500)
Loops = length(DistSeq)

i=1
for (i in 1:Loops) {
  Dist = DistSeq[i]
  KmDistText = as.character(Dist)
  ColName = paste0("Dist_",KmDistText)
  NoLondonWalesLSOA21ListFilename = paste0("./England_LSOA21_withHoles_",KmDistText,"metreBuffer.csv")
  Working = read.csv(NoLondonWalesLSOA21ListFilename)
  Working = Working %>% mutate(NewCol = 1) %>% select(LSOA21CD,NewCol)
  names(Working)[2]=ColName 
  LSOA21.List = LSOA21.List %>% left_join(Working, by = "LSOA21CD")
  LSOA21.List = LSOA21.List %>% replace(is.na(.),0)
}
head(LSOA21.List)
nrow(LSOA21.List) # 35672

# Remove not England
LSOA21.List = LSOA21.List %>% mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(Country))
nrow(LSOA21.List) # 33755

# Add to spatial file
LSOA21_BGC = st_read("Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BGC_V5_-6970154227154374572.gpkg")
LSOA21_BGC = LSOA21_BGC %>% dplyr::mutate(Country = substr(LSOA21CD,1,1)) %>% dplyr::filter(Country == "E") %>% dplyr::select(-c(Country))
nrow(LSOA21_BGC)
mapview(LSOA21_BGC)

LSOA21.Points = LSOA21_Centroids %>%  dplyr::mutate(Country = substr(LSOA21CD,1,1)) %>% dplyr::filter(Country == "E") %>% dplyr::select(-c(Country))
nrow(LSOA21.Points)

# Extract polygon of London
Regions = st_read("Regions_December_2024_Boundaries_EN_BFC_8318178770890567257.gpkg")
mapview(Regions)
head(Regions)
London = Regions %>% dplyr::filter(RGN24NM == "London")
mapview(London)

# Extract list of LSOA21CDs in the London region


# Convert Dissolved_Coastline_BSC to line and truncate to exclude Scotland
Coastline_coords <- as.data.frame(st_coordinates(Dissolved_Coastline_BSC))
nrow(Coastline_coords) # 35864
head(Coastline_coords)
Coastline_coords <- Coastline_coords %>% filter(Y < 669174) # removes any points with northing > 669174 (we could be cleverer - but this will do)
nrow(Coastline_coords) # 15585
CoastalPointsInput = Coastline_coords |>  st_as_sf(coords = c("X", "Y"), crs = "EPSG:27700") 
nrow(CoastalPointsInput) # 15585
mapview(CoastalPointsInput)
CoastalPointsInput[4689,]


East = c(382790,311664,269014,256093,215517,160684,77934,84476,170951,459099,647218,659417,396697,382790)
North = c(669174,563546,518703,446138,388056,223222,10612,747,5048,54509,127417,320845,671539,669174)

df <- data.frame(x = East, y = North, Grouper = 1)
snipper <- df %>% st_as_sf(coords = c("x", "y"), crs = "EPSG:27700") %>%
  dplyr::group_by(Grouper) %>% dplyr::summarize(geometry = st_union(geometry)) %>%
  st_convex_hull() 
mapview(snipper)

TestWhetherIn = data.frame(as.logical(st_intersects(CoastalPointsInput, snipper, sparse = FALSE)))
names(TestWhetherIn)[1]="Test"
Subset = cbind(CoastalPointsInput,TestWhetherIn)
nrow(Subset) # 15585
CoastalPoints = Subset %>% dplyr::filter(Test == TRUE)
nrow(CoastalPoints) # 12675
mapview(CoastalPoints)
head(CoastalPoints)

CoastPointsDF = data.frame(st_coordinates(CoastalPoints))
head(CoastPointsDF)


##################################################################################################
##################################################################################################
##################################################################################################

# This is to work with all LSOAs - finding distance AND whether cuts London
LSOA21.Points = LSOA21_Centroids %>% dplyr::select(LSOA21CD) %>%  
                                     dplyr::mutate(Country = substr(LSOA21CD,1,1)) %>% 
                                     dplyr::filter(Country == "E") %>% dplyr::select(-c(Country))
head(LSOA21.Points)
nrow(LSOA21.Points) # 33755


# The distance thresholds 
Thresholds = seq(40000,500,-500)
ThresholdLoops = length(Thresholds)
ThresholdColNames = paste0("Dist_",as.character(Thresholds))

Results <- data.frame(matrix(, nrow = nrow(LSOA21.Points), ncol = ThresholdLoops+1))
names(Results)[1] = "LSOA21CD"
EndCol = ThresholdLoops+1
names(Results)[2:EndCol] = ThresholdColNames

LSOALoops = nrow(LSOA21.Points)
#TestLoops = sample(c(1:LSOALoops),100)

Results[,1] = LSOA21.Points$LSOA21CD

StartTime = Sys.time()

### Temporary update to add a run of 20km down to ###########################################
Thresholds = seq(19500,500,-500)
ThresholdLoops = length(Thresholds)
ThresholdColNames = paste0("Dist_",as.character(Thresholds))

AddonResults <- data.frame(matrix(, nrow = nrow(LSOA21.Points), ncol = ThresholdLoops))
EndCol = ThresholdLoops
names(AddonResults)[1:EndCol] = ThresholdColNames

Results = cbind(Results,AddonResults)
names(Results)
head(Results)
##########################################################################################

Thresholds = seq(40000,500,-500)
ThresholdLoops = length(Thresholds)
ThresholdColNames = paste0("Dist_",as.character(Thresholds))

j = 43
ThresholdColNames[j]

# Loop through all threshold distances
#j=3
#for (j in 1:ThresholdLoops){
for (j in 43:ThresholdLoops){
    StartTime = Sys.time()
    ThresholdDist = Thresholds[j]
    ThresholdDistText = as.character(ThresholdDist)
    
    # Loop through all LSOAs
    #i = 80
    for (i in 1:LSOALoops){
    #for (i in 1:10){
      #i=1000
      #for (i in 1:20){
      #for (i in TestLoops) {
      
      # Progress report
      if(i%%100 == 0) {
        print(paste0("Threshold = ",ThresholdDistText," with LSOA i = ",i," of ",LSOALoops))
      }
      
      TargetLSOA = LSOA21.Points[i,] # This is the LSOA code for the LSOA i
      CurrentCol = j+1
      PreviousCol = j
      # Only test for distance and crossing London if TargetLSOA is not in London and (if j > 1) if previous LSOA value ==1
      if(CurrentCol > 2 & Results[i,PreviousCol] == 0) { # < FIND PREVIOUS RESULT <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        #print("LSOA coastal status for previous distance == 0 -> set value to 0")
        Results[i,CurrentCol] = 0
        
      } else if (TargetLSOA$LSOA21CD %in% London_LSOA21){
        #print("LSOA London - set value to 0")
        Results[i,CurrentCol] = 0
        
        
      } else {
          # Calculate all distances to the coast
          Distances = as.vector(st_distance(TargetLSOA,CoastalPoints))
          Distances.DF = data.frame(Distance = Distances, Eastings = CoastPointsDF$X, Northings = CoastPointsDF$Y)
          Distances.DF = Distances.DF %>% arrange(Distance)
          #head(Distances.DF)
          #nrow(Distances.DF) # 12675 (all points legitimate around the coast)
          
          
          # for j in 1:ThresholdLoops
          # I think we will want to reverse this (if not coastal at 40000 it won't be at 39500)
          Distances.DF = Distances.DF %>% dplyr::filter(Distance<ThresholdDist)
          Possibles = nrow(Distances.DF) # 147 less than threshold
           if (Possibles > 0){
              # which of these are legitimate in that NO straight line to coast goes through London
              #head(Distances.DF)
              LegitCoastalPoints = Distances.DF[,c(2:3)] |>  st_as_sf(coords = c("Eastings", "Northings"), crs = "EPSG:27700") 
              #mapview(LegitCoastalPoints) + mapview(TargetLSOA, col.regions = "red",color = "red")
              
              
              nearest_lines = st_nearest_points(TargetLSOA, LegitCoastalPoints)
              intersectiontest <- as.numeric(st_intersects(nearest_lines, London))
              if(sum(is.na(intersectiontest))>0) {
                #print("LSOA not in London - Within current threshold - Doesn't intersect with London -> set to 1")
                Results[i,CurrentCol] = 1
              } else {
                #print("LSOA not in London - within current threshold -does intersect with London -> set to 0")
                Results[i,CurrentCol] = 0
              }
          } else {
            #print("LSOA not in London - not within current threshold distance -> set to 0")
            Results[i,CurrentCol] = 0
          }
       }
    
    } # end of i loop
    print(Sys.time()-StartTime)
    write.csv(Results,"./TestResults_Update.csv", row.names=FALSE)
} # end of j loop


write.csv(Results,"./FinalLOSOAAttribution500mto40km.csv", row.names=FALSE)

head(Results)

Results = Results %>% mutate(Collated = case_when(Dist_500 == 1 ~ "Dist_500",
                                                  Dist_1000 == 1 ~ "Dist_1000",
                                                  Dist_1500 == 1 ~ "Dist_1500",
                                                  Dist_2000 == 1 ~ "Dist_2000",
                                                  Dist_2500 == 1 ~ "Dist_2500",
                                                  Dist_3000 == 1 ~ "Dist_3000",
                                                  Dist_3500 == 1 ~ "Dist_3500",
                                                  Dist_4000 == 1 ~ "Dist_4000",
                                                  Dist_4500 == 1 ~ "Dist_4500",
                                                  Dist_5000 == 1 ~ "Dist_5000",
                                                  Dist_5500 == 1 ~ "Dist_5500",
                                                  Dist_6000 == 1 ~ "Dist_6000",
                                                  Dist_6500 == 1 ~ "Dist_6500",
                                                  Dist_7000 == 1 ~ "Dist_7000",
                                                  Dist_7500 == 1 ~ "Dist_7500",
                                                  Dist_8000 == 1 ~ "Dist_8000",
                                                  Dist_8500 == 1 ~ "Dist_8500",
                                                  Dist_9000 == 1 ~ "Dist_9000",
                                                  Dist_9500 == 1 ~ "Dist_9500",
                                                  Dist_10000 == 1 ~ "Dist_10000",
                                                  Dist_10500 == 1 ~ "Dist_10500",
                                                  Dist_11000 == 1 ~ "Dist_11000",
                                                  Dist_11500 == 1 ~ "Dist_11500",
                                                  Dist_12000 == 1 ~ "Dist_12000",
                                                  Dist_12500 == 1 ~ "Dist_12500",
                                                  Dist_13000 == 1 ~ "Dist_13000",
                                                  Dist_13500 == 1 ~ "Dist_13500",
                                                  Dist_14000 == 1 ~ "Dist_14000",
                                                  Dist_14500 == 1 ~ "Dist_14500",
                                                  Dist_15000 == 1 ~ "Dist_15000",
                                                  Dist_15500 == 1 ~ "Dist_15500",
                                                  Dist_16000 == 1 ~ "Dist_16000",
                                                  Dist_16500 == 1 ~ "Dist_16500",
                                                  Dist_17000 == 1 ~ "Dist_17000",
                                                  Dist_17500 == 1 ~ "Dist_17500",
                                                  Dist_18000 == 1 ~ "Dist_18000",
                                                  Dist_18500 == 1 ~ "Dist_18500",
                                                  Dist_19000 == 1 ~ "Dist_19000",
                                                  Dist_19500 == 1 ~ "Dist_19500",
                                                  Dist_20000 == 1 ~ "Dist_20000",
                                                  Dist_20500 == 1 ~ "Dist_20500",
                                                  Dist_21000 == 1 ~ "Dist_21000",
                                                  Dist_21500 == 1 ~ "Dist_21500",
                                                  Dist_22000 == 1 ~ "Dist_22000",
                                                  Dist_22500 == 1 ~ "Dist_22500",
                                                  Dist_23000 == 1 ~ "Dist_23000",
                                                  Dist_23500 == 1 ~ "Dist_23500",
                                                  Dist_24000 == 1 ~ "Dist_24000",
                                                  Dist_24500 == 1 ~ "Dist_24500",
                                                  Dist_25000 == 1 ~ "Dist_25000",
                                                  Dist_25500 == 1 ~ "Dist_25500",
                                                  Dist_26000 == 1 ~ "Dist_26000",
                                                  Dist_26500 == 1 ~ "Dist_26500",
                                                  Dist_27000 == 1 ~ "Dist_27000",
                                                  Dist_27500 == 1 ~ "Dist_27500",
                                                  Dist_28000 == 1 ~ "Dist_28000",
                                                  Dist_28500 == 1 ~ "Dist_28500",
                                                  Dist_29000 == 1 ~ "Dist_29000",
                                                  Dist_29500 == 1 ~ "Dist_29500",
                                                  Dist_30000 == 1 ~ "Dist_30000",
                                                  Dist_30500 == 1 ~ "Dist_30500",
                                                  Dist_31000 == 1 ~ "Dist_31000",
                                                  Dist_31500 == 1 ~ "Dist_31500",
                                                  Dist_32000 == 1 ~ "Dist_32000",
                                                  Dist_32500 == 1 ~ "Dist_32500",
                                                  Dist_33000 == 1 ~ "Dist_33000",
                                                  Dist_33500 == 1 ~ "Dist_33500",
                                                  Dist_34000 == 1 ~ "Dist_34000",
                                                  Dist_34500 == 1 ~ "Dist_34500",
                                                  Dist_35000 == 1 ~ "Dist_35000",
                                                  Dist_35500 == 1 ~ "Dist_35500",
                                                  Dist_36000 == 1 ~ "Dist_36000",
                                                  Dist_36500 == 1 ~ "Dist_36500",
                                                  Dist_37000 == 1 ~ "Dist_37000",
                                                  Dist_37500 == 1 ~ "Dist_37500",
                                                  Dist_38000 == 1 ~ "Dist_38000",
                                                  Dist_38500 == 1 ~ "Dist_38500",
                                                  Dist_39000 == 1 ~ "Dist_39000",
                                                  Dist_39500 == 1 ~ "Dist_39500",
                                                  Dist_40000 == 1 ~ "Dist_40000",
                                                  .default = "Never Coastal"))





Results %>% filter(Collated == "Dist_500") %>% group_by(Collated) %>% summarise(LSOAs = n())

Results %>% filter(Dist_500 ==1) %>% nrow(.) 

names(Results)
T1 = Results[,c(1,82)]
T2 = Results[,c(2:81)]
NewResults = cbind(T1,T2 %>% select(Dist_500:Dist_40000, everything()))
names(NewResults)[2] = "MinCategory"

NewResults = NewResults %>% mutate(NumericCat = case_when(Dist_500 == 1 ~ 1,
                                                  Dist_1000 == 1 ~ 2,
                                                  Dist_1500 == 1 ~ 3,
                                                  Dist_2000 == 1 ~ 4,
                                                  Dist_2500 == 1 ~ 5,
                                                  Dist_3000 == 1 ~ 6,
                                                  Dist_3500 == 1 ~ 7,
                                                  Dist_4000 == 1 ~ 8,
                                                  Dist_4500 == 1 ~ 9,
                                                  Dist_5000 == 1 ~ 10,
                                                  Dist_5500 == 1 ~ 11,
                                                  Dist_6000 == 1 ~ 12,
                                                  Dist_6500 == 1 ~ 13,
                                                  Dist_7000 == 1 ~ 14,
                                                  Dist_7500 == 1 ~ 15,
                                                  Dist_8000 == 1 ~ 16,
                                                  Dist_8500 == 1 ~ 17,
                                                  Dist_9000 == 1 ~ 18,
                                                  Dist_9500 == 1 ~ 19,
                                                  Dist_10000 == 1 ~ 20,
                                                  Dist_10500 == 1 ~ 21,
                                                  Dist_11000 == 1 ~ 22,
                                                  Dist_11500 == 1 ~ 23,
                                                  Dist_12000 == 1 ~ 24,
                                                  Dist_12500 == 1 ~ 25,
                                                  Dist_13000 == 1 ~ 26,
                                                  Dist_13500 == 1 ~ 27,
                                                  Dist_14000 == 1 ~ 28,
                                                  Dist_14500 == 1 ~ 29,
                                                  Dist_15000 == 1 ~ 30,
                                                  Dist_15500 == 1 ~ 31,
                                                  Dist_16000 == 1 ~ 32,
                                                  Dist_16500 == 1 ~ 33,
                                                  Dist_17000 == 1 ~ 34,
                                                  Dist_17500 == 1 ~ 35,
                                                  Dist_18000 == 1 ~ 36,
                                                  Dist_18500 == 1 ~ 37,
                                                  Dist_19000 == 1 ~ 38,
                                                  Dist_19500 == 1 ~ 39,
                                                  Dist_20000 == 1 ~ 40,
                                                  Dist_20500 == 1 ~ 41,
                                                  Dist_21000 == 1 ~ 42,
                                                  Dist_21500 == 1 ~ 43,
                                                  Dist_22000 == 1 ~ 44,
                                                  Dist_22500 == 1 ~ 45,
                                                  Dist_23000 == 1 ~ 46,
                                                  Dist_23500 == 1 ~ 47,
                                                  Dist_24000 == 1 ~ 48,
                                                  Dist_24500 == 1 ~ 49,
                                                  Dist_25000 == 1 ~ 50,
                                                  Dist_25500 == 1 ~ 51,
                                                  Dist_26000 == 1 ~ 52,
                                                  Dist_26500 == 1 ~ 53,
                                                  Dist_27000 == 1 ~ 54,
                                                  Dist_27500 == 1 ~ 55,
                                                  Dist_28000 == 1 ~ 56,
                                                  Dist_28500 == 1 ~ 57,
                                                  Dist_29000 == 1 ~ 58,
                                                  Dist_29500 == 1 ~ 59,
                                                  Dist_30000 == 1 ~ 60,
                                                  Dist_30500 == 1 ~ 61,
                                                  Dist_31000 == 1 ~ 62,
                                                  Dist_31500 == 1 ~ 63,
                                                  Dist_32000 == 1 ~ 64,
                                                  Dist_32500 == 1 ~ 65,
                                                  Dist_33000 == 1 ~ 66,
                                                  Dist_33500 == 1 ~ 67,
                                                  Dist_34000 == 1 ~ 68,
                                                  Dist_34500 == 1 ~ 69,
                                                  Dist_35000 == 1 ~ 70,
                                                  Dist_35500 == 1 ~ 71,
                                                  Dist_36000 == 1 ~ 72,
                                                  Dist_36500 == 1 ~ 73,
                                                  Dist_37000 == 1 ~ 74,
                                                  Dist_37500 == 1 ~ 75,
                                                  Dist_38000 == 1 ~ 76,
                                                  Dist_38500 == 1 ~ 77,
                                                  Dist_39000 == 1 ~ 78,
                                                  Dist_39500 == 1 ~ 79,
                                                  Dist_40000 == 1 ~ 80,
                                                  .default = 81))

names(NewResults)
T1 = NewResults[,c(1:2,83)]
T2 = NewResults[,c(3:82)]
NewResults = cbind(T1,T2)
names(NewResults)
head(NewResults)

write.csv(NewResults,"./LSOA_CoastalDistCategories_500mto40km.csv", row.names=FALSE)


################################################################################
################################################################################


Mismatch = read.csv("./Mismatch.csv")

Mismatch.Map = LSOA21_BFC %>% dplyr::left_join(Mismatch, by = "LSOA21CD") %>% dplyr::filter(!is.na(Diff))
head(Mismatch.Map)
nrow(Mismatch.Map)
mapview(Mismatch.Map, col.regions = "red")




length(nearest_lines) # 267 

as.vector(st_distance(WorkingPoints[1,], CoastalPoints))
intersectiontest <- as.numeric(st_intersects(nearest_lines, London))


AllLines = nearest_lines(TargetLSOA,LegitCoastalPoints[1,])

mapview(LegitCoastalPoints) + 
  mapview(TargetLSOA, col.regions = "red",color = "red") +
  mapview(nearest_lines ,color = "yellow")


Easting = Coastline_coords$X
Northing = Coastline_coords$Y

length(Distances)
nearest_lines = st_nearest_points(TargetLSOA, CoastalPoints)



# THis if we base on existing classification

# Attach centroid-based coastal distance threshold flag (0/1) to each LSOA
# THus if == 1 it means we have initially decided it is coastal
LSOA21.Points = LSOA21.Points %>% dplyr::select(LSOA21CD) %>% dplyr::left_join(LSOA21.List)
head(LSOA21.Points) # 42 cols
names(LSOA21.Points)
nrow(LSOA21.Points)

# So we need to process cols 2:41 for all LSOAs
# Extract centroids of all LSOA21CDs which appear within each distance threshold


TargetLSOA = LSOA21.Points[1,] # This is the LSOA code for the first LSOA in 
Distances = as.vector(st_distance(TargetLSOA,CoastalPoints))
Easting = Coastline_coords$X
Northing = Coastline_coords$Y
length(Distances)
nearest_lines = st_nearest_points(TargetLSOA, CoastalPoints)



#Loop through i - where i is the distance column
i = 2 # This is the distance column
Extract = LSOA21.Points %>% st_drop_geometry() %>% select(!!i)
WorkingPoints = LSOA21.Points[Extract==1,c(1,i)]

WorkingPoints$LSOA21CD[1] # This is the LSOA code for the first LSOA in the set that are within the current coastal threshold
nrow(WorkingPoints)       # 12287 > the number of points within threshold distance of WorkingPoints$LSOA21CD[1]
head(WorkingPoints)

# Find 
Distances = as.vector(st_distance(WorkingPoints[1,],CoastalPoints))
Easting = coords$X
Northing = coords$Y
DF = data.frame(Distance = Distances, Eastings = Easting, Northings = Northing)
nrow(DF)
DF = DF %>% filter(Distances < 20000)
nrow(DF) # 256
CoastalPoints = DF[,c(2:3)] |>  st_as_sf(coords = c("Eastings", "Northings"), crs = "EPSG:27700") 
nrow(CoastalPoints)

# Now - do any of the lines connecting the LSOA point to the coast NOT pass through London
DistError <- as.vector(st_distance(WorkingPoints[1,],CoastalPoints[1,])) - DF$Distance[1]
nearest_lines = st_nearest_points(WorkingPoints[1,], CoastalPoints)
as.vector(st_distance(WorkingPoints[1,], CoastalPoints))
intersectiontest <- as.numeric(st_intersects(nearest_lines, London))


if(sum(is.na(intersectiontest))>0) {
  "Doesn't intersect with London"
} else {
  "Intersects London"
}

# Find lines
which(LSOA21.Points$LSOA21CD == "E01017783")
# 21864



Test = c(1,1,1,1)
sum(is.na(Test))

Test = c(1,NA,1,1)
sum(is.na(Test))


# if any are empty then we are OK 
if(is.na(as.numeric(intersectiontest))) {
  print("Doesn't intersect")
} else {
  print("Intersects")
}




distance <- st_distance(LSOA21.Points[21864,], OuterCoast)



# Find lines
which(LSOA21.Points$LSOA21CD == "E01017783")

nearest_lines <- st_nearest_points(LSOA21.Points[21864,], OuterCoast)
distance <- st_distance(LSOA21.Points[21864,], OuterCoast)
intersectiontest <- st_intersects(nearest_lines, London)
if(is.na(as.numeric(intersectiontest))) {
  print("Doesn't intersect")
} else {
  print("Intersects")
}


nearest_lines <- st_nearest_points(WorkingPoints[1,], OuterCoast)
distance <- st_distance(WorkingPoints[1,], OuterCoast)
intersectiontest <- st_intersects(nearest_lines, London)
if(is.na(as.numeric(intersectiontest))) {
  print("Doesn't intersect")
} else {
  print("Intersects")
}


mapview(nearest_lines,color="red")

coords <- as.data.frame(st_coordinates(OuterCoast))
CoastalPoints = coords |>  st_as_sf(coords = c("X", "Y"), crs = "EPSG:27700") 
nrow(coords)



T1 = nearest_lines[2]
intersections <- st_intersects(T1, polygon_sf)

distance <- st_distance(T1)

coords <- st_coordinates(nearest_lines)
distance <- st_distance(nearest_lines)
st_endpoint(nearest_lines)

nnrow(nearest_lines)
st_write(LSOA21.Map,"./")





head(LSOA21_BGC)
LSOA21.Map = LSOA21_BGC %>% dplyr::select(LSOA21CD) %>% dplyr::left_join(LSOA21.List)
head(LSOA21.Map)

# Take London/Wales off the 40km data set - and save resulting file

Extract.LSOAs = st_read("./Extract.LSOAs_withHoles_40kmBuffer.gpkg")

England.Extract.LSOAs = Extract.LSOAs %>% dplyr::filter(! LSOA21CD %in% London_Wales)
#nrow(England.Extract.LSOAs) # 11977
#mapview(England.Extract.LSOAs)
NoLondonWalesLSOA21BoundariesFilename = paste0("./England_LSOA21_withHoles_40000metreBuffer.gpkg")
st_write(England.Extract.LSOAs, NoLondonWalesLSOA21BoundariesFilename, append=FALSE)


NoLondonWalesLSOA21ListFilename = paste0("./England_LSOA21_withHoles_40000metreBuffer.csv")
Extract.LSOA.List = Extract.LSOAs %>% st_drop_geometry() %>% select(LSOA21CD,LSOA21NM)
England.Extract.LSOA.List = Extract.LSOA.List %>% filter(! LSOA21CD %in% London_Wales)
TotalEnglandLSOAs = nrow(England.Extract.LSOA.List) # 18145
#mapview(England.Extract.LSOAs)
write.csv(England.Extract.LSOA.List, NoLondonWalesLSOA21ListFilename, row.names = FALSE)


# Deal with outer London overlaps
IgAlways = read.csv("./IgAlways.csv")
Ig13000 = read.csv("./Ig13000.csv")
Ig13500 = read.csv("./Ig13500.csv")
Ig14000 = read.csv("./Ig14000.csv")
Ig14500 = read.csv("./Ig14500.csv")
Ig15000 = read.csv("./Ig15000.csv")
Ig15500 = read.csv("./Ig15500.csv")
Ig16000 = read.csv("./Ig16000.csv")
Ig16500 = read.csv("./Ig16500.csv")
Ig17000 = read.csv("./Ig17000.csv")
Ig17500 = read.csv("./Ig17500.csv")
Ig18000 = read.csv("./Ig18000.csv")
Ig18500 = read.csv("./Ig18500.csv")
Ig19000 = read.csv("./Ig19000.csv")
Ig19500 = read.csv("./Ig19500.csv")
Ig20000 = read.csv("./Ig20000.csv")

ExcludeListAlways = IgAlways %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
ExcludeList13000 = Ig13000 %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
ExcludeList13500 = Ig13500 %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
ExcludeList14000 = Ig14000 %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
ExcludeList14500 = Ig14500 %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
ExcludeList15000 = Ig15000 %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
ExcludeList15500 = Ig15500 %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
ExcludeList16000 = Ig16000 %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
ExcludeList16500 = Ig16500 %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
ExcludeList17000 = Ig17000 %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
ExcludeList17500 = Ig17500 %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
ExcludeList18000 = Ig18000 %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
ExcludeList18500 = Ig18500 %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
ExcludeList19000 = Ig19000 %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
ExcludeList19500 = Ig19500 %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
ExcludeList20000 = Ig20000 %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)

ExcludeList13000 = c(ExcludeListAlways,ExcludeList13000)
ExcludeList13500 = c(ExcludeListAlways,ExcludeList13500)
ExcludeList14000 = c(ExcludeListAlways,ExcludeList14000)
ExcludeList14500 = c(ExcludeListAlways,ExcludeList14500)
ExcludeList15000 = c(ExcludeListAlways,ExcludeList15000)
ExcludeList15500 = c(ExcludeListAlways,ExcludeList15500)
ExcludeList16000 = c(ExcludeListAlways,ExcludeList16000)
ExcludeList16500 = c(ExcludeListAlways,ExcludeList16500)
ExcludeList17000 = c(ExcludeListAlways,ExcludeList17000)
ExcludeList17500 = c(ExcludeListAlways,ExcludeList17500)
ExcludeList18000 = c(ExcludeListAlways,ExcludeList18000)
ExcludeList18500 = c(ExcludeListAlways,ExcludeList18500)
ExcludeList19000 = c(ExcludeListAlways,ExcludeList19000)
ExcludeList19500 = c(ExcludeListAlways,ExcludeList19500)
ExcludeList20000 = c(ExcludeListAlways,ExcludeList20000)



# Get list of all LSOA21 in England
LSOA21List = st_read("LLSOA_Dec_2021_PWC_for_England_and_Wales_2022_-7826745778656378855.gpkg")
LSOA21List = LSOA21List %>% st_drop_geometry() %>% select(LSOA21CD) %>% 
                            mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>%
                            select(LSOA21CD)
nrow(LSOA21List) # 33755


# Start Loop here
DistSeq = seq(20500,40000, 500)
Loops = length(DistSeq)

for (i in 1:Loops) {
  Dist = DistSeq[i]
  if (Dist < 13500) {
    InputFilename = paste0("./England_LSOA21_withHoles_",as.character(Dist),"metreBuffer.gpkg")
    Input.geography = st_read(InputFilename, quiet = TRUE)
    Input.List = Input.geography %>% st_drop_geometry() %>% select(LSOA21CD)
    Input.List = Input.List %>% mutate(Coastal = 1)
    names(Input.List)[2] = paste0("Dist_",as.character(Dist))
    InLength = nrow(Input.List) # 1283
    Output.List = Input.List %>% filter(! LSOA21CD %in% ExcludeList13000)
    OutLength = nrow(Output.List) # 1283
    LSOA21List = LSOA21List %>% left_join(Output.List, by = "LSOA21CD")
    LSOA21List = LSOA21List %>% replace(is.na(.), 0)
    ColNum = i+1
    CheckTot = sum(LSOA21List[,..ColNum])
    print(paste0("Processed i = ",i," which is dist = ",Dist," where LSOA count goes from ",InLength," to ",OutLength," (",CheckTot,")"))
    
  } else { # what to do from 13500 to 20000
    InputFilename = paste0("./England_LSOA21_withHoles_",as.character(Dist),"metreBuffer.gpkg")
    Input.geography = st_read(InputFilename, quiet = TRUE)
    Input.List = Input.geography %>% st_drop_geometry() %>% select(LSOA21CD)
    Input.List = Input.List %>% mutate(Coastal = 1)
    names(Input.List)[2] = paste0("Dist_",as.character(Dist))
    InLength = nrow(Input.List) # 1283
      if (Dist == 13500) {
        Output.List = Input.List %>% filter(! LSOA21CD %in% ExcludeList13500)
      } else if (Dist == 14000) {
        Output.List = Input.List %>% filter(! LSOA21CD %in% ExcludeList14000)
      } else if (Dist == 14500) {
        Output.List = Input.List %>% filter(! LSOA21CD %in% ExcludeList14500)
      } else if (Dist == 15000) {
        Output.List = Input.List %>% filter(! LSOA21CD %in% ExcludeList15000)
      } else if (Dist == 15500) {
        Output.List = Input.List %>% filter(! LSOA21CD %in% ExcludeList15500)
      } else if (Dist == 16000) {
        Output.List = Input.List %>% filter(! LSOA21CD %in% ExcludeList16000)
      } else if (Dist == 16500) {
        Output.List = Input.List %>% filter(! LSOA21CD %in% ExcludeList16500)
      } else if (Dist == 17000) {
        Output.List = Input.List %>% filter(! LSOA21CD %in% ExcludeList17000)
      } else if (Dist == 17500) {
        Output.List = Input.List %>% filter(! LSOA21CD %in% ExcludeList17500)
      } else if (Dist == 18000) {
        Output.List = Input.List %>% filter(! LSOA21CD %in% ExcludeList18000)
      } else if (Dist == 18500) {
        Output.List = Input.List %>% filter(! LSOA21CD %in% ExcludeList18500)
      } else if (Dist == 19000) {
        Output.List = Input.List %>% filter(! LSOA21CD %in% ExcludeList19000)
      } else if (Dist == 19500) {
        Output.List = Input.List %>% filter(! LSOA21CD %in% ExcludeList19500)
      } else if (Dist == 20000) {
        Output.List = Input.List %>% filter(! LSOA21CD %in% ExcludeList20000)
      }
 
    OutLength = nrow(Output.List) # 1283
    LSOA21List = LSOA21List %>% left_join(Output.List, by = "LSOA21CD")
    LSOA21List = LSOA21List %>% replace(is.na(.), 0)
    ColNum = i+1
    CheckTot = sum(LSOA21List[,..ColNum])
    print(paste0("Processed i = ",i," which is dist = ",Dist," where LSOA count goes from ",InLength," to ",OutLength," (",CheckTot,")"))
    
  }
}


names(LSOA21List)


write.csv(LSOA21List,"./UncheckedResults.csv", row.names = FALSE)



# Check whether any cols values are 1 when there are subsequent 0s
LSOA21List = LSOA21List %>% arrange(LSOA21CD)
head(LSOA21List)
RowNum = nrow(LSOA21List)
ColNum = ncol(LSOA21List)

#for (i in 1:33755){

Problist = as.numeric()
for (i in 12000:33755){
  Escape1 = 0
  Escape2 = 0
  Escape3 = 0
  for (j in 2:41) {
    if (LSOA21List[i,..j] == 1) {
      if(j<41){
        nextj = j+1
        for (k in nextj:41) {
          if (LSOA21List[i,..k] == 0){
            print(paste0("Problem with i - ",i))
            Problist = c(Problist,i)
            Escape1 = 1
            break
          }
        }
        if (Escape1 == 1) {
          Escape2 == 1
          break
        }
      }
    }
    if (Escape2 == 1) {
      Escape3 == 1
      break
    }
  }
  #print(paste0("No Problem with i - ",i))
}


unlist(as.vector(LSOA21List[17876,]))

LSOA21List[17876,] = 1

write.csv(LSOA21List,"./CentroidBasedDistanceCoastalDefinition.csv", row.names = FALSE)


LSOA21_BFC = st_read(dsn = "Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10)", layer = "Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10)")

names(LSOA21_BFC)

LSOA21_BFC = LSOA21_BFC %>% dplyr::select(LSOA21CD,LSOA21NM,geometry)
LSOA21_BFC = LSOA21_BFC %>% dplyr::left_join(LSOA21List, by = "LSOA21CD")

LSOA21_BFC = LSOA21_BFC %>% mutate(Country = substr(LSOA21CD,1,1)) %>% dplyr::filter(Country == "E") %>% select(-c(Country))

st_write(LSOA21_BFC, "./CentroidBasedDistanceCoastalDefinition.gpkg", append=FALSE)
nrow(LSOA21_BFC)
tail(LSOA21_BFC)




# Read-in BUA2022 from shapefile
BUA2022 = st_read("./BUA_2022_GB_8703850870315979820.gpkg")
BUA2022
mapview(BUA2022)


BUA2022 %>% dplyr::filter(BUA22CD == "E63006926")








# Start Loop here
DistSeq = seq(-20000,-500, 500)
Loops = length(DistSeq)

for (i in 1:Loops) {
  Dist = DistSeq[i]
  
  KmDistText = as.character(-1*Dist)
  FinalOutListFilename = paste0("./buffer_",KmDistText,"metre.gpkg")
  LSOA21CentroidsSpatialFilename = paste0("./Extract_LSOA21Centroids_",KmDistText,"metreBuffer.gpkg")
  LSOA21BoundariesFilename = paste0("./Extract_LSOA21_withHoles_",KmDistText,"metreBuffer.gpkg")
  LSOA21ListFilename = paste0("./Extract_LSOA21_withHoles_",KmDistText,"metreBuffer.csv")
  
















# Read-in BUA2011 from geopackage
BUA2011 = read_sf("./Built_up_Areas_Dec_2011_Boundaries_V2_2022_-9186664123850227917.gpkg")
BUA2011

# Extract info on whether has SubDivision
BUA11Info = BUA2011 %>% st_drop_geometry() %>% select(BUA11CD,BUA11NM,HAS_SD)
write.csv(BUA11Info,"./BUA11Info.csv",row.names=FALSE)


# Read-in BUA2011 SubDivisions from geopackage
BUASD2011 = read_sf("./Built_up_Area_Sub_Divisions_Dec_2011_Boundaries_2022_-1065549065639234308.gpkg")
BUASD2011
BUASD2011 %>% group_by(SD_TYPE) %>% summarise(Count = n())
Joined = BUASD2011 %>% dplyr::filter(SD_TYPE == "Joined")
Discrete = BUASD2011 %>% dplyr::filter(SD_TYPE == "Discrete")

mapview(BUA2011, col.regions = "red") + mapview(Joined, col.regions = "green")
mapview(BUA2011, col.regions = "red") + mapview(Discrete, col.regions = "green")

# Extract info on whether Discrete
BUASD11Info = BUASD2011 %>% st_drop_geometry() %>% select(BUASD11CD,BUASD11NM,SD_TYPE)
write.csv(BUASD11Info,"./BUASD11Info.csv",row.names=FALSE)



# Read-in BUA2022 from shapefile
BUA2022 = st_read("BUA_2022_GB_8703850870315979820.gpkg")
BUA2022

# Define small area to test ################################

# Get BUA11 centroids
BUA11Centroids <- BUA2011 %>% st_centroid() %>% st_geometry() %>% st_coordinates()
BUA11Centroids <- as.data.frame(BUA11Centroids)
names(BUA11Centroids) = c("BNG_E","BNG_N")
BUA2011 = cbind(BUA2011,BUA11Centroids)
BUA2011

# Get BUASD11 centroids
BUASD11Centroids <- BUASD2011 %>% st_centroid() %>% st_geometry() %>% st_coordinates()
BUASD11Centroids <- as.data.frame(BUASD11Centroids)
names(BUASD11Centroids) = c("BNG_E","BNG_N")
BUASD2011 = cbind(BUASD2011,BUASD11Centroids)
BUASD2011


BUA2011.Extract = BUA2011 %>% dplyr::filter(BNG_E < 340906 & BNG_N < 155995)
BUASD2011.Extract = BUASD2011 %>% dplyr::filter(BNG_E < 340906 & BNG_N < 155995)
BUA2022.Extract = BUA2022 %>% dplyr::filter(BNG_E < 340906 & BNG_N < 155995)

mapview(BUA2011.Extract)
mapview(BUASD2011.Extract)
mapview(BUA2022.Extract)

# Spatial Operations ###########################################################
sf_use_s2(FALSE) # spherical geography on/off - sometimes an issue if not FALSE



# LSOA21 linkage with 2011 BUAs ############################################################

# Read list of BUA2011 & BUASD2011 used by ONS
BUA11.ONS.List = read.csv("./BUA11List.csv")
BUA11.ONS.List # 1082

# Collate spatial file including all BUA11 & BUASD11 used by ONS
BUA2011.ONS = BUA2011 %>% dplyr::filter(BUA11CD %in% BUA11.ONS.List$TOWN11CD)
BUASD2011.ONS = BUASD2011 %>% dplyr::filter(BUASD11CD %in% BUA11.ONS.List$TOWN11CD)

nrow(BUA2011)       # 5830
nrow(BUASD2011)     # 1826
nrow(BUA2011.ONS)   # 447
nrow(BUASD2011.ONS) # 635
nrow(BUA2011.ONS) + nrow(BUASD2011.ONS) # 1082
nrow(BUA11.ONS.List) # 1082

# Read-in LSOA21 from shapefile
LSOA21 = read_sf(dsn = "Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10)",layer="Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10)")



# Get list of all LSOA21 which intersect with each BUA11
Loops = nrow(BUA2011.ONS)
Collated.BUA11.LSOA.Intersects = data.frame(LSOA21CD=character(), BUA11CD=character())

for (i in 1:Loops){
  #for (i in 1:10){
  print(paste0("Dealing with ",i," of ",Loops))
  Extract = LSOA21[BUA2011.ONS[i,], op = st_intersects]
  Extract = st_drop_geometry(Extract)
  Addon = as.data.frame(Extract$LSOA21CD)
  if (nrow(Addon) > 0) {
    names(Addon)="LSOA21CD"
    Addon$BUA11CD = BUA2011.ONS[i,]$BUA11CD
    Collated.BUA11.LSOA.Intersects = rbind(Collated.BUA11.LSOA.Intersects,Addon)
  }
}
Collated.BUA11.LSOA.Intersects # 486


# Get list of all LSOA21 which intersect with each BUASD11
Loops = nrow(BUASD2011.ONS)
Collated.BUASD11.LSOA.Intersects = data.frame(LSOA21CD=character(), BUA11CD=character())

for (i in 1:Loops){
  #for (i in 1:10){
  print(paste0("Dealing with ",i," of ",Loops))
  Extract = LSOA21[BUASD2011.ONS[i,], op = st_intersects]
  Extract = st_drop_geometry(Extract)
  Addon = as.data.frame(Extract$LSOA21CD)
  if (nrow(Addon) > 0) {
    names(Addon)="LSOA21CD"
    Addon$BUA11CD = BUASD2011.ONS[i,]$BUASD11CD
    Collated.BUASD11.LSOA.Intersects = rbind(Collated.BUASD11.LSOA.Intersects,Addon)
  }
}
Collated.BUASD11.LSOA.Intersects # 486

# Save as csv files just in case!
write.csv(Collated.BUA11.LSOA.Intersects,"./Collated.BUA11.LSOA.Intersects.csv", row.names=FALSE)
write.csv(Collated.BUASD11.LSOA.Intersects,"./Collated.BUASD11.LSOA.Intersects.csv", row.names=FALSE)



# Now we need to find % intersection

# Merge the two Collated Lists
nrow(Collated.BUA11.LSOA.Intersects)   #  5209
nrow(Collated.BUASD11.LSOA.Intersects) # 18036
Collated.Intersects = rbind(Collated.BUA11.LSOA.Intersects,Collated.BUASD11.LSOA.Intersects)
Collated.Intersects
nrow(Collated.Intersects) # 23245 (as expected)

Collated.BUA11.LSOA.Intersects %>% group_by(BUA11CD) %>% summarise(Count = n()) %>% nrow(.)   #  447
Collated.BUASD11.LSOA.Intersects %>% group_by(BUA11CD) %>% summarise(Count = n()) %>% nrow(.) #  635
Collated.Intersects %>% group_by(BUA11CD) %>% summarise(Count = n()) %>% nrow(.) # 1082 (as expected)

Collated.Intersects$Proportion.LSOA.in.BUA = 0

Loops = nrow(Collated.Intersects)

for (i in 1:Loops){ 
  print(paste0("Dealing with ",i," which is BUA11 ",Collated.Intersects$BUA11CD[i]," and LSOA21 ",Collated.Intersects$LSOA21CD[i]))
  if (substr(Collated.Intersects$BUA11CD[i],1,3) == "E34") {
    BUApolygon = BUA2011 %>% dplyr::filter(BUA11CD == Collated.Intersects$BUA11CD[i])
  } else {
    BUApolygon = BUASD2011 %>% dplyr::filter(BUASD11CD == Collated.Intersects$BUA11CD[i])
  }
  LSOApolygon = LSOA21 %>% dplyr::filter(LSOA21CD == Collated.Intersects$LSOA21CD[i])
  intersection.area <- st_area(st_intersection(BUApolygon, LSOApolygon))
  LSOA21.area <- st_area(LSOApolygon)
  Collated.Intersects$Proportion.LSOA.in.BUA[i] = intersection.area / LSOA21.area
}

write.csv(Collated.Intersects,"./Collated.Intersects.csv", row.names=FALSE)




# Do we need to check the entirely within?
# Do we need to check the overlap?

# st_within ####################################################################
# Get list of all LSOA21 which lie entirely within each BUA11
Loops = nrow(BUA2011.ONS)
Collated.BUA11.LSOA.Within = data.frame(LSOA21CD=character(), BUA11CD=character())

for (i in 1:Loops){
  #for (i in 1:10){
  print(paste0("Dealing with ",i," of ",Loops))
  Extract = LSOA21[BUA2011.ONS[i,], op = st_within]
  Extract = st_drop_geometry(Extract)
  Addon = as.data.frame(Extract$LSOA21CD)
  if (nrow(Addon) > 0) {
    names(Addon)="LSOA21CD"
    Addon$BUA11CD = BUA2011.ONS[i,]$BUA11CD
    Collated.BUA11.LSOA.Within = rbind(Collated.BUA11.LSOA.Within,Addon)
  }
}
Collated.BUA11.LSOA.Within # 486


# Get list of all LSOA21 which intersect with each BUASD11
Loops = nrow(BUASD2011.ONS)
Collated.BUASD11.LSOA.Within = data.frame(LSOA21CD=character(), BUA11CD=character())

for (i in 1:Loops){
  #for (i in 1:10){
  print(paste0("Dealing with ",i," of ",Loops))
  Extract = LSOA21[BUASD2011.ONS[i,], op = st_within]
  Extract = st_drop_geometry(Extract)
  Addon = as.data.frame(Extract$LSOA21CD)
  if (nrow(Addon) > 0) {
    names(Addon)="LSOA21CD"
    Addon$BUA11CD = BUASD2011.ONS[i,]$BUASD11CD
    Collated.BUASD11.LSOA.Within = rbind(Collated.BUASD11.LSOA.Within,Addon)
  }
}
Collated.BUASD11.LSOA.Within # 486

# Save as csv files just in case!
write.csv(Collated.BUA11.LSOA.Within,"./Collated.BUA11.LSOA.Within.csv", row.names=FALSE)
write.csv(Collated.BUASD11.LSOA.Within,"./Collated.BUASD11.LSOA.Within.csv", row.names=FALSE)



# Now we need to find % intersection

# Merge the two Collated Lists
nrow(Collated.BUA11.LSOA.Within)   #  5209
nrow(Collated.BUASD11.LSOA.Within) # 18036
Collated.Within = rbind(Collated.BUA11.LSOA.Within,Collated.BUASD11.LSOA.Within)
Collated.Within
nrow(Collated.Within) # 23245 (as expected)

Collated.BUA11.LSOA.Within %>% group_by(BUA11CD) %>% summarise(Count = n()) %>% nrow(.)   #  447
Collated.BUASD11.LSOA.Within %>% group_by(BUA11CD) %>% summarise(Count = n()) %>% nrow(.) #  635
Collated.Within %>% group_by(BUA11CD) %>% summarise(Count = n()) %>% nrow(.) # 1082 (as expected)

Collated.Within$Proportion.LSOA.in.BUA = 0

Loops = nrow(Collated.Within)

for (i in 1:Loops){ 
  print(paste0("Dealing with ",i," which is BUA11 ",Collated.Within$BUA11CD[i]," and LSOA21 ",Collated.Within$LSOA21CD[i]))
  if (substr(Collated.Within$BUA11CD[i],1,3) == "E34") {
    BUApolygon = BUA2011 %>% dplyr::filter(BUA11CD == Collated.Within$BUA11CD[i])
  } else {
    BUApolygon = BUASD2011 %>% dplyr::filter(BUASD11CD == Collated.Within$BUA11CD[i])
  }
  LSOApolygon = LSOA21 %>% dplyr::filter(LSOA21CD == Collated.Within$LSOA21CD[i])
  intersection.area <- st_area(st_intersection(BUApolygon, LSOApolygon))
  LSOA21.area <- st_area(LSOApolygon)
  Collated.Within$Proportion.LSOA.in.BUA[i] = intersection.area / LSOA21.area
}

write.csv(Collated.Within,"./Collated.Within.csv", row.names=FALSE)




# st_overlaps ####################################################################
# Get list of all LSOA21 which overlap each BUA11
Loops = nrow(BUA2011.ONS)
Collated.BUA11.LSOA.Overlap = data.frame(LSOA21CD=character(), BUA11CD=character())

for (i in 1:Loops){
  #for (i in 1:10){
  print(paste0("Dealing with ",i," of ",Loops))
  Extract = LSOA21[BUA2011.ONS[i,], op = st_overlaps]
  Extract = st_drop_geometry(Extract)
  Addon = as.data.frame(Extract$LSOA21CD)
  if (nrow(Addon) > 0) {
    names(Addon)="LSOA21CD"
    Addon$BUA11CD = BUA2011.ONS[i,]$BUA11CD
    Collated.BUA11.LSOA.Overlap = rbind(Collated.BUA11.LSOA.Overlap,Addon)
  }
}
Collated.BUA11.LSOA.Overlap # 486


# Get list of all LSOA21 which overlap each BUASD11
Loops = nrow(BUASD2011.ONS)
Collated.BUASD11.LSOA.Overlap = data.frame(LSOA21CD=character(), BUA11CD=character())

for (i in 1:Loops){
  #for (i in 1:10){
  print(paste0("Dealing with ",i," of ",Loops))
  Extract = LSOA21[BUASD2011.ONS[i,], op = st_overlap]
  Extract = st_drop_geometry(Extract)
  Addon = as.data.frame(Extract$LSOA21CD)
  if (nrow(Addon) > 0) {
    names(Addon)="LSOA21CD"
    Addon$BUA11CD = BUASD2011.ONS[i,]$BUASD11CD
    Collated.BUASD11.LSOA.Overlap = rbind(Collated.BUASD11.LSOA.Overlap,Addon)
  }
}
Collated.BUASD11.LSOA.Overlap # 486

# Save as csv files just in case!
write.csv(Collated.BUA11.LSOA.Overlap,"./Collated.BUA11.LSOA.Overlap.csv", row.names=FALSE)
write.csv(Collated.BUASD11.LSOA.Overlap,"./Collated.BUASD11.LSOA.Overlap.csv", row.names=FALSE)



# Now we need to find % intersection

# Merge the two Collated Lists
nrow(Collated.BUA11.LSOA.Overlap)   #  5209
nrow(Collated.BUASD11.LSOA.Overlap) # 18036
Collated.Overlap = rbind(Collated.BUA11.LSOA.Overlap,Collated.BUASD11.LSOA.Overlap)
Collated.Overlap
nrow(Collated.Overlap) # 23245 (as expected)

Collated.BUA11.LSOA.Overlap %>% group_by(BUA11CD) %>% summarise(Count = n()) %>% nrow(.)   #  447
Collated.BUASD11.LSOA.Overlap %>% group_by(BUA11CD) %>% summarise(Count = n()) %>% nrow(.) #  635
Collated.Overlap %>% group_by(BUA11CD) %>% summarise(Count = n()) %>% nrow(.) # 1082 (as expected)

Collated.Overlap$Proportion.LSOA.in.BUA = 0

Loops = nrow(Collated.Overlap)

for (i in 1:Loops){ 
  print(paste0("Dealing with ",i," which is BUA11 ",Collated.Overlap$BUA11CD[i]," and LSOA21 ",Collated.Overlap$LSOA21CD[i]))
  if (substr(Collated.Overlap$BUA11CD[i],1,3) == "E34") {
    BUApolygon = BUA2011 %>% dplyr::filter(BUA11CD == Collated.Overlap$BUA11CD[i])
  } else {
    BUApolygon = BUASD2011 %>% dplyr::filter(BUASD11CD == Collated.Overlap$BUA11CD[i])
  }
  LSOApolygon = LSOA21 %>% dplyr::filter(LSOA21CD == Collated.Overlap$LSOA21CD[i])
  intersection.area <- st_area(st_intersection(BUApolygon, LSOApolygon))
  LSOA21.area <- st_area(LSOApolygon)
  Collated.Overlap$Proportion.LSOA.in.BUA[i] = intersection.area / LSOA21.area
}

write.csv(Collated.Overlap,"./Collated.Overlap.csv", row.names=FALSE)


#######################################################################################################
#######################################################################################################

# Was all the above stupid?  Much easier to do point in polygon analysis
# So if LSOA21 centroid (which is population weighted) is in BUA then we attach the LSOA to the BUA

LSOA21Centroid = read_sf("./LLSOA_Dec_2021_PWC_for_England_and_Wales_2022_-7826745778656378855.gpkg")
mapview(LSOA21Centroid)


# Explore 1 - define BUA11 to map how BUA11, LSOA21 and LSOA21 centroids look
# Get BUA11 codes (which start "E34") from Town11_Collated.xlsx
Extract.BUA = BUA2011.ONS %>% dplyr::filter(BUA11CD == "E34001461")
Extract.Points = LSOA21Centroid[Extract.BUA, op = st_within]
Extract.LSOAs = LSOA21 %>% dplyr::filter(LSOA21CD %in% Extract.Points$LSOA21CD)
mapview(Extract.BUA, col.regions = "red") + mapview(Extract.LSOAs) + mapview(Extract.Points, color = "green", cex = 3)

# Explore 2 - define BUASD11 to map how BUASD11, LSOA21 and LSOA21 centroids look
# Get BUASD11 codes (which start "E35") from Town11_Collated.xlsx
Extract.BUA = BUASD2011.ONS %>% dplyr::filter(BUASD11CD == "E35000499")
Extract.Points = LSOA21Centroid[Extract.BUA, op = st_within]
Extract.LSOAs = LSOA21 %>% dplyr::filter(LSOA21CD %in% Extract.Points$LSOA21CD)
mapview(Extract.BUA, col.regions = "red") + mapview(Extract.LSOAs) + mapview(Extract.Points, color = "green", cex = 3)




# Extract list of centroids associated with each BUA2011.ONS
Loops = nrow(BUA2011.ONS)
Collated.LSOA21.Centroids = data.frame(LSOA21CD = character(), BUA11 = character())
for (i in 1:Loops){
  print(paste0("Dealing with i= ",i," BUA2011 = ",BUA2011.ONS[i,]$BUA11CD))
  Extract.Centroids = LSOA21Centroid[BUA2011.ONS[i,], op = st_within]
  Extract.LSOAs = as.data.frame(Extract.Centroids$LSOA21CD)
  names(Extract.LSOAs) = "LSOA21CD"
  Extract.LSOAs$BUA11 = BUA2011.ONS[i,]$BUA11CD
  Collated.LSOA21.Centroids = rbind(Collated.LSOA21.Centroids,Extract.LSOAs)
}
Collated.LSOA21.Centroids

# And append list of centroids associated with each BUASD2011.ONS
Loops = nrow(BUASD2011.ONS)
for (i in 1:Loops){
  print(paste0("Dealing with i= ",i," BUASD2011 = ",BUASD2011.ONS[i,]$BUASD11CD))
  Extract.Centroids = LSOA21Centroid[BUASD2011.ONS[i,], op = st_within]
  Extract.LSOAs = as.data.frame(Extract.Centroids$LSOA21CD)
  names(Extract.LSOAs) = "LSOA21CD"
  Extract.LSOAs$BUA11 = BUASD2011.ONS[i,]$BUASD11CD
  Collated.LSOA21.Centroids = rbind(Collated.LSOA21.Centroids,Extract.LSOAs)
}
Collated.LSOA21.Centroids

write.csv(Collated.LSOA21.Centroids,"./Collated.LSOA21.Centroids.csv", row.names=FALSE)


# What 2011 BUAs are not included on the ONS Towns & Cities List?

# Find LSOA centroids associated with the large BUAs not included in the ONS analysis

# What 2011 BUAs are not included on the ONS Towns & Cities List?
CityBUAs = c("E35000260","E35001398","E35001312","E35001237","E35001358",
             "E35001179","E35001253","E35001335","E35001316",
             "E35001376","E35001211","E35001399","E35001333",
             "E35000486","E35000930","E35001261","E35001470","E35001166")


BUASD2011.City.Extract = BUASD2011 %>% dplyr::filter( BUASD11CD %in% CityBUAs)
nrow(BUASD2011.City.Extract)

Loops = nrow(BUASD2011.City.Extract)
Collated.City.LSOA21.Centroids = data.frame(LSOA21CD = character(), BUA11 = character())

for (i in 1:Loops){
  print(paste0("Dealing with i= ",i," BUASD2011 = ",BUASD2011.City.Extract[i,]$BUASD11CD))
  Extract.Centroids = LSOA21Centroid[BUASD2011.City.Extract[i,], op = st_within]
  Extract.LSOAs = as.data.frame(Extract.Centroids$LSOA21CD)
  names(Extract.LSOAs) = "LSOA21CD"
  Extract.LSOAs$BUA11 = BUASD2011.City.Extract[i,]$BUASD11CD
  Collated.City.LSOA21.Centroids = rbind(Collated.City.LSOA21.Centroids,Extract.LSOAs)
}
Collated.City.LSOA21.Centroids
length(unique(Collated.City.LSOA21.Centroids$BUA11))    #   18
length(unique(Collated.City.LSOA21.Centroids$LSOA21CD)) # 4412

write.csv(Collated.City.LSOA21.Centroids,"./Collated_City_LSOA21_Centroids.csv", row.names=FALSE)


