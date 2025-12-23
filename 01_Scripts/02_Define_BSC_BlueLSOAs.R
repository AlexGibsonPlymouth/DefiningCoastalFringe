
# 02_Define_BSC_BlueLSOAs.R

# Establish which Englsh LSOA21s touch the coastline - where latter defined
# using the BGC and BSC resolution maps.


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

# Objectives
# Download Countries_December_2024_Boundaries_UK_BSC from the Open Geography Portal
# Dissolve countries
# Create 1k - 20km buffers around coastline

# (1) Download LSOA21 data from Open Geography Portal
# (2) Save as geopackage
# (3) Download 

# I need to speed this up by creating hierarchical approach - e.g. buffers with regions



# (1) Get the data 

# (a) Read in the BSC countries spatial file
Coastline_BSC = st_read("../02_InputData/Countries_December_2024_Boundaries_UK_BSC_9221418745258737265.gpkg")
Coastline_BSC

# (b) Read in 2021 BSC LSOA spatial file
LSOA21_BSC = st_read("../02_InputData/Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BSC_V4_6453788336260919790.gpkg")
LSOA21_BSC

# (c) Read in the Region BSC spatial file
Region_BSC = st_read("../02_InputData/Regions_December_2024_Boundaries_EN_BSC_-5107433749138478884.gpkg")
Region_BSC

# (d) Read in 2021 LSOA > Region lookup file
Lookup = read.csv("../02_InputData/OA21CD_LSOA21CD_MSOA21CD_LAD23CD_UTLA23CD_RGN23CD_England.csv")
LSOA_Region = Lookup %>% group_by(LSOA21CD,RGN23CD) %>% summarise(Count = n()) %>%
  ungroup() %>% select(LSOA21CD,RGN23CD)
Regions = unique(LSOA_Region$RGN23CD)
# There are 9 regions

# (e) Read in 50 metre buffer 
buffer = st_read("../03_Buffers/BSC_Buffers/BSC_Buffer_5m.gpkg")


# (2) Deal with LSOAs within Region 1 (to set up output)
print(paste0("Dealing with region 1 of 9"))
i=1
  # (a) Extract LSOAs in the region
  LSOAList = LSOA_Region %>% filter(RGN23CD == Regions[i]) %>% select(LSOA21CD) %>% pull(.)
  LSOASet = LSOA21_BSC %>% dplyr::filter(LSOA21CD %in% LSOAList)
  #mapview(LSOASet)
  
  # (b) Extract the Buffer for the region
  WorkingRegion = Region_BSC %>% dplyr::filter(RGN24CD == Regions[i])
  WorkingBuffer = st_intersection(WorkingRegion,buffer)
  #mapview(WorkingBuffer)
  
  # (c) Find LSOAs on the region that intersect with the region buffer
  LSOAIntersectsBSC = LSOASet[WorkingBuffer, op = st_intersects] 
  #mapview(LSOAIntersectsBSC)
  
  # KEEP THIS AS BLUE LSOAs WITH HOLES <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  LSOAIntersectsBSC.WithHoles = LSOAIntersectsBSC
  
  # (d) Dissolved the set of LSOAs into a single polygon
  Dissolved_LSOAs = LSOAIntersectsBSC %>% dplyr::group_by() %>% dplyr::summarise()
  #mapview(Dissolved_LSOAs)
  
  # (e) Fill holes in the single dissolved polygon
  #     Note that the 500km2 threshold is somewhat arbitrary
  area_thresh <- units::set_units(500, km^2)
  Filled_Dissolved_LSOAs = fill_holes(Dissolved_LSOAs, threshold = area_thresh)
  #mapview(Filled_Dissolved_LSOAs, col.regions="red") + mapview(Dissolved_LSOAs) 
  
  # (f) Find all LSOAs within the single dissolved polygon without holes
  LSOAWithinBSC = LSOASet[Filled_Dissolved_LSOAs, op = st_within] 
  #mapview(LSOAWithinBSC, col.regions="red")
  
  # (g) Merge the original LSOAs with any that were found within the dissolved Polygon
  LSOAIntersectsBSC = rbind(LSOAIntersectsBSC,LSOAWithinBSC)
  FinalBSC = unique(LSOAIntersectsBSC)
  #mapview(LSOAIntersectsBSC)


# (8) Loop through remaining regions and rbind to LSOAIntersectsBSC
for (i in 2:9){
  print(paste0("Dealing with region ",i," of 9"))
  # Extract LSOAs in the region
  LSOAList = LSOA_Region %>% filter(RGN23CD == Regions[i]) %>% select(LSOA21CD) %>% pull(.)
  LSOASet = LSOA21_BSC %>% dplyr::filter(LSOA21CD %in% LSOAList)

  # Extract the Buffer for the region
  WorkingRegion = Region_BSC %>% dplyr::filter(RGN24CD == Regions[i])
  WorkingBuffer = st_intersection(WorkingRegion,buffer)

  # Find LSOAs on the region that intersect with the region buffer
  LSOAIntersectsBSC = LSOASet[WorkingBuffer, op = st_intersects] 

  if (nrow(LSOAIntersectsBSC) > 0) {
    # ADD THIS TO BLUE LSOAs WITH HOLES <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    LSOAIntersectsBSC.WithHoles = rbind(LSOAIntersectsBSC.WithHoles,LSOAIntersectsBSC)

    # Dissolved the set of LSOAs into a single polygon
    Dissolved_LSOAs = LSOAIntersectsBSC %>% dplyr::group_by() %>% dplyr::summarise()

    # Fill holes in the single dissolved polygon
    area_thresh <- units::set_units(500, km^2)
    Filled_Dissolved_LSOAs = fill_holes(Dissolved_LSOAs, threshold = area_thresh)
    
    # Find all LSOAs within the single dissolved polygon without holes
    LSOAWithinBSC = LSOASet[Filled_Dissolved_LSOAs, op = st_within] 

    # Merge the original LSOAs with any that were found within the dissolved Polygon
    LSOAIntersectsBSC = rbind(LSOAIntersectsBSC,LSOAWithinBSC)
    LSOAIntersectsBSC = unique(LSOAIntersectsBSC)

    FinalBSC = rbind(FinalBSC,LSOAIntersectsBSC)
  }
}
  
#mapview(LSOAIntersectsBSC.WithHoles)
st_write(LSOAIntersectsBSC.WithHoles, "../10_LSOACoastalGeographies/BlueLSOAs_BSC_withHoles_withLondon.gpkg", append=FALSE)
LSOA21_BSC_BlueWithHoles_List = LSOAIntersectsBSC.WithHoles %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
write.csv(LSOA21_BSC_BlueWithHoles_List,"../20_LSOACoastalLists/BlueLSOAs_BSC_withHoles_withLondon_List.csv", row.names = FALSE)

#mapview(FinalBSC)
st_write(FinalBSC, "../10_LSOACoastalGeographies/BlueLSOAs_BSC_noHoles_withLondon.gpkg", append=FALSE)
LSOA21_BSC_BlueNoHoles_List = FinalBSC %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
write.csv(LSOA21_BSC_BlueNoHoles_List,"../20_LSOACoastalLists/BlueLSOAs_BSC_noHoles_withLondon_List.csv", row.names = FALSE)



## NEED TO CREATE NO LONDON VERSIONS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
London.LSOA.List = Lookup %>% group_by(LSOA21CD,RGN23NM) %>% summarise(Count = n()) %>%
  ungroup() %>% filter(RGN23NM == "London") %>% select(LSOA21CD) %>% pull(.)
London.LSOA.List

# Deal with the 'with holes' version
LSOAIntersectsBSC.WithHoles.NoLondon = LSOAIntersectsBSC.WithHoles %>% dplyr::filter(!LSOA21CD %in% London.LSOA.List)
#mapview(LSOAIntersectsBSC.WithHoles.NoLondon)
st_write(LSOAIntersectsBSC.WithHoles.NoLondon, "../10_LSOACoastalGeographies/BlueLSOAs_BSC_withHoles_noLondon.gpkg", append=FALSE)
LSOAIntersectsBSC.WithHoles.NoLondon_List = LSOAIntersectsBSC.WithHoles.NoLondon %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
write.csv(LSOAIntersectsBSC.WithHoles.NoLondon_List,"../20_LSOACoastalLists/BlueLSOAs_BSC_withHoles.noLondon_List.csv", row.names = FALSE)


# Deal with the 'no holes' version
LSOA21_BSC_BlueNoHoles_NoLondon = FinalBSC %>% dplyr::filter(!LSOA21CD %in% London.LSOA.List)
#mapview(LSOA21_BSC_BlueNoHoles_NoLondon)
st_write(LSOA21_BSC_BlueNoHoles_NoLondon, "../10_LSOACoastalGeographies/BlueLSOAs_BSC_noHoles_noLondon.gpkg", append=FALSE)
LSOA21_BSC_BlueNoHoles_NoLondon_List = LSOA21_BSC_BlueNoHoles_NoLondon %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
write.csv(LSOA21_BSC_BlueNoHoles_NoLondon_List,"../20_LSOACoastalLists/BlueLSOAs_BSC_noHoles_noLondon_List.csv", row.names = FALSE)


#################################################################################
#################################################################################
#################################################################################

# Find BlueProximate LSOAs according to the BSC resolution
# This uses the FinalBSC Blue geography (i.e. no holes and with London)
Loops = length(Regions)

# Start with the first region (to set up output)
i = 1
Working.Blue.BSC.LSOAs = FinalBSC %>% dplyr::left_join(LSOA_Region, by = "LSOA21CD") %>% 
  dplyr::filter(RGN23CD == Regions[i])

RegionBox = st_bbox(Region_BSC[Region_BSC$RGN24CD == Regions[i],])
bbox_poly <- st_as_sfc(RegionBox)
RegionBox_extended <- st_buffer(bbox_poly, dist = 1000)
WorkingLSOAs = LSOA21_BSC[st_intersects(LSOA21_BSC, RegionBox_extended, sparse = FALSE), ]
#mapview(Region_BSC[Region_BSC$RGN24CD == Regions[i],]) + mapview(WorkingLSOAs)

# Extract LSOAs in the region (this will have holes)
Neighbours = WorkingLSOAs[Working.Blue.BSC.LSOAs, op = st_intersects] 
#mapview(Neighbours, col.regions = "blue") + mapview(Working.Blue.BSC.LSOAs, col.regions = "red")

# Fill holes
# Dissolve the set of LSOAs into a single polygon
Dissolved_Neighbours = Neighbours %>% dplyr::group_by() %>% dplyr::summarise()
#mapview(Dissolved_LSOAs)

# Fill holes in the single dissolved polygon
area_thresh <- units::set_units(500, km^2)
Filled_Dissolved_Neighbours = fill_holes(Dissolved_Neighbours, threshold = area_thresh)
#mapview(Filled_Dissolved_LSOAs, col.regions="red") + mapview(Dissolved_LSOAs) 

# Find all LSOAs within the single dissolved polygon without holes
BlueProximateLSOAs = WorkingLSOAs[Filled_Dissolved_Neighbours, op = st_within] 
#mapview(BlueProximateLSOAs, col.regions="red") + mapview(Neighbours, col.regions = "blue") 

FinalNeighbours = Neighbours                  # This includes holes
FinalBlueProximateLSOAs = BlueProximateLSOAs  # This excludes holes

#mapview(Neighbours)
#mapview(BlueProximateLSOAs)


#j = 1
# Now loop through the remaing regions
for (i in 2:Loops){
  print(paste0("Dealing with i = ",i," of ",Loops))
  Working.Blue.BSC.LSOAs = FinalBSC %>% dplyr::left_join(LSOA_Region, by = "LSOA21CD") %>% 
    dplyr::filter(RGN23CD == Regions[i])
  
  RegionBox = st_bbox(Region_BSC[Region_BSC$RGN24CD == Regions[i],])
  bbox_poly <- st_as_sfc(RegionBox)
  RegionBox_extended <- st_buffer(bbox_poly, dist = 1000)
  WorkingLSOAs = LSOA21_BSC[st_intersects(LSOA21_BSC, RegionBox_extended, sparse = FALSE), ]

  # Extract LSOAs in the region
  Neighbours = WorkingLSOAs[Working.Blue.BSC.LSOAs, op = st_intersects] 

  if (nrow(Neighbours) > 0) {
    # Fill holes
    # Dissolved the set of LSOAs into a single polygon
    Dissolved_Neighbours = Neighbours %>% dplyr::group_by() %>% dplyr::summarise()

    # Fill holes in the single dissolved polygon
    area_thresh <- units::set_units(500, km^2)
    Filled_Dissolved_Neighbours = fill_holes(Dissolved_Neighbours, threshold = area_thresh)

    # Find all LSOAs within the single dissolved polygon without holes
    BlueProximateLSOAs = WorkingLSOAs[Filled_Dissolved_Neighbours, op = st_within] 
  }
  FinalNeighbours = rbind(FinalNeighbours,Neighbours)
  FinalBlueProximateLSOAs = rbind(FinalBlueProximateLSOAs,BlueProximateLSOAs)
  
}


#mapview(FinalBSC, col.regions = "lightblue", legend =FALSE)
#mapview(FinalNeighbours, col.regions = "lightblue", legend =FALSE)
#mapview(FinalBlueProximateLSOAs, col.regions = "lightblue", legend =FALSE)


st_write(FinalNeighbours,"../10_LSOACoastalGeographies/BlueProximateLSOAs_BSC_withHoles_withLondon.gpkg", append=FALSE)
FinalNeighbours_List = FinalNeighbours %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
write.csv(FinalNeighbours_List,"../20_LSOACoastalLists/BlueProximateLSOAs_BSC_withHoles_withLondon.csv")


st_write(FinalBlueProximateLSOAs,"../10_LSOACoastalGeographies/BlueProximateLSOAs_BSC_noHoles_withLondon.gpkg", append=FALSE)
FinalBlueProximateLSOAs_List = FinalBlueProximateLSOAs %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
write.csv(FinalBlueProximateLSOAs_List,"../20_LSOACoastalLists/BlueProximateLSOAs_BSC_NoHoles_withLondon_List.csv")


#################################################################################
#################################################################################
#################################################################################

## NEED TO CREATE NO LONDON VERSIONS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Deal with the 'with holes' version
Neighbours.WithHoles.NoLondon = FinalNeighbours %>% dplyr::filter(!LSOA21CD %in% London.LSOA.List)
#mapview(Neighbours.WithHoles.NoLondon)
st_write(Neighbours.WithHoles.NoLondon, "../10_LSOACoastalGeographies/BlueProximateLSOAs_BSC_withHoles_noLondon.gpkg", append=FALSE)
Neighbours.WithHoles.NoLondon_List = Neighbours.WithHoles.NoLondon %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
write.csv(Neighbours.WithHoles.NoLondon_List,"../20_LSOACoastalLists/BlueProximateLSOAs_BSC_withHoles_noLondon_List.csv", row.names = FALSE)


# Deal with the 'no holes' version
LSOA21_BSC_BlueProximateNoHoles_NoLondon = FinalBlueProximateLSOAs %>% dplyr::filter(!LSOA21CD %in% London.LSOA.List)
#mapview(LSOA21_BSC_BlueProximateNoHoles_NoLondon)
st_write(LSOA21_BSC_BlueProximateNoHoles_NoLondon, "../10_LSOACoastalGeographies/BlueProximateLSOAs_BSC_noHoles_noLondon.gpkg", append=FALSE)
LSOA21_BSC_BlueProximateNoHoles_NoLondon_List = LSOA21_BSC_BlueProximateNoHoles_NoLondon %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
write.csv(LSOA21_BSC_BlueProximateNoHoles_NoLondon_List,"../20_LSOACoastalLists/BlueProximateLSOAs_BSC_noHoles_noLondon_List.csv", row.names = FALSE)


#################################################################################


LSOA21_BSC_BlueProximateNoHoles_NoLondon = st_read("../10_LSOACoastalGeographies/BlueProximateLSOAs_BSC_noHoles_noLondon.gpkg")
mapview(LSOA21_BSC_BlueProximateNoHoles_NoLondon)
