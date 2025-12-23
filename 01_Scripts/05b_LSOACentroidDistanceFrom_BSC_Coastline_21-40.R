
# 05b_LSOACentroidDistanceFrom_BSC_Coastline_21-40.R

# Establish which LSOA population-weighted centroids lie within 500m - 40km of the BSC Coastline

# There is doubtless a better (quicker!) way to do this, but this script takes a 
# long time to run, depending on the machine.

# Thus four versions to cover the 80 distance buffers from the BSC coastline
# (i.e. 500m intervals from 500m to 40km)

# As set on lines 36-40, this script deals with buffers 21-40, which covers 30km to 20.5km

# Also see:
# 05a_LSOACentroidDistanceFrom_BSC_Coastline_1-20.R - covering 40km to 30.5km
# 05c_LSOACentroidDistanceFrom_BSC_Coastline_41-60.R - covering 20km to 10.5km
# 05d_LSOACentroidDistanceFrom_BSC_Coastline_61-80.R - covering 10Km to 500m


# Preliminaries
library(tidyverse)  # contains dplyr, ggplot2, readr, tidyr, stringr, purrr and forcats
library(sf)         # 'simple features' for mapping
library(mapview)    # for simple display
library(smoothr)    # to fill_holes (in polygons)
library(data.table) # for rbindlist
library(rlang)      # to use !!sym() for dynamic naming

setwd("H:/000_Coastal_Related/00_ESRC Coastal Classification/00_SpatialDefinitionofCoastal_R/01_Scripts")
getwd()

################################################################################

# EDIT THIS FOR EACH SET OF DISTANCES

# Need to define the distance range to be covered
PreLoop = 21
StartLoop = 22
EndLoop = 40
# And to define the output filename suffix
FilenameSuffix = "21-40"
################################################################################


# Get the data we need #########################################################
Region = st_read("../02_InputData/Regions_December_2024_Boundaries_EN_BSC_-5107433749138478884.gpkg")
RegionList = unique(Region$RGN24CD)

Lookup = read.csv("../02_InputData/OA21CD_LSOA21CD_MSOA21CD_LAD23CD_UTLA23CD_RGN23CD_England.csv")
LSOA_Region = Lookup %>% group_by(LSOA21CD,RGN23CD) %>% summarise(Count = n()) %>%
  ungroup() %>% select(LSOA21CD,RGN23CD) %>% rename(RGN24CD = RGN23CD)
# Set up the list of LSOAs in the country which are going to be tested to see 
# whether within each of the distance buffers
LSOA.df = Lookup %>% select(LSOA21CD) %>% distinct(LSOA21CD) %>% arrange(LSOA21CD)

# (2) Read in 2021 LSOA spatial centroids file 
LSOA21_Centroids = st_read("../02_InputData/LLSOA_Dec_2021_PWC_for_England_and_Wales_2022_-7826745778656378855.gpkg")
LSOA21_Centroids = LSOA21_Centroids %>% dplyr::left_join(LSOA_Region, by = "LSOA21CD")

LSOA21_BSC = st_read("../02_InputData/Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BSC_V4_6453788336260919790.gpkg")

CoastalPoints = st_read("../04_CoastlinesAsPoints/BSC_Coastline_asPoints.gpkg")

# Get a list of all BSC buffer files and arrange in decsending size
# Note that the file.list also includes files for 300m, 200m and 100m but we won't process these
FileList = data.frame(Filename = list.files("../03_Buffers/BSC_Buffers/"))
FileList = FileList %>% mutate(Order = as.numeric(str_remove_all(Filename, "BSC_Buffer_|m.gpkg"))) %>% arrange(desc(Order))
#Loops = nrow(FileList)  


# To speed things up a bit we look at each on the nine regions separately

# Look at first region (i=1)

i = 1
# Define a bbox for the region and extend 40k in all directions
# Note that the Extended bbox (RegionBox_extended) is given CRS for National Grid (i.e. 27700)
 WorkingRegion = Region %>% dplyr::filter(RGN24CD == RegionList[i])
 RegionBox = st_bbox(WorkingRegion)
 bbox_poly <- st_as_sfc(RegionBox)
 RegionBox_extended <- st_buffer(bbox_poly, dist = 40000)
 RegionBox_extended <- st_set_crs(RegionBox_extended, 27700)
 #mapview(WorkingRegion)
 
 # Find centroids within the original region bbox - not the extended version
 RegionPoints =  LSOA21_Centroids %>% dplyr::filter(RGN24CD == RegionList[i])
 #mapview(RegionPoints) + mapview(WorkingRegion) + mapview(RegionBox_extended)

 # Read in the buffer sf file for the current distance and extract that part that
 # lies within the extended_buffer (so it extends 40km from the edge of the region)
 
 j = PreLoop
 buffer = st_read(paste0("../03_Buffers/BSC_Buffers/",FileList$Filename[j]))
 #mapview(RegionPoints) + mapview(WorkingRegion) + mapview(RegionBox_extended) + mapview(buffer, col.regions = "red")
 RegionBuffer = st_intersection(buffer,RegionBox_extended)
 
 # Find centroids in the current region which lie within the extended buffer
 BufferPoints =  st_intersection(RegionPoints,RegionBuffer)
 #mapview(BufferPoints)
 
 # Create a dataframe called BufferPointList with all the LSOAs that lie within the
 # current buffer, and give the dataframe column names of LSOA21CD and the current buffer
 colname = str_remove(FileList$Filename[j],".gpkg")
 BufferPointList = BufferPoints %>% st_drop_geometry() %>%
   select(LSOA21CD) %>% mutate(!!sym(colname) := 1)
 
 # Use left-join to add the column to the LSOA.df dataframe containing all LSOAs in the country
 LSOA.df = LSOA.df %>% left_join(BufferPointList, by = "LSOA21CD")
 
 # now loop through all remaining buffers (which are closer) - as these are closer we are
 # only wanting to test those centroids that are within the previous buffer  - these are 
 # those in BufferPoints
 for (j in StartLoop:EndLoop){
    print(paste0("Dealing with i = ",i," of 9 and j = ",j," of ",EndLoop))
    RegionPoints =  BufferPoints
    buffer = st_read(paste0("../03_Buffers/BSC_Buffers/",FileList$Filename[j]))
    RegionBuffer = st_intersection(buffer,RegionBox_extended)
    BufferPoints =  st_intersection(RegionPoints,RegionBuffer)
    
    colname = str_remove(FileList$Filename[j],".gpkg")
    BufferPointList = BufferPoints %>% st_drop_geometry() %>%
    select(LSOA21CD) %>% mutate(!!sym(colname) := 1)
     
    # Add column to LSOA.df
    LSOA.df = LSOA.df %>% left_join(BufferPointList, by = "LSOA21CD")
 }
 
 # Replace all NAs in the LSOA.df dataframe with zeros
 LSOA.df = LSOA.df %>% mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
 nrow(LSOA.df)
 names(LSOA.df)
    
 # So LSOA.df has 0/1 for the first region - where 0 means not coastal for the 
 # specified distance and 1 means coastal
 
 # We now do remaining regions - we mustn't overwrite, but we can add to existing
 
 for (i in 2:9){
   
   # Define a bbox for the region and extend 40k in all directions
   # Note that the Extended bbox (RegionBox_extended) is given CRS for National Grid (i.e. 27700)
   WorkingRegion = Region %>% dplyr::filter(RGN24CD == RegionList[i])
   RegionBox = st_bbox(WorkingRegion)
   bbox_poly <- st_as_sfc(RegionBox)
   RegionBox_extended <- st_buffer(bbox_poly, dist = 40000)
   RegionBox_extended <- st_set_crs(RegionBox_extended, 27700)
   
   # Find centroids within the region bbox - not the extended version
   RegionPoints =  LSOA21_Centroids %>% dplyr::filter(RGN24CD == RegionList[i])
   #mapview(RegionPoints)
   
   # Read in the buffer sf file for the current distance and extract that part that
   # lies within the extended_buffer (so it extends 40km from the edge of he region)
   j = PreLoop
   buffer = st_read(paste0("../03_Buffers/BSC_Buffers/",FileList$Filename[j]))
   
   RegionBuffer = st_intersection(buffer,RegionBox_extended)
   #mapview(RegionBuffer)
   
   # Find centroids in the current region which lie within the extended buffer
   BufferPoints =  st_intersection(RegionPoints,RegionBuffer)
   #mapview(BufferPoints)

   # Create a dataframe called BufferPointList with all the LSOAs that lie within the
   # current buffer, and give the dataframe column names of LSOA21CD and the current buffer
   colname = str_remove(FileList$Filename[j],".gpkg")
   BufferPointList = BufferPoints %>% st_drop_geometry() %>%
     select(LSOA21CD) %>% mutate(TempCol = 1) %>% mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
   
   # Use left-join to add the column to the LSOA.df dataframe containing all LSOAs in the country
   # Note that because points within a region can be found in multiple extended buffers we
   # need to make sure that we don't overwrite an existing 1 with a zero - hence the following
   LSOA.df = LSOA.df %>% left_join(BufferPointList, by = "LSOA21CD")
   LSOA.df = LSOA.df %>% mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
   LSOA.df = LSOA.df %>% mutate(!!sym(colname) := !!sym(colname) + TempCol)
   LSOA.df = LSOA.df %>% select(-c(TempCol))

   # now loop through all buffers using the previous set of points
   for (j in StartLoop:EndLoop){
     print(paste0("Dealing with i = ",i," of 9 and j = ",j," of ",EndLoop))
     RegionPoints =  BufferPoints
     buffer = st_read(paste0("../03_Buffers/BSC_Buffers/",FileList$Filename[j]))
     RegionBuffer = st_intersection(buffer,RegionBox_extended)
     BufferPoints =  st_intersection(RegionPoints,RegionBuffer)
     
     colname = str_remove(FileList$Filename[j],".gpkg")
     BufferPointList = BufferPoints %>% st_drop_geometry() %>%
       select(LSOA21CD) %>% mutate(TempCol = 1) %>% mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
     
     # Add new data to column in LSOA.df
     LSOA.df = LSOA.df %>% left_join(BufferPointList, by = "LSOA21CD")
     LSOA.df = LSOA.df %>% mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
     LSOA.df = LSOA.df %>% mutate(!!sym(colname) := !!sym(colname) + TempCol)
     LSOA.df = LSOA.df %>% select(-c(TempCol))
     
   }  # end of j
 } # end of i

# Check by plotting a few sets of points for the whole country
#Extract = LSOA.df %>% select(1,2)
#CentroidTest = LSOA21_Centroids %>% dplyr::left_join(Extract, by="LSOA21CD") %>% dplyr::filter(.[[4]] == 1)
#mapview(CentroidTest, col.regions = "yellow", cex = 1)

# And how does it look with polygons?
#Extract = LSOA.df %>% select(1,2)
#Test = LSOA21_BSC %>% dplyr::left_join(Extract, by="LSOA21CD") %>% dplyr::filter(.[[10]] == 1)
#mapview(Test) + mapview(CentroidTest, col.regions = "red", cex = 2)


# All seems to work, so save this dataset - it includes London and still has gaps (holes)
#write.csv(LSOA.df,"../20_LSOACoastalLists/BSC_LSOA21_Distance_withLondon_withGaps_1-20.csv", row.names = FALSE)
write.csv(LSOA.df,paste0("../20_LSOACoastalLists/BSC_LSOA21_Distance_withLondon_withGaps_",FilenameSuffix,".csv"), row.names = FALSE)

#######################################################################################################
#######################################################################################################
#######################################################################################################

# Now we need to fill any holes at each level > to create BSC_LSOA21_Distance_withLondon_NoGaps.csv

# Create AllLSOAs as dataframe with all LSOAs - to which we will add new coastal/non-coastal definition 
# once we have filled any holes

LSOA21_BSC = LSOA21_BSC %>% dplyr::left_join(LSOA_Region, by = "LSOA21CD")
AllLSOAs = LSOA21_BSC %>% st_drop_geometry() %>% select(LSOA21CD)


# Need to loop over all distances in the current range
ColNames = names(LSOA.df)
Loops = length(ColNames)
LSOANoHoles.df = LSOA.df %>% select(LSOA21CD)
area_thresh <- units::set_units(200, km^2)   # This is somewhat arbitrary

# Loop for each of the distances
for (i in 2:Loops){
  print(paste0("Sys.time = ",Sys.time(),"  Dealing with i = ",i," of ",Loops," which is ",ColNames[i]))
  # Extract the current distance (i.e. for loop i)
  Temp = LSOA.df %>% dplyr::select(LSOA21CD,ColNames[i])
  # Extract the subset of the full LSOAset where coastal - cleaning so no distance column name
  LSOASet = LSOA21_BSC %>% dplyr::left_join(Temp, by = "LSOA21CD") %>%
    dplyr::filter(!!sym(ColNames[i]) == 1) %>% dplyr::select(-c(!!sym(ColNames[i])))

  # For the first region (j=1)
  j = 1
  # Extract the region as a polygon
  WorkingRegion = Region %>% dplyr::filter(RGN24CD == RegionList[j])
  # Extract the centroids which lie in the region
  WorkingLSOASet = LSOASet %>% dplyr::filter(RGN24CD == RegionList[j])
  
  # As long as we have some points in the WorkingLSOASet do the following:
  if (nrow(WorkingLSOASet)>0){
    # Dissolve the LSOA sf file so we get a single polygon
    Working_Dissolved_LSOAs = WorkingLSOASet %>% dplyr::group_by() %>% dplyr::summarise()
    # Find all the LSOAs that are within the region
    RegionLSOAs = LSOA21_BSC %>% dplyr::filter(RGN24CD == RegionList[j])
    # Fill the holes (if any) in Working_Dissolved_LSOAs to create single polygon
    Filled_Working_Dissolved_LSOAs = fill_holes(Working_Dissolved_LSOAs, threshold = area_thresh)
    # Now find all LSOAs in the region that are within the dissolved LSOA polygon with holes filled
    WithinLSOAs = RegionLSOAs[Filled_Working_Dissolved_LSOAs, op = st_within]
    # Extract unique (distinct) LSOAs and add a column to the dataframe with name 
    # from original distance column name and all values set to 1
    FinalLSOAs = WithinLSOAs %>% dplyr::distinct(LSOA21CD) %>% mutate(!!sym(ColNames[i]) := 1)
    print(paste0("Print done region j = ",j," of 9 having now found a total of ",nrow(FinalLSOAs)," LSOAs"))
  } else {
    print(paste0("Skipping region j = ",j," as no LSOAs in buffer - still found a total of ",nrow(FinalLSOAs)," LSOAs"))
  }
  
  # Do the same for the remaining 8 regions, rbinding the list of LSOAs found at the specified distance to
  # the bottom of the FinalLSOAs dataframe
  for (j in 2:9) {
    WorkingRegion = Region %>% dplyr::filter(RGN24CD == RegionList[j])
    WorkingLSOASet = LSOASet %>% dplyr::filter(RGN24CD == RegionList[j])
    if (nrow(WorkingLSOASet)>0){
      Working_Dissolved_LSOAs = WorkingLSOASet %>% dplyr::group_by() %>% dplyr::summarise()
      RegionLSOAs = LSOA21_BSC %>% dplyr::filter(RGN24CD == RegionList[j])
      Filled_Working_Dissolved_LSOAs = fill_holes(Working_Dissolved_LSOAs, threshold = area_thresh)
      WithinLSOAs = RegionLSOAs[Filled_Working_Dissolved_LSOAs, op = st_within]
      WorkingLSOAs = WithinLSOAs %>% st_drop_geometry() %>% select(LSOA21CD) %>% mutate(!!sym(ColNames[i]) := 1)
       FinalLSOAs = rbindlist(list(FinalLSOAs,WorkingLSOAs))
      print(paste0("Print done region j = ",j," of 9 having now found a total of ",nrow(FinalLSOAs)," LSOAs"))
    } else {
      print(paste0("Skipping region j = ",j," as no LSOAs in buffer - still found a total of ",nrow(FinalLSOAs)," LSOAs"))
    }
  }
  
 
  # Use left_join to add the distance column to AllLSOAs
  AllLSOAs = AllLSOAs %>% left_join(FinalLSOAs, by = "LSOA21CD")
  
  
  
  }  
  
# Make sure only dealing with English LSOAs
AllLSOAs = AllLSOAs %>% mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(Country))

# Save to file
write.csv(AllLSOAs,paste0("../20_LSOACoastalLists/BSC_LSOA21_Distance_withLondon_NoGaps_",FilenameSuffix,".csv"), row.names = FALSE)

# Now we need to establish whether points are in London *or* can only be reached through the London region
# Need to create both BSC_LSOA21_Distance_NoLondon_withGaps.csv
#                 and BSC_LSOA21_Distance_NoLondon_NoGaps.csv
# We will have to implement separately

# We need to find if LSOAs only accessible to coastline at given distance via a 
# straight-line that passes through London
# NOTE we only need to do this for LSOAs in SE and EoE region or, better, within 
# bbox c(480847,218812),c(567783,218812),c(480847,128254),c(567783,128254)

# And only need to check against points withing bbox
# c(480847,218812),c(626646,218812),c(480847,128254),c(626646,128254)

# Copy the AllLSOAs dataset to the one we will edit, i.e. NotLondonLSOAs
NotLondonLSOAs = AllLSOAs

# Extract a list of all LSOAs in London
LondonLSOAList = Lookup %>% group_by(LSOA21CD,RGN23NM) %>% summarise(Count = n()) %>%
  ungroup() %>% select(LSOA21CD,RGN23NM) %>% filter(RGN23NM == "London") %>% 
  select(LSOA21CD) %>% pull(.)
length(LondonLSOAList)

# Set all London LSOAs to zero for all distance columns
# There will be a non-looping way to do this ... but ....
# As it is - takes 5 mins or so
Loops = nrow(NotLondonLSOAs)
ColCount = ncol(NotLondonLSOAs)
for (i in 1:Loops) {
  print(i)
  if(NotLondonLSOAs$LSOA21CD[i] %in% LondonLSOAList) {
    NotLondonLSOAs[i,2:ColCount] = 0
  }
}

# Check
#TestDist = NotLondonLSOAs %>% select(LSOA21CD,Buffer_20000m)
#Test = LSOA21_BSC %>% dplyr::left_join(TestDist, by = "LSOA21CD") %>% dplyr::filter(Buffer_20000m == 1)
#head(Test)
#mapview(Test) # i.e. all LSOAs in the 40000 buffer

# Extract polygon of London
Regions = st_read("../02_InputData/Regions_December_2024_Boundaries_EN_BSC_-5107433749138478884.gpkg")
LondonPoly = Regions %>% dplyr::filter(RGN24NM == "London")
#mapview(TargetLSOAs, cex =2) + mapview(TargetCoastPoints, col.regions = "yellow", cex =2)  + mapview(LondonPoly, col.regions = "red")


# define a bbox around London - to catch all London fringe LSOAs
poly = st_polygon(list(rbind(
  c(470000,230000),c(567783,230000),c(567783,128254),c(470000,128254),c(470000,230000)
  )))
sf_poly <- st_sf(geometry = st_sfc(poly))
FringeCookieCutter <- st_set_crs(sf_poly, 27700)


# Extract LSOAs within the Cookie cutter but not within London itself
#LSOA21_Centroids = st_read("../02_InputData/LLSOA_Dec_2021_PWC_for_England_and_Wales_2022_-7826745778656378855.gpkg")
#Regions = st_read("../02_InputData/Regions_December_2024_Boundaries_EN_BSC_-2638963433149857560.gpkg")
#Lookup = read.csv("../02_InputData/OA21CD_LSOA21CD_MSOA21CD_LAD23CD_UTLA23CD_RGN23CD_England.csv")
LSOA_Region_CutterList = Lookup %>% group_by(LSOA21CD,RGN23NM) %>% summarise(Count = n()) %>%
  ungroup() %>% select(LSOA21CD,RGN23NM) %>% 
  filter(RGN23NM == "South East" | RGN23NM == "East of England") %>%
  select(LSOA21CD) %>% pull(.)
LondonFingeLSOAs = LSOA21_Centroids %>% dplyr::filter(LSOA21CD %in% LSOA_Region_CutterList)
TargetLSOAs = st_intersection(LondonFingeLSOAs,FringeCookieCutter)


# Now extract that set of coastal points that we want to test against
poly = st_polygon(list(rbind(
  c(420000,250000),c(650000,250000),c(650000,90000),c(420000,90000),c(420000,250000)
)))
sf_poly <- st_sf(geometry = st_sfc(poly))
SECookieCutter <- st_set_crs(sf_poly, 27700)
#mapview(SECookieCutter) + mapview(TargetLSOAs, col.regions = "yellow", cex =2)

TargetCoastPoints = st_intersection(CoastalPoints,SECookieCutter)

# But we don't want the CoastalPoints within London
#mapview(TargetCoastPoints) + mapview(LondonPoly) 
ThamesExclusionPoly = st_polygon(list(rbind(
  c(553668,169000),c(500000,169000),c(500000,195000),c(553668,195000),c(553668,169000)
)))
sf_poly <- st_sf(geometry = st_sfc(ThamesExclusionPoly))
ThamesCookieCutter <- st_set_crs(sf_poly, 27700)

inside <- st_within(TargetCoastPoints, ThamesCookieCutter, sparse = FALSE)
TargetCoastPoints <- TargetCoastPoints[!inside, ]
#mapview(TargetCoastPoints) + mapview(LondonPoly) 

# And don't want the CoastalPoints around the Isle of Wight (because inefficient 
# as there will certainly be coastal points closer to the London Fringe than those
# around the Isle of Wight)

WightExclusionPoly = st_polygon(list(rbind(
  c(439606,111840),c(476320,82691),c(451793,67580),c(413080,89050),c(439606,111840)
)))
sf_poly <- st_sf(geometry = st_sfc(WightExclusionPoly))
WightCookieCutter <- st_set_crs(sf_poly, 27700)
#mapview(WightCookieCutter) + mapview(TargetCoastPoints, col.regions = "yellow", cex =2)

inside <- st_within(TargetCoastPoints, WightCookieCutter, sparse = FALSE)
TargetCoastPoints <- TargetCoastPoints[!inside, ]
#mapview(TargetCoastPoints) + mapview(LondonPoly) 



# Now find all 1s at each buffer distance and see whether there is a straight-line 
# to the coastline that doesn't go through London ... if NOT ... then set to 0

# This is the job:
TotCols = length(AllLSOAs) # i.e. the number of columns in AllLSOAs
ColNames = names(AllLSOAs)[2:TotCols] # i.e. the names of the distance columns
BufferLoops = length(ColNames)  # i.e. the number of distance datsets to be tested

for (k in 1:BufferLoops){
  # Get the column name
  WorkingBufferName = ColNames[k]
  # Get the distance (metres)
  WorkingBufferDist = as.numeric(str_remove_all(WorkingBufferName, "BSC_Buffer_|m"))
  
  # Extract the set of LSOAs according to the current distance 
  WorkingLSOAExtract = AllLSOAs %>% select(LSOA21CD,!!sym(WorkingBufferName))
  # 
  # Create an sf file which comprises the LSOAs that are coastal according to the current distance 
  WorkingTargetLSOAs = TargetLSOAs %>% dplyr::left_join(WorkingLSOAExtract, by = "LSOA21CD") %>% 
    dplyr::filter(!!sym(WorkingBufferName) == 1)

  # Extract a list of those LSOAs that are coastal according to the current distance definition
  WorkingTargetLSOAList =  WorkingTargetLSOAs %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)

  
  LSOA.Loops = nrow(WorkingTargetLSOAs)
  CoastPoint.Loops = nrow(TargetCoastPoints)
  
  WorkingTargetLSOAs.XY = data.frame(st_coordinates(WorkingTargetLSOAs))
  TargetCoastPoints.XY = data.frame(st_coordinates(TargetCoastPoints))
    
  # So loop through every Fringe Centroid (n = 3293) to every SE coast point (n = 4607)
  # Is it less than X metres
  # If yes, does line cut through the LondonPoly
  # If no, then accept and move on to next Fringe Centroid
  
  BreakFlag = 0
  for (i in 1:LSOA.Loops) {
    print(paste0("Dealing with ",i," of ",LSOA.Loops))
    #i = 1
    for (j in 1:CoastPoint.Loops) {
      #j = 1
      Dist = sqrt((((WorkingTargetLSOAs.XY$X[i] - TargetCoastPoints.XY$X[j]))^2) + (((WorkingTargetLSOAs.XY$Y[i] - TargetCoastPoints.XY$Y[j]))^2))
      if (Dist < WorkingBufferDist){
        #print("Found")
        p1 <- st_sfc(st_point(c(WorkingTargetLSOAs.XY$X[i],WorkingTargetLSOAs.XY$Y[i])), crs = 27700)
        p2 <- st_sfc(st_point(c(TargetCoastPoints.XY$X[j],TargetCoastPoints.XY$Y[j])), crs = 27700)
        line <- st_sfc(st_linestring(rbind(st_coordinates(p1),st_coordinates(p2))),crs = st_crs(p1))
        if(st_intersects(line, LondonPoly, sparse = FALSE) == FALSE){
          #print("Doesn't go through London")
          BreakFlag = 1
          break
        }
      }
    }
    if (BreakFlag == 1){
      BreakFlag = 0
      #print(paste0("Leave alone as this doesn't go through London: i = ",i," and j = ",j))
      next
    }
    print(paste0("Setting to zero as no routes don't go through London: i = ",i," where LSOA = ",WorkingTargetLSOAList[i]))
    BreakFlag = 0
    # Update AllLSOAs for the specified LSOA and distance
    rowIndex <- which(AllLSOAs$LSOA21CD == WorkingTargetLSOAList[i])
    colIndex =  k+1
    #AllLSOAs[rowIndex,colIndex] = 0
    AllLSOAs[rowIndex,2:TotCols] = 0
    
  }

}  
  

# Check
#TestDist = AllLSOAs %>% select(LSOA21CD,BSC_Buffer_40000m)
#TestMap = LSOA21_BSC %>% dplyr::left_join(TestDist, by = "LSOA21CD") %>% dplyr::filter(BSC_Buffer_40000m == 1)
#mapview(TestMap) # i.e. all LSOAs in the 40000 buffer

# We still need to extract LSOAs in the London Region (I thought I had done this!)
London.LSOA.List = Lookup %>% group_by(LSOA21CD,RGN23NM) %>% summarise(Count = n()) %>%
  ungroup() %>% select(LSOA21CD,RGN23NM) %>% 
  filter(RGN23NM == "London") %>%
  select(LSOA21CD) %>% pull(.)

NoLondonAllLSOAs = AllLSOAs %>% filter(!LSOA21CD %in% London.LSOA.List)
  
# Check
#TestDist = NoLondonAllLSOAs %>% select(LSOA21CD,BSC_Buffer_40000m)
#TestMap = LSOA21_BSC %>% dplyr::left_join(TestDist, by = "LSOA21CD") %>% dplyr::filter(BSC_Buffer_40000m == 1)
#mapview(TestMap) # i.e. all LSOAs in the 40000 buffer

names(NoLondonAllLSOAs)
# And save files
#write.csv(NoLondonAllLSOAs,"../20_LSOACoastalLists/BSC_LSOA21_Distance_NoLondon_NoGaps_1-20.csv",row.names = FALSE)
write.csv(NoLondonAllLSOAs,paste0("../20_LSOACoastalLists/BSC_LSOA21_Distance_NoLondon_NoGaps_",FilenameSuffix,".csv"), row.names = FALSE)


