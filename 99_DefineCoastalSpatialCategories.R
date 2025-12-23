
# DefineCoastalSpatialCategories.R

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

setwd("H:/000_Coastal_Related/00_ESRC Coastal Classification/00_SpatialDefinitionofCoastal_R")
getwd()

# Objectives
# Download Countries_December_2024_Boundaries_UK_BSC from the Open Geography Portal
# Dissolve countries
# Create 1k - 20km buffers around coastline

# (1) Download LSOA21 data from Open Geography Portal
# (2) Save as geopackage
# (3) Download 

# I need to speed this up by creating hierachical approah - e.g. buffers with regions



# Get the data we need #########################################################
# (1) Read in BSC version of the countries spatial file
Coastline_BSC = st_read("Countries_December_2024_Boundaries_UK_BSC_9221418745258737265.gpkg")
#Coastline_BSC
#mapview(Coastline_BSC)

# (2) Read in BFC version of the 2021 LSOA spatial file
LSOA21_BFC = st_read(dsn = "Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10)", layer = "Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10)")
#LSOA21_BFC
#mapview(LSOA21_BFC)

# (3) Read in 2021 LSOA centroids file
LSOA21_Centroids = st_read("LLSOA_Dec_2021_PWC_for_England_and_Wales_2022_-7826745778656378855.gpkg")
#LSOA21_Centroids
#mapview(LSOA21_Centroids)


# Process the data for the 'baseline' 20km #############################################################

# (1) Dissolve countries into single unit
Dissolved_Coastline_BSC = Coastline_BSC %>% dplyr::group_by() %>% dplyr::summarise()
#Dissolved_Coastline_BSC
#mapview(Dissolved_Coastline_BSC)
st_write(Dissolved_Coastline_BSC, "./Dissolved_Coastline_BSC.gpkg")



# (2) Create buffer
buff_20km = st_buffer(Dissolved_Coastline_BSC, -20000)
mapview(Dissolved_Coastline_BSC,col.regions = "red") + mapview(buff_20km)
buffer20km = st_difference(Dissolved_Coastline_BSC,buff_20km) 
mapview(buffer20km,col.regions = "red")
st_write(buffer20km, "./buffer20km.gpkg")


# (3) Find all LSOAs with population-weighted centroids within the buffer
# This takes a very long time as it has to test each of the 33,755 LSOAs (plus those in Wales which I could have excluded)
# Can I speed up each pass by only looking at LSOAs in the previous pass?
Extract.Centroids = LSOA21_Centroids[buffer20km, op = st_within]
# Save file (in this case as GeoPackage) - will fail if file with name already exists
st_write(Extract.Centroids, "./Extract.Centroids_20kmBuffer.gpkg")

mapview(Extract.Centroids, color = "green", cex = 3)

Extract.LSOA.Codes = as.data.frame(Extract.Centroids$LSOA21CD)
names(Extract.LSOA.Codes) = "LSOA21CD"

Extract.LSOAs = LSOA21_BFC %>% dplyr::filter(LSOA21CD %in% Extract.LSOA.Codes$LSOA21CD)
mapview(Extract.LSOAs) + mapview(Extract.Centroids, color = "red", cex = 3)
st_write(Extract.LSOAs, "./Extract.LSOAs_withHoles_20kmBuffer.gpkg")


# (4) find & remove holes 
Dissolve.Extract.LSOAs = Extract.LSOAs %>% dplyr::group_by() %>% dplyr::summarise()
mapview(Dissolve.Extract.LSOAs) 

NoHole <- nngeo::st_remove_holes(Dissolve.Extract.LSOAs)
mapview(LSOA21_BFC, col.regions = "red") + mapview(Dissolve.Extract.LSOAs)

# (5) Find all LSOAs in no holes region (st_within)
# I let this run for 12 hours - hadn't finished!
# It might have been looping blindly (there was heavy processor use) and I might have been doing the 
# wrong thing, but I suspect the task was just too big!  
# It can be done by eye relatively quickly - so have used QGIS to identify the 
# very few problematic LSOAs

# Because centroid outside irregular coastline - MUST be included
"E01018849"

# Because surrounded by coastal
c("E01018946","E01018948","E01018949","E01018950","E01034848")
c("E01022220","E01034789","E01022219")
c("E01006253","E01006254","E01006260","E01006372","E01006373")
"E01020054"

# But (apart from "E01018848") there is logic in using the centroids - even if there are visual holes!

# Our baseline list of all E&W LSOAs (accepting holes) is:
Addon = data.frame(LSOA21CD = "E01018849")
nrow(Addon) # 1
LSOAList = Extract.Centroids %>% st_drop_geometry()%>% select(LSOA21CD)
nrow(LSOAList) # 18721
LSOA21_EW_20km_withholes = rbind(LSOAList,Addon)
nrow(LSOA21_EW_20km_withholes) # 18722
write.csv(LSOA21_EW_20km_withholes,"LSOA21_EW_20km_withholes.csv", row.names=FALSE)

# Our list of all E&W LSOAs (having removed holes) is:
AddonList = c("E01018849","E01018946","E01018948","E01018949","E01018950","E01034848",
              "E01022220","E01034789","E01022219","E01006253","E01006254",
              "E01006260","E01006372","E01006373","E01020054")
Addon = data.frame(LSOA21CD = AddonList)
nrow(Addon) # 15
LSOAList = Extract.Centroids %>% st_drop_geometry()%>% select(LSOA21CD)
nrow(LSOAList) # 18721
LSOA21_EW_20km_noholes = rbind(LSOAList,Addon)
nrow(LSOA21_EW_20km_noholes) # 18736
write.csv(LSOA21_EW_20km_noholes,"LSOA21_EW_20km_noholes.csv", row.names=FALSE)


# Loop to create 'with holes' versions for 19.5km - 0.5km geographies


LSOA21_EW_20km_noholes = read.csv("./LSOA21_EW_20km_noholes.csv")
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

# (1) Extract subset of E&W LSOA21s centroids within the 20km buffer
# NB - I will use the 'no holes' list
LSOA20km_List = LSOA21_EW_20km_noholes %>% st_drop_geometry() %>% select(LSOA21CD) %>% pull(.)
length(LSOA20km_List) # 18736
LSOA21_Centroids_20km = LSOA21_Centroids %>% dplyr::filter(LSOA21CD %in% LSOA20km_List)
LSOA21_Centroids_20km
nrow(LSOA21_Centroids_20km) # 18736

# Start Loop here
DistSeq = seq(-19500,-500, 500)
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
    st_write(buffer_current, BufferOutFilename)
    
    
    # (3) Find all LSOAs with population-weighted centroids within the buffer
    # This takes a long time as it has to test each of the LSOAs
    # I have improved things a bit by 
    # Can I speed up each pass by only looking at LSOAs in the previous pass?
    Extract.Centroids = LSOA21_Centroids_20km[buffer_current, op = st_within]
    # Save file (in this case as GeoPackage) - will fail if file with name already exists
    st_write(Extract.Centroids, LSOA21CentroidsSpatialFilename)
    
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
    st_write(Extract.LSOAs, LSOA21BoundariesFilename)
    
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
    st_write(England.Extract.LSOAs, NoLondonWalesLSOA21BoundariesFilename)
    
    #Extract.LSOA.List = read.csv(LSOA21ListFilename)
    #nrow(Extract.LSOA.List) # 18545
    England.Extract.LSOA.List = Extract.LSOA.List %>% filter(! LSOA21CD %in% London_Wales)
    TotalEnglandLSOAs = nrow(England.Extract.LSOA.List) # 11977
    #mapview(England.Extract.LSOAs)
    write.csv(England.Extract.LSOA.List, NoLondonWalesLSOA21ListFilename)
    
    # Print report to screen
    print(paste0("Have processed i = ",i," of ",Loops," where Dist = ",KmDistText," resulting in ",TotalEWLSOAs," E&W LSOAs and ",TotalEnglandLSOAs," English LSOAs"))
    
}

# End loop




# Take London/Wales off the 20km data set - and save resulting file

Extract.LSOAs = st_read("./Extract.LSOAs_withHoles_20kmBuffer.gpkg")

England.Extract.LSOAs = Extract.LSOAs %>% dplyr::filter(! LSOA21CD %in% London_Wales)
#nrow(England.Extract.LSOAs) # 11977
#mapview(England.Extract.LSOAs)
NoLondonWalesLSOA21BoundariesFilename = paste0("./England_LSOA21_withHoles_20000metreBuffer.gpkg")
st_write(England.Extract.LSOAs, NoLondonWalesLSOA21BoundariesFilename)


NoLondonWalesLSOA21ListFilename = paste0("./England_LSOA21_withHoles_20000metreBuffer.csv")
Extract.LSOA.List = Extract.LSOAs %>% st_drop_geometry() %>% select(LSOA21CD,LSOA21NM)
England.Extract.LSOA.List = Extract.LSOA.List %>% filter(! LSOA21CD %in% London_Wales)
TotalEnglandLSOAs = nrow(England.Extract.LSOA.List) # 12134
#mapview(England.Extract.LSOAs)
write.csv(England.Extract.LSOA.List, NoLondonWalesLSOA21ListFilename)


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
DistSeq = seq(500,20000, 500)
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

st_write(LSOA21_BFC, "./CentroidBasedDistanceCoastalDefinition.gpkg")
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


