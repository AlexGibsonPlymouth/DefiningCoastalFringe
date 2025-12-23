
# ExtractCoastlineBuffers.R

# Preliminaries

library(tidyverse)  # contains dplyr, ggplot2, readr, tidyr, stringr, purrr and forcats
library(sf)         # 'simple features' for mapping
library(mapview)    # for simple display

# Set directory
setwd("H:/000_Coastal_Related/00_ESRC Coastal Classification/00_SpatialDefinitionofCoastal_R")
getwd()

# Input the Geography Data Portal geopackage for Counties (to ultimately get the coastline) ####
# Note this is the BFC version of the countries geopacakage
Coastline_BSC = st_read("Countries_December_2024_Boundaries_UK_BSC_9221418745258737265.gpkg")
names(Coastline_BSC)

# Dissolve the counties into one dataset
Dissolved_Coastline_BSC = Coastline_BSC %>% dplyr::group_by() %>% dplyr::summarise()
mapview(Dissolved_Coastline_BSC)

# Create a +/- 500metre buffer around the BSC coastline
buff_BSC_minus500m = st_buffer(Dissolved_Coastline_BSC, -500)
buff_BSC_plus500m = st_buffer(Dissolved_Coastline_BSC, 500)
buffer_BSC_500m = st_difference(buff_BSC_plus500m,buff_BSC_minus500m) 
mapview(buffer_BSC_500m)
st_write(buffer_BSC_500m, "./buffer_BSC_plusminus500m.gpkg")
# So this is a buffer 500 metres around the BSC coastline - now find which PoI lie within it

# Read in the March25 PoI data downloaded from Edina (there is now more recent data)
PoI_March25 = st_read("H:/000_Coastal_Related/00_ESRC Coastal Classification/02_DataCollection/01_InProgress/PoI Retail/ONS_Download_PoI_March25_2765063/poi_5998061/poi_5998061.gpkg")
names(PoI_March25)
Counties = PoI_March25 %>% st_drop_geometry() %>% group_by(geographic_county) %>% summarise(Count = n()) %>% select(geographic_county) %>% pull(.)
Counties

# We want just the English ones:
EnglishCounties = Counties[c(6:7,9:10,12:13,20:24,27,30:32,34:36,39:41,43:44,47:50,55:59,61,67,69:70,72:73,75:76,80:81,84:86,89:90)]
England_PoI = PoI_March25 %>% dplyr::filter(geographic_county %in% EnglishCounties)

# Simple Check by geography (takes a minute for mapview to show)
SampleIndex = sample(nrow(England_PoI),100000)
SampleSet = England_PoI[SampleIndex,]
mapview(SampleSet)

# Save PoI for England
nrow(PoI_March25) # 3676140
nrow(England_PoI) # 2975664
st_write(England_PoI, "./England_PoI.gpkg")

# Now we have to find points in the 500 metre buffer
# Speend things up a lot by getting rid of some of the counties I know are nowhere near the coast!
InlandCounties = c("Bedfordshire","Berkshire","Buckinghamshire","Derbyshire",
                   "Greater London","Leicestershire","Northamptonshire",
                   "Nottinghamshire","Oxfordshire","Rutland","Staffordshire","Warwickshire")
  
# Check my geography! (Mapview takes a minute)
InlandCoutriesExtract = England_PoI %>% dplyr::filter(geographic_county %in% InlandCounties)
SampleIndex = sample(nrow(InlandCoutriesExtract),50000)
SampleSet = InlandCoutriesExtract[SampleIndex,]
mapview(SampleSet)
# Looks ok!

CoastalCoutriesExtract = England_PoI %>% dplyr::filter(! geographic_county %in% InlandCounties) %>% 
                           dplyr::select(ref_no,name,pointx_class,groupname,categoryname,classname,feature_easting,feature_northing)

SampleIndex = sample(nrow(CoastalCoutriesExtract),50000)
SampleSet = CoastalCoutriesExtract[SampleIndex,]
mapview(SampleSet)
# Still looks OK
nrow(CoastalCoutriesExtract) # 2163763 (still a lot!)

# We can't do this by brute for - it would take weeks to run, so must take tree approach
# Get buffer in each LA, get points in each LA, find points in each LA in the buffer
# Read in the 2025 LAD data set downloaded from Open Geography Portal
LAD25_BSC = st_read(dsn = "./LADs25", layer = "Local_Authority_Districts_(May_2025)_Boundaries_UK_BSC_(V2)")
LAD25_BSC = LAD25_BSC %>% mutate(Country = substr(LAD25CD,1,1)) %>% dplyr::filter(Country == "E") %>% dplyr::select(!c(Country))
nrow(LAD25_BSC) # 296
LADList = LAD25_BSC %>% st_drop_geometry() %>% select(LAD25NM) %>% pull(.)
Loops = length(LADList)


PoIExtractList <- vector(mode = "list", length = Loops)

for (i in 1:Loops){
  print(paste0("Dealing with i = ",i," which is ",LADList[i]))
  # Create subset of Buffer that lies within LAD25_BSC(i)
  CurrentLAD = LAD25_BSC %>% dplyr::filter(LAD25NM == LADList[i])
  # Add buffer to the LAD25 to catch over-the-coastline points
  buff_CuurentLAD = st_buffer(CurrentLAD, 500)
  
  CurrentBuffer = st_intersection(buff_CuurentLAD, buffer_BSC_500m, join = st_union())
  
  # Find box of buff_CuurentLAD
  bbox_coords <- st_bbox(CurrentBuffer)
  
  # Extract PoI within box
  #names(CoastalCoutriesExtract)
  PoI_SubsetList = CoastalCoutriesExtract %>% st_drop_geometry() %>% 
                                            dplyr::filter(feature_easting >= bbox_coords$xmin[1] & feature_easting <= bbox_coords$xmax[1] &
                                                          feature_northing >= bbox_coords$ymin[1] & feature_northing <= bbox_coords$ymax[1]) %>%
                                            dplyr::select(ref_no) %>% pull(.)
  PoI_Subset =  CoastalCoutriesExtract %>% dplyr::filter(ref_no %in% PoI_SubsetList)
  
  #nrow(PoI_Subset) # 3927
  #mapview(CurrentLAD, col.regions = "red") + mapview(buffer_BSC_500m) + mapview(CurrentBuffer, col.regions = "yellow") + mapview(PoI_Subset, cex = 1)
  
  # Now find which of those are actually in the buffer
  points_in_poly <-  PoI_Subset[CurrentBuffer, op = st_within]
  print(paste0("Found ",nrow(points_in_poly)," in the coastal buffer for ",LADList[i]))
  #mapview(CurrentLAD, col.regions = "red") + mapview(buffer_BSC_500m) + mapview(CurrentBuffer, col.regions = "yellow") + mapview(points_in_poly, cex = 1)
  
  PoIExtractList[[i]] = points_in_poly
  
} # end of loop  
  

# Create a single file with all coastal PoIs

PoIExtractList[[1]]
PoIExtractList[[Loops]]
  
Combined=do.call("rbind", PoIExtractList)
nrow(Combined) # 14877
head(Combined) 
tail(Combined)

# Now attach LSOA to each point
# Read in LSOA21 boundary data downloaded from Open Geography Portal
LSOA21_BFC = st_read(dsn = "./Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10)", layer = "Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFC_(V10)")
head(LSOA21_BFC)
LSOA21_BFC = LSOA21_BFC %>% dplyr::select(LSOA21CD,LSOA21NM) %>% mutate(Country = substr(LSOA21CD,1,1)) %>% dplyr::filter(Country == "E") %>% dplyr::select(-c(Country))
Coastal500mPoints = Combined %>% st_join(LSOA21_BFC, join = st_intersects)
Coastal500mPoints %>% filter(is.na(LSOA21CD)) # 5616 featues not in a LSOA - probably because in sea

#Get List of found Coastal LSOAs - the bad points will certainly be closest to on of these
GoodLSOAList = Coastal500mPoints %>% st_drop_geometry() %>% filter(!is.na(LSOA21CD)) %>% select(LSOA21CD) %>% 
                                      distinct(LSOA21CD) %>% arrange(LSOA21CD) %>% pull(.)
length(GoodLSOAList) # 2344
GoodLSOAs = LSOA21_BFC %>% dplyr::filter(LSOA21CD %in% GoodLSOAList)

BadCoastalPoints = Coastal500mPoints %>% filter(is.na(LSOA21CD)) 
Loops = nrow(BadCoastalPoints)
# So find nearest GoodLSOAs polygon #this takes 10 mins or so
for (i in 1:Loops){
  print(paste0("Dealing with ",i," of ",Loops))
  dist <- st_distance(BadCoastalPoints[i,], GoodLSOAs)
  Endc = length(dist)
  BadCoastalPoints$LSOA21CD[i] = GoodLSOAs[which.min(dist[,c(1:Endc)]),]$LSOA21CD
  BadCoastalPoints$LSOA21NM[i] = GoodLSOAs[which.min(dist[,c(1:Endc)]),]$LSOA21NM
}

# Check that no NAs
BadCoastalPoints %>% dplyr::filter(is.na(LSOA21CD))
# Good - All points linked to a LSOA

# Now join the original good points with the bad points

GoodExtract = Coastal500mPoints %>% dplyr::filter(!is.na(LSOA21CD))
head(GoodExtract)
nrow(GoodExtract) # 142879
nrow(BadCoastalPoints) # 5616
nrow(Coastal500mPoints) # 148495
nrow(GoodExtract) + nrow(BadCoastalPoints) - nrow(Coastal500mPoints) # so got them all


FinalCoastalExtract = rbind(GoodExtract,BadCoastalPoints)
head(FinalCoastalExtract)
nrow(FinalCoastalExtract) # 148495


# All good - so save as geopackage
st_write(FinalCoastalExtract, "./CoastalEdge_500m_PoI.gpkg")
# and extract csv data and save
FinalCoastalExtract_CSV = FinalCoastalExtract %>% st_drop_geometry()
nrow(FinalCoastalExtract_CSV) # 148495 features
write.csv(FinalCoastalExtract_CSV,"./FinalCoastalExtract,csv", row.names = FALSE)


#########################################################################################
#########################################################################################
#########################################################################################

# Now let's have a quick look at the composition of coastal-edge LSOAs ###############################
names(FinalCoastalExtract)

FinalCoastalExtract %>% st_drop_geometry() %>% group_by(groupname) %>% summarise(Count = n())

LSOA.Groups = FinalCoastalExtract %>% st_drop_geometry() %>% group_by(LSOA21CD, groupname) %>% summarise(Count = n())
head(LSOA.Groups)
LSOA.by.Group = pivot_wider(LSOA.Groups, names_from = groupname, values_from = Count)
head(LSOA.by.Group)
LSOA.by.Group = LSOA.by.Group %>% replace(is.na(.), 0)
head(LSOA.by.Group)

# What about a k-means analysis

# Decide how many clusters to look at
n_clusters <- 10

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:n_clusters) {
  # Fit the model: km.out
  print(paste0("Dealing with i = ",i))
  km.out <- kmeans(LSOA.by.Group[,2:10], centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')
scree_plot

# Select number of clusters
k <- 4
set.seed(123)
# Build model with k clusters: km.out
km.out <- kmeans(LSOA.by.Group[,2:10], centers = k, nstart = 20)

km.out
Pattern = as.data.frame(km.out$centers)
write.csv(Pattern,"./Cluster4Pattern.csv", row.names = FALSE)
Pattern = Pattern %>% rowwise() %>% mutate(Total = sum(c_across(`Manufacturing and Production`:`Transport`)))
1 = "Sparse Features"
2 = "Intermediate Commercial"
3 = "Dense Features"
4 = "Intermediate Public"

km.out$cluster

LSOA.by.Group.withCluster4 = cbind(LSOA.by.Group,km.out$cluster)
names(LSOA.by.Group.withCluster4)[11] = "Cluster4"

# map the clusters
LSOACluster4 = LSOA.by.Group.withCluster4 %>% select(LSOA21CD, Cluster4)
LSOACluster4Map = LSOA21_BFC %>% left_join(LSOACluster4List, by = "LSOA21CD") %>% dplyr::filter(!is.na(Cluster4))
LSOACluster4Map = LSOACluster4Map %>% mutate(TextCategory = case_when(Cluster4 == 1 ~ "SparseFeatures",
                                                                      Cluster4 == 2 ~ "Intermedate Commercial",
                                                                      Cluster4 == 3 ~ "Dense Features",
                                                                      Cluster4 == 4 ~ "Intermediate Public",
                                                                      .default = "Other"))

CoastalLSOACluster4Map = st_intersection(LSOACluster4Map, buffer_BSC_500m, join = st_union())

mapview(CoastalLSOACluster4Map, zcol = "TextCategory")

