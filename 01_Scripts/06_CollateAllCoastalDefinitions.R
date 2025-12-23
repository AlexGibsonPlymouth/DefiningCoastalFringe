
# 06_CollateAllCoastalDefinitions.R

# Preliminaries
library(tidyverse)  # contains dplyr, ggplot2, readr, tidyr, stringr, purrr and forcats
library(sf)
library(mapview)
library(smoothr)    # to fill_holes (in polygons)

setwd("H:/000_Coastal_Related/00_ESRC Coastal Classification/00_SpatialDefinitionofCoastal_R/01_Scripts")
getwd()


BGC_LSOA21_Distance_NoLondon_NoGaps_Files = list.files("../20_LSOACoastalLists")[1:4]
BGC_LSOA21_Distance_withLondon_NoGap_Files = list.files("../20_LSOACoastalLists")[5:8]
BGC_LSOA21_Distance_withLondon_withGaps_Files = list.files("../20_LSOACoastalLists")[9:12]

BSC_LSOA21_Distance_NoLondon_NoGaps_Files = list.files("../20_LSOACoastalLists")[28:31]
BSC_LSOA21_Distance_withLondon_NoGap_Files = list.files("../20_LSOACoastalLists")[32:35]
BSC_LSOA21_Distance_withLondon_withGaps_Files = list.files("../20_LSOACoastalLists")[36:39]

# We are primarily interested in the BGC and BSC NoLondon_NoGaps versions 
# We will create BlueGreenCategorisation.csv below (and do lots of checking)


# OK, time to create BlueGreenCategorisation.csv 

# First, look at the BGC data ##################################################

# Set up full list of LSOAs in England
Collated = read.csv("../20_LSOACoastalLists/BSC_LSOA21_Distance_withLondon_withGaps_1-20.csv")
nrow(Collated)
Collated = Collated %>% select(LSOA21CD) %>% arrange(LSOA21CD)
head(Collated)
tail(Collated)

for (i in 1:4){
  Input = read.csv(paste0("../20_LSOACoastalLists/",BGC_LSOA21_Distance_NoLondon_NoGaps_Files[i]))
  Collated = Collated %>% left_join(Input, by = "LSOA21CD")
}
Collated = Collated %>% mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
nrow(Collated)
length(Collated)
names(Collated)

ColNames = c("LSOA21CD","BGC_40000","BGC_39500","BGC_39000","BGC_38500","BGC_38000","BGC_37500","BGC_37000",
             "BGC_36500","BGC_36000","BGC_35500","BGC_35000","BGC_34500","BGC_34000","BGC_33500",
             "BGC_33000","BGC_32500","BGC_32000","BGC_31500","BGC_31000","BGC_30500",
             "BGC_30000","BGC_29500","BGC_29000","BGC_28500","BGC_28000","BGC_27500","BGC_27000",
             "BGC_26500","BGC_26000","BGC_25500","BGC_25000","BGC_24500","BGC_24000","BGC_23500",
             "BGC_23000","BGC_22500","BGC_22000","BGC_21500","BGC_21000","BGC_20500",
             "BGC_20000","BGC_19500","BGC_19000","BGC_18500","BGC_18000","BGC_17500","BGC_17000",
             "BGC_16500","BGC_16000","BGC_15500","BGC_15000","BGC_14500","BGC_14000","BGC_13500",
             "BGC_13000","BGC_12500","BGC_12000","BGC_11500","BGC_11000","BGC_10500",
             "BGC_10000","BGC_9500","BGC_9000","BGC_8500","BGC_8000","BGC_7500","BGC_7000",
             "BGC_6500","BGC_6000","BGC_5500","BGC_5000","BGC_4500","BGC_4000","BGC_3500",
             "BGC_3000","BGC_2500","BGC_2000","BGC_1500","BGC_1000","BGC_500")
length(ColNames)

names(Collated) = ColNames
names(Collated)

NewOrder = seq(81,2,-1)
Ordered = Collated[,c(1,NewOrder)]
names(Ordered)

write.csv(Ordered,"../20_LSOACoastalLists/BGC_LSOA21_Distance_NoLondon_NoGaps_All.csv", row.names = FALSE)


# Second, look at the BSC version ##############################################

# Set up full list of LSOAs in England
Collated = read.csv("../20_LSOACoastalLists/BSC_LSOA21_Distance_withLondon_withGaps_1-20.csv")
nrow(Collated)
Collated = Collated %>% select(LSOA21CD) %>% arrange(LSOA21CD)
head(Collated)
tail(Collated)

for (i in 1:4){
  Input = read.csv(paste0("../20_LSOACoastalLists/",BSC_LSOA21_Distance_NoLondon_NoGaps_Files[i]))
  Collated = Collated %>% left_join(Input, by = "LSOA21CD")
}
Collated = Collated %>% mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
nrow(Collated)
length(Collated)
names(Collated)

ColNames = c("LSOA21CD","BSC_40000","BSC_39500","BSC_39000","BSC_38500","BSC_38000","BSC_37500","BSC_37000",
             "BSC_36500","BSC_36000","BSC_35500","BSC_35000","BSC_34500","BSC_34000","BSC_33500",
             "BSC_33000","BSC_32500","BSC_32000","BSC_31500","BSC_31000","BSC_30500",
             "BSC_30000","BSC_29500","BSC_29000","BSC_28500","BSC_28000","BSC_27500","BSC_27000",
             "BSC_26500","BSC_26000","BSC_25500","BSC_25000","BSC_24500","BSC_24000","BSC_23500",
             "BSC_23000","BSC_22500","BSC_22000","BSC_21500","BSC_21000","BSC_20500",
             "BSC_20000","BSC_19500","BSC_19000","BSC_18500","BSC_18000","BSC_17500","BSC_17000",
             "BSC_16500","BSC_16000","BSC_15500","BSC_15000","BSC_14500","BSC_14000","BSC_13500",
             "BSC_13000","BSC_12500","BSC_12000","BSC_11500","BSC_11000","BSC_10500",
             "BSC_10000","BSC_9500","BSC_9000","BSC_8500","BSC_8000","BSC_7500","BSC_7000",
             "BSC_6500","BSC_6000","BSC_5500","BSC_5000","BSC_4500","BSC_4000","BSC_3500",
             "BSC_3000","BSC_2500","BSC_2000","BSC_1500","BSC_1000","BSC_500")
length(ColNames)

names(Collated) = ColNames
names(Collated)

NewOrder = seq(81,2,-1)
Ordered = Collated[,c(1,NewOrder)]
names(Ordered)

write.csv(Ordered,"../20_LSOACoastalLists/BSC_LSOA21_Distance_NoLondon_NoGaps_All.csv", row.names = FALSE)


# Read back in and check
LSOAMap = st_read("../02_InputData/Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BGC_V5_-6970154227154374572.gpkg")

BGC.Set = read.csv("../20_LSOACoastalLists/BGC_LSOA21_Distance_NoLondon_NoGaps_All.csv")
Extract = BGC.Set[,c(1,2)]
names(Extract)
TestMap = LSOAMap %>% left_join(Extract, by = "LSOA21CD") %>% filter(.[[9]] == 1)
mapview(TestMap)

Extract = BGC.Set[,c(1,21)]
names(Extract)
TestMap = LSOAMap %>% left_join(Extract, by = "LSOA21CD") %>% filter(.[[9]] == 1)
mapview(TestMap)

Extract = BGC.Set[,c(1,41)]
names(Extract)
TestMap = LSOAMap %>% left_join(Extract, by = "LSOA21CD") %>% filter(.[[9]] == 1)
mapview(TestMap)

Extract = BGC.Set[,c(1,61)]
names(Extract)
TestMap = LSOAMap %>% left_join(Extract, by = "LSOA21CD") %>% filter(.[[9]] == 1)
mapview(TestMap)

Extract = BGC.Set[,c(1,81)]
names(Extract)
TestMap = LSOAMap %>% left_join(Extract, by = "LSOA21CD") %>% filter(.[[9]] == 1)
mapview(TestMap)


# So there is a problem here - we always miss E01018849 because the LSOA centroid is off the coast

# Fix E01018849
BGC.Set %>% filter(LSOA21CD == "E01018849")

BGC.Set = BGC.Set %>%
  mutate(across(2:81, ~ if_else(LSOA21CD == "E01018849", 1, .))
  )

BGC.Set %>% filter(LSOA21CD == "E01018849")
head(BGC.Set)

# Save the corrected version
write.csv(BGC.Set,"../20_LSOACoastalLists/BGC_LSOA21_Distance_NoLondon_NoGaps_All.csv", row.names = FALSE)


# We also find some holes in the BGC maps - it looks because there are occasional rivers
# that break the outer boundary of the filled polygon.  Are these fixed if we 
# combine with Blue or Blue Proximate LSOAs?  We will check that out below.


# Check "E01018849" on the BSC map
BSC.Set = read.csv("../20_LSOACoastalLists/BSC_LSOA21_Distance_NoLondon_NoGaps_All.csv")

Extract = BSC.Set[,c(1,2)]
names(Extract)
TestMap = LSOAMap %>% left_join(Extract, by = "LSOA21CD") %>% filter(.[[9]] == 1)
mapview(TestMap)

Extract = BSC.Set[,c(1,21)]
names(Extract)
TestMap = LSOAMap %>% left_join(Extract, by = "LSOA21CD") %>% filter(.[[9]] == 1)
mapview(TestMap)

Extract = BSC.Set[,c(1,41)]
names(Extract)
TestMap = LSOAMap %>% left_join(Extract, by = "LSOA21CD") %>% filter(.[[9]] == 1)
mapview(TestMap)

Extract = BSC.Set[,c(1,61)]
names(Extract)
TestMap = LSOAMap %>% left_join(Extract, by = "LSOA21CD") %>% filter(.[[9]] == 1)
mapview(TestMap)

Extract = BSC.Set[,c(1,81)]
names(Extract)
TestMap = LSOAMap %>% left_join(Extract, by = "LSOA21CD") %>% filter(.[[9]] == 1)
mapview(TestMap)


# So "E01018849" is captured as coastal on the BSC map - so that is ggod

# And we don't seem to have problems with holes, such as around Arundel



# Now - what about those holes in the BGC set?
# I have looked at this down to 2km - at which point it becomes a real mess!
# Should I redo this whole thing using boundaries rather than centroids - probably wouldn't help!!!

TestMap = LSOAMap %>% left_join(BGC.Set, by = "LSOA21CD") %>% filter(BGC_10000 == 1)
# need to look at this in QGIS as its quicker than mapview()

# THIS WAS A MISTAKE - IT TOOK AGES!

# NOTE THAT HOLES AND ISLANDS OF UP TO 4 LSOAS HAVE BEEN REMOVED.

st_write(TestMap, "../TestMap.gpkg")

names(TestMap)
mapview(TestMap, zcol = "BGC_10000" , col.region = "red")

# To become coastal ####################################
# North Linclshire E01013250, E01013252 (4)
# Norwich E01033441 (30 >> 13, 12.5, 12, 11.5, 11. 10.5, 10, 9.5, 9, 8.5, 8, 7.5, 7, 6.5, 6, 5.5, 5, 4.5, 4)
# Thorrock E01034371 (9.5, 9, 8.5, 8, 7.5, 7, 6.5, 6, 5.5, 5, 4.5, 4)
# Kings Lynn E01026711,E01026712, E01026713 (5)
# Lincolnshire E01013247, E01013249, E01013248, E01013243, E01013244, E01013242 (5, 4.5, 4) 
# East Riding E01012926, E01013034, E01013035, E01013030, E01013028, E01013033, E01013085, E01013081 (5, 4.5)
# Isle of Wight E01017296, E01017297, E01017369 (5, 4.5, 4)
# Dorset E01020466 (4)

# Tendring E01022051,  E01022019,  E01021972 (5) # end
# Tendring E01022051,  E01022019,  E01021972, E01022018, E01022017  (4.5) #end
#
# East Riding E01013081 (5.5)
# Arundel E01031392, E01031391 (30 >> 13, 12.5, 12, 11.5, 11, 10.5, 10, 9.5, 9, 8.5, 8, 7.5, 7, 6.5, 6, 5.5, 5)# end
# Sunderland E01008789 (9.5 > and all the way to 0.5) # end
# South Holland E01026249 (12, 11.5)#end
# Mid Sussex E01031763, E01035352, E01032839, E01035348, E01031751, E01031749, E01031750 (12)#end
# South Holland E01026249
# Chelmsford E01021635 (20, 19.5, 19, 18.5, 18, 17.5, 17, 16.5, 16, 15.5, 15, 14.5)#end
# Cotswolds E01034789, E01022220, E01022219 (20)#end
# Tunbridge E01024828 (20)#end
# Sevenoaks E01024463 (20)#end
# Bassetlaw E01028001, E01028002 (21.5)#end
# Crawley; E01035329 (30, 29.5, 29, 28.5, 28, 27.5, 27, 26.5, 26, 25.5, 25, 24.5, 24, 23.5, 23, 22.5, 22)#end
# Sevenoaks: E01024456 (30, 29.5, 29, 28.5, 28, 27.5, 27, 26.5, 26, 25.5)#end
# Wealden: E01021230 (30, 29.5, 29, 28.5, 28, 27.5, 27, 26.5, 26, 25.5)#end
# Northumberland E01027503 (40, 39.5, 39)#end

Backup.BGC.Set=BGC.Set
# Undertake above changes - 0>1 as coastal not non-coastal
BGC.Set <- BGC.Set %>% mutate(across(9, ~ if_else(LSOA21CD == "E01013250" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(9, ~ if_else(LSOA21CD == "E01013252" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(9:61, ~ if_else(LSOA21CD == "E01033441" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(9:20, ~ if_else(LSOA21CD == "E01034371" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(11, ~ if_else(LSOA21CD == "E01026711" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(11, ~ if_else(LSOA21CD == "E01026712" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(11, ~ if_else(LSOA21CD == "E01026713" & . == 0, 1, .)))
#BGC.Set <- BGC.Set %>% mutate(across(9:11, ~ if_else(LSOA21CD == "E01013247" & . == 0, 1, .)))
#BGC.Set <- BGC.Set %>% mutate(across(9:11, ~ if_else(LSOA21CD == "E01013249" & . == 0, 1, .)))
#BGC.Set <- BGC.Set %>% mutate(across(9:11, ~ if_else(LSOA21CD == "E01013248" & . == 0, 1, .)))
#BGC.Set <- BGC.Set %>% mutate(across(9:11, ~ if_else(LSOA21CD == "E01013243" & . == 0, 1, .)))
#BGC.Set <- BGC.Set %>% mutate(across(9:11, ~ if_else(LSOA21CD == "E01013244" & . == 0, 1, .)))
#BGC.Set <- BGC.Set %>% mutate(across(9:11, ~ if_else(LSOA21CD == "E01013242" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(9:10, ~ if_else(LSOA21CD == "E01013242" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(9:10, ~ if_else(LSOA21CD == "E01013242" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(9:10, ~ if_else(LSOA21CD == "E01013242" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(9:10, ~ if_else(LSOA21CD == "E01013242" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(9:10, ~ if_else(LSOA21CD == "E01013242" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(9:10, ~ if_else(LSOA21CD == "E01013242" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(9:10, ~ if_else(LSOA21CD == "E01013242" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(9:10, ~ if_else(LSOA21CD == "E01013242" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(9:11, ~ if_else(LSOA21CD == "E01017296" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(9:11, ~ if_else(LSOA21CD == "E01017297" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(9:11, ~ if_else(LSOA21CD == "E01017369" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(9, ~ if_else(LSOA21CD == "E01020466" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(11, ~ if_else(LSOA21CD == "E01022051" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(11, ~ if_else(LSOA21CD == "E01022019" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(11, ~ if_else(LSOA21CD == "E01021972" & . == 0, 1, .)))
#BGC.Set <- BGC.Set %>% mutate(across(10, ~ if_else(LSOA21CD == "E01022051" & . == 0, 1, .)))
#BGC.Set <- BGC.Set %>% mutate(across(10, ~ if_else(LSOA21CD == "E01022019" & . == 0, 1, .)))
#BGC.Set <- BGC.Set %>% mutate(across(10, ~ if_else(LSOA21CD == "E01021972" & . == 0, 1, .)))
#BGC.Set <- BGC.Set %>% mutate(across(10, ~ if_else(LSOA21CD == "E01022018" & . == 0, 1, .)))
#BGC.Set <- BGC.Set %>% mutate(across(10, ~ if_else(LSOA21CD == "E01022017" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(12, ~ if_else(LSOA21CD == "E01013081" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(11:61, ~ if_else(LSOA21CD == "E01031392" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(11:61, ~ if_else(LSOA21CD == "E01031391" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(2:20, ~ if_else(LSOA21CD == "E01008789" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(24:25, ~ if_else(LSOA21CD == "E01026249" & . == 0, 1, .)))
#BGC.Set <- BGC.Set %>% mutate(across(25, ~ if_else(LSOA21CD == "E01031763" & . == 0, 1, .)))
#BGC.Set <- BGC.Set %>% mutate(across(25, ~ if_else(LSOA21CD == "E01035352" & . == 0, 1, .)))
#BGC.Set <- BGC.Set %>% mutate(across(25, ~ if_else(LSOA21CD == "E01032839" & . == 0, 1, .)))
#BGC.Set <- BGC.Set %>% mutate(across(25, ~ if_else(LSOA21CD == "E01035348" & . == 0, 1, .)))
#BGC.Set <- BGC.Set %>% mutate(across(25, ~ if_else(LSOA21CD == "E01031751" & . == 0, 1, .)))
#BGC.Set <- BGC.Set %>% mutate(across(25, ~ if_else(LSOA21CD == "E01031749" & . == 0, 1, .)))
#BGC.Set <- BGC.Set %>% mutate(across(25, ~ if_else(LSOA21CD == "E01031750" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(30:41, ~ if_else(LSOA21CD == "E01021635" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(41, ~ if_else(LSOA21CD == "E01034789" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(41, ~ if_else(LSOA21CD == "E01022220" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(41, ~ if_else(LSOA21CD == "E01022219" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(41, ~ if_else(LSOA21CD == "E01024828" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(41, ~ if_else(LSOA21CD == "E01024463" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(44, ~ if_else(LSOA21CD == "E01028001" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(44, ~ if_else(LSOA21CD == "E01028002" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(45:61, ~ if_else(LSOA21CD == "E01035329" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(54:61, ~ if_else(LSOA21CD == "E01024456" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(54:61, ~ if_else(LSOA21CD == "E01021230" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(79:81, ~ if_else(LSOA21CD == "E01027503" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(54:61, ~ if_else(LSOA21CD == "E01024851" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(53:61, ~ if_else(LSOA21CD == "E01021231" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(54:61, ~ if_else(LSOA21CD == "E01024456" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(54:61, ~ if_else(LSOA21CD == "E01021230" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(53:54, ~ if_else(LSOA21CD == "E01026366" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(53:54, ~ if_else(LSOA21CD == "E01026082" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(53:54, ~ if_else(LSOA21CD == "E01026121" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(53:54, ~ if_else(LSOA21CD == "E01026066" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(25, ~ if_else(LSOA21CD == "E01035352" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(10, ~ if_else(LSOA21CD == "E01026711" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(10, ~ if_else(LSOA21CD == "E01026712" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(10, ~ if_else(LSOA21CD == "E01026713" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(9, ~ if_else(LSOA21CD == "E01013241" & . == 0, 1, .)))

BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01033441" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01021906" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01021904" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01021905" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01034371" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01026701" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01017297" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01017296" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01017369" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01020466" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01019006" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01018954" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01026636" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01026608" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01026539" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01033441" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01033443" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01021906" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01021904" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01021905" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01034371" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01017297" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01017296" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01017369" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01020466" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01019006" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01018954" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01018987" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01026636" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01026608" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01026539" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01026512" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01026513" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01033441" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01026899" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01026944" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01030259" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01033443" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01030216" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01021904" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01021905" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01021906" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01021907" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01034371" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01035300" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01020466" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01019006" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01018987" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01018989" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01007121" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01007124" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01007159" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01033441" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01033443" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01030198" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01030217" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01030214" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01030215" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01030216" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01021904" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01021905" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01021906" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01021907" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01034371" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01035300" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01020466" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01020468" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01020172" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01019005" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01019006" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01018989" & . == 0, 1, .)))

BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01024535" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01022700" & . == 0, 1, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01018987" & . == 0, 1, .)))

BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01022700" & . == 0, 1, .)))



# save to check (using QGIS) that the removal of all holes has worked
write.csv(BGC.Set,"../20_LSOACoastalLists/Corrected_BGC_LSOA21_Distance_NoLondon_NoGaps_All.csv", row.names = FALSE)
CorrectedTestMap = LSOAMap %>% left_join(BGC.Set, by = "LSOA21CD")
# need to look at this in QGIS as its quicker than mapview()
st_write(CorrectedTestMap, "../CorrectedTestMap.gpkg")




# Islands > to become inland:

# Cornwall E01018848
# Fenland E01018059,  E01018064,  E01018072,  E01018058,  E01018075,  E01018074,  E01018061,  E01018098,  E01018099,  E01018100,  E01018101 (4)
# Tendring E012022052 (4)
# Kings Lynn E01026653, E01026655, E01026654, E01026701, E01026702 (4.5, 4)
# West Devon E01020334, E01020335, E01020336, E01020338 (5, 4.5) 
# Cambridgeshire E01018020, E01018021, E01018076, E01018056,  E01018055,  E01018022, E01018123, E01018124, E01018198, E01018199, E01018121, E01018122, E01018284, E01018304, E01018305, E01018306, E01018307, E01018285  (5, 4.5, 4)
# Lewes E01021024 (5)

# Copeland E01019264 (5.5)#end
# Chorley E01024969 (5.5)#end
# Warrington E01012467, E01012470 (5.5)#end
# Chichester E01031545(5.5, 5)#end
# NE Lincolnshire E01013224 (6.5)#end
# Fenland E01018086,E01018081, E01018085, E01018087, E01018088, E01018083, E01018082, E01018084, E01018077, E01018079, E01018080, E01018078 (7, 6.5, 6, 5.5)# end
# Test Valley E01023150, E01023151, E01023152, E01023175, E01023176, E01023206, E01023209, E01023207, E01023208  (7, 6.5)#end

# NewForest E01023000 (7)#end
# South Cambridgeshire (6)#end
# Dorset E01020525 (7.5)# end
# Horsham E01031677, E01031678 (8.5, 8)#end
# Stroud E0122390 (8.5)# end
# Dover E01035314 (9, 8.5)# end
# Durham E01020791 (9)# end
# Dover E01035315, E0124192 (9)#end
# Newark E01028307 (9.5)# end
# Chelmsford E01021562 (9.5)#end
# Cheshire E01018599 (11.5, 11, 10.5, 10, 9.5)#end
# Hambleton E01027599 (10)#end
# Chelmsford E01021553, E01021554 (10)#end
# South Holland E01026240 (11)#end
# Dorset E01020496, E01020495 (11)#end
# Dorset E01020510 (13, 12.5, 12, 11.5)#end
# Wakefield E01011745 (12)#end
# Bath E01014477 (12.5)#end
# Durham E01020665 (13.5)#end
# East Lindsay E01026088, E01026108 (13.5, 13)# end
# East Lindsay E01026087, E01032987, E01026090 (13.5)# end
# Leeds E01011705 (14)# end
# Mid Devon E01020060, E01020042, E01020041 (15, 14.5, 14)#end
# Chelmsdford E01021636 (14.5)#end
# South Somerset E01029198 (14.5)#end
# Mid Devon E01020060, E01020042 (15, 14.5)#end
# Teweksbury E01022438 (15)#end
# Ashford E01024041 (15.5)#end
# Carlise E01019201 (17, 16.5)#end 
# Carlise E01019202(17) #end
# Brentwood E01020471,E01021444 (18)#end
# Durham E01020798 (18)#end
# Durham E01020805 (18.5)# end
# Tunbridge: E01024827 (19.5)#end
# Wiltshire: E01031930 (19.5)#end
# Suffolk: E01029944 (20.5, 20)#end
# Tunbridge: E01024788 (21, 20.5)#end
# Darlington E01012335 (21)#end
# Northumberland E01027478 (33, 32.5, 32)#end
# Eden: E01019330 (33)#end
# Big set around Guilford & Mole valley ( just 31 and 30.5)#end

BGC.Set <- BGC.Set %>% mutate(across(9, ~ if_else(LSOA21CD == "E01018848" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(9, ~ if_else(LSOA21CD == "E012022052" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(9:10, ~ if_else(LSOA21CD == "E01026653" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(9:10, ~ if_else(LSOA21CD == "E01026655" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(9:10, ~ if_else(LSOA21CD == "E01026654" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(9:10, ~ if_else(LSOA21CD == "E01026701" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(9:10, ~ if_else(LSOA21CD == "E01026702" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(10:11, ~ if_else(LSOA21CD == "E01020334" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(10:11, ~ if_else(LSOA21CD == "E01020335" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(10:11, ~ if_else(LSOA21CD == "E01020336" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(10:11, ~ if_else(LSOA21CD == "E01020338" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(11, ~ if_else(LSOA21CD == "E01021024" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(12, ~ if_else(LSOA21CD == "E01019264" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(12, ~ if_else(LSOA21CD == "E01024969" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(12, ~ if_else(LSOA21CD == "E01012467" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(12, ~ if_else(LSOA21CD == "E01012470" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(11:12, ~ if_else(LSOA21CD == "E01031545" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(14, ~ if_else(LSOA21CD == "E01013224" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(15, ~ if_else(LSOA21CD == "E01013224" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(16, ~ if_else(LSOA21CD == "E01020525" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(17:18, ~ if_else(LSOA21CD == "E01031677" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(17:18, ~ if_else(LSOA21CD == "E01031678" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(18:19, ~ if_else(LSOA21CD == "E01035314" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(19, ~ if_else(LSOA21CD == "E01020791" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(19, ~ if_else(LSOA21CD == "E01035315" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(20, ~ if_else(LSOA21CD == "E01028307" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(20, ~ if_else(LSOA21CD == "E01021562" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(20:24, ~ if_else(LSOA21CD == "E01018599" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(21, ~ if_else(LSOA21CD == "E01027599" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(21, ~ if_else(LSOA21CD == "E01021553" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(21, ~ if_else(LSOA21CD == "E01021554" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(23, ~ if_else(LSOA21CD == "E01026240" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(23, ~ if_else(LSOA21CD == "E01020496" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(23, ~ if_else(LSOA21CD == "E01020495" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(24:27, ~ if_else(LSOA21CD == "E01020510" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(25, ~ if_else(LSOA21CD == "E01011745" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(26, ~ if_else(LSOA21CD == "E01014477" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(28, ~ if_else(LSOA21CD == "E01020665" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(27:28, ~ if_else(LSOA21CD == "E01026088" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(27:28, ~ if_else(LSOA21CD == "E01026108" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(28, ~ if_else(LSOA21CD == "E01026087" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(28, ~ if_else(LSOA21CD == "E01032987" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(28, ~ if_else(LSOA21CD == "E01026090" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(29, ~ if_else(LSOA21CD == "E01011705" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(29:31, ~ if_else(LSOA21CD == "E01020060" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(29:31, ~ if_else(LSOA21CD == "E01020042" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(29:31, ~ if_else(LSOA21CD == "E01020041" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(30, ~ if_else(LSOA21CD == "E01021636" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(30, ~ if_else(LSOA21CD == "E01029198" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(30:31, ~ if_else(LSOA21CD == "E01020060" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(30:31, ~ if_else(LSOA21CD == "E01020042" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(31, ~ if_else(LSOA21CD == "E01022438" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(32, ~ if_else(LSOA21CD == "E01024041" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(34:35, ~ if_else(LSOA21CD == "E01019201" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(35, ~ if_else(LSOA21CD == "E01019202" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(37, ~ if_else(LSOA21CD == "E01020471" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(37, ~ if_else(LSOA21CD == "E01021444" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(37, ~ if_else(LSOA21CD == "E01020798" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(38, ~ if_else(LSOA21CD == "E01020805" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(40, ~ if_else(LSOA21CD == "E01024827" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(40, ~ if_else(LSOA21CD == "E01031930" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(41:42, ~ if_else(LSOA21CD == "E01029944" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(42:43, ~ if_else(LSOA21CD == "E01024788" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(43, ~ if_else(LSOA21CD == "E01012335" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(65:67, ~ if_else(LSOA21CD == "E01027478" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(67, ~ if_else(LSOA21CD == "E01019330" & . == 1, 0, .)))

BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01027443" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01012989" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01026702" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01026701" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01026653" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01020304" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01027443" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01029142" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01019272" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01027443" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01028350" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01035436" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01029271" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01029279" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01025106" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01027443" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01027446" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01013176" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01013177" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01018064" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01028100" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01018058" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01018091" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01024623" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01024702" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01024704" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01029271" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01029279" & . == 1, 0, .)))

BGC.Set <- BGC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01018848" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01017291" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01020598" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01028350" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01017291" & . == 1, 0, .)))

BGC.Set <- BGC.Set %>% mutate(across(6, ~ if_else(LSOA21CD == "E01029138" & . == 1, 0, .)))
BGC.Set <- BGC.Set %>% mutate(across(5, ~ if_else(LSOA21CD == "E01018100" & . == 1, 0, .)))






# save to check (using QGIS) that the removal of all holes has worked
write.csv(BGC.Set,"../20_LSOACoastalLists/Corrected_BGC_LSOA21_Distance_NoLondon_NoGaps_All.csv", row.names = FALSE)
CorrectedTestMap = LSOAMap %>% left_join(BGC.Set, by = "LSOA21CD")
# need to look at this in QGIS as its quicker than mapview()
st_write(CorrectedTestMap, "../CorrectedTestMap.gpkg")


################################################################################

# NOW CHECK THE BSC MAP FOR HOLES AND ISLANDS

BSC.Set = read.csv("../20_LSOACoastalLists/BSC_LSOA21_Distance_NoLondon_NoGaps_All.csv")
BSCTestMap = LSOAMap %>% left_join(BSC.Set, by = "LSOA21CD")
st_write(BSCTestMap, "../BSCTestMap.gpkg")

# Now having a look using QGIS

# AND THESE ARE THE CHANGES THAT NEED TO BE MADE - Note - only gone down to 3000 (3km)

# Non-coastal > coastal
BSC.Set %>% filter(LSOA21CD == "E01018849")
BSC.Set <- BSC.Set %>% mutate(across(2:81, ~ if_else(LSOA21CD == "E01018849" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(60, ~ if_else(LSOA21CD == "E01029203" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(54:60, ~ if_else(LSOA21CD == "E01020138" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(58:59, ~ if_else(LSOA21CD == "E01024806" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(17:57, ~ if_else(LSOA21CD == "E01034354" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(54:55, ~ if_else(LSOA21CD == "E01020049" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(54, ~ if_else(LSOA21CD == "E01020045" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(53, ~ if_else(LSOA21CD == "E01029261" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(48, ~ if_else(LSOA21CD == "E01024036" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(36, ~ if_else(LSOA21CD == "E01032810" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(36, ~ if_else(LSOA21CD == "E01032812" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(30, ~ if_else(LSOA21CD == "E01018782" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(30, ~ if_else(LSOA21CD == "E01018783" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(24:26, ~ if_else(LSOA21CD == "E01020176" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(24:25, ~ if_else(LSOA21CD == "E01020177" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(21, ~ if_else(LSOA21CD == "E01013035" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(21, ~ if_else(LSOA21CD == "E01019052" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(21, ~ if_else(LSOA21CD == "E01019053" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(21, ~ if_else(LSOA21CD == "E01019054" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(21, ~ if_else(LSOA21CD == "E01019055" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(15:16, ~ if_else(LSOA21CD == "E01018863" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(15:16, ~ if_else(LSOA21CD == "E01018894" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(15:16, ~ if_else(LSOA21CD == "E01018916" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(15:16, ~ if_else(LSOA21CD == "E01018917" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(15, ~ if_else(LSOA21CD == "E01018820" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(13, ~ if_else(LSOA21CD == "E01013081" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(8:13, ~ if_else(LSOA21CD == "E01017296" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(8:12, ~ if_else(LSOA21CD == "E01017369" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(8:11, ~ if_else(LSOA21CD == "E01017397" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(10, ~ if_else(LSOA21CD == "E01021883" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(8:10, ~ if_else(LSOA21CD == "E01007144" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(8:10, ~ if_else(LSOA21CD == "E01018571" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(9:10, ~ if_else(LSOA21CD == "E01018569" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(9, ~ if_else(LSOA21CD == "E01024535" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(7:10, ~ if_else(LSOA21CD == "E01020466" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(7:9, ~ if_else(LSOA21CD == "E01021904" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(7:9, ~ if_else(LSOA21CD == "E01021905" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(7:9, ~ if_else(LSOA21CD == "E01021906" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(7:8, ~ if_else(LSOA21CD == "E01033443" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01016073" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01017294" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01019006" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01018954" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(7:8, ~ if_else(LSOA21CD == "E01019148" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(7:8, ~ if_else(LSOA21CD == "E01019151" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(7:8, ~ if_else(LSOA21CD == "E01019150" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(7:8, ~ if_else(LSOA21CD == "E01019146" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01018763" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01018987" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01020091" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01019147" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(7, ~ if_else(LSOA21CD == "E01019149" & . == 0, 1, .)))


# Coastal to Non-coastal
BSC.Set <- BSC.Set %>% mutate(across(2:78, ~ if_else(LSOA21CD == "E01029942" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:78, ~ if_else(LSOA21CD == "E01029944" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:74, ~ if_else(LSOA21CD == "E01011298" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:73, ~ if_else(LSOA21CD == "E01027749" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:72, ~ if_else(LSOA21CD == "E01028025" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:72, ~ if_else(LSOA21CD == "E01018061" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:72, ~ if_else(LSOA21CD == "E01022533" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:70, ~ if_else(LSOA21CD == "E01032096" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:68, ~ if_else(LSOA21CD == "E01027478" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:67, ~ if_else(LSOA21CD == "E01019331" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:67, ~ if_else(LSOA21CD == "E01015631" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:63, ~ if_else(LSOA21CD == "E01027784" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:61, ~ if_else(LSOA21CD == "E01026938" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:60, ~ if_else(LSOA21CD == "E01024827" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:58, ~ if_else(LSOA21CD == "E01020905" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:55, ~ if_else(LSOA21CD == "E01021231" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:55, ~ if_else(LSOA21CD == "E01007601" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:54, ~ if_else(LSOA21CD == "E01026902" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:54, ~ if_else(LSOA21CD == "E01032007" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:54, ~ if_else(LSOA21CD == "E01032005" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:50, ~ if_else(LSOA21CD == "E01020873" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:50, ~ if_else(LSOA21CD == "E01024788" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:50, ~ if_else(LSOA21CD == "E01012335" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:49, ~ if_else(LSOA21CD == "E01027901" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:44, ~ if_else(LSOA21CD == "E01021471" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:43, ~ if_else(LSOA21CD == "E01014477" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:42, ~ if_else(LSOA21CD == "E01026927" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:41, ~ if_else(LSOA21CD == "E01022218" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:41, ~ if_else(LSOA21CD == "E01019385" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:39, ~ if_else(LSOA21CD == "E01020058" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:38, ~ if_else(LSOA21CD == "E01020041" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:38, ~ if_else(LSOA21CD == "E01020042" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:38, ~ if_else(LSOA21CD == "E01020060" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:38, ~ if_else(LSOA21CD == "E01020061" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:35, ~ if_else(LSOA21CD == "E01021636" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:35, ~ if_else(LSOA21CD == "E01019201" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:35, ~ if_else(LSOA21CD == "E01019202" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:34, ~ if_else(LSOA21CD == "E01027482" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:32, ~ if_else(LSOA21CD == "E01029320" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:31, ~ if_else(LSOA21CD == "E01020830" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:30, ~ if_else(LSOA21CD == "E01018599" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:30, ~ if_else(LSOA21CD == "E01026087" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:30, ~ if_else(LSOA21CD == "E01026088" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:30, ~ if_else(LSOA21CD == "E01026108" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:28, ~ if_else(LSOA21CD == "E01024043" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:27, ~ if_else(LSOA21CD == "E01020510" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:27, ~ if_else(LSOA21CD == "E01021220" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:26, ~ if_else(LSOA21CD == "E01021553" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:26, ~ if_else(LSOA21CD == "E01021554" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:24, ~ if_else(LSOA21CD == "E01018368" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:24, ~ if_else(LSOA21CD == "E01035010" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:23, ~ if_else(LSOA21CD == "E01027599" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:23, ~ if_else(LSOA21CD == "E01025282" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:22, ~ if_else(LSOA21CD == "E01024963" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:21, ~ if_else(LSOA21CD == "E01018770" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:21, ~ if_else(LSOA21CD == "E01018935" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:17, ~ if_else(LSOA21CD == "E01023150" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:17, ~ if_else(LSOA21CD == "E01023151" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:17, ~ if_else(LSOA21CD == "E01023152" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:17, ~ if_else(LSOA21CD == "E01023175" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:17, ~ if_else(LSOA21CD == "E01023176" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:17, ~ if_else(LSOA21CD == "E01023206" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:17, ~ if_else(LSOA21CD == "E01023207" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:17, ~ if_else(LSOA21CD == "E01023208" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:17, ~ if_else(LSOA21CD == "E01023209" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:17, ~ if_else(LSOA21CD == "E01034627" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:16, ~ if_else(LSOA21CD == "E01023000" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:16, ~ if_else(LSOA21CD == "E01020304" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:16, ~ if_else(LSOA21CD == "E01022346" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:15, ~ if_else(LSOA21CD == "E01013224" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:15, ~ if_else(LSOA21CD == "E01023207" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:15, ~ if_else(LSOA21CD == "E01020222" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:14, ~ if_else(LSOA21CD == "E01020492" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:12, ~ if_else(LSOA21CD == "E01022052" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:12, ~ if_else(LSOA21CD == "E01031616" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:12, ~ if_else(LSOA21CD == "E01019264" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:11, ~ if_else(LSOA21CD == "E01022052" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:11, ~ if_else(LSOA21CD == "E01018848" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:10, ~ if_else(LSOA21CD == "E01021883" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:10, ~ if_else(LSOA21CD == "E01014985" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:9, ~ if_else(LSOA21CD == "E01026653" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:9, ~ if_else(LSOA21CD == "E01026701" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:9, ~ if_else(LSOA21CD == "E01026702" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:9, ~ if_else(LSOA21CD == "E01021906" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:9, ~ if_else(LSOA21CD == "E01024542" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:9, ~ if_else(LSOA21CD == "E01014987" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:8, ~ if_else(LSOA21CD == "E01027443" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:7, ~ if_else(LSOA21CD == "E01017291" & . == 1, 0, .)))

write.csv(BSC.Set,"../20_LSOACoastalLists/Corrected_BSC_LSOA21_Distance_NoLondon_NoGaps_All.csv", row.names = FALSE)

CorrectedBSCTestMap = LSOAMap %>% left_join(BSC.Set, by = "LSOA21CD")
st_write(CorrectedBSCTestMap, "../CorrectedBSCTestMap.gpkg")



# Second iteration #####################################################################
BSC.Set <- BSC.Set %>% mutate(across(2:75, ~ if_else(LSOA21CD == "E01029929" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:72, ~ if_else(LSOA21CD == "E01022535" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:62, ~ if_else(LSOA21CD == "E01023218" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:62, ~ if_else(LSOA21CD == "E01020547" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:54, ~ if_else(LSOA21CD == "E01035133" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:53, ~ if_else(LSOA21CD == "E01026459" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:53, ~ if_else(LSOA21CD == "E01014377" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:52, ~ if_else(LSOA21CD == "E01004907" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:52, ~ if_else(LSOA21CD == "E01004909" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:47, ~ if_else(LSOA21CD == "E01024365" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:27, ~ if_else(LSOA21CD == "E01029220" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:17, ~ if_else(LSOA21CD == "E01034354" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:16, ~ if_else(LSOA21CD == "E01007067" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:14, ~ if_else(LSOA21CD == "E01018750" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:10, ~ if_else(LSOA21CD == "E01027400" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:9, ~ if_else(LSOA21CD == "E01012996" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:9, ~ if_else(LSOA21CD == "E01013003" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:9, ~ if_else(LSOA21CD == "E01013004" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:7, ~ if_else(LSOA21CD == "E01007120" & . == 1, 0, .)))
BSC.Set <- BSC.Set %>% mutate(across(2:7, ~ if_else(LSOA21CD == "E01025106" & . == 1, 0, .)))

BSC.Set <- BSC.Set %>% mutate(across(58:59, ~ if_else(LSOA21CD == "E01034354" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(50, ~ if_else(LSOA21CD == "E01012335" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(29:30, ~ if_else(LSOA21CD == "E01018782" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(29, ~ if_else(LSOA21CD == "E01018783" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(7:9, ~ if_else(LSOA21CD == "E01021906" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(7:9, ~ if_else(LSOA21CD == "E01019006" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(8, ~ if_else(LSOA21CD == "E01018569" & . == 0, 1, .)))
BSC.Set <- BSC.Set %>% mutate(across(8:11, ~ if_else(LSOA21CD == "E01017297" & . == 0, 1, .)))


write.csv(BSC.Set,"../20_LSOACoastalLists/Corrected_BSC_LSOA21_Distance_NoLondon_NoGaps_All.csv", row.names = FALSE)

CorrectedBSCTestMap = LSOAMap %>% left_join(BSC.Set, by = "LSOA21CD")
st_write(CorrectedBSCTestMap, "../CorrectedBSCTestMap.gpkg")

# Right - that was a nightmare - but done!!!!! And only have to do once!

#################################################################################
#################################################################################
#################################################################################
#################################################################################


# Now check that the Blue and BlueProximate maps look OK #######################

#BlueGreenCategorisation.csv file. This therefore includes columns with the following 
# prefixes followed by:
#(a) ‘withgaps’ or ‘nogaps’ and 
#(b) ‘withLondon’ or ‘noLondon’.  

#BSC_Blue: Including all LSOAs which physically touch the BSC (no tidal rivers) coastline,
#BSC_BlueProxminate: Including all BSC_Blue LSOAs and LSOAs which share a boundary with BSC_Blue LSOAs,
#BGC_Blue: Including all LSOAs which physically touch the BGC (with tidal rivers) coastline, and
#BGC_BlueProximate: Including all BGC_Blue LSOAs and LSOAs which share a boundary with BGC_Blue LSOAs .


# These are the BGC maps
st_write(LSOAIntersectsBGC.WithHoles, "../10_LSOACoastalGeographies/BlueLSOAs_BGC_withHoles_withLondon.gpkg", append=FALSE)
st_write(FinalBGC, "../10_LSOACoastalGeographies/BlueLSOAs_BGC_noHoles_withLondon.gpkg", append=FALSE)
st_write(LSOAIntersectsBGC.WithHoles.NoLondon, "../10_LSOACoastalGeographies/BlueLSOAs_BGC_withHoles_noLondon.gpkg", append=FALSE)
st_write(LSOA21_BGC_BlueNoHoles_NoLondon, "../10_LSOACoastalGeographies/BlueLSOAs_BGC_noHoles_noLondon.gpkg", append=FALSE)

# And these are the accompanying BGC .csv lists
write.csv(LSOA21_BGC_BlueWithHoles_List,"../20_LSOACoastalLists/BlueLSOAs_BGC_withHoles_withLondon_List.csv", row.names = FALSE)
write.csv(LSOA21_BGC_BlueNoHoles_List,"../20_LSOACoastalLists/BlueLSOAs_BGC_noHoles_withLondon_List.csv", row.names = FALSE)
write.csv(LSOAIntersectsBGC.WithHoles.NoLondon_List,"../20_LSOACoastalLists/BlueLSOAs_BGC_withHoles.noLondon_List.csv", row.names = FALSE)
write.csv(LSOA21_BGC_BlueNoHoles_NoLondon_List,"../20_LSOACoastalLists/BlueLSOAs_BGC_noHoles_noLondon_List.csv", row.names = FALSE)


# These are the BSC maps
st_write(FinalNeighbours,"../10_LSOACoastalGeographies/BlueProximateLSOAs_BSC_withHoles_withLondon.gpkg", append=FALSE)
st_write(FinalBlueProximateLSOAs,"../10_LSOACoastalGeographies/BlueProximateLSOAs_BSC_noHoles_withLondon.gpkg", append=FALSE)
st_write(Neighbours.WithHoles.NoLondon, "../10_LSOACoastalGeographies/BlueProximateLSOAs_BSC_withHoles_noLondon.gpkg", append=FALSE)
st_write(LSOA21_BSC_BlueProximateNoHoles_NoLondon, "../10_LSOACoastalGeographies/BlueProximateLSOAs_BSC_noHoles_noLondon.gpkg", append=FALSE)


# And these are the accompanying BSC .csv lists
write.csv(FinalNeighbours_List,"../20_LSOACoastalLists/BlueProximateLSOAs_BSC_withHoles_withLondon.csv")
write.csv(FinalBlueProximateLSOAs_List,"../20_LSOACoastalLists/BlueProximateLSOAs_BSC_NoHoles_withLondon_List.csv")
write.csv(Neighbours.WithHoles.NoLondon_List,"../20_LSOACoastalLists/BlueProximateLSOAs_BSC_withHoles_noLondon_List.csv", row.names = FALSE)
write.csv(LSOA21_BSC_BlueProximateNoHoles_NoLondon_List,"../20_LSOACoastalLists/BlueProximateLSOAs_BSC_noHoles_noLondon_List.csv", row.names = FALSE)


# Bring together all the BGC lists and check using QGIS

BGC_Blue_WithHoles_WithLondon = read.csv("../20_LSOACoastalLists/BlueLSOAs_BGC_withHoles_withLondon_List.csv")
BGC_Blue_NoHoles_WithLondon = read.csv("../20_LSOACoastalLists/BlueLSOAs_BGC_noHoles_withLondon_List.csv")
BGC_Blue_WithHoles_NoLondon = read.csv("../20_LSOACoastalLists/BlueLSOAs_BGC_withHoles.noLondon_List.csv")
BGC_Blue_NoHoles_NoLondon = read.csv("../20_LSOACoastalLists/BlueLSOAs_BGC_noHoles_noLondon_List.csv")

head(BGC_Blue_WithHoles_WithLondon)
head(BGC_Blue_NoHoles_WithLondon)
head(BGC_Blue_WithHoles_NoLondon)
head(BGC_Blue_NoHoles_NoLondon)

# Remove Wales and remove duplicates
BGC_Blue_WithHoles_WithLondon = BGC_Blue_WithHoles_WithLondon %>% rename("LSOA21CD" = "x") %>% 
  mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(Country)) %>% distinct()
nrow(BGC_Blue_WithHoles_WithLondon)
length(unique(BGC_Blue_WithHoles_WithLondon$LSOA21CD)) # 2336

BGC_Blue_NoHoles_WithLondon = BGC_Blue_NoHoles_WithLondon %>% rename("LSOA21CD" = "x") %>% 
  mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(Country)) %>% distinct()
nrow(BGC_Blue_NoHoles_WithLondon)
length(unique(BGC_Blue_NoHoles_WithLondon$LSOA21CD)) # 3065

BGC_Blue_WithHoles_NoLondon = BGC_Blue_WithHoles_NoLondon %>% rename("LSOA21CD" = "x") %>% 
  mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(Country)) %>% distinct() 
nrow(BGC_Blue_WithHoles_NoLondon)
length(unique(BGC_Blue_WithHoles_NoLondon$LSOA21CD)) # 2165

BGC_Blue_NoHoles_NoLondon = BGC_Blue_NoHoles_NoLondon %>% rename("LSOA21CD" = "x") %>% 
  mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(Country)) %>% distinct()
nrow(BGC_Blue_NoHoles_NoLondon)
length(unique(BGC_Blue_NoHoles_NoLondon$LSOA21CD)) # 2851

# Add data columns (with approporiate names and all =1) attach to list of all 33755 English LSOAs (thereby and collating)

BGC_Blue_WithHoles_WithLondon = BGC_Blue_WithHoles_WithLondon %>% mutate(BGC_Blue_withGaps_withLondon = 1)
BGC_Blue_NoHoles_WithLondon = BGC_Blue_NoHoles_WithLondon %>% mutate(BGC_Blue_noGaps_withLondon = 1)
BGC_Blue_WithHoles_NoLondon = BGC_Blue_WithHoles_NoLondon %>% mutate(BGC_Blue_Gaps_noLondon = 1)
BGC_Blue_NoHoles_NoLondon = BGC_Blue_NoHoles_NoLondon %>% mutate(BGC_Blue_noGaps_NoLondon = 1)

# Create list of all 33755 LSOAs
BGC_BlueLSOAList = read.csv("../20_LSOACoastalLists/BSC_LSOA21_Distance_withLondon_withGaps_1-20.csv")
BGC_BlueLSOAList = BGC_BlueLSOAList %>% select(LSOA21CD) %>% arrange(LSOA21CD)
nrow(BGC_BlueLSOAList)
head(BGC_BlueLSOAList)

# And left_join the BGC blue variants
BGC_BlueLSOAList = BGC_BlueLSOAList %>% left_join(BGC_Blue_WithHoles_WithLondon, by = "LSOA21CD")
BGC_BlueLSOAList = BGC_BlueLSOAList %>% left_join(BGC_Blue_NoHoles_WithLondon, by = "LSOA21CD")
BGC_BlueLSOAList = BGC_BlueLSOAList %>% left_join(BGC_Blue_WithHoles_NoLondon, by = "LSOA21CD")
BGC_BlueLSOAList = BGC_BlueLSOAList %>% left_join(BGC_Blue_NoHoles_NoLondon, by = "LSOA21CD")
BGC_BlueLSOAList = BGC_BlueLSOAList %>% mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
head(BGC_BlueLSOAList)
tail(BGC_BlueLSOAList)
sum(BGC_BlueLSOAList[,2])
sum(BGC_BlueLSOAList[,3])
sum(BGC_BlueLSOAList[,4])
sum(BGC_BlueLSOAList[,5])

# Now the same for BGC BlueProximate
BGC_BlueProximate_WithHoles_WithLondon = read.csv("../20_LSOACoastalLists/BlueProximateLSOAs_BGC_withHoles_withLondon.csv")
BGC_BlueProximate_NoHoles_WithLondon = read.csv("../20_LSOACoastalLists/BlueProximateLSOAs_BGC_NoHoles_withLondon_List.csv")
BGC_BlueProximate_WithHoles_NoLondon = read.csv("../20_LSOACoastalLists/BlueProximateLSOAs.withHoles.noLondon_List.csv")
BGC_BlueProximate_NoHoles_NoLondon = read.csv("../20_LSOACoastalLists/BlueProximateLSOAs_BGC_noHoles_noLondon_List.csv")

head(BGC_BlueProximate_WithHoles_WithLondon) # X & x
head(BGC_BlueProximate_NoHoles_WithLondon) # X & x
head(BGC_BlueProximate_WithHoles_NoLondon) # x
head(BGC_BlueProximate_NoHoles_NoLondon) # x

# Remove Wales and remove duplicates
BGC_BlueProximate_WithHoles_WithLondon = BGC_BlueProximate_WithHoles_WithLondon %>% rename("LSOA21CD" = "x") %>% 
  mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(X,Country)) %>% distinct()
nrow(BGC_BlueProximate_WithHoles_WithLondon)
length(unique(BGC_BlueProximate_WithHoles_WithLondon$LSOA21CD)) # 6218

BGC_BlueProximate_NoHoles_WithLondon = BGC_BlueProximate_NoHoles_WithLondon %>% rename("LSOA21CD" = "x") %>% 
  mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(X,Country)) %>% distinct()
nrow(BGC_BlueProximate_NoHoles_WithLondon)
length(unique(BGC_BlueProximate_NoHoles_WithLondon$LSOA21CD)) # 7560

BGC_BlueProximate_WithHoles_NoLondon = BGC_BlueProximate_WithHoles_NoLondon %>% rename("LSOA21CD" = "x") %>% 
  mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(Country)) %>% distinct()
nrow(BGC_BlueProximate_WithHoles_NoLondon)
length(unique(BGC_BlueProximate_WithHoles_NoLondon$LSOA21CD)) # 5640

BGC_BlueProximate_NoHoles_NoLondon = BGC_BlueProximate_NoHoles_NoLondon %>% rename("LSOA21CD" = "x") %>% 
  mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(Country)) %>% distinct()
nrow(BGC_BlueProximate_NoHoles_NoLondon)
length(unique(BGC_BlueProximate_NoHoles_NoLondon$LSOA21CD)) # 6972


# Add data columns (with approporiate names and all =1) attach to list of all 33755 English LSOAs (thereby and collating)
BGC_BlueProximate_WithHoles_WithLondon = BGC_BlueProximate_WithHoles_WithLondon %>% mutate(BGC_BlueProximate_withGaps_withLondon = 1)
BGC_BlueProximate_NoHoles_WithLondon = BGC_BlueProximate_NoHoles_WithLondon %>% mutate(BGC_BlueProximate_noGaps_withLondon = 1)
BGC_BlueProximate_WithHoles_NoLondon = BGC_BlueProximate_WithHoles_NoLondon %>% mutate(BGC_BlueProximate_Gaps_noLondon = 1)
BGC_BlueProximate_NoHoles_NoLondon = BGC_BlueProximate_NoHoles_NoLondon %>% mutate(BGC_BlueProximate_noGaps_NoLondon = 1)

# Create list of all 33755 LSOAs
BGC_BlueProximateLSOAList = read.csv("../20_LSOACoastalLists/BSC_LSOA21_Distance_withLondon_withGaps_1-20.csv")
BGC_BlueProximateLSOAList = BGC_BlueProximateLSOAList %>% select(LSOA21CD) %>% arrange(LSOA21CD)
nrow(BGC_BlueProximateLSOAList)
head(BGC_BlueProximateLSOAList)

# And left_join the BGC blue variants
BGC_BlueProximateLSOAList = BGC_BlueProximateLSOAList %>% left_join(BGC_BlueProximate_WithHoles_WithLondon, by = "LSOA21CD")
BGC_BlueProximateLSOAList = BGC_BlueProximateLSOAList %>% left_join(BGC_BlueProximate_NoHoles_WithLondon, by = "LSOA21CD")
BGC_BlueProximateLSOAList = BGC_BlueProximateLSOAList %>% left_join(BGC_BlueProximate_WithHoles_NoLondon, by = "LSOA21CD")
BGC_BlueProximateLSOAList = BGC_BlueProximateLSOAList %>% left_join(BGC_BlueProximate_NoHoles_NoLondon, by = "LSOA21CD")
BGC_BlueProximateLSOAList = BGC_BlueProximateLSOAList %>% mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
head(BGC_BlueProximateLSOAList)
tail(BGC_BlueProximateLSOAList)
sum(BGC_BlueProximateLSOAList[,2]) # 6218
sum(BGC_BlueProximateLSOAList[,3]) # 7560
sum(BGC_BlueProximateLSOAList[,4]) # 5640
sum(BGC_BlueProximateLSOAList[,5]) # 6972

# And combine
BGC_BlueLSOAs = BGC_BlueLSOAList %>% left_join(BGC_BlueProximateLSOAList, by = "LSOA21CD")
names(BGC_BlueLSOAs)
sum(BGC_BlueLSOAs[,2]) # 2336
sum(BGC_BlueLSOAs[,3]) # 3065
sum(BGC_BlueLSOAs[,4]) # 2126
sum(BGC_BlueLSOAs[,5]) # 2851
sum(BGC_BlueLSOAs[,6]) # 6218
sum(BGC_BlueLSOAs[,7]) # 7560
sum(BGC_BlueLSOAs[,8]) # 5640
sum(BGC_BlueLSOAs[,9]) # 6972

# Save and create test map (for checking using QGIS)
write.csv(BGC_BlueLSOAs,"../20_LSOACoastalLists/BGC_BlueLSOAs_All.csv", row.names = FALSE)

LSOAMap = st_read("../02_InputData/Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BGC_V5_-6970154227154374572.gpkg")
BGCBlue_TestMap = LSOAMap %>% left_join(BGC_BlueLSOAs, by = "LSOA21CD")
st_write(BGCBlue_TestMap, "../BGCBlue_TestMap.gpkg")


# A visual examination makes me think that we need the no gaps and no London version
# Either just Blue OR Blue and Blue Proximate - plus a distance defined set of LSOAs

# And I am thinking BGC blue/blueproximate plus BSC distance

# Whatever, we need to fix any missed gaps (and there shouldn't be islands)

# Just three problems from visal check
head(BGC_BlueLSOAs)
names(BGC_BlueLSOAs)
BGC_BlueLSOAs <- BGC_BlueLSOAs %>% mutate(across(5, ~ if_else(LSOA21CD == "E01026402" & . == 0, 1, .)))
BGC_BlueLSOAs <- BGC_BlueLSOAs %>% mutate(across(9, ~ if_else(LSOA21CD == "E01026250" & . == 0, 1, .)))
BGC_BlueLSOAs <- BGC_BlueLSOAs %>% mutate(across(9, ~ if_else(LSOA21CD == "E01026251" & . == 0, 1, .)))

write.csv(BGC_BlueLSOAs,"../20_LSOACoastalLists/BGC_BlueLSOAs_All.csv", row.names = FALSE)



################################################################################
################################################################################
# And now the same for the BSC Blue set
# And these are the accompanying BSC .csv lists

# Bring together all the BSC lists and check using QGIS

BSC_Blue_WithHoles_WithLondon = read.csv("../20_LSOACoastalLists/BlueLSOAs_BSC_withHoles_withLondon_List.csv")
BSC_Blue_NoHoles_WithLondon = read.csv("../20_LSOACoastalLists/BlueLSOAs_BSC_noHoles_withLondon_List.csv")
BSC_Blue_WithHoles_NoLondon = read.csv("../20_LSOACoastalLists/BlueLSOAs_BSC_withHoles.noLondon_List.csv")
BSC_Blue_NoHoles_NoLondon = read.csv("../20_LSOACoastalLists/BlueLSOAs_BSC_noHoles_noLondon_List.csv")

head(BSC_Blue_WithHoles_WithLondon)
head(BSC_Blue_NoHoles_WithLondon)
head(BSC_Blue_WithHoles_NoLondon)
head(BSC_Blue_NoHoles_NoLondon)

# Remove Wales and remove duplicates
BSC_Blue_WithHoles_WithLondon = BSC_Blue_WithHoles_WithLondon %>% rename("LSOA21CD" = "x") %>% 
  mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(Country)) %>% distinct()
nrow(BSC_Blue_WithHoles_WithLondon)
length(unique(BSC_Blue_WithHoles_WithLondon$LSOA21CD)) # 1824

BSC_Blue_NoHoles_WithLondon = BSC_Blue_NoHoles_WithLondon %>% rename("LSOA21CD" = "x") %>% 
  mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(Country)) %>% distinct()
nrow(BSC_Blue_NoHoles_WithLondon)
length(unique(BSC_Blue_NoHoles_WithLondon$LSOA21CD)) # 2224

BSC_Blue_WithHoles_NoLondon = BSC_Blue_WithHoles_NoLondon %>% rename("LSOA21CD" = "x") %>% 
  mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(Country)) %>% distinct() 
nrow(BSC_Blue_WithHoles_NoLondon)
length(unique(BSC_Blue_WithHoles_NoLondon$LSOA21CD)) # 1639

BSC_Blue_NoHoles_NoLondon = BSC_Blue_NoHoles_NoLondon %>% rename("LSOA21CD" = "x") %>% 
  mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(Country)) %>% distinct()
nrow(BSC_Blue_NoHoles_NoLondon)
length(unique(BSC_Blue_NoHoles_NoLondon$LSOA21CD)) # 2036

# Add data columns (with appropriate names and all =1) attach to list of all 33755 English LSOAs (thereby and collating)

BSC_Blue_WithHoles_WithLondon = BSC_Blue_WithHoles_WithLondon %>% mutate(BSC_Blue_withGaps_withLondon = 1)
BSC_Blue_NoHoles_WithLondon = BSC_Blue_NoHoles_WithLondon %>% mutate(BSC_Blue_noGaps_withLondon = 1)
BSC_Blue_WithHoles_NoLondon = BSC_Blue_WithHoles_NoLondon %>% mutate(BSC_Blue_Gaps_noLondon = 1)
BSC_Blue_NoHoles_NoLondon = BSC_Blue_NoHoles_NoLondon %>% mutate(BSC_Blue_noGaps_NoLondon = 1)

# Create list of all 33755 LSOAs
BSC_BlueLSOAList = read.csv("../20_LSOACoastalLists/BSC_LSOA21_Distance_withLondon_withGaps_1-20.csv")
BSC_BlueLSOAList = BSC_BlueLSOAList %>% select(LSOA21CD) %>% arrange(LSOA21CD)
nrow(BSC_BlueLSOAList)
head(BSC_BlueLSOAList)

# And left_join the BSC blue variants
BSC_BlueLSOAList = BSC_BlueLSOAList %>% left_join(BSC_Blue_WithHoles_WithLondon, by = "LSOA21CD")
BSC_BlueLSOAList = BSC_BlueLSOAList %>% left_join(BSC_Blue_NoHoles_WithLondon, by = "LSOA21CD")
BSC_BlueLSOAList = BSC_BlueLSOAList %>% left_join(BSC_Blue_WithHoles_NoLondon, by = "LSOA21CD")
BSC_BlueLSOAList = BSC_BlueLSOAList %>% left_join(BSC_Blue_NoHoles_NoLondon, by = "LSOA21CD")
BSC_BlueLSOAList = BSC_BlueLSOAList %>% mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
head(BSC_BlueLSOAList)
tail(BSC_BlueLSOAList)
sum(BSC_BlueLSOAList[,2])
sum(BSC_BlueLSOAList[,3])
sum(BSC_BlueLSOAList[,4])
sum(BSC_BlueLSOAList[,5])

# Now the same for BSC BlueProximate
BSC_BlueProximate_WithHoles_WithLondon = read.csv("../20_LSOACoastalLists/BlueProximateLSOAs_BSC_withHoles_withLondon.csv")
BSC_BlueProximate_NoHoles_WithLondon = read.csv("../20_LSOACoastalLists/BlueProximateLSOAs_BSC_NoHoles_withLondon_List.csv")
BSC_BlueProximate_WithHoles_NoLondon = read.csv("../20_LSOACoastalLists/BlueProximateLSOAs_BSC_withHoles_noLondon_List.csv")
BSC_BlueProximate_NoHoles_NoLondon = read.csv("../20_LSOACoastalLists/BlueProximateLSOAs_BSC_noHoles_noLondon_List.csv")

head(BSC_BlueProximate_WithHoles_WithLondon) # X & x
head(BSC_BlueProximate_NoHoles_WithLondon) # X & x
head(BSC_BlueProximate_WithHoles_NoLondon) # x
head(BSC_BlueProximate_NoHoles_NoLondon) # x

# Remove Wales and remove duplicates
BSC_BlueProximate_WithHoles_WithLondon = BSC_BlueProximate_WithHoles_WithLondon %>% rename("LSOA21CD" = "x") %>% 
  mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(X,Country)) %>% distinct()
nrow(BSC_BlueProximate_WithHoles_WithLondon)
length(unique(BSC_BlueProximate_WithHoles_WithLondon$LSOA21CD)) # 4895

BSC_BlueProximate_NoHoles_WithLondon = BSC_BlueProximate_NoHoles_WithLondon %>% rename("LSOA21CD" = "x") %>% 
  mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(X,Country)) %>% distinct()
nrow(BSC_BlueProximate_NoHoles_WithLondon)
length(unique(BSC_BlueProximate_NoHoles_WithLondon$LSOA21CD)) # 5761

BSC_BlueProximate_WithHoles_NoLondon = BSC_BlueProximate_WithHoles_NoLondon %>% rename("LSOA21CD" = "x") %>% 
  mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(Country)) %>% distinct()
nrow(BSC_BlueProximate_WithHoles_NoLondon)
length(unique(BSC_BlueProximate_WithHoles_NoLondon$LSOA21CD)) # 4389

BSC_BlueProximate_NoHoles_NoLondon = BSC_BlueProximate_NoHoles_NoLondon %>% rename("LSOA21CD" = "x") %>% 
  mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(Country)) %>% distinct()
nrow(BSC_BlueProximate_NoHoles_NoLondon)
length(unique(BSC_BlueProximate_NoHoles_NoLondon$LSOA21CD)) # 5245


# Add data columns (with approporiate names and all =1) attach to list of all 33755 English LSOAs (thereby and collating)
BSC_BlueProximate_WithHoles_WithLondon = BSC_BlueProximate_WithHoles_WithLondon %>% mutate(BSC_BlueProximate_withGaps_withLondon = 1)
BSC_BlueProximate_NoHoles_WithLondon = BSC_BlueProximate_NoHoles_WithLondon %>% mutate(BSC_BlueProximate_noGaps_withLondon = 1)
BSC_BlueProximate_WithHoles_NoLondon = BSC_BlueProximate_WithHoles_NoLondon %>% mutate(BSC_BlueProximate_Gaps_noLondon = 1)
BSC_BlueProximate_NoHoles_NoLondon = BSC_BlueProximate_NoHoles_NoLondon %>% mutate(BSC_BlueProximate_noGaps_NoLondon = 1)

# Create list of all 33755 LSOAs
BSC_BlueProximateLSOAList = read.csv("../20_LSOACoastalLists/BSC_LSOA21_Distance_withLondon_withGaps_1-20.csv")
BSC_BlueProximateLSOAList = BSC_BlueProximateLSOAList %>% select(LSOA21CD) %>% arrange(LSOA21CD)
nrow(BSC_BlueProximateLSOAList)
head(BSC_BlueProximateLSOAList)

# And left_join the BSC blue variants
BSC_BlueProximateLSOAList = BSC_BlueProximateLSOAList %>% left_join(BSC_BlueProximate_WithHoles_WithLondon, by = "LSOA21CD")
BSC_BlueProximateLSOAList = BSC_BlueProximateLSOAList %>% left_join(BSC_BlueProximate_NoHoles_WithLondon, by = "LSOA21CD")
BSC_BlueProximateLSOAList = BSC_BlueProximateLSOAList %>% left_join(BSC_BlueProximate_WithHoles_NoLondon, by = "LSOA21CD")
BSC_BlueProximateLSOAList = BSC_BlueProximateLSOAList %>% left_join(BSC_BlueProximate_NoHoles_NoLondon, by = "LSOA21CD")
BSC_BlueProximateLSOAList = BSC_BlueProximateLSOAList %>% mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
head(BSC_BlueProximateLSOAList)
tail(BSC_BlueProximateLSOAList)
sum(BSC_BlueProximateLSOAList[,2]) # 4895
sum(BSC_BlueProximateLSOAList[,3]) # 5761
sum(BSC_BlueProximateLSOAList[,4]) # 4389
sum(BSC_BlueProximateLSOAList[,5]) # 5245

# And combine
BSC_BlueLSOAs = BSC_BlueLSOAList %>% left_join(BSC_BlueProximateLSOAList, by = "LSOA21CD")
names(BSC_BlueLSOAs)
sum(BSC_BlueLSOAs[,2]) # 1824
sum(BSC_BlueLSOAs[,3]) # 2224
sum(BSC_BlueLSOAs[,4]) # 1639
sum(BSC_BlueLSOAs[,5]) # 2036
sum(BSC_BlueLSOAs[,6]) # 4895
sum(BSC_BlueLSOAs[,7]) # 5761
sum(BSC_BlueLSOAs[,8]) # 5640
sum(BSC_BlueLSOAs[,9]) # 525

# Save and create test map (for checking using QGIS)
write.csv(BSC_BlueLSOAs,"../20_LSOACoastalLists/BSC_BlueLSOAs_All.csv", row.names = FALSE)

LSOAMap = st_read("../02_InputData/Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BGC_V5_-6970154227154374572.gpkg")
BSCBlue_TestMap = LSOAMap %>% left_join(BSC_BlueLSOAs, by = "LSOA21CD")
st_write(BSCBlue_TestMap, "../BSCBlue_TestMap.gpkg")


# Once again it is the noGapsNoLondon set that seems best
# So visual check required

# A few 'errors' probably to do with the simplification of the BSC coast
head(BSC_BlueLSOAs)
names(BSC_BlueLSOAs)
BSC_BlueLSOAs <- BSC_BlueLSOAs %>% mutate(across(5, ~ if_else(LSOA21CD == "E01024527" & . == 0, 1, .)))
BSC_BlueLSOAs <- BSC_BlueLSOAs %>% mutate(across(5, ~ if_else(LSOA21CD == "E01024525" & . == 0, 1, .)))
BSC_BlueLSOAs <- BSC_BlueLSOAs %>% mutate(across(5, ~ if_else(LSOA21CD == "E01008764" & . == 0, 1, .)))
BSC_BlueLSOAs <- BSC_BlueLSOAs %>% mutate(across(5, ~ if_else(LSOA21CD == "E01020910" & . == 0, 1, .)))
BSC_BlueLSOAs <- BSC_BlueLSOAs %>% mutate(across(5, ~ if_else(LSOA21CD == "E01031535" & . == 0, 1, .)))
BSC_BlueLSOAs <- BSC_BlueLSOAs %>% mutate(across(5, ~ if_else(LSOA21CD == "E01022762" & . == 0, 1, .)))
BSC_BlueLSOAs <- BSC_BlueLSOAs %>% mutate(across(5, ~ if_else(LSOA21CD == "E01020553" & . == 0, 1, .)))
BSC_BlueLSOAs <- BSC_BlueLSOAs %>% mutate(across(5, ~ if_else(LSOA21CD == "E01018907" & . == 0, 1, .)))
BSC_BlueLSOAs <- BSC_BlueLSOAs %>% mutate(across(5, ~ if_else(LSOA21CD == "E01025088" & . == 0, 1, .)))
BSC_BlueLSOAs <- BSC_BlueLSOAs %>% mutate(across(5, ~ if_else(LSOA21CD == "E01025169" & . == 0, 1, .)))
BSC_BlueLSOAs <- BSC_BlueLSOAs %>% mutate(across(9, ~ if_else(LSOA21CD == "E01025169" & . == 0, 1, .)))
BSC_BlueLSOAs <- BSC_BlueLSOAs %>% mutate(across(5, ~ if_else(LSOA21CD == "E01019353" & . == 0, 1, .)))

# Save
write.csv(BSC_BlueLSOAs,"../20_LSOACoastalLists/BSC_BlueLSOAs_All.csv", row.names = FALSE)


# So now bring it all together #################################################


BSC.Set = read.csv("../20_LSOACoastalLists/Corrected_BSC_LSOA21_Distance_NoLondon_NoGaps_All.csv")
BGC.Set = read.csv("../20_LSOACoastalLists/Corrected_BGC_LSOA21_Distance_NoLondon_NoGaps_All.csv")
BSC_BlueLSOAs = read.csv("../20_LSOACoastalLists/BSC_BlueLSOAs_All.csv")
BGC_BlueLSOAs = read.csv("../20_LSOACoastalLists/BGC_BlueLSOAs_All.csv")


BlueLSOAs_All = BSC.Set %>% left_join(BGC.Set, by = "LSOA21CD") %>% 
  left_join(BSC_BlueLSOAs, by = "LSOA21CD") %>%
  left_join(BGC_BlueLSOAs, by = "LSOA21CD")
names(BlueLSOAs_All)

# Save final collated version
write.csv(BlueLSOAs_All,"../20_LSOACoastalLists/BlueGreenCategorisation.csv", row.names = FALSE)


