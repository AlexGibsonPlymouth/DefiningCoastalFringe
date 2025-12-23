
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



# BGC_LSOA21_Distance_NoLondon_NoGaps <<< This is the type we really want #######

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


# BSC_LSOA21_Distance_NoLondon_NoGaps <<< This is the type we really want #######

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

# OK - before checking out the holes on the BGC maps, let's fix the Blue and BlueProximate lists

BlueLSOAs = read.csv("../20_LSOACoastalLists/BlueLSOAs_BGC_noHoles_noLondon_List.csv")
names(BlueLSOAs)
BlueLSOAs = BlueLSOAs %>% rename("LSOA21CD" = "x") %>% mutate(BlueLSOA = 1) %>% arrange(LSOA21CD)
head(BlueLSOAs)
tail(BlueLSOAs)
nrow(BlueLSOAs) # 2851
# So make into a 0/1 list for the whole country

# Get a fill list of LSOAs 
BlueLSOAList = read.csv("../20_LSOACoastalLists/BSC_LSOA21_Distance_withLondon_withGaps_1-20.csv")
nrow(BlueLSOAList)
BlueLSOAList = BlueLSOAList %>% select(LSOA21CD) %>% arrange(LSOA21CD)
head(BlueLSOAList)
BlueLSOAList = BlueLSOAList %>% left_join(BlueLSOAs, by = "LSOA21CD")
head(BlueLSOAList)
BlueLSOAList = BlueLSOAList %>% mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
head(BlueLSOAList)
sum(BlueLSOAList$BlueLSOA) # 2851



BlueProximateLSOAs = read.csv("../20_LSOACoastalLists/BlueProximateLSOAs_BGC_noHoles_noLondon_List.csv")
nrow(BlueProximateLSOAs) # 7704
head(BlueProximateLSOAs)
length(unique(BlueProximateLSOAs$x)) # 6982

BlueProximateLSOAs = BlueProximateLSOAs %>% rename("LSOA21CD" = "x") %>% mutate(BlueProximateLSOA = 1) %>% arrange(LSOA21CD)
head(BlueProximateLSOAs)
tail(BlueProximateLSOAs) # There are some welsh!
BlueProximateLSOAs = BlueProximateLSOAs %>% mutate(Country = substr(LSOA21CD,1,1)) %>% filter(Country == "E") %>% select(-c(Country))
head(BlueProximateLSOAs)
tail(BlueProximateLSOAs) # No welsh now!
nrow(BlueProximateLSOAs) # 7694
sum(BlueProximateLSOAs$BlueProximateLSOA) # 7694
length(unique(BlueProximateLSOAs$LSOA21CD)) # 6972

BlueProximateLSOAs = BlueProximateLSOAs %>% distinct()
nrow(BlueProximateLSOAs) # 6972
sum(BlueProximateLSOAs$BlueProximateLSOA) # 6972
length(unique(BlueProximateLSOAs$LSOA21CD)) # 6972


# So make into a 0/1 list for the whole country
# Start by getting a list of all 33755 LSOAs
BlueProximateLSOAList = read.csv("../20_LSOACoastalLists/BSC_LSOA21_Distance_withLondon_withGaps_1-20.csv")
nrow(BlueProximateLSOAList)
BlueProximateLSOAList = BlueProximateLSOAList %>% select(LSOA21CD) %>% arrange(LSOA21CD)
head(BlueProximateLSOAList)
BlueProximateLSOAList = BlueProximateLSOAList %>% left_join(BlueProximateLSOAs, by = "LSOA21CD")
head(BlueProximateLSOAList)
BlueProximateLSOAList = BlueProximateLSOAList %>% mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
head(BlueProximateLSOAList)
sum(BlueProximateLSOAList$BlueProximateLSOA) # 6972



AllBlueLSOAs = BlueLSOAList %>% left_join(BlueProximateLSOAList, by = "LSOA21CD")
head(AllBlueLSOAs)
tail(AllBlueLSOAs)
sum(AllBlueLSOAs$BlueLSOA) # 2851
sum(AllBlueLSOAs$BlueProximateLSOA) # 6972


write.csv(AllBlueLSOAs,"../20_LSOACoastalLists/BlueLSOAs_All.csv", row.names = FALSE)



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


# Now check that the Blue and BlueProximate maps look OK #######################

AllBlueLSOAs = read.csv("../20_LSOACoastalLists/BlueLSOAs_All.csv")
head(AllBlueLSOAs)
BlueTestMap = LSOAMap %>% left_join(AllBlueLSOAs, by = "LSOA21CD")
head(BlueTestMap)
st_write(BlueTestMap, "../BlueTestMap.gpkg")





# So now bring it all together #################################################


mapview(CorrectedBSCTestMap)





BGC.Set %>% filter(LSOA21CD == "E01013250")
BGC.Set <- BGC.Set %>% mutate(across(9, ~ if_else(LSOA21CD == "E01013250" & . == 0, 1, .)))












TestMap = LSOAMap %>% left_join(BGC.Set, by = "LSOA21CD")
mapview(TestMap, zcol = "BGC_10000" , col.regions = c("green","red"), color = NA, lwd = 0)


WorkingList = unique(c(BlueLSOAs$x,BGC.Set[BGC.Set$BGC_20000==1,1]))
length(WorkingList)
WorkingMap = LSOAMap %>% dplyr::filter(LSOA21CD %in% WorkingList)
mapview(WorkingMap) + mapview(LSOAMap, col.region = "red")

WorkingList = unique(c(BlueProximateLSOAs$x,BGC.Set[BGC.Set$BGC_20000==1,1]))
length(WorkingList)
WorkingMap = LSOAMap %>% dplyr::filter(LSOA21CD %in% WorkingList)
mapview(WorkingMap)

WorkingList = BlueProximateLSOAs$x
length(WorkingList)
WorkingMap = LSOAMap %>% dplyr::filter(LSOA21CD %in% WorkingList)
mapview(WorkingMap)


# Now we need to fill holes - this takes such a long time - and there are so few
# holes, I will do this manually!!! (Ugh - but has to be done!)

BGC.Corrected.Set = BGC.Set

TestMap = LSOAMap %>% left_join(BGC.Corrected.Set, by = "LSOA21CD") %>% filter(BGC_4000 == 1)
names(TestMap)
mapview(TestMap, zcol = "BGC_4000" , col.region = "red")


mapview(TestMap, zcol = "BGC_500" , col.regions = c("white","red"), color = NA, lwd = 0)


ColNames = names(BGC.Corrected.Set)
Loops = length(ColNames)
LSOANoHoles.df = BGC.Corrected.Set %>% select(LSOA21CD)
area_thresh <- units::set_units(500000, km^2)   # This is somewhat arbitrary

for (i in 2:Loops){
  i=20
  print(paste0("Sys.time = ",Sys.time(),"  Dealing with i = ",i," of ",Loops," which is ",ColNames[i]))
  # Extract the current distance (i.e. for loop i)
  Temp = BGC.Corrected.Set %>% dplyr::select(LSOA21CD,ColNames[i])
  names(Temp)
  # Extract the subset of the full LSOAset where coastal - cleaning so no distance column name
  names(LSOAMap)
  LSOASet = LSOAMap %>% dplyr::left_join(Temp, by = "LSOA21CD") %>%
    dplyr::filter(!!sym(ColNames[i]) == 1) %>% dplyr::select(-c(!!sym(ColNames[i])))

  # Dissolve the LSOA sf file so we get a single polygon
  Working_Dissolved_LSOAs = LSOASet %>% dplyr::group_by() %>% dplyr::summarise()
  mapview(Working_Dissolved_LSOAs)
  
  # Fill any holes that are within the Working_Dissolved_LSOAs
  Filled_Working_Dissolved_LSOAs = fill_holes(Working_Dissolved_LSOAs, threshold = area_thresh)
  mapview(Filled_Working_Dissolved_LSOAs)
  
  # Now find all LSOAs that are within the dissolved LSOA polygon() with holes filled
  WithinLSOAs = LSOAMap[Filled_Working_Dissolved_LSOAs, op = st_within]
  mapview(WithinLSOAs)
  
  # Extract unique (distinct) LSOAs and add a column to the dataframe with name 
  # from original distance column name and all values set to 1
  FinalLSOAs = WithinLSOAs %>% dplyr::distinct(LSOA21CD) %>% mutate(!!sym(ColNames[i]) := 1)
  print(paste0("Print done region j = ",j," of 9 having now found a total of ",nrow(FinalLSOAs)," LSOAs"))
} else {
  print(paste0("Skipping region j = ",j," as no LSOAs in buffer - still found a total of ",nrow(FinalLSOAs)," LSOAs"))
}




} # end of loop



Blue_LSOA21_NoLondonNoGaps = list.files("../20_LSOACoastalLists")[36:39]
