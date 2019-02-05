### Code for combining all datasets ###

# (= LEDA, TRY, TTT, PLANTS, BioFlor, Eflora_Cal, Ecoflora)

# by Kata 4.2.2019

rm(list=ls())
setwd("T:/vm/inh/botanisk/Bruker/FunctionalBiogeography/Final gaps")
getwd()

library(dplyr)
library(reshape2)
library(withr)
library(labeling)
library(ggplot2)



### 1. Clean and produce comparable long formats of all datasets ------------


## * 1.1. TRAITS dataset -----------------------------------------------------

# This is data from Traits-script on chosen traits (LEDA, PLANTS, BioFlor, Eflora_Cal, Ecoflora).

# Read data.

TRAITS<-read.csv(file = "TRAITSavaiable&lacking.csv",
                 header = TRUE,
                 dec = ".")
str(TRAITS)


# Get rid of the original name column and rename synonyme column as Species.

TRAITS$original_name<-NULL
colnames(TRAITS)[colnames(TRAITS)=="synonym_tr8"] <- "Species"   

# Recode factorial colums to numerical so that we can melt the table later.

unique(TRAITS$li_span)
TRAITS$li_span<-recode(TRAITS$li_span, 
                       'p (pluriennial-pollakanthic)'=0, 
                       'a (annual) - b (biennial) - h (pluriennial-hapaxanthic) - p (pluriennial-pollakanthic)'=1, 
                       'a (annual)'=2, 
                       'a (annual) - p (pluriennial-pollakanthic)'=3,
                       'b (biennial) - h (pluriennial-hapaxanthic'=4, 
                       'b (biennial) - p (pluriennial-pollakanthic)'=5, 
                       'b (biennial)'=6, 
                       'a (annual) - b (biennial)'=7,
                       'a (annual) - b (biennial) - h (pluriennial-hapaxanthic)'=8, 
                       'p (pluriennial-pollakanthic) - p (pluriennial-pollakanthic)'=9, 
                       'a (annual) - b (biennial) - p (pluriennial-pollakanthic)'=10, 
                       'b (biennial) - h (pluriennial-hapaxanthic) - p (pluriennial-pollakanthic)'=11,
)
TRAITS$li_span<-as.numeric(as.character(TRAITS$li_span))

TRAITS$li_span<-as.numeric(as.character(TRAITS$li_span))
TRAITS$h_max<-as.numeric(as.character(TRAITS$h_max))
TRAITS$h_min<-as.numeric(as.character(TRAITS$h_min))
TRAITS$le_area<-as.numeric(as.character(TRAITS$le_area))
TRAITS$le_long<-as.numeric(as.character(TRAITS$le_long))
TRAITS$seed_wght<-as.numeric(as.character(TRAITS$seed_wght))

unique(TRAITS$life_span)
checktable_life_span<-table(TRAITS$life_span)
# View(checktable_life_span)
# Takes time to recode this... for gap finding purposes, just change
# all available values to 1 and retain NAs.
TRAITS <- TRAITS %>%
  mutate(life_span = ifelse(life_span == "NA",NA,1))

unique(TRAITS$C.N.Ratio)
TRAITS$C.N.Ratio<-recode(TRAITS$C.N.Ratio, 
                         'Low'=1, 
                         'Medium'=2, 
                         'High'=3)
TRAITS$C.N.Ratio<-as.numeric(as.character(TRAITS$C.N.Ratio))
colnames(TRAITS)[colnames(TRAITS)=="C.N.Ratio"] <- "C.N.Ratio_3levels"   

unique(TRAITS$Lifespan)
TRAITS$Lifespan<-recode(TRAITS$Lifespan, 
                        'Short'=1, 
                        'Moderate'=2, 
                        'Long'=3)
TRAITS$Lifespan<-as.numeric(as.character(TRAITS$Lifespan))
colnames(TRAITS)[colnames(TRAITS)=="Lifespan"] <- "Lifespan_3levels" 

unique(TRAITS$seed_pro_cal)
TRAITS$seed_pro_cal<-as.numeric(as.character(TRAITS$seed_pro_cal))

unique(TRAITS$leaf_N_area)
unique(TRAITS$leaf_N_mass)
unique(TRAITS$max_height_cal)
unique(TRAITS$sla_cal)
# Apparently Eflora_cal does not include any data on these traits for
# our species list, so can as well get rid of them.
TRAITS$leaf_N_area<-NULL
TRAITS$leaf_N_mass<-NULL
TRAITS$max_height_cal<-NULL
TRAITS$sla_cal<-NULL

# Now all traits are numerical. 
# Next: melt.

TRAITS<-melt(TRAITS, by="Species")
str(TRAITS)
colnames(TRAITS)[colnames(TRAITS)=="variable"] <- "Trait_orig"   

# Give traits more informative names (from databases)
unique(TRAITS$Trait_orig)
TRAITS$Trait<-NA
TRAITS$Trait[TRAITS$Trait_orig=="h_max"] <- "Maximum height"
TRAITS$Trait[TRAITS$Trait_orig=="h_min"] <- "Minimum height"
TRAITS$Trait[TRAITS$Trait_orig=="canopy_height"] <- "Canopy height"
TRAITS$Trait[TRAITS$Trait_orig=="Height..Mature"] <- "Mature height"
TRAITS$Trait[TRAITS$Trait_orig=="le_area"] <- "Leaf area"
TRAITS$Trait[TRAITS$Trait_orig=="le_long"] <- "Leaf longevity"
TRAITS$Trait[TRAITS$Trait_orig=="leaf_dmc"] <- "LDMC"
TRAITS$Trait[TRAITS$Trait_orig=="leaf_mass"] <- "Leaf mass"
TRAITS$Trait[TRAITS$Trait_orig=="leaf_size"] <- "Leaf areaLEDA"
TRAITS$Trait[TRAITS$Trait_orig=="seed_wght"] <- "Seed weight mean"
TRAITS$Trait[TRAITS$Trait_orig=="seed_number_per_shoot"] <- "Seed number per shoot"
TRAITS$Trait[TRAITS$Trait_orig=="seed_mass"] <- "Seedmass1"
TRAITS$Trait[TRAITS$Trait_orig=="seed_mas_cal"] <- "Seedmass2" 
TRAITS$Trait[TRAITS$Trait_orig=="seed_pro_cal"] <- "Annual seed production per plant" 
TRAITS$Trait[TRAITS$Trait_orig=="li_span"] <- "Lifespan1"
TRAITS$Trait[TRAITS$Trait_orig=="life_span"] <- "Lifespan2" 
TRAITS$Trait[TRAITS$Trait_orig=="Lifespan_3levels"] <- "Lifespan_3levels" 
TRAITS$Trait[TRAITS$Trait_orig=="C.N.Ratio_3levels"] <- "CNRato_3lev"
unique(TRAITS$Trait)

# Make a column for trait type
TRAITS$Trait_type<-NA
TRAITS$Trait_type[TRAITS$Trait_orig=="h_max"] <- "Height"
TRAITS$Trait_type[TRAITS$Trait_orig=="h_min"] <- "Height"
TRAITS$Trait_type[TRAITS$Trait_orig=="canopy_height"] <- "Height"
TRAITS$Trait_type[TRAITS$Trait_orig=="Height..Mature"] <- "Height"
TRAITS$Trait_type[TRAITS$Trait_orig=="le_area"] <- "Leaf"
TRAITS$Trait_type[TRAITS$Trait_orig=="le_long"] <- "Longevity"
TRAITS$Trait_type[TRAITS$Trait_orig=="leaf_dmc"] <- "Leaf"
TRAITS$Trait_type[TRAITS$Trait_orig=="leaf_mass"] <- "Leaf"
TRAITS$Trait_type[TRAITS$Trait_orig=="leaf_size"] <- "Leaf"
TRAITS$Trait_type[TRAITS$Trait_orig=="seed_wght"] <- "Seed"
TRAITS$Trait_type[TRAITS$Trait_orig=="seed_number_per_shoot"] <- "Seed"
TRAITS$Trait_type[TRAITS$Trait_orig=="seed_mass"] <- "Seed"
TRAITS$Trait_type[TRAITS$Trait_orig=="seed_mas_cal"] <- "Seed" 
TRAITS$Trait_type[TRAITS$Trait_orig=="seed_pro_cal"] <- "Seed" 
TRAITS$Trait_type[TRAITS$Trait_orig=="li_span"] <- "Longevity"
TRAITS$Trait_type[TRAITS$Trait_orig=="life_span"] <- "Longevity" 
TRAITS$Trait_type[TRAITS$Trait_orig=="Lifespan_3levels"] <- "Longevity" 
TRAITS$Trait_type[TRAITS$Trait_orig=="C.N.Ratio_3levels"] <- "Chemical"
unique(TRAITS$Trait_type)

# Make a column for Database
TRAITS$Database<-NA
TRAITS$Database[TRAITS$Trait_orig=="h_max"] <- "Ecoflora"
TRAITS$Database[TRAITS$Trait_orig=="h_min"] <- "Ecoflora"
TRAITS$Database[TRAITS$Trait_orig=="canopy_height"] <- "LEDA"
TRAITS$Database[TRAITS$Trait_orig=="Height..Mature"] <- "PLANTS"
TRAITS$Database[TRAITS$Trait_orig=="le_area"] <- "Ecoflora"
TRAITS$Database[TRAITS$Trait_orig=="le_long"] <- "Ecoflora"
TRAITS$Database[TRAITS$Trait_orig=="leaf_dmc"] <- "LEDA"
TRAITS$Database[TRAITS$Trait_orig=="leaf_mass"] <- "LEDA"
TRAITS$Database[TRAITS$Trait_orig=="leaf_size"] <- "LEDA"
TRAITS$Database[TRAITS$Trait_orig=="seed_wght"] <- "Ecoflora"
TRAITS$Database[TRAITS$Trait_orig=="seed_number_per_shoot"] <- "LEDA"
TRAITS$Database[TRAITS$Trait_orig=="seed_mass"] <- "LEDA"
TRAITS$Database[TRAITS$Trait_orig=="seed_mas_cal"] <- "Eflora_cal" 
TRAITS$Database[TRAITS$Trait_orig=="seed_pro_cal"] <- "Eflora_cal" 
TRAITS$Database[TRAITS$Trait_orig=="li_span"] <- "BiolFlor"
TRAITS$Database[TRAITS$Trait_orig=="life_span"] <- "LEDA" 
TRAITS$Database[TRAITS$Trait_orig=="Lifespan_3levels"] <- "PLANTS" 
TRAITS$Database[TRAITS$Trait_orig=="C.N.Ratio_3levels"] <- "PLANTS"
unique(TRAITS$Database)

# We hve no information about units for this dataset, 
# so for now, let's just create NA-column for Units
TRAITS$Units<-NA

TRAITS$Units[TRAITS$Trait=="Leaf areaLEDA"]<-"mm2"

## * 1.2. TTT dataset ---------------------------------------------------------

# This is TTT dataset cleaned by Kata: all traits & species


# read data.

TTT<-read.csv(file = "TTT_cleaned_dataset_v1.csv",
              header = TRUE,
              dec = ".")
str(TTT)




# This is in long format already, so just need some reduction & renaming

colnames(TTT)[colnames(TTT)=="AccSpeciesName"] <- "Species"   
colnames(TTT)[colnames(TTT)=="Trait"] <- "Trait_orig"   
TTT<-data.frame(TTT$Species,TTT$Trait_orig,TTT$Value, TTT$Units)
colnames(TTT)[colnames(TTT)=="TTT.Species"] <- "Species"
colnames(TTT)[colnames(TTT)=="TTT.Value"] <- "Value"
colnames(TTT)[colnames(TTT)=="TTT.Trait_orig"] <- "Trait_orig"
colnames(TTT)[colnames(TTT)=="TTT.Units"] <- "Units"

# We need to remove species that are not present in Norway, but
# leave in the ones that are on our species list but not in TTT.
# Define common species:
specieslist<-read.table("species_names.txt")
common <- intersect(TTT$Species, specieslist$V1)  
# subset these species rows from TTT:
TTT<-filter(TTT, Species %in% common)

unique(TTT$Trait_orig)
# 18 traits
# Now we only want to retain a same trait names as in TRAITS dataset if we can assume
# that these are completely comparable, so rename traits on this basis.

TTT$Trait<-NA
TTT$Trait[TTT$Trait_orig=="Leaf area"] <- "Leaf area" # can assume that this is comparable to TRAITS (need to check units though)
TTT$Trait[TTT$Trait_orig=="Leaf area per leaf dry mass (specific leaf area, SLA)"] <- "SLA"
TTT$Trait[TTT$Trait_orig=="Leaf carbon (C) content per leaf dry mass"] <- "Leaf C per leaf dry mass"
TTT$Trait[TTT$Trait_orig=="Leaf carbon (C) isotope discrimination (delta 13C)"] <- "Cisotopedisc"
TTT$Trait[TTT$Trait_orig=="Leaf carbon/nitrogen (C/N) ratio"] <- "LeafCNratio_num" # not same as in TRAITS
TTT$Trait[TTT$Trait_orig=="Leaf dry mass"] <- "Leaf dry mass" # Is TRAITS Leaf mass dry or fresh? Might be able to combine...
TTT$Trait[TTT$Trait_orig=="Leaf dry mass per leaf fresh mass (Leaf dry matter content, LDMC)"] <- "LDMC" # comparable to TRAITS for sure
TTT$Trait[TTT$Trait_orig=="Leaf fresh mass"] <- "Leaf fresh mass" # Is TRAITS Leaf mass dry or fresh? Might be able to combine...
TTT$Trait[TTT$Trait_orig=="Leaf nitrogen (N) content per leaf dry mass"] <- "Leaf N per leaf dry mass"
TTT$Trait[TTT$Trait_orig=="Leaf nitrogen (N) isotope signature (delta 15N)"] <- "Nisotopesign"
TTT$Trait[TTT$Trait_orig=="Leaf nitrogen/phosphorus (N/P) ratio"] <- "LeafNPRatio"
TTT$Trait[TTT$Trait_orig=="Leaf phosphorus (P) content per leaf dry mass"] <- "Leaf P per leaf dry mass"
TTT$Trait[TTT$Trait_orig=="Rooting depth"] <- "Rooting depth"
TTT$Trait[TTT$Trait_orig=="Stem diameter"] <- "Stem diameter"
TTT$Trait[TTT$Trait_orig=="Stem dry mass per stem fresh volume (stem specific density, SSD)"] <- "Stem fresh per stem dry"
TTT$Trait[TTT$Trait_orig=="Seed dry mass"] <- "Seed dry mass" # these might be comparable to TRAITS seed values
TTT$Trait[TTT$Trait_orig=="Plant height, reproductive"] <- "Reproductive height"
TTT$Trait[TTT$Trait_orig=="Plant height, vegetative"] <- "Vegetative height"
unique(TTT$Trait)                   

# And create Trait type column.
TTT$Trait_type<-NA
TTT$Trait_type[TTT$Trait_orig=="Leaf area"] <- "Leaf"
TTT$Trait_type[TTT$Trait_orig=="Leaf area per leaf dry mass (specific leaf area, SLA)"] <- "Leaf"
TTT$Trait_type[TTT$Trait_orig=="Leaf carbon (C) content per leaf dry mass"] <- "Chemical"
TTT$Trait_type[TTT$Trait_orig=="Leaf carbon (C) isotope discrimination (delta 13C)"] <- "Chemical"
TTT$Trait_type[TTT$Trait_orig=="Leaf carbon/nitrogen (C/N) ratio"] <- "Chemical"
TTT$Trait_type[TTT$Trait_orig=="Leaf dry mass"] <- "Leaf"
TTT$Trait_type[TTT$Trait_orig=="Leaf dry mass per leaf fresh mass (Leaf dry matter content, LDMC)"] <- "Leaf"
TTT$Trait_type[TTT$Trait_orig=="Leaf fresh mass"] <- "Leaf" 
TTT$Trait_type[TTT$Trait_orig=="Leaf nitrogen (N) content per leaf dry mass"] <- "Chemical"
TTT$Trait_type[TTT$Trait_orig=="Leaf nitrogen (N) isotope signature (delta 15N)"] <- "Chemical"
TTT$Trait_type[TTT$Trait_orig=="Leaf nitrogen/phosphorus (N/P) ratio"] <- "Chemical"
TTT$Trait_type[TTT$Trait_orig=="Leaf phosphorus (P) content per leaf dry mass"] <- "Chemical"
TTT$Trait_type[TTT$Trait_orig=="Rooting depth"] <- "Rooting"
TTT$Trait_type[TTT$Trait_orig=="Stem diameter"] <- "Stem"
TTT$Trait_type[TTT$Trait_orig=="Stem dry mass per stem fresh volume (stem specific density, SSD)"] <- "Stem"
TTT$Trait_type[TTT$Trait_orig=="Seed dry mass"] <- "Seed" 
TTT$Trait_type[TTT$Trait_orig=="Plant height, reproductive"] <- "Height"
TTT$Trait_type[TTT$Trait_orig=="Plant height, vegetative"] <- "Height"
unique(TTT$Trait_type)

# And create Database column.
TTT$Database<-"TTT"


## * 1.3. TRY dataset --------------------------------------------------------

# This is the txt file 4659 Tanja downloaded.

# Read data.

TRY1<-read.csv(file = "TRY1.csv",
               header = TRUE,
               dec = ".")
TRY2<-read.csv(file = "TRY2.csv",
               header = TRUE,
               dec = ".")
TRY<-rbind(TRY1, TRY2)
str(TRY)


# Reduce & rename
TRY<-data.frame(TRY$AccSpeciesName,TRY$TraitName,TRY$OrigValueStr,TRY$Dataset, TRY$OrigUnitStr)
colnames(TRY)[colnames(TRY)=="TRY.AccSpeciesName"] <- "Species"
colnames(TRY)[colnames(TRY)=="TRY.TraitName"] <- "Trait_orig"
colnames(TRY)[colnames(TRY)=="TRY.OrigValueStr"] <- "Value"
colnames(TRY)[colnames(TRY)=="TRY.Dataset"] <- "Database"
colnames(TRY)[colnames(TRY)=="TRY.OrigUnitStr"] <- "Units"
str(TRY)

# Get rid of NA Trait_orig rows, this isn't traits data but metadata.
TRY<-TRY[!is.na(TRY$Trait_orig),]
unique(TRY$Trait_orig)
# View(TRY)

# Many traits factorial here, recode these.

# This is actually a huge mess, as some of 
# the numerical traits have some puching errors as
# factorial... And, even worse: different method is used under
# same trait names between databases...

# Need to go throught all 33 traits to see what type of values they get, and
# give better names to traits and correct  pucnhing errors.

# [1] Crown (canopy) height (base to top)
x<-subset(TRY, TRY$Trait_orig=="Crown (canopy) height (base to top)")
unique(x$Value)
# View(x)
# OK, only numerical values

# [2] Leaf area
x<-subset(TRY, TRY$Trait_orig=="Leaf area")
unique(x$Value)
# View(x)          
# OK, only numerical values

# [3] Leaf area (in case of compond leaves: leaflet, petiole and rachis excluded)
x<-subset(TRY, TRY$Trait_orig=="Leaf area (in case of compond leaves: leaflet, petiole and rachis excluded)")
unique(x$Value)
# View(x)          
# OK, only numerical values (one na)

# [4] Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole and rachis are in- or excluded)
x<-subset(TRY, TRY$Trait_orig=="Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole and rachis are in- or excluded)")
unique(x$Value)
# View(x)          
# Here we have both absolute numerical values
# as well as some classes.
unique(x$Database) 
# Apparently it is Ecological Flora of the British Isles that has this class system.
y<-subset(x, !x$Database=="Ecological Flora of the British Isles")
# View(y)
# But it looks like chinese database has some meso/micro/nanoplyll classification
# I think we sould get rid of the chinese data (small amount of data and not necesarily so 
# relevant for our area):
TRY<-subset(TRY, !(TRY$Trait_orig=="Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole and rachis are in- or excluded)" & TRY$Database=="Chinese Traits"))
# But recode British data: 
y<-subset(x, x$Database=="Ecological Flora of the British Isles")
unique(y$Value)
TRY$Value[TRY$Trait_orig=="Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole and rachis are in- or excluded)" & TRY$Value=="<0.1"] <- 1
TRY$Value[TRY$Trait_orig=="Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole and rachis are in- or excluded)" & TRY$Value=="0.1-1"] <- 2
TRY$Value[TRY$Trait_orig=="Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole and rachis are in- or excluded)" & TRY$Value=="01-Oct"] <- 3
TRY$Value[TRY$Trait_orig=="Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole and rachis are in- or excluded)" & TRY$Value=="10-100"] <- 4
TRY$Value[TRY$Trait_orig=="Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole and rachis are in- or excluded)" & TRY$Value=="100-1000"] <- 5
TRY$Value[TRY$Trait_orig=="Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole and rachis are in- or excluded)" & TRY$Value==">1000"] <- 6
# and rename this recoded trait to distinguish it from continuos values:
TRYforrenaming<-TRY[TRY$Trait_orig=="Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole and rachis are in- or excluded)" & TRY$Database=="Ecological Flora of the British Isles",]
TRYforrenaming$Trait_orig<-"Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole and rachis are in- or excluded)_BRIT6CLASSRECODED"
TRY<-subset(TRY, !(TRY$Database=="Ecological Flora of the British Isles" & TRY$Trait_orig=="Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole and rachis are in- or excluded)"))
TRY<-rbind(TRY, TRYforrenaming)

# [5] Leaf area (in case of compound leaves: leaf, petiole excluded)
x<-subset(TRY, TRY$Trait_orig=="Leaf area (in case of compound leaves: leaf, petiole excluded)")
unique(x$Value)
# View(x)
# OK, only numerical       

# [6] Leaf area (in case of compound leaves: leaflet, petiole and rachis included)
x<-subset(TRY, TRY$Trait_orig=="Leaf area (in case of compound leaves: leaflet, petiole and rachis included)")
unique(x$Value)
# x$Value <-as.numeric(as.character(x$Value))
# View(x)
# OK, only numerical

# [7] Leaf area (in case of compund leaves: leaf, petiole included)
x<-subset(TRY, TRY$Trait_orig=="Leaf area (in case of compund leaves: leaf, petiole included)")
x$Value <-as.numeric(as.character(x$Value))
# OK, only numerical

# [8] Leaf area (in case of compund leaves: leaf, undefined if petiole in- or excluded)
x<-subset(TRY, TRY$Trait_orig=="Leaf area (in case of compund leaves: leaf, undefined if petiole in- or excluded)")
# x$Value <-as.numeric(as.character(x$Value))
# View(x)                                          
# BROT Plant Trait Database has some unuseful class values here, get rid if these:
TRY<-subset(TRY, !(TRY$Trait_orig=="Leaf area (in case of compund leaves: leaf, undefined if petiole in- or excluded)" & TRY$Database=="BROT Plant Trait Database"))

# [9] Leaf area (in case of compund leaves: leaflet, undefined if petiole and rachis are in- or excluded)  
x<-subset(TRY, TRY$Trait_orig=="Leaf area (in case of compund leaves: leaflet, undefined if petiole and rachis are in- or excluded)")
# x$Value <-as.numeric(as.character(x$Value)
# OK, only numerical values

# [10] Leaf area per leaf dry mass (SLA or 1/LMA): petiole and rachis excluded 
x<-subset(TRY, TRY$Trait_orig=="Leaf area per leaf dry mass (SLA or 1/LMA): petiole and rachis excluded")
# x$Value <-as.numeric(as.character(x$Value)
# OK, only numerical values

# [11] Leaf area per leaf dry mass (SLA or 1/LMA): petiole and rachis included  
x<-subset(TRY, TRY$Trait_orig=="Leaf area per leaf dry mass (SLA or 1/LMA): petiole and rachis included")
# x$Value <-as.numeric(as.character(x$Value)
# OK, only numerical values

# [12] Leaf area per leaf dry mass (SLA or 1/LMA): undefined if petiole and rachis are in- or excluded                         
x<-subset(TRY, TRY$Trait_orig=="Leaf area per leaf dry mass (SLA or 1/LMA): undefined if petiole and rachis are in- or excluded")
# x$Value <-as.numeric(as.character(x$Value)
# OK, only numerical values

# [13] Leaf area per leaf dry mass (specific leaf area, SLA)                                                                   
x<-subset(TRY, TRY$Trait_orig=="Leaf area per leaf dry mass (specific leaf area, SLA)")
# x$Value <-as.numeric(as.character(x$Value)
# OK, only numerical values

# [14] Leaf area per leaf fresh mass (specific leaf area (SLA) based on leaf fresh mass)                                       
x<-subset(TRY, TRY$Trait_orig=="Leaf area per leaf fresh mass (specific leaf area (SLA) based on leaf fresh mass)")
# x$Value <-as.numeric(as.character(x$Value)
# OK, only numerical values

# [15] Leaf carbon/nitrogen (C/N) ratio                                                                                        
x<-subset(TRY, TRY$Trait_orig=="Leaf carbon/nitrogen (C/N) ratio")
# x$Value <-as.numeric(as.character(x$Value)
# OK, only numerical values

# [16] Leaf dry mass
x<-subset(TRY, TRY$Trait_orig=="Leaf dry mass")
# x$Value <-as.numeric(as.character(x$Value)
# OK, only numerical values

# [17] Leaf fresh mass                                                                                                         
x<-subset(TRY, TRY$Trait_orig=="Leaf fresh mass")
# x$Value <-as.numeric(as.character(x$Value)
# OK, only numerical values

# [18] Leaf length                                                                                                        
x<-subset(TRY, TRY$Trait_orig=="Leaf length")
# x$Value <-as.numeric(as.character(x$Value)
# OK, only numerical values

# [19] Leaf lifespan (longevity)
x<-subset(TRY, TRY$Trait_orig=="Leaf lifespan (longevity)")
x$Value <-as.numeric(as.character(x$Value)
# OK, only numerical values
                     
# [20] Leaf nitrogen (N) content per leaf area                                                                                 
x<-subset(TRY, TRY$Trait_orig=="Leaf nitrogen (N) content per leaf area")
x$Value <-as.numeric(as.character(x$Value)
# OK, only numerical values
                                          
# [21] Leaf nitrogen (N) content per leaf dry mass
x<-subset(TRY, TRY$Trait_orig=="Leaf nitrogen (N) content per leaf dry mass")
x$Value <-as.numeric(as.character(x$Value)
# OK, only numeric
                                                               
# [22] Leaf volume    
x<-subset(TRY, TRY$Trait_orig=="Leaf volume")
x$Value <-as.numeric(as.character(x$Value)
# OK, only numeric
                                                                                    
# [23] Leaf width
x<-subset(TRY, TRY$Trait_orig=="Leaf width")
x$Value <-as.numeric(as.character(x$Value)
                                                                                                         # OK, only numeric
                                                                                                         
                                                                                                         # [24] Plant carbon/nitrogen (C/N) ratio
                                                                                                         x<-subset(TRY, TRY$Trait_orig=="Plant carbon/nitrogen (C/N) ratio")
                                                                                                         x$Value<-as.numeric(as.character(x$Value))
                                                                                                         # OK, only numerical
                                                                                                         
                                                                                                         # [25] Plant height generative 
                                                                                                         x<-subset(TRY, TRY$Trait_orig=="Plant height generative")
                                                                                                         x$Value<-as.numeric(as.character(x$Value))
                                                                                                         # View(x)
                                                                                                         # this is actually really just numerical values, R just gets
                                                                                                         # confused because some NAs are coded as nas...
                                                                                                         TRY$Value[TRY$Value=="na"] <- NA
                                                                                                         
                                                                                                         # [26] Plant height vegetative 
                                                                                                         x<-subset(TRY, TRY$Trait_orig=="Plant height vegetative")
                                                                                                         x$Value<-as.numeric(as.character(x$Value))
                                                                                                         # View(x)
                                                                                                         # Here we have one "." value which causes issues      
                                                                                                         TRY$Value[TRY$Value=="." & TRY$Trait_orig=="Plant height vegetative"] <- NA
                                                                                                         
                                                                                                         # [27] Plant lifespan (longevity)  
                                                                                                         x<-subset(TRY, TRY$Trait_orig=="Plant lifespan (longevity)")
                                                                                                         # x$Value<-as.numeric(as.character(x$Value))
                                                                                                         # View(x)
                                                                                                         unique(x$Database)                                                                                                             
                                                                                                         # This is crazy mess... Probably wont be used, easier to determine ourselves rather than
                                                                                                         # try to make sense out of this...
                                                                                                         # Let's just exclude it for now
                                                                                                         TRY<-TRY[!(TRY$Trait_orig=="Plant lifespan (longevity)"),]
                                                                                                         # CHANGE THIS IF WE WNAT TO HAVW THIS IN EVENTUALLY
                                                                                                         
                                                                                                         # [28] Seed dry mass                                                                                              
                                                                                                         x<-subset(TRY, TRY$Trait_orig=="Seed dry mass")
                                                                                                         x$Value<-as.numeric(as.character(x$Value))
                                                                                                         # View(x)                                                                                    
                                                                                                         # there are simply some NAs that R has interpreted as somethin ekse that NA, so it's OK
                                                                                                         # when these get to NAs when we do the final conersion to numerical
                                                                                                         
                                                                                                         # [29] Seed number per flower                                                                                              
                                                                                                         x<-subset(TRY, TRY$Trait_orig=="Seed number per flower")
                                                                                                         # x$Value<-as.numeric(as.character(x$Value))
                                                                                                         # View(x) 
                                                                                                         unique(x$Database)
                                                                                                         # this is all from  Ecological Flora of the British Isles!!!
                                                                                                         # But different systems within this: some actual counts, some just as classes
                                                                                                         str(x)
                                                                                                         # I won't start recoding this now, but can be done later
                                                                                                         TRY<-TRY[!(TRY$Trait_orig=="Seed number per flower"),]
                                                                                                         
                                                                                                         # [30] Seed number per inflorescence (total, fertile, infertile)                                                                                              
                                                                                                         x<-subset(TRY, TRY$Trait_orig=="Seed number per inflorescence (total, fertile, infertile)")
                                                                                                         x$Value<-as.numeric(as.character(x$Value))
                                                                                                         # View(x) 
                                                                                                         # OK, only numerical
                                                                                                         
                                                                                                         # [31] Seed number per plant                                                                                             
                                                                                                         x<-subset(TRY, TRY$Trait_orig=="Seed number per plant")
                                                                                                         # x$Value<-as.numeric(as.character(x$Value))
                                                                                                         # View(x) # This one we definitely want to recode!
                                                                                                         unique(x$Database)
                                                                                                         # Only Ecological Flora of the British Isles has these factorial classes
                                                                                                         y<-subset(x, x$Database=="Ecological Flora of the British Isles")
                                                                                                         unique(y$Value)
                                                                                                         TRY$Value[TRY$Trait_orig=="Seed number per plant" & TRY$Value=="01-Oct"] <- 1
                                                                                                         TRY$Value[TRY$Trait_orig=="Seed number per plant" & TRY$Value=="10-100 "] <- 2
                                                                                                         TRY$Value[TRY$Trait_orig=="Seed number per plant" & TRY$Value=="100-1000"] <- 3
                                                                                                         TRY$Value[TRY$Trait_orig=="Seed number per plant" & TRY$Value=="1000-10000"] <- 4
                                                                                                         TRY$Value[TRY$Trait_orig=="Seed number per plant" & TRY$Value==">10000"] <- 5
                                                                                                         TRY$Value[TRY$Trait_orig=="Seed number per plant" & TRY$Value=="40.5"] <- NA
                                                                                                         # and rename this recoded trait to distinguish it from continuos values:
                                                                                                         TRYforrenaming<-TRY[TRY$Trait_orig=="Seed number per plant" & TRY$Database=="Ecological Flora of the British Isles",]
                                                                                                         TRYforrenaming$Trait_orig<-"Seed number per plant_BRIT5CLASSRECODED"
                                                                                                         TRY<-subset(TRY, !(TRY$Database=="Ecological Flora of the British Isles" & TRY$Trait_orig=="Seed number per plant"))
                                                                                                         TRY<-rbind(TRY, TRYforrenaming)
                                                                                                         
                                                                                                         # [32] Seed number per ramet   
                                                                                                         x<-subset(TRY, TRY$Trait_orig=="Seed number per ramet")
                                                                                                         # apparenty this one does not actually have observations
                                                                                                         
                                                                                                         # [33] Seed number per reproducton unit
                                                                                                         x<-subset(TRY, TRY$Trait_orig=="Seed number per reproducton unit")
                                                                                                         # x$Value<-as.numeric(as.character(x$Value))
                                                                                                         # View(x)
                                                                                                         # looks like it is one 0.2 - 0.56, one 0,171, one 143 - 2108 value  that causes that problem
                                                                                                         TRY$Value[TRY$Trait_orig=="Seed number per reproducton unit" & TRY$Value=="0.2 - 0.56"] <- NA
                                                                                                         TRY$Value[TRY$Trait_orig=="Seed number per reproducton unit" & TRY$Value=="0,171"] <- 0.171
                                                                                                         TRY$Value[TRY$Trait_orig=="Seed number per reproducton unit" & TRY$Value=="143 - 2108"] <- NA
                                                                                                         # now this should be OK
                                                                                                         
                                                                                                         # Now we should have all variables recoded so that when we convert Value to numerical,
                                                                                                         # all introduced NAs are real NAs
                                                                                                         TRY$Value <-as.numeric(as.character(TRY$Value))
                                                                                                         
                                                                                                         # Add better Trait names column
                                                                                                         TRY$Trait<-NA
                                                                                                         TRY$Trait[TRY$Trait_orig=="Crown (canopy) height (base to top)"] <- "Crown height"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf area"] <- "Leaf area" # probably safe to assume that this correspons same name in TTT and TRAITS
                                                                                                         # crazy amount of these more specified leaf area traits, probably can be combined but let's number these for nowe
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf area (in case of compond leaves: leaflet, petiole and rachis excluded)"] <- "Leaf area1"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole and rachis are in- or excluded)"] <- "Leaf area2"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf area (in case of compound leaves: leaf, petiole excluded)"] <- "Leaf area3"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf area (in case of compound leaves: leaflet, petiole and rachis included)"] <- "Leaf area4"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf area (in case of compund leaves: leaf, petiole included)"] <- "Leaf area5"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf area (in case of compund leaves: leaf, undefined if petiole in- or excluded)"] <- "Leaf area6"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf area (in case of compund leaves: leaflet, undefined if petiole and rachis are in- or excluded)"] <- "Leaf area7"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf area per leaf dry mass (SLA or 1/LMA): petiole and rachis excluded"] <- "SLA1"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf area per leaf dry mass (SLA or 1/LMA): petiole and rachis included"] <- "SLA2"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf area per leaf dry mass (SLA or 1/LMA): undefined if petiole and rachis are in- or excluded"] <- "SLA3"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf area per leaf dry mass (specific leaf area, SLA)"] <- "SLA4"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf area per leaf fresh mass (specific leaf area (SLA) based on leaf fresh mass)"] <- "SLA5"
                                                                                                         # and some the leaf variables:                                                              
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf carbon/nitrogen (C/N) ratio"] <- "LeafCNRatio2" # might or might not be comparable
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf dry mass"] <- "Leaf dry mass"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf fresh mass"] <- "Leaf fresh mass"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf length"] <- "Leaf length"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf lifespan (longevity)"] <- "Leaf lifespan 2" # might be comparable but maybe not...
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf nitrogen (N) content per leaf area"] <- "Leaf N per leaf area"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf nitrogen (N) content per leaf dry mass"] <- "Leaf N per leaf dry mass 2"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf volume"] <- "Leaf volume"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf width"] <- "Leaf width"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Plant carbon/nitrogen (C/N) ratio"] <- "PlantCNRatio"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Plant height generative"] <- "Generative height"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Plant height vegetative"] <- "Vegetative height"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Seed dry mass"] <- "Seed dry mass 2"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Seed number per inflorescence (total, fertile, infertile)"] <- "Seed number per inflorescence"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Seed number per plant"] <- "Seed number per plant"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Seed number per ramet"] <- "Seed number per ramet"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Seed number per reproducton unit"] <- "Seed number per reproducton unit"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole and rachis are in- or excluded)_BRIT6CLASSRECODED"] <- "LeafArBRIT6CLASSFACTORIAL"
                                                                                                         TRY$Trait[TRY$Trait_orig=="Seed number per plant_BRIT5CLASSRECODED"] <- "Seed number per plant BRIT5CLASSFACTORIAL"
                                                                                                         unique(TRY$Trait)
                                                                                                         unique(TRY$Trait_orig)
                                                                                                         
                                                                                                         # Add Trait type column
                                                                                                         TRY$Trait_type<-NA
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Crown (canopy) height (base to top)"] <- "Height"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf area"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf area (in case of compond leaves: leaflet, petiole and rachis excluded)"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole and rachis are in- or excluded)"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf area (in case of compound leaves: leaf, petiole excluded)"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf area (in case of compound leaves: leaflet, petiole and rachis included)"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf area (in case of compund leaves: leaf, petiole included)"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf area (in case of compund leaves: leaf, undefined if petiole in- or excluded)"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf area (in case of compund leaves: leaflet, undefined if petiole and rachis are in- or excluded)"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf area per leaf dry mass (SLA or 1/LMA): petiole and rachis excluded"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf area per leaf dry mass (SLA or 1/LMA): petiole and rachis included"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf area per leaf dry mass (SLA or 1/LMA): undefined if petiole and rachis are in- or excluded"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf area per leaf dry mass (specific leaf area, SLA)"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf area per leaf fresh mass (specific leaf area (SLA) based on leaf fresh mass)"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf carbon/nitrogen (C/N) ratio"] <- "Chemical"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf dry mass"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf fresh mass"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf length"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf lifespan (longevity)"] <- "Longevity" # might be comparable but maybe not...
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf nitrogen (N) content per leaf area"] <- "Chemical"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf nitrogen (N) content per leaf dry mass"] <- "Chemical"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf volume"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf width"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Plant carbon/nitrogen (C/N) ratio"] <- "Chemical"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Plant height generative"] <- "Height"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Plant height vegetative"] <- "Height"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Seed dry mass"] <- "Seed" 
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Seed number per inflorescence (total, fertile, infertile)"] <- "Seed"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Seed number per plant"] <- "Seed"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Seed number per ramet"] <- "Seed"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Seed number per reproducton unit"] <- "Seed"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole and rachis are in- or excluded)_BRIT6CLASSRECODED"] <- "Leaf"
                                                                                                         TRY$Trait_type[TRY$Trait_orig=="Seed number per plant_BRIT5CLASSRECODED"] <- "Seed"
                                                                                                         
                                                                                                         ### 2. Merge datasets ----------------------------------------------------
                                                                                                         
                                                                                                         colnames(TRAITS)[colnames(TRAITS)=="value"] <- "Value"   
                                                                                                         df<-rbind(TRAITS, TTT, TRY)
                                                                                                         str(df)
                                                                                                         
                                                                                                         # ged rid of NA rows (some data imports just include these)
                                                                                                         
                                                                                                         df<-subset(df, !is.na(df$Value))
                                                                                                         
                                                                                                         ### 3. For gap finding purposes, get rid of not-so-relevant traits ------------
                                                                                                         
                                                                                                         # all Chemical, Longevity, Rooting and Stem traits
                                                                                                         df<-subset(df, df$Trait_type=="Height"|df$Trait_type=="Seed"|df$Trait_type=="Leaf")
                                                                                                         
                                                                                                         checktable<-table(df$Trait)
                                                                                                         # View(checktable)
                                                                                                         
                                                                                                         
                                                                                                         ### 4. Evaluate which traits are comparable and can be combined -------------
                                                                                                         
                                                                                                         # There might still be differneces between ways traits are measured between different databases
                                                                                                         # that I didn't manage to get rid of in the code above, e.g. if a trait was originally
                                                                                                         # factorial but classes coded as numbers.
                                                                                                         
                                                                                                         # In addition, need to bring in unit information and make these comparable between databases
                                                                                                         # for final analysis
                                                                                                         
                                                                                                         # For each trait, we ask:
                                                                                                         # 1) are observations within trait comparable? If not, reclassify & change units
                                                                                                         # 2) can this trait be combined with another trait(s)? (i.e. does it have a different name
                                                                                                         # just because of different naming practices between databases?)
                                                                                                         
                                                                                                         
                                                                                                         ## * 4.1. Height traits ------------------------------------------------------
                                                                                                         
                                                                                                         dfheight<-subset(df, df$Trait_type=="Height")
                                                                                                         checktable<-table(dfheight$Trait)
                                                                                                         # View(checktable)
                                                                                                         unique(dfheight$Trait)
                                                                                                         
                                                                                                         # 6 traits:
                                                                                                         # "Canopy height"       
                                                                                                         # "Mature height"       
                                                                                                         # "Reproductive height" 
                                                                                                         # "Vegetative height"   
                                                                                                         # "Crown height"       
                                                                                                         # "Generative height"
                                                                                                         
                                                                                                         table(dfheight$Trait, dfheight$Units)
                                                                                                         
                                                                                                         dfheight$Units[dfheight$Units=="(cm)"]<-"cm"
                                                                                                         dfheight$Units[dfheight$Units==" cm"]<-"cm"
                                                                                                         
                                                                                                         # Canopy height
                                                                                                         # These are all from LEDA, and it is clear that Units is meters.
                                                                                                         # Assign Units:
                                                                                                         dfheight$Units[dfheight$Trait=="Canopy height"]<-"m"
                                                                                                         # This trait is coherent now.
                                                                                                         # Potential to combine with others: yes, this seems to be
                                                                                                         # general height measure, most likely just with vegetative parts
                                                                                                         # but to be it looks that it is well possible that we have here
                                                                                                         # reproductive height measurements as well.
                                                                                                         # Checked LEDA webpage: no metedata on this traits avaiable, but
                                                                                                         # this is the only height trait in th database, so I would assume
                                                                                                         # that this can be any height, both vegetative and reproductive.
                                                                                                         
                                                                                                         # Crown height
                                                                                                         # These are all from ECOCRAFT and in meters, so this is OK as well.
                                                                                                         # not so many observations and these are all from trees (vegetative).
                                                                                                         
                                                                                                         # Generative height
                                                                                                         dfheightgenerative<-subset(dfheight, dfheight$Trait=="Generative height")
                                                                                                         table(dfheightgenerative$Database, dfheightgenerative$Units)
                                                                                                         # Getting more complicated...
                                                                                                         # Here generative/vegetative makes a difference, like for grasses
                                                                                                         # a big thing especially...
                                                                                                         # No NA Units here.
                                                                                                         # Cannot see any problems in coherency within this trait, just need to change Units
                                                                                                         # eventually.
                                                                                                         
                                                                                                         # Mature height
                                                                                                         # This is all from PLANTS with NA Units.
                                                                                                         # Units is cm for sure:
                                                                                                         dfheight$Units[dfheight$Trait=="Mature height"]<-"cm"
                                                                                                         # Based on values of grasses, I think this is 
                                                                                                         # vegetative height only, but cannot be 100% sure.
                                                                                                         
                                                                                                         # Reproductive height
                                                                                                         # All from TTT in meters, so this is OK.
                                                                                                         
                                                                                                         # Vegetative height
                                                                                                         dfheightvegetaative<-subset(dfheight, dfheight$Trait=="Vegetative height")
                                                                                                         table(dfheightvegetaative$Database, dfheightvegetaative$Units)
                                                                                                         # Wow this is a mess, crazy amount of databases...
                                                                                                         # But cannot see why there would be incoherency in this trait,
                                                                                                         # just a matter of changing units.
                                                                                                         
                                                                                                         # Check remaining NA units.
                                                                                                         # None remaining.
                                                                                                         
                                                                                                         # Change all Units to meters.
                                                                                                         table(dfheight$Trait, dfheight$Units)
                                                                                                         dfheight$Value[dfheight$Units=="cm"]<-(dfheight$Value[dfheight$Units=="cm"])/100
                                                                                                         dfheight$Value[dfheight$Units=="mm"]<-(dfheight$Value[dfheight$Units=="mm"])/1000
                                                                                                         dfheight$Value[dfheight$Units=="feet"]<-(dfheight$Value[dfheight$Units=="feet"])*0.3048
                                                                                                         dfheight$Units<-"m"
                                                                                                         
                                                                                                         # There are puching errors in Units/values for sure, need to dela with these
                                                                                                         # before final analysis, bot for gap finding purposes OK.
                                                                                                         
                                                                                                         # I think we want to have 3 height traits here:
                                                                                                         # one with vegetative growth only, only with generative growth only, 
                                                                                                         # and one where we don't know if measurement are vegetative or generative.
                                                                                                         # (as in some databases generative andvegetative aren't separted).
                                                                                                         
                                                                                                         # "Canopy height"         => Height veg&gen   
                                                                                                         # "Mature height"        => Height veg&gen  
                                                                                                         # "Reproductive height"  => Height gen
                                                                                                         # "Vegetative height"     => Height veg
                                                                                                         # "Crown height"          => Height veg     
                                                                                                         # "Generative height"    => Height gen
                                                                                                         
                                                                                                         dfheight$Trait[dfheight$Trait=="Canopy height"]<-"Height veg&gen"
                                                                                                         dfheight$Trait[dfheight$Trait=="Mature height"]<-"Height veg&gen"
                                                                                                         dfheight$Trait[dfheight$Trait=="Reproductive height"]<-"Height gen"
                                                                                                         dfheight$Trait[dfheight$Trait=="Vegetative height"]<-"Height veg"
                                                                                                         dfheight$Trait[dfheight$Trait=="Crown height"]<-"Height veg"
                                                                                                         dfheight$Trait[dfheight$Trait=="Generative height"]<-"Height gen"
                                                                                                         
                                                                                                         unique(dfheight$Trait)
                                                                                                         
                                                                                                         ## * 4.2. Leaf traits --------------------------------------------------------
                                                                                                         
                                                                                                         dfleaf<-subset(df, df$Trait_type=="Leaf")
                                                                                                         checktable<-table(dfleaf$Trait)
                                                                                                         # View(checktable)
                                                                                                         unique(dfleaf$Trait)
                                                                                                         
                                                                                                         # 21 traits
                                                                                                         
                                                                                                         
                                                                                                         # ** 4.2.1. Mass traits ------------------------------------------------------
                                                                                                         
                                                                                                         # LDMC
                                                                                                         dfleafldmc<-subset(dfleaf, dfleaf$Trait=="LDMC")
                                                                                                         # View (dfleafldmc)
                                                                                                         table(dfleafldmc$Database, dfleafldmc$Units)
                                                                                                         unique(dfleafldmc$Database)
                                                                                                         # These values come from TTT with units and from LEDA without units.
                                                                                                         # LEDA Units is mg/g whereas TTT Units is g/g, change LEDA and give Units:
                                                                                                         dfleaf$Value[dfleaf$Database=="LEDA"&dfleaf$Trait=="LDMC"]<-(dfleaf$Value[dfleaf$Database=="LEDA"&dfleaf$Trait=="LDMC"])/1000
                                                                                                         dfleaf$Units[dfleaf$Database=="LEDA"&dfleaf$Trait=="LDMC"]<-"g/g"
                                                                                                         # This trait cannot be combined to anything straight, but it might
                                                                                                         # make sense to use avaiable dry mass and fress mass information to
                                                                                                         # potentially widen the cover of this... Need to discss this later.
                                                                                                         
                                                                                                         # Leaf mass
                                                                                                         check<-subset(dfleaf, dfleaf$Trait=="Leaf mass")
                                                                                                         # View (check)
                                                                                                         unique(check$Database)
                                                                                                         table(check$Database, check$Units)
                                                                                                         # All from LEDA with no Units.# LEDA webpage says this Units is mg.
                                                                                                         # Change to g and give Units:
                                                                                                         dfleaf$Value[dfleaf$Database=="LEDA"&dfleaf$Trait=="Leaf mass"]<-(dfleaf$Value[dfleaf$Database=="LEDA"&dfleaf$Trait=="Leaf mass"])/1000
                                                                                                         dfleaf$Units[dfleaf$Database=="LEDA"&dfleaf$Trait=="Leaf mass"]<-"g/g"
                                                                                                         # We dont know if this is fresh or dyry mass or both,
                                                                                                         # cannot find any metadata at LEDA webpage!
                                                                                                         
                                                                                                         # Leaf dry mass
                                                                                                         dfleaf$Units[dfleaf$Units==" mg"]<-"mg"
                                                                                                         dfleaf$Units[dfleaf$Units=="g DM"]<-"g"
                                                                                                         check<-subset(dfleaf, dfleaf$Trait=="Leaf dry mass")
                                                                                                         # View (check)
                                                                                                         unique(check$Database)
                                                                                                         unique(check$Units)
                                                                                                         table(check$Database, check$Units)
                                                                                                         # change mg to g:
                                                                                                         dfleaf$Value[dfleaf$Trait=="Leaf dry mass"&dfleaf$Units=="mg"]<-(dfleaf$Value[dfleaf$Trait=="Leaf dry mass"&dfleaf$Units=="mg"])/1000
                                                                                                         dfleaf$Units[dfleaf$Trait=="Leaf dry mass"&dfleaf$Units=="mg"]<-"g"
                                                                                                         
                                                                                                         # Leaf fresh mass
                                                                                                         check<-subset(dfleaf, dfleaf$Trait=="Leaf fresh mass")
                                                                                                         # View (check)
                                                                                                         table(check$Database, check$Units)
                                                                                                         # change mg to g:
                                                                                                         dfleaf$Value[dfleaf$Trait=="Leaf fresh mass"&dfleaf$Units=="mg"]<-(dfleaf$Value[dfleaf$Trait=="Leaf fresh mass"&dfleaf$Units=="mg"])/1000
                                                                                                         dfleaf$Units[dfleaf$Trait=="Leaf fresh mass"&dfleaf$Units=="mg"]<-"g"
                                                                                                         
                                                                                                         
                                                                                                         # ** 4.2.2. Area traits ------------------------------------------------------
                                                                                                         
                                                                                                         # Leaf area
                                                                                                         check<-subset(dfleaf, dfleaf$Trait=="Leaf area")
                                                                                                         # View (check)
                                                                                                         table(check$Database, check$Units)
                                                                                                         # change cm2 to mm2:
                                                                                                         dfleaf$Value[dfleaf$Trait=="Leaf area"&dfleaf$Units=="cm2"]<-(dfleaf$Value[dfleaf$Trait=="Leaf area"&dfleaf$Units=="cm2"])*100
                                                                                                         dfleaf$Units[dfleaf$Trait=="Leaf area"&dfleaf$Units=="cm2"]<-"mm2"
                                                                                                         # Probably safe to assume comparability between databases?
                                                                                                         
                                                                                                         # Leaf area1-7
                                                                                                         # We have a bunch of these more precisely defined
                                                                                                         # measurements from TRY dataset.
                                                                                                         check<-subset(dfleaf, dfleaf$Trait=="Leaf area1"|
                                                                                                                         dfleaf$Trait=="Leaf area2"|
                                                                                                                         dfleaf$Trait=="Leaf area3"|
                                                                                                                         dfleaf$Trait=="Leaf area4"|
                                                                                                                         dfleaf$Trait=="Leaf area5"|
                                                                                                                         dfleaf$Trait=="Leaf area6"|
                                                                                                                         dfleaf$Trait=="Leaf area7")
                                                                                                         table(check$Trait)
                                                                                                         table(check$Database)
                                                                                                         # These observations are quite numerous - it is possible that
                                                                                                         # we want to be sure of comparability of leaf definition here,
                                                                                                         # in which case we might want to use some of these traits rather that general
                                                                                                         # leaf area trait; so I won't combine these with anything else for now...
                                                                                                         unique(check$Units)
                                                                                                         dfleaf$Units[dfleaf$Units=="cm?"]<-"cm2"
                                                                                                         dfleaf$Units[dfleaf$Units=="mm?"]<-"mm2"
                                                                                                         dfleaf$Units[dfleaf$Units==" cm2"]<-"cm2"
                                                                                                         
                                                                                                         # SLA
                                                                                                         check<-subset(dfleaf, dfleaf$Trait=="SLA")
                                                                                                         # View (check)
                                                                                                         table(check$Database, check$Units)
                                                                                                         # OK, only from TTT with Units
                                                                                                         
                                                                                                         # SLA1-5
                                                                                                         # We have a bunch of these more precisely defined
                                                                                                         # measurements from TRY dataset also for SLA.
                                                                                                         check<-subset(dfleaf, dfleaf$Trait=="SLA1"|
                                                                                                                         dfleaf$Trait=="SLA2"|
                                                                                                                         dfleaf$Trait=="SLA3"|
                                                                                                                         dfleaf$Trait=="SLA4"|
                                                                                                                         dfleaf$Trait=="SLA5")
                                                                                                         table(check$Trait)
                                                                                                         table(check$Database)
                                                                                                         # Yes, big amount of data in this case as well
                                                                                                         unique(check$Units)
                                                                                                         # wow need to do a lot of work here at some point with units.
                                                                                                         
                                                                                                         
                                                                                                         # LeafArBRIT6CLASSFACTORIAL
                                                                                                         check<-subset(dfleaf, dfleaf$Trait=="LeafArBRIT6CLASSFACTORIAL")
                                                                                                         # View (check)
                                                                                                         table(check$Database, check$Units)
                                                                                                         # OK
                                                                                                         
                                                                                                         
                                                                                                         # ** 4.2.3. Other traits -----------------------------------------------------
                                                                                                         
                                                                                                         # Leaf size
                                                                                                         check<-subset(dfleaf, dfleaf$Trait=="Leaf size")
                                                                                                         table(check$Database, check$Units)
                                                                                                         # This actually hasn't any data
                                                                                                         
                                                                                                         check<-subset(dfleaf, dfleaf$Trait=="Leaf areaLEDA")
                                                                                                         table(check$Database, check$Units)
                                                                                                         
                                                                                                         dfleaf$Trait[dfleaf$Trait=="Leaf areaLEDA"]<-"Leaf area"
                                                                                                         
                                                                                                         
                                                                                                         
                                                                                                         # Leaf length
                                                                                                         check<-subset(dfleaf, dfleaf$Trait=="Leaf length")
                                                                                                         table(check$Database, check$Units)
                                                                                                         # change mm to cm
                                                                                                         dfleaf$Value[dfleaf$Trait=="Leaf length"&dfleaf$Units=="mm"]<-(dfleaf$Value[dfleaf$Trait=="Leaf length"&dfleaf$Units=="mm"])/10
                                                                                                         dfleaf$Units[dfleaf$Trait=="Leaf length"&dfleaf$Units=="mm"]<-"cm"
                                                                                                         # OK now
                                                                                                         
                                                                                                         # Leaf volume
                                                                                                         check<-subset(dfleaf, dfleaf$Trait=="Leaf volume")
                                                                                                         table(check$Database, check$Units)
                                                                                                         dfleaf$Value[dfleaf$Trait=="Leaf volume"&dfleaf$Units=="mm3"]<-(dfleaf$Value[dfleaf$Trait=="Leaf volume"&dfleaf$Units=="mm3"])/1000
                                                                                                         dfleaf$Units[dfleaf$Trait=="Leaf volume"&dfleaf$Units=="mm3"]<-"cm3"
                                                                                                         # OK now
                                                                                                         
                                                                                                         # Leaf width
                                                                                                         check<-subset(dfleaf, dfleaf$Trait=="Leaf width")
                                                                                                         table(check$Database, check$Units)
                                                                                                         dfleaf$Value[dfleaf$Trait=="Leaf width"&dfleaf$Units=="mm"]<-(dfleaf$Value[dfleaf$Trait=="Leaf width"&dfleaf$Units=="mm"])/10
                                                                                                         dfleaf$Units[dfleaf$Trait=="Leaf width"&dfleaf$Units=="mm"]<-"cm"
                                                                                                         # OK now
                                                                                                         
                                                                                                         
                                                                                                         # ** 4.2.4. Conclusions about leaf traits ---------------------------------
                                                                                                         
                                                                                                         # Before deciding if we want to combine any of the leaf traits,
                                                                                                         # we need to see how dfferent traits cover our species: if more precise
                                                                                                         # measurements cover well, could leave less accurate ones out; if coverage isnt?
                                                                                                         # that great, could combine all potential to not-so-accurately-defined
                                                                                                         # area & mass traits
                                                                                                         
                                                                                                         # So, we need to create test datasets to see how using different classifications
                                                                                                         # affects this.
                                                                                                         
                                                                                                         ## * 4.3. Seed traits --------------------------------------------------------
                                                                                                         
                                                                                                         dfseed<-subset(df, df$Trait_type=="Seed")
                                                                                                         checktable<-table(dfseed$Trait)
                                                                                                         # View(checktable)
                                                                                                         unique(dfseed$Trait)
                                                                                                         
                                                                                                         # 9 Traits
                                                                                                         
                                                                                                         
                                                                                                         # ** 4.3.1. Mass traits ------------------------------------------------------
                                                                                                         
                                                                                                         # Seedmass1
                                                                                                         check<-subset(dfseed, dfseed$Trait=="Seedmass1")
                                                                                                         table(check$Database)
                                                                                                         # all from LEDA, and database webpag says it mg:
                                                                                                         dfseed$Units[dfseed$Trait=="Seedmass1"]<-"mg"
                                                                                                         range(check$Value)
                                                                                                         
                                                                                                         # Seedmass2
                                                                                                         check<-subset(dfseed, dfseed$Trait=="Seedmass2")
                                                                                                         table(check$Database)
                                                                                                         # only from Eflora_cal, doesn't tell us units.
                                                                                                         dfseed$Units[dfseed$Trait=="Seedmass2"]<-"mg"
                                                                                                         
                                                                                                         # It might be safe to assume that these two are comparable...?
                                                                                                         dfseed$Trait[dfseed$Trait=="Seedmass2"]<-"Seedmass"
                                                                                                         dfseed$Trait[dfseed$Trait=="Seedmass1"]<-"Seedmass"
                                                                                                         
                                                                                                         # Seed dry mass
                                                                                                         check<-subset(dfseed, dfseed$Trait=="Seed dry mass")
                                                                                                         table(check$Database, check$Units)
                                                                                                         # only from TTT and as mg, so this is OK
                                                                                                         
                                                                                                         # Seed dry mass 2
                                                                                                         check<-subset(dfseed, dfseed$Trait=="Seed dry mass 2")
                                                                                                         checktable<-table(check$Database, check$Units)
                                                                                                         # Wow this is a mess...
                                                                                                         unique(check$Units)
                                                                                                         table(check$Units)
                                                                                                         # change to give values mg per seed:
                                                                                                         dfseed$Units[dfseed$Units=="(mg)"]<-"mg"
                                                                                                         dfseed$Units[dfseed$Units=="mg per seed"]<-"mg"
                                                                                                         dfseed$Value[dfseed$Trait=="Seed dry mass 2"&dfseed$Units=="g"]<-(dfseed$Value[dfseed$Trait=="Seed dry mass 2"&dfseed$Units=="g"])*1000
                                                                                                         dfseed$Units[dfseed$Trait=="Seed dry mass 2"&dfseed$Units=="g"]<-"mg"
                                                                                                         dfseed<-dfseed[!(dfseed$Trait=="Seed dry mass 2"&dfseed$Units=="g ?"),]
                                                                                                         dfseed$Units[dfseed$Trait=="Seed dry mass 2"&dfseed$Units=="g / 1000 seeds"]<-"mg"
                                                                                                         dfseed$Value[dfseed$Trait=="Seed dry mass 2"&dfseed$Units=="g/100 seeds"]<-(dfseed$Value[dfseed$Trait=="Seed dry mass 2"&dfseed$Units=="g/100 seeds"])*10
                                                                                                         dfseed$Units[dfseed$Trait=="Seed dry mass 2"&dfseed$Units=="g/100 seeds"]<-"mg"
                                                                                                         dfseed$Units[dfseed$Trait=="Seed dry mass 2"&dfseed$Units=="g/1000"]<-"mg"
                                                                                                         table(check$Units=="micro gx10")
                                                                                                         dfseed$Value[dfseed$Trait=="Seed dry mass 2"&dfseed$Units=="micro gx10"]<-(dfseed$Value[dfseed$Trait=="Seed dry mass 2"&dfseed$Units=="micro gx10"])/100
                                                                                                         dfseed$Units[dfseed$Trait=="Seed dry mass 2"&dfseed$Units=="micro gx10"]<-"mg"
                                                                                                         dfseed$Value[dfseed$Trait=="Seed dry mass 2"&dfseed$Units=="1/kg"]<-1000000/(dfseed$Value[dfseed$Trait=="Seed dry mass 2"&dfseed$Units=="1/kg"])
                                                                                                         dfseed$Units[dfseed$Trait=="Seed dry mass 2"&dfseed$Units=="1/kg"]<-"mg"
                                                                                                         dfseed$Value[dfseed$Trait=="Seed dry mass 2"&dfseed$Units=="1/pound"]<-453592/(dfseed$Value[dfseed$Trait=="Seed dry mass 2"&dfseed$Units=="1/pound"])
                                                                                                         dfseed$Units[dfseed$Trait=="Seed dry mass 2"&dfseed$Units=="1/pound"]<-"mg"
                                                                                                         # OK now, assuming that when nothing else specified,
                                                                                                         # values are per seed...
                                                                                                         
                                                                                                         # It might be safe to assume that these two are comparable...?
                                                                                                         dfseed$Trait[dfseed$Trait=="Seed dry mass 2"]<-"Seed dry mass"
                                                                                                         
                                                                                                         # ** 4.3.2. Seed number traits -----------------------------------------------
                                                                                                         
                                                                                                         # Seed number per shoot
                                                                                                         check<-subset(dfseed, dfseed$Trait=="Seed number per shoot")
                                                                                                         table(check$Units)
                                                                                                         # All from LEDA
                                                                                                         
                                                                                                         # Seed number per plant  
                                                                                                         check<-subset(dfseed, dfseed$Trait=="Seed number per plant")
                                                                                                         table(check$Units)
                                                                                                         
                                                                                                         # Probably safe to combine these two
                                                                                                         dfseed$Trait[dfseed$Trait=="Seed number per shoot"]<-"Seed number per plant"
                                                                                                         
                                                                                                         
                                                                                                         # Annual seed production per plant
                                                                                                         check<-subset(dfseed, dfseed$Trait=="Annual seed production per plant")
                                                                                                         table(check$Units)
                                                                                                         # no data
                                                                                                         
                                                                                                         # Seed number per inflorescence    
                                                                                                         check<-subset(dfseed, dfseed$Trait=="Seed number per inflorescence")
                                                                                                         table(check$Units)
                                                                                                         
                                                                                                         # Seed number per reproducton unit
                                                                                                         check<-subset(dfseed, dfseed$Trait=="Seed number per reproducton unit")
                                                                                                         table(check$Units)
                                                                                                         
                                                                                                         # Probably safe to combine these two
                                                                                                         dfseed$Trait[dfseed$Trait=="Seed number per reproducton unit"]<-"Seed number per inflorescence"
                                                                                                         
                                                                                                         # Seed number per plant BRIT5CLASSFACTORIAL
                                                                                                         check<-subset(dfseed, dfseed$Trait=="Seed number per plant BRIT5CLASSFACTORIAL")
                                                                                                         table(check$Units)
                                                                                                         
                                                                                                         
                                                                                                         ## * 4.4 Combine changed Traits type sets ----------------------------------
                                                                                                         
                                                                                                         df<-rbind(dfseed, dfheight, dfleaf)
                                                                                                         unique(df$Trait_type)
                                                                                                         unique(df$Trait)
                                                                                                         
                                                                                                         
                                                                                                         ### 5. Deal with species issues --------------------------------------------
                                                                                                         
                                                                                                         # Doble-check that all species we hve in datasets are included in our species list
                                                                                                         
                                                                                                         str(specieslist)
                                                                                                         ALLspecies<-unique(specieslist$V1)
                                                                                                         # 1256
                                                                                                         
                                                                                                         # in each dataset we have...
                                                                                                         TRAITSspecies<-unique(TRAITS$Species)
                                                                                                         TTTspecies<-unique(TTT$Species)
                                                                                                         TRYspecies<-unique(TRY$Species)
                                                                                                         str(TRAITSspecies) # 1253
                                                                                                         str(TTTspecies) # 1005
                                                                                                         str(TRYspecies) # 992
                                                                                                         
                                                                                                         setdiff(TTTspecies, ALLspecies) # TTT is OK as it should be...
                                                                                                         setdiff(TRAITSspecies, ALLspecies)
                                                                                                         setdiff(TRYspecies, ALLspecies)
                                                                                                         # ...but in TRAITS and TRY there are species that we don't have in species list!
                                                                                                         # I assume these are actually same species as in species list
                                                                                                         # but with wrong names.
                                                                                                         
                                                                                                         
                                                                                                         # so this has still some dublicate species!!!
                                                                                                         df2 <- dcast(df,                          
                                                                                                                      value.var = "Value",         # Name of the column whose values will be filled to cast
                                                                                                                      Species~Trait,             # LHS ~ RHS formula
                                                                                                                      fun.aggregate = mean) 
                                                                                                         
                                                                                                         str(df2)
                                                                                                         
                                                                                                         ### 6. Irrelevant databases? ----------------------------------------------
                                                                                                         
                                                                                                         # There might be some databases that have trait information that is not
                                                                                                         # applicable for our area, do we want to exclude these?
                                                                                                         
                                                                                                         
                                                                                                         ### 7. Notes for the future ----------------------------------------------
                                                                                                         
                                                                                                         # Some databases include tens of observations for one species,
                                                                                                         # some might originate from one study or a small population;
                                                                                                         # if we just average these, we might get funny values!
                                                                                                         
                                                                                                         # It is evident that there are errors in these datasets!!!
                                                                                                         # E.g. height: sometimes when Units are stated to be "m",
                                                                                                         # actual values make it clear that we are dealing with centimeters...
                                                                                                         
                                                                                                         # LDMC: might
                                                                                                         # make sense to use avaiable dry mass and fress mass information to
                                                                                                         # potentially widen the cover of this?
                                                                                                         
                                                                                                         # Leaf area: comparability between databases: can we assume that these are comparable,
                                                                                                         # e.g. in how leaf is determined? We have some that are just defined as "Leaf area" but also some
                                                                                                         # from TRY that are better defined - do we want bigger or better data?
                                                                                                         # SAME APPLIES FOR SLA!
                                                                                                         
                                                                                                         # Seed mass from LEDA and from Eflora_cal; can we be sure that there
                                                                                                         # mean same for both databases?
                                                                                                         
                                                                                                         
                                                                                                         
                                                                                                         
                                                                                                         ### 8. Plot -----------------------------------------------------------------
                                                                                                         
                                                                                                         df2[df2<0 | df2>0| df2==0]<-1
                                                                                                         sums<-colSums(df2[,2:32], na.rm=T)
                                                                                                         barplot(sums)
                                                                                                         
                                                                                                         sums<-as.data.frame(sums)
                                                                                                         names <- rownames(sums)
                                                                                                         rownames(sums) <- NULL
                                                                                                         sums <- cbind(names,sums)
                                                                                                         colnames(sums)[colnames(sums)=="names"] <- "Traits"
                                                                                                         colnames(sums)[colnames(sums)=="sums"] <- "Frequency"
                                                                                                         sums$Traits <- factor(sums$Traits, levels = sums$Traits[order(-sums$Frequency)])
                                                                                                         
                                                                                                         p<-ggplot(data=sums, aes(x=Traits, y=Frequency)) +
                                                                                                           geom_bar(stat="identity")
                                                                                                         p
                                                                                                         p + coord_flip()
                                                                                                         # this plot shows of how many species we have observations on each trait
                                                                                                         
                                                                                                         # same for each Trait type separately
                                                                                                         
                                                                                                         dfheight<-subset(df, df$Trait_type=="Height")
                                                                                                         dfheight2 <- dcast(dfheight,                          
                                                                                                                            value.var = "Value",         # Name of the column whose values will be filled to cast
                                                                                                                            Species~Trait,             # LHS ~ RHS formula
                                                                                                                            fun.aggregate = mean)
                                                                                                         dfheight2[dfheight2<0 | dfheight2>0| dfheight2==0]<-1
                                                                                                         sums<-colSums(dfheight2[,2:4], na.rm=T)
                                                                                                         barplot(sums)
                                                                                                         sums<-as.data.frame(sums)
                                                                                                         names <- rownames(sums)
                                                                                                         rownames(sums) <- NULL
                                                                                                         sums <- cbind(names,sums)
                                                                                                         colnames(sums)[colnames(sums)=="names"] <- "Traits"
                                                                                                         colnames(sums)[colnames(sums)=="sums"] <- "Frequency"
                                                                                                         sums$Traits <- factor(sums$Traits, levels = sums$Traits[order(-sums$Frequency)])
                                                                                                         p<-ggplot(data=sums, aes(x=Traits, y=Frequency)) +
                                                                                                           geom_bar(stat="identity")
                                                                                                         p
                                                                                                         p + coord_flip()
                                                                                                         
                                                                                                         dfheight<-subset(df, df$Trait_type=="Seed")
                                                                                                         dfheight2 <- dcast(dfheight,                          
                                                                                                                            value.var = "Value",         # Name of the column whose values will be filled to cast
                                                                                                                            Species~Trait,             # LHS ~ RHS formula
                                                                                                                            fun.aggregate = mean)
                                                                                                         dfheight2[dfheight2<0 | dfheight2>0| dfheight2==0]<-1
                                                                                                         sums<-colSums(dfheight2[,2:7], na.rm=T)
                                                                                                         barplot(sums)
                                                                                                         sums<-as.data.frame(sums)
                                                                                                         names <- rownames(sums)
                                                                                                         rownames(sums) <- NULL
                                                                                                         sums <- cbind(names,sums)
                                                                                                         colnames(sums)[colnames(sums)=="names"] <- "Traits"
                                                                                                         colnames(sums)[colnames(sums)=="sums"] <- "Frequency"
                                                                                                         sums$Traits <- factor(sums$Traits, levels = sums$Traits[order(-sums$Frequency)])
                                                                                                         p<-ggplot(data=sums, aes(x=Traits, y=Frequency)) +
                                                                                                           geom_bar(stat="identity")
                                                                                                         p
                                                                                                         p + coord_flip()
                                                                                                         
                                                                                                         dfheight<-subset(df, df$Trait_type=="Leaf")
                                                                                                         dfheight2 <- dcast(dfheight,                          
                                                                                                                            value.var = "Value",         # Name of the column whose values will be filled to cast
                                                                                                                            Species~Trait,             # LHS ~ RHS formula
                                                                                                                            fun.aggregate = mean)
                                                                                                         dfheight2[dfheight2<0 | dfheight2>0| dfheight2==0]<-1
                                                                                                         sums<-colSums(dfheight2[,2:23], na.rm=T)
                                                                                                         barplot(sums)
                                                                                                         sums<-as.data.frame(sums)
                                                                                                         names <- rownames(sums)
                                                                                                         rownames(sums) <- NULL
                                                                                                         sums <- cbind(names,sums)
                                                                                                         colnames(sums)[colnames(sums)=="names"] <- "Traits"
                                                                                                         colnames(sums)[colnames(sums)=="sums"] <- "Frequency"
                                                                                                         # sums$Traits <- factor(sums$Traits, levels = sums$Traits[order(-sums$Frequency)])
                                                                                                         p<-ggplot(data=sums, aes(x=Traits, y=Frequency)) +
                                                                                                           geom_bar(stat="identity")
                                                                                                         p
                                                                                                         p + coord_flip()
                                                                                                         
                                                                                                         
                                                                                                         
                                                                                                         
