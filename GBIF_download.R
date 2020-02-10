#########################################
##             GBIF DATA             ####
##  OBS! Go to line 102 or 119       ####
##  to retrieve directly from folder ####
#########################################
getwd()

## 1. DOWNLOAD GBIF DATA                 ####
## 1.1 LOAD PACKAGES AND ADD CREDENTIALS ####
##---------------------------------------####

# Load packages
library(rgbif)
library(stringr) # string manipulations (not needed, may also be done by base R)
library(rio)     # data import (not needed, may also be done by base R)
library(dplyr)   # for data-wrangling
library(wicket)  # check WKT strings
library(sp)
library(maptools)
library(raster)
library(rgdal)
library (data.table)
library(dplyr)
library(rgeos)
library(ggplot2)
library(picante) 
library(sf)

# Make R ask for you login credentials: OBS! Make sure that these are typed in correctly, as the following functions
# will not work otherwise. R does not tell you, if you type it in wrong (it took Tanja 3 hours to figure out)
options(gbif_user=rstudioapi::askForPassword("my gbif username"))
options(gbif_email=rstudioapi::askForPassword("my registred gbif e-mail"))
options(gbif_pwd=rstudioapi::askForPassword("my gbif password"))

## 1.1 MAKE A DOWNLOAD KEY ####
##-------------------------####
# This section of code is only if you want to request an entirely new download.
# If we are to use the original inquery from 20/02-2019, it can be found as 
# "GBIF Occurrence Download 10.15468/dl.sxzezh accessed via GBIF.org on 2019-02-20"

 # Get a kingdom-key for plants
key <- name_suggest(q='Plantae', rank='kingdom')$key[1] 
key
occ_search(taxonKey=key, limit=20)   # Check that it works

# Make a download key. NB! Maximum of 3 download requests handled simultaneously
download_key <- occ_download(
  'hasCoordinate = TRUE',             # Must have coordinates/be georeferenced
  'hasGeospatialIssue = FALSE',       # No known spatial issues
  'country = NO',                     # Be classified as "in Norway"
  'taxonKey = 6',                     # Retrieved from 'key' - within Plantae
  type = "and"
) %>% 
  occ_download_meta

# Let some time pass before continuing, as GBIF needs time to prepare the download

## 1.2 CREATE THE 'COFFEE BREAK'-FUNCTION TO RETRIEVE THE DATA    ####
##     OBS! This has been done, and should thus not be done again ####
##----------------------------------------------------------------####

# Make the "Coffee Break"-function to retrieve the requested data
# define function
download_GBIF_API <- function(download_key,n_try,Sys.sleep_duration,destfile_name){
  start_time <- Sys.time()
  n_try_count <- 1
  
  download_url <- paste("http://api.gbif.org/v1/occurrence/download/request/",
                        download_key[1],sep="")
  
  try_download <- try(download.file(url=download_url,destfile=destfile_name,
                                    quiet=TRUE),silent = TRUE)
  
  while (inherits(try_download, "try-error") & n_try_count < n_try) {   
    Sys.sleep(Sys.sleep_duration)
    n_try_count <- n_try_count+1
    try_download <- try(download.file(url=download_url,destfile=destfile_name,
                                      quiet=TRUE),silent = TRUE)
    print(paste("trying... Download link not ready. Time elapsed (min):",
                round(as.numeric(paste(difftime(Sys.time(),start_time, units = "mins"))),2)))
  }
}


## 1.3 GET THE DATA ####
##------------------####

# OBS - the first parts of this section is for use if you have made a new download - otherwise, skip straight to importing the .txt-file with 'fread()'

# Call function
# Create zip-file for download
download_GBIF_API(download_key=download_key,destfile_name="tmp.zip",n_try=10,Sys.sleep_duration=180)   # Try 10 times, wait 3 minutes in between each try
s

# The one above might not work- in that case, use this one instead:
download.file(url=paste("http://api.gbif.org/v1/occurrence/download/request/",
                        download_key[1], sep=""),
              destfile="temp.zip",
              quiet=TRUE, mode="wb")

# UNZIPPING THE FILE:
# Get a list of the files within the archive by using "list=TRUE" in the unzip function.
archive_files <- unzip("temp.zip", files = "NULL", list = T)   # Name the file according to the .zip-folder worked above

# Get the occurrence.txt file in as a dataframe (using import from rio)
NorPlant_occur <- import(unzip("temp.zip",                             # Name the file according to the .zip-folder worked above
                           files="occurrence.txt"),header=T,sep="\t")

### If the code above has been run before, just use the following:
# THIS IS THE LINE TO RUN FROM NOW ON!
NorPlant_occur <- fread("occurrence_10_15468_dl_sxzezh.txt")   # The file is too big to run on desktop, unfortunately.
                                                               # The .txt file is too large to have on GitHub, it can be found in the folder instead

## Cite your data! 
# Finally but not at least, remember to cite your data properly:
paste("GBIF Occurrence Download", download_key[2], "accessed via GBIF.org on", Sys.Date())
    # "GBIF Occurrence Download 10.15468/dl.sxzezh accessed via GBIF.org on 2019-02-20"


## 2. SPATIAL 'PLAYING AROUND ' ####
##------------------------------####
# First, I will remove some of the unnecessary columns to save some space and memory while working
# (I'm removing the ones that I don't think we need - the original dataset is of course saved in the original .txt-file)
names(NorPlant_occur)
NorPlant <- NorPlant_occur[, -c(2:15, 17:36, 38:56, 65:71, 75:79, 81:87, 90:95, 100:102, 106:122, 126:132, 137, 139:182, 185:190, 200:219, 222:229, 233:237)]

# Write a .txt-file, if you want to be able to load this directly
{
write.table(NorPlant, file="occurrence_stripped.txt")
NorPlant <- fread("occurrence_stripped.txt")
}

# Make some columns factors at once - can be modified as necessary
NorPlant[,c("basisOfRecord", "occurrenceStatus", "kingdom", "phylum", "class", "order",
           "family","genus", "subgenus", "specificEpithet",
           "infraspecificEpithet", "species")] <- lapply(NorPlant[,c("basisOfRecord", "occurrenceStatus", "kingdom", "phylum", "class", "order",
                                                                    "family","genus", "subgenus", "specificEpithet",
                                                                    "infraspecificEpithet", "species")], factor)
str(NorPlant)

# Keep only the vascular plants
NorPlant_vasc <- NorPlant[NorPlant$phylum=="Tracheophyta",]

# It is important to note here that some records have the occurrenceStatus "absent" - in essence, those are true absences (or so we can assume)
# These should be removed from the dataset
table(NorPlant_vasc$occurrenceStatus)
NorPlant_vasc <- NorPlant_vasc[!NorPlant_vasc$occurrenceStatus=="absent",]

NorPlant_vasc <- droplevels(NorPlant_vasc)   # Drop unused levels

### For the spatial analyses, I suggest doing most things with the 'sf'-package, as that is a lot faster than 'sp'
norway <- getData('GADM', country='Norway', level=0)    # Get the Global Administrative border of Norway
plot(norway)
norway@proj4string   # Check the CRS
norway_UTM <- spTransform(norway, CRS("+proj=utm +zone=32"))  # Reproject the CRS to the same as Ida used
norway_UTM@proj4string
# Transform to an 'sf'-object
norway_UTM <- st_as_sf(norway_UTM)

# Add a 2 km buffer around the border (as was done in Ida's thesis), to allow for some spatial uncertainty - the size of this can be discussed
norway_UTM_buff <- st_buffer(norway_UTM, dist = 2000)   # OBS on the '+units' in the CRS - should be 'm'

# Make the occurrence data spatial
NorPlant_vasc <- st_as_sf(NorPlant_vasc, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Change the CRS to match the norway-polygon:
st_crs(norway_UTM_buff)   # Check the name of the CRS
NorPlant_vasc <- st_transform(NorPlant_vasc, crs = st_crs(norway_UTM_buff))   # Takes a little while to run

# Only keep records of the taxa found in Ida's analyses
taxonKeys <- fread("taxonKey.txt")
taxonKeys <- c(taxonKeys$`GBIF Taxon key`)  # Make it a vector
NorPlant_vasc <- NorPlant_vasc[NorPlant_vasc$taxonKey %in% taxonKeys |
                                 NorPlant_vasc$acceptedTaxonKey %in% taxonKeys , ]  # 5,156,372 records -->  3,794,299 records

# Only keep records within the Norwegian buffer-border (making sure that we do not have any "marine strays")
# OBS Even with 'sf' and on the server, this is a very time-consuming task!
# It might seem intuitive to use the 'st_intersection'-function - don't! That one takes a long time to run. Instead, a work-around is to use
# 'st_join' instead, as that one uses the faster 'st_intersects'  (this one takes a couple of minutes vs. hours). 'left=F' is needed to get and 
# 'inner_join', otherwise we'll get a 'left_join'
NorPlant_buff <- st_join(NorPlant_vasc, norway_UTM_buff, join=st_intersects, left=FALSE)  # 3,786,868 records


## 2.1 MAKE A NORWAY-GRID ####
##------------------------####
# 20km grid cells
raster20k <- raster(ext=extent(as(NorPlant_buff, "Spatial")), crs=crs(as(NorPlant_buff, "Spatial")), res=20000, vals=1)
plot(raster20k)

raster20km <- mask(raster20k, as(NorPlant_buff, "Spatial"))
plot(raster20km)

r20 <- raster20km
rm(raster20km)

# Convert raster to individual polygons
NorGrid <- st_as_sf(rasterToPolygons(r20))

# Find out which pixel each observation belongs to, and save that as 'Pixelnr' in the dataframe
NorGrid$Pixelnr <- 1:nrow(NorGrid)
NorPlant_buff <- st_join(NorPlant_buff, NorGrid, join=st_within)

# Have a look at the potential number of species
NorPlant_buff$species <- droplevels(NorPlant_buff$species)
nlevels(NorPlant_buff$species)  


## 3. RASTERIZE THE DATASETS       ####
##---------------------------------####
# Rasterize the species occurrence data sets, both in terms of presence absence (fun='last') or number of occurrences (fun='count')
# Loop through and save files individually
# I here do that for all species in found in the downloaded GBIF data, regardless of whether or not we have trait-data for those

for (i in 1:length(levels(as.factor(NorPlant_buff$species_construct)))){
  print(i)
  r20_occ<-rasterize(NorPlant_buff[NorPlant_buff$species_construct==levels(as.factor(NorPlant_buff$species_construct))[i],],r20,field=1,fun='count')
  r20_occ[is.na(r20_occ[])] <- 0
  r20_occ <- mask(r20_occ, as(norway_UTM_buff, 'Spatial'), updatevalue=NA)
  
  r20_pa<-rasterize(NorPlant_buff[NorPlant_buff$species_construct==levels(as.factor(NorPlant_buff$species_construct))[i],],r20,field=1,fun='last')
  r20_pa[is.na(r20_pa[])] <- 0
  r20_pa <- mask(r20_pa, as(norway_UTM_buff, 'Spatial'), updatevalue=NA)
  
  namevec_20o<-paste('/home/ahomez/t/tanjakp/export/NorwFlorTrait/rasters/occ20k/',levels(as.factor(NorPlant_buff$species_construct))[i],sep='_')  # OBS on the name/path - this is in my Franklin-export folder
  namevec_20pa<-paste('/home/ahomez/t/tanjakp/export/NorwFlorTrait/rasters/pa20k/',levels(as.factor(NorPlant_buff$species_construct))[i],sep='_')
  
  writeRaster(r20_occ,filename=namevec_20o,format='GTiff',overwrite=T)
  writeRaster(r20_pa,filename=namevec_20pa,format='GTiff',overwrite=T)
}


# Import the rasters
# List and download files (If there are other files in the folder, need to speficiy those with string e.g. '.tif')
          # Important note: when working with the rasterstack-objects, R fetches them from the indicated location everytime
          # So do not move or rename anything while you're working!
occ20k_files<-list.files('/home/ahomez/t/tanjakp/export/NorwFlorTrait/rasters/occ20k/',full.names=T)
occ20k_stack<-stack(occ20k_files)

pa20k_files<-list.files('/home/ahomez/t/tanjakp/export/NorwFlorTrait/rasters/pa20k/',full.names=T)
pa20k_stack<-stack(pa20k_files)

rm(occ20k_files)  # Remove files we do not need anymore
rm(pa20k_files)

# Plot the rasters - if you want it all, just use e.g. plot(occ20k_stack). Here I have shown an example for a single species
# Just change the species name (but keep the format) - here is an example plot:
par(mfrow=c(1,2))
plot(occ20k_stack[["X_Hepatica_nobilis"]], main="Hepatica nobilis, \n# of occurrence records")
plot(pa20k_stack[["X_Hepatica_nobilis"]], main="\nPresence/absence")

# Make a raster with number of species and number of observations in each grid cell
    # OBS! Takes quite a long time to run, even on the server
ras_nspec <- sum(pa20k_stack, na.rm=TRUE)
ras_noccur <- sum(occ20k_stack, na.rm=TRUE)

writeRaster(ras_nspec,filename='/home/ahomez/t/tanjakp/export/NorwFlorTrait/rasters/nspec',format='GTiff',overwrite=T)
writeRaster(r20_pa,filename='/home/ahomez/t/tanjakp/export/NorwFlorTrait/rasters/noccur',format='GTiff',overwrite=T)   

#ras_nspec <- raster("rasters/nspec.tif")     # If we want to import the rasters from the folder
#ras_noccur <- raster("rasters/noccur.tif")

plot(ras_nspec, main="Number of species in grid cells")
plot(ras_noccur, main="Number of occurrences in grid cells")


## OBS! DO NOT RUN PAST HERE - THESE SECTIONS ARE EITHER IRRELEVANT OR SHOULD BE UPDATED ####
## WITH THE FINAL TRAIT-FILES ####
## 4. GEOGRAPHICAL RANGE OF EACH SPECIES ####
##   (should be redone for all names)    ####
##---------------------------------------####
# Calculate the number of cells with P/A for each species, based on the pa20k_stack
ws <- data.frame(name=names(pa20k_stack), ncell=NA)
for(i in 1:nlayers(pa20k_stack)){
  print(i)
  ws[i,"ncell"] <- freq(pa20k_stack[[i]], value=1)
}

# Rename the species to match the columns in ATA2
ws$name2 <- gsub("^.*?_", "", ws$name)
ws$name2 <- gsub('_', ' ', ws$name2)
ws$name2 <- gsub('\\.', '-', ws$name2)
ATA2$Species_syn <- gsub('\\.', '-', ATA2$Species_syn)   # Just to be sure!

 # Insert calculations in the ATA dataframe
ATA2 <- merge(ATA2, ws[,c(2,3)], all=TRUE, by.x="Species_syn", by.y="name2")
ATA2[is.na(ATA2$ncell), "ncell"] <- 0      # Insert zeros if we have no occurrence records
ATA2[is.na(ATA2$Freq), "Freq"] <- 0      # Insert zeros if we have no occurrence records

# Calculate the proportional geographic range:
ATA2$prop.ncell <- ATA2$ncell/freq(r20, value=1)

G0_ATA <- ATA2[ATA2$sum_Grouped==0, c(1,2,32:36,44:47)]
G1_ATA <- ATA2[ATA2$sum_Grouped==1, c(1,2,32:36,44:47)]
G2_ATA <- ATA2[ATA2$sum_Grouped==2, c(1,2,32:36,44:47)]
G3_ATA <- ATA2[ATA2$sum_Grouped==3, c(1,2,32:36,44:47)]

# Histograms
library(dplyr)
library(forcats)
# Frequency
{G0_ATA[G0_ATA$Freq<100,] %>%
  mutate(Species = fct_reorder(Species, Freq)) %>%
  ggplot( aes(x=Species, y=Freq)) +
  geom_bar(stat="identity") +
  ggtitle("0 trait groups, Freq<100") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) }
{G1_ATA[G1_ATA$Freq<100,] %>%
  mutate(Species = fct_reorder(Species, Freq)) %>%
  ggplot( aes(x=Species, y=Freq)) +
  geom_bar(stat="identity") +
    ggtitle("1 trait group, Freq<100") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))}
{G2_ATA[G2_ATA$Freq<100,] %>%
  mutate(Species = fct_reorder(Species, Freq)) %>%
  ggplot( aes(x=Species, y=Freq)) +
  geom_bar(stat="identity") +
    ggtitle("2 trait groups, Freq<100") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) }
{G3_ATA[G3_ATA$Freq<100,] %>%
  mutate(Species = fct_reorder(Species, Freq)) %>%
  ggplot( aes(x=Species, y=Freq)) +
  geom_bar(stat="identity") +
    ggtitle("3 trait groups, Freq<100") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) }

# Proportion of grid cells
{G0_ATA %>%
    mutate(Species = fct_reorder(Species, prop.ncell)) %>%
    ggplot( aes(x=Species, y=prop.ncell)) +
    geom_bar(stat="identity") +
    ggtitle("0 trait groups") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) }
{G1_ATA %>%
    mutate(Species = fct_reorder(Species, prop.ncell)) %>%
    ggplot( aes(x=Species, y=prop.ncell)) +
    geom_bar(stat="identity") +
    ggtitle("1 trait group") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))}
{G2_ATA %>%
    mutate(Species = fct_reorder(Species, prop.ncell)) %>%
    ggplot( aes(x=Species, y=prop.ncell)) +
    geom_bar(stat="identity") +
    ggtitle("2 trait groups") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) }
{G3_ATA %>%
    mutate(Species = fct_reorder(Species, prop.ncell)) %>%
    ggplot( aes(x=Species, y=prop.ncell)) +
    geom_bar(stat="identity") +
    ggtitle("3 trait groups") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) }

# Species with data for 0 or 1 trait groups, and a geographic range of >1/3 of the grid cells
{G0_ATA[G0_ATA$prop.ncell>0.33,] %>%
    mutate(Species = fct_reorder(Species, prop.ncell)) %>%
    ggplot( aes(x=Species, y=prop.ncell)) +
    geom_bar(stat="identity") +
    ggtitle("0 trait groups, range > 33%") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))}
{G1_ATA[G1_ATA$prop.ncell>0.33,] %>%
    mutate(Species = fct_reorder(Species, prop.ncell)) %>%
    mutate(col.bar = case_when(Height_Grouped==1 ~ "Leaf and seed",
                               Leaf_Grouped==1 ~ "Height and seed",
                               Seed_Grouped==1 ~ "Height and leaf"))  %>%
  ggplot( aes(x=Species, y=prop.ncell, fill=col.bar)) +
    geom_bar(stat="identity") +
    ggtitle("1 trait group, range > 33%") +
    scale_fill_manual("Missing trait groups", values = c("Leaf and seed" = "gray15",
                                                         "Height and seed" = "gray40",
                                                         "Height and leaf" = "gray65")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))}


## 5. Plots for ForBio ####
##---------------------####
# No. occurrences in Norway:
png("Nspecies.png", width = 16, height=19, units = "cm", pointsize = 10, res=300)
par(mar=c(0.1,0.1,0.1,0.1))
plot(nspec, main="", axes=FALSE, box=FALSE, legend=FALSE)
plot(nspec, main="", axes=FALSE, box=FALSE, legend.only=TRUE,
     legend.args=list(text="Number of vascular plant species (GBIF)", side=4, font=2, line=3),
     smallplot=c(0.55,0.6, 0.1,0.5))
dev.off()

# Example of species
par(mfrow=c(1,2))
par(mar=c(1,3,5,3))
plot(occ20k_stack[["X_Rubus_stereacanthos"]], main="Rubus stereacanthos, \n# of occurrence records", axes=F, box=F, legend=F)
      plot(occ20k_stack[["X_Rubus_stereacanthos"]], main="", axes=FALSE, box=FALSE, legend.only=TRUE,
          legend.args=list(text="No. records", side=4, font=2, line=4, cex=0.75, cex.axis=0.75),
          smallplot=c(0.55,0.6, 0.1,0.5))
plot(norway_UTM_buff, add=T, border="gray40", lty=2)
plot(occ20k_stack[["X_Bistorta_vivipara"]], main="Bistorta vivipara, \n# of occurrence records", axes=F, box=F, legend=F)
      plot(occ20k_stack[["X_Bistorta_vivipara"]], main="", axes=FALSE, box=FALSE, legend.only=TRUE,
          legend.args=list(text="No. records", side=4, font=2, line=4, cex=0.75, cex.axis=0.75),
          smallplot=c(0.55,0.6, 0.1,0.5))
plot(norway_UTM_buff, add=T, border="gray40", lty=2)

# Number of species with x trait groups:
ATA2$HiLo <- c(rep("Hi", nrow(ATA2)))  # A grouping factor for colouring
ATA2[ATA2$sum_Grouped<2 & ATA2$prop.ncell<=0.33, "HiLo"] <- "Lo"   # 'Hi' means we're including it in the analyses, 'Lo' means both low data amount and coverage

# Plot with all
ggplot(ATA2, aes(x=factor(sum_Grouped)))  +
  geom_bar(stat="count", width=0.7, fill="steelblue") +
  xlab("Number of trait groups with data") + ylab("Number of species") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text =  element_text(size = 12)) +
  coord_flip()

# Coloured by priority
ggplot(ATA2, aes(x=factor(sum_Grouped), fill=factor(HiLo)))  +
  geom_bar(stat="count", width=0.7) +
  xlab("Number of trait groups with data") + ylab("Number of species") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text =  element_text(size = 12)) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("steelblue", "red3")) +
  coord_flip()


## 6. AVAILABLE TRAIT DATA ####
##----------------------------####
# Import the species names from the "AllTraitsAvailability.csv" (I've collected all three sheets in one file)
# This should be updated according to final file with trait values
ATA2 <- read.csv2("AllTraitsAvailability.csv", header=TRUE)      # (csv2 as we're using Norwegian pc's)
str(ATA2)

# Check if all of our species exist in the GBIF data --> return species names not in the occurrence data
# The list is checked manually to find the errors and synonyms.
# The names are compared with the 'original_name' and synonym_tr8'
# column from the 'TRAITSavailable&lacking.xlsx', and compared
# with View(as.data.frame(levels(NorPlant_buff@data$species)))
# To find the appropriate GBIF names (can be double checked with
# name_backbone(name='Carex ×stenolepis', kingdom='plants')
# Unfortunately, 'The Plant List' is currently unavailable through 'Taxonstand',
# instead, I checked through Kew Gardens' 'Plants of the World' and 'name_backbone()
#library(Taxonstand)
#TPLck("xx xx")
# It turns out, that the columns "genus" and "specificEpithet" are not always gathered directly to make up the species name
# This is potentially where we encounter some issues. Therefore, I do the sorting a bit more cumbersome,
# but hopefully we'll get the correct species then. We know that we have the right species, as these were matched according to Ida's taxonKeys
NorPlant_buff$species_construct <- paste(NorPlant_buff$genus, NorPlant_buff$specificEpithet,
                                         sep = " ", collapse = NULL)
NorPlant_buff$species_construct <- as.factor(NorPlant_buff$species_construct)  # We have now also lost the 'empty' species

# Check what species are not found in the trait-file:
ATA2$Species[!(ATA2$Species %in% NorPlant_buff$species |
                 ATA2$Species %in% NorPlant_buff$species_construct)]  # The species printed here are the ones where we likely have a mismatch
# in the nomenclature

# Rename where needed - check GBIF, ThePlantList and Artsnavnebase
{ATA2$Species_syn <- as.character(ATA2$Species)
  ATA2$Species_syn[ATA2$Species_syn == 'Acinos arvensis'] <- 'Clinopodium acinos'
  ATA2$Species_syn[ATA2$Species_syn == 'Agrostis arundinacea'] <- 'Calamagrostis arundinacea'
  ATA2$Species_syn[ATA2$Species_syn == 'Agrostis canescens'] <- 'Calamagrostis canescens'
  ATA2$Species_syn[ATA2$Species_syn == 'Agrostis chalybaea'] <- 'Calamagrostis chalybaea'         # OBS! This potentially needs to be validated
  ATA2$Species_syn[ATA2$Species_syn == 'Agrostis epigejos'] <- 'Calamagrostis epigejos'        
  ATA2$Species_syn[ATA2$Species_syn == 'Agrostis lapponica'] <- 'Calamagrostis lapponica'         # From 'TRAITSavailable&lacking.xlsx'
  ATA2$Species_syn[ATA2$Species_syn == 'Agrostis neglecta'] <- 'Agrostis stolonifera'             # From 'TRAITSavailable&lacking.xlsx'
  ATA2$Species_syn[ATA2$Species_syn == 'Agrostis phragmitoides'] <- 'Calamagrostis phragmitoides'  # OBS! No synonyms found, This potentially needs to be validated
  ATA2$Species_syn[ATA2$Species_syn == 'Anagallis minima'] <- 'Lysimachia minima'
  ATA2$Species_syn[ATA2$Species_syn == 'Anisantha sterilis'] <- 'Bromus sterilis'
  ATA2$Species_syn[ATA2$Species_syn == 'Antennaria lapponica'] <- 'Antennaria alpina'
  ATA2$Species_syn[ATA2$Species_syn == 'Arctous alpinus'] <- 'Arctostaphylos alpinus'          
  ATA2$Species_syn[ATA2$Species_syn == 'Aristavena setacea'] <- 'Deschampsia setacea'
  ATA2$Species_syn[ATA2$Species_syn == 'Carex \xd7stenolepis'] <- 'Carex stenolepis'
  ATA2$Species_syn[ATA2$Species_syn == 'Carex \xd7vacillans'] <- 'Carex vacillans'
  ATA2$Species_syn[ATA2$Species_syn == 'Centaurea x moncktonii'] <- 'Centaurea moncktonii'
  ATA2$Species_syn[ATA2$Species_syn == 'Coptidium lapponicum'] <- 'Ranunculus lapponicus'      
  ATA2$Species_syn[ATA2$Species_syn == 'Diphasiastrum \xd7zeilleri'] <- 'Diphasiastrum zeilleri'
  ATA2$Species_syn[ATA2$Species_syn == 'Drymochloa sylvatica'] <- 'Festuca altissima'          
  ATA2$Species_syn[ATA2$Species_syn == 'Eleogiton fluitans'] <- 'Isolepis fluitans'            
  ATA2$Species_syn[ATA2$Species_syn == 'Eriophorum \xd7medium'] <- 'Eriophorum medium'
  ATA2$Species_syn[ATA2$Species_syn == 'Hieracium atratum '] <- 'Hieracium atratum'            # 'space' in the end
  ATA2$Species_syn[ATA2$Species_syn == 'Hieracium bifidum '] <- 'Hieracium bifidum'            
  ATA2$Species_syn[ATA2$Species_syn == 'Hieracium caesium '] <- 'Hieracium caesium'
  ATA2$Species_syn[ATA2$Species_syn == 'Hieracium crocatum '] <- 'Hieracium crocatum'
  ATA2$Species_syn[ATA2$Species_syn == 'Hieracium diaphanum '] <- 'Hieracium diaphanum'
  ATA2$Species_syn[ATA2$Species_syn == 'Hieracium dovrense '] <- 'Hieracium dovrense'
  ATA2$Species_syn[ATA2$Species_syn == 'Hieracium epimedium '] <- 'Hieracium froelichianum'    
  ATA2$Species_syn[ATA2$Species_syn == 'Hieracium murorum '] <- 'Hieracium murorum'
  ATA2$Species_syn[ATA2$Species_syn == 'Hieracium nigrescens '] <- 'Hieracium nigrescens'
  ATA2$Species_syn[ATA2$Species_syn == 'Hieracium ramosum '] <- 'Hieracium ramosum'
  ATA2$Species_syn[ATA2$Species_syn == 'Hierochloe hirta'] <- 'Anthoxanthum nitens'            # OBS! Potentially needs validation
  ATA2$Species_syn[ATA2$Species_syn == 'Mentha \xd7verticillata'] <- 'Mentha verticillata'
  ATA2$Species_syn[ATA2$Species_syn == 'Mulgedium sibiricum'] <- 'Lactuca sibirica'            
  ATA2$Species_syn[ATA2$Species_syn == 'Nigritella nigra'] <- 'Gymnadenia nigra'               
  ATA2$Species_syn[ATA2$Species_syn == 'Poa \xd7jemtlandica'] <- 'Poa jemtlandica'
  ATA2$Species_syn[ATA2$Species_syn == 'Ranunculus auricomus '] <- 'Ranunculus auricomus'
  ATA2$Species_syn[ATA2$Species_syn == 'Salix \xd7arctogena'] <- 'Salix arctogena'
  ATA2$Species_syn[ATA2$Species_syn == 'Sedum rupestre'] <- 'Petrosedum rupestre'              
  ATA2$Species_syn[ATA2$Species_syn == 'Taraxacum officinale '] <- 'Taraxacum officinale'
  ATA2$Species_syn[ATA2$Species_syn == 'Eleocharis macrostachya'] <- 'Eleocharis macrostachya'   # Correct name in all databases - genuinely no records
  ATA2$Species_syn[ATA2$Species_syn == 'Stellaria alsine var. alsine'] <- 'Stellaria alsine'
  ATA2$Species_syn[ATA2$Species_syn == 'Zostera noltii '] <- 'Zostera noltii'
  ATA2$Species_syn[ATA2$Species_syn == 'Botrychium multifidum'] <- 'Sceptridium multifidum'
  ATA2$Species_syn[ATA2$Species_syn == 'Hymenophyllum tunbrigense'] <- 'Hymenophyllum tunbrigense'   # Correct name in all databases - genuinely no records
  ATA2$Species_syn[ATA2$Species_syn == 'Lavatera arborea'] <- 'Malva arborea'
  ATA2$Species_syn[ATA2$Species_syn == 'Mycelis muralis'] <- 'Lactuca muralis'
  ATA2$Species_syn[ATA2$Species_syn == 'Listera cordata'] <- 'Neottia cordata'
  ATA2$Species_syn[ATA2$Species_syn == 'Minuartia stricta'] <- 'Sabulina stricta'
  ATA2$Species_syn[ATA2$Species_syn == 'Coeloglossum viride'] <- 'Dactylorhiza viridis'
  ATA2$Species_syn[ATA2$Species_syn == 'Listera ovata'] <- 'Neottia ovata'
  ATA2$Species_syn[ATA2$Species_syn == 'Minuartia rubella'] <- 'Sabulina rubella'
  ATA2$Species_syn[ATA2$Species_syn == 'Elytrigia juncea'] <- 'Thinopyrum junceum'             
  ATA2$Species_syn[ATA2$Species_syn == 'Atocion rupestre'] <- 'Heliosperma pusillum'           
  ATA2$Species_syn[ATA2$Species_syn == 'Glaux maritima'] <- 'Lysimachia maritima'
  ATA2$Species_syn[ATA2$Species_syn == 'Polystichum setiferum'] <- 'Polystichum setiferum'     # Correct name in all databases - genuinely no records
  ATA2$Species_syn[ATA2$Species_syn == 'Hierochloe alpina'] <- 'Anthoxanthum monticola'        
  ATA2$Species_syn[ATA2$Species_syn == 'Minuartia biflora'] <- 'Cherleria biflora'             
  ATA2$Species_syn[ATA2$Species_syn == 'Chamaepericlymenum suecicum'] <- 'Cornus suecica'
  ATA2$Species_syn[ATA2$Species_syn == 'Schedonorus giganteus'] <- 'Lolium giganteum'          
  ATA2$Species_syn[ATA2$Species_syn == 'Blysmopsis rufa'] <- 'Blysmus rufus'
  ATA2$Species_syn[ATA2$Species_syn == 'Chamerion angustifolium'] <- 'Epilobium angustifolium'
  ATA2$Species_syn[ATA2$Species_syn == 'Kobresia simpliciuscula'] <- 'Carex simpliciuscula'
  ATA2$Species_syn[ATA2$Species_syn == 'Calamistrum globuliferum'] <- 'Pilularia globulifera'  
  ATA2$Species_syn[ATA2$Species_syn == 'Ligusticum scothicum'] <- 'Ligusticum scoticum'
  ATA2$Species_syn[ATA2$Species_syn == 'Viscaria alpina'] <- 'Silene suecica'                  
  ATA2$Species_syn[ATA2$Species_syn == 'Monotropa hypopitys'] <- 'Hypopitys monotropa'
  ATA2$Species_syn[ATA2$Species_syn == 'Ophioglossum vulgatum'] <- 'Ophioglossum vulgatum'     # Correct name in all databases, no downloaded records (unsure why?)
  ATA2$Species_syn[ATA2$Species_syn == 'Lycopodium annotinum'] <- 'Spinulum annotinum'
  ATA2$Species_syn[ATA2$Species_syn == 'Bromopsis benekenii'] <- 'Bromus benekenii'
  ATA2$Species_syn[ATA2$Species_syn == 'Kobresia myosuroides'] <- 'Carex myosuroides'
  ATA2$Species_syn[ATA2$Species_syn == 'Lappula deflexa'] <- 'Hackelia deflexa'
  ATA2$Species_syn[ATA2$Species_syn == 'Seseli libanotis'] <- 'Libanotis pyrenaica'            
  ATA2$Species_syn[ATA2$Species_syn == 'Hierochloe odorata'] <- 'Anthoxanthum nitens'          # OBS!Anthoxanthum nitens is also synonym for Hierochloe hirta
  ATA2$Species_syn[ATA2$Species_syn == 'Littorella uniflora'] <- 'Littorella uniflora'         # Correct name - genuinely no downloaded records
  ATA2$Species_syn[ATA2$Species_syn == 'Bromopsis ramosa'] <- 'Bromus ramosus'
  ATA2$Species_syn[ATA2$Species_syn == 'Avenula pratensis'] <- 'Helictochloa pratensis'
  ATA2$Species_syn[ATA2$Species_syn == 'Tetragonolobus maritimus'] <- 'Lotus maritimus'
  ATA2$Species_syn[ATA2$Species_syn == 'Lychnis flos-cuculi'] <- 'Silene flos-cuculi'
  ATA2$Species_syn[ATA2$Species_syn == 'Ranunculus ficaria'] <- 'Ficaria verna'
  ATA2$Species_syn[ATA2$Species_syn == 'Anisantha tectorum'] <- 'Bromus tectorum'
  ATA2$Species_syn[ATA2$Species_syn == 'Elytrigia repens'] <- 'Elymus repens'
  ATA2$Species_syn[ATA2$Species_syn == 'Ligustrum vulgare'] <- 'Syringa vulgaris'              
  ATA2$Species_syn[ATA2$Species_syn == 'Potentilla anserina'] <- 'Argentina anserina'
  ATA2$Species_syn[ATA2$Species_syn == 'Schedonorus arundinaceus'] <- 'Scolochloa festucacea'  # OBS! Potentially doubtful
  ATA2$Species_syn[ATA2$Species_syn == 'Oxycoccus palustris'] <- 'Vaccinium oxycoccos'
}

ATA2$Species_syn <- as.factor(ATA2$Species_syn)
ATA2$Species_syn <- droplevels(ATA2$Species_syn)
ATA2$Species_syn[!(ATA2$Species_syn %in% NorPlant_buff$species |
                     ATA2$Species_syn %in% NorPlant_buff$species_construct)]

# Define a single name for future use:
NorPlant_buff$species <- as.character(NorPlant_buff$species)
NorPlant_buff$species_construct <- as.character(NorPlant_buff$species_construct)
ATA2$Species <- as.character(ATA2$Species)
ATA2$Species_syn <- as.character(ATA2$Species_syn)


NorPlant_buffa$species_ATA2 <- NA
NorPlant_buff$species_ATA2 <- ifelse(NorPlant_buff$species_construct %in% ATA2$Species_syn,
                                     paste(NorPlant_buff$species_construct),
                                     NA)
NorPlant_buff$species_ATA2 <- ifelse(is.na(NorPlant_buff$species_ATA2),
                                     paste(NorPlant_buff$species),
                                     paste(NorPlant_buff$species_ATA2))

# Calculate the frequencies of each of the downloaded species, and add that to the dataframe
freq <- as.data.frame(table(as.factor(NorPlant_buff$species_ATA2[NorPlant_buff$species_ATA2 %in% ATA2$Species_syn])))

ATA2 <- merge(ATA2, freq,
              by.x="Species_syn", by.y="Var1", all=TRUE)

# View the number of available traits and the number of occurrences
View(ATA2[,c("Species_syn", "Species", "sum_all_traits", "sum_Grouped", "sum_Selected", "Freq")])

# Visualize it in a plot
panel.cor <- function(x, y, digits=1, prefix="", cex.cor = 6)  # Function from Zuur-course for correlations in pair-plot 
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) { cex <- 0.9/strwidth(txt) } else {
    cex = cex.cor}
  text(0.5, 0.5, txt, cex = cex * r)
}
pairs(ATA2[, c("sum_all_traits", "sum_Grouped", "sum_Selected", "Freq")],
      lower.panel=panel.cor)

# How many missing species in each trait category and frequency
nrow(ATA2[ATA2$Freq==0, ])
nrow(ATA2[ATA2$sum_all_traits==0, ])
nrow(ATA2[ATA2$sum_Grouped==0, ])
nrow(ATA2[ATA2$sum_Selected==0, ])
nrow(ATA2[ATA2$Height_Grouped==0, ])
nrow(ATA2[ATA2$Leaf_Grouped==0, ])
nrow(ATA2[ATA2$Seed_Grouped==0, ])

# ForBio presentation, plot:
#barplot(table(ATA2$sum_Grouped))
ggplot(ATA2, aes(x=factor(sum_Grouped)))  +
  geom_bar(stat="count", width=0.7, fill="steelblue") +
  xlab("Number of trait groups with data") + ylab("Number of species") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text =  element_text(size = 12)) +
  coord_flip()
