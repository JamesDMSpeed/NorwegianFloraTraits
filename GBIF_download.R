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

# Make R ask for you login credentials: OBS! Make sure that these are typed in correctly, as the following functions
# will not work otherwise. R does not tell you, if you type it in wrong (it took Tanja 3 hours to figure out)
options(gbif_user=rstudioapi::askForPassword("my gbif username"))
options(gbif_email=rstudioapi::askForPassword("my registred gbif e-mail"))
options(gbif_pwd=rstudioapi::askForPassword("my gbif password"))

## 1.1 MAKE A DOWNLOAD KEY ####
##-------------------------####
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

# If the code above has been run before, just use the following:
# THIS IS THE LINE TO RUN FROM NOW ON!
NorPlant_occur <- fread("occurrence.txt")   # The file is too big to run on desktop, unfortunately

## Cite your data! 
# Finally but not at least, remember to cite your data properly:
paste("GBIF Occurrence Download", download_key[2], "accessed via GBIF.org on", Sys.Date())
    # "GBIF Occurrence Download 10.15468/dl.sxzezh accessed via GBIF.org on 2019-02-20"


## 2. SPATIAL 'PLAYING AROUND ' ####
##------------------------------####
# First, I will create a new dataframe with fewer columns to save some space
# (I'm removing the ones that I don't think we need - the original dataset is of course saved in the original .txt-file)
names(NorPlant_occur)
NorPlant <- NorPlant_occur[, -c(2:15, 17:36, 38:56, 65:71, 75:87, 90:95, 100:102, 106:122, 126:132, 137, 139:182, 185:190, 200:229, 233:237)]

# Write a .txt-file
write.table(NorPlant, file="occurrence_stripped.txt")
NorPlant <- fread("occurrence_stripped.txt")   

# Make some columns factors at once - can be modified as necessary
NorPlant[,c("basisOfRecord", "kingdom", "phylum", "class", "order",
           "family","genus", "subgenus", "specificEpithet",
           "infraspecificEpithet", "species")] <- lapply(NorPlant[,c("basisOfRecord", "kingdom", "phylum", "class", "order",
                                                                    "family","genus", "subgenus", "specificEpithet",
                                                                    "infraspecificEpithet", "species")], factor)
str(NorPlant)
 
# Keep only the vascular plants
NorPlant_vasc <- NorPlant[NorPlant$phylum=="Tracheophyta",]
NorPlant_vasc <- droplevels(NorPlant_vasc)

norway <- getData('GADM', country='Norway', level=0)    # Get the Global Administrative border of Norway
plot(norway)
norway@proj4string   # Check the CRS
norway_UTM <- spTransform(norway, CRS("+proj=utm +zone=32"))  # Reproject the CRS
norway_UTM@proj4string

# Add a 2 km buffer around the border (as was done in Ida's thesis), to allow for some spatial uncertainty - the size of this can be discussed
norway_UTM_buff <- gBuffer(norway_UTM, width = 2000)

# Make the occurrence data spatial
coordinates(NorPlant_vasc) <- ~decimalLongitude+decimalLatitude

# Define the original CRS and change it to match the norway-polygon:
proj4string(NorPlant_vasc) <- CRS("+init=epsg:4326")
NorPlant <- spTransform(NorPlant_vasc, CRS("+proj=utm +zone=32"))

# Only keep records within the Norwegian buffer-border (making sure that we do not have any "marine strays")
NorPlant_buff <- NorPlant_vasc[norway_UTM_buff,]              # 15719 records are lost
# Remove records not identified to species level
NorPlant_buff <- NorPlant_buff[!NorPlant_buff@data$species=="",]          # 126646 records lost
NorPlant_buff@data <- droplevels(NorPlant_buff@data)     # To make sure that we don't have any empty factor levels


## 2.1 MAKE A NORWAY-GRID ####
##------------------------####
# 20km grid cells
raster20k <- raster(ext=extent(NorPlant_buff), crs=crs(NorPlant_buff), res=20000, vals=1)
plot(raster20k)

raster20km <- mask(raster20k, NorPlant_buff)
plot(raster20km)

r20 <- raster20km
rm(raster20km)

# Convert raster to individual polygons
NorGrid <- rasterToPolygons(r20)

# Find out which pixel each observation belongs to
NorGrid$id <- 1:length(NorGrid)
GridPoint <- over(NorPlant_buff, NorGrid)

# Add that to the dataframe
NorPlant_buff@data$Pixelnr <- GridPoint$id

# Have a look at the potential number of species
nlevels(NorPlant_buff@data$species)   # 3668 - seems a litlle high. Likely due to species not in our 'native' list
      #View(as.data.frame(levels(NorPlant_buff@data$species)))


## 2.1.1 AVAILABLE TRAIT DATA ####
##----------------------------####
# Import the species names from the "AllTraitsAvailability.csv" (I've collected all three sheets in one file)
ATA <- read.csv2("AllTraitsAvailability.csv", header=TRUE)      # (csv2 as we're using Norwegian pc's)
str(ATA)

# Check if all of our species exist in the GBIF data --> return species names not in the occurrence data
ATA$Species[!ATA$Species %in% NorPlant_buff@data$species]   # 127 species - some are due to errors in the names. 
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

# Rename the ATA data set where appropriate (to make the names match GBIF):
{ATA$Species_syn <- as.character(ATA$Species)
ATA$Species_syn[ATA$Species_syn == 'Agrostis arundinacea'] <- 'Calamagrostis arundinacea'
ATA$Species_syn[ATA$Species_syn == 'Agrostis canescens'] <- 'Calamagrostis canescens'
ATA$Species_syn[ATA$Species_syn == 'Agrostis chalybaea'] <- 'Calamagrostis chalybaea'      # OBS! This needs to be validated
ATA$Species_syn[ATA$Species_syn == 'Agrostis epigejos'] <- 'Calamagrostis epigejos'        # OBS! Calamagrostis epigeios also a synonym
ATA$Species_syn[ATA$Species_syn == 'Agrostis lapponica'] <- 'Calamagrostis lapponica'
ATA$Species_syn[ATA$Species_syn == 'Agrostis neglecta'] <- 'Agrostis stolonifera'       
ATA$Species_syn[ATA$Species_syn == 'Agrostis phragmitoides'] <- 'Agrostis phragmitoides'   # OBS! No synonyms found
ATA$Species_syn[ATA$Species_syn == 'Anisantha sterilis'] <- 'Bromus sterilis'
ATA$Species_syn[ATA$Species_syn == 'Antennaria lapponica'] <- 'Antennaria alpina'
ATA$Species_syn[ATA$Species_syn == 'Antennaria villifera'] <- 'Antennaria lanata'
ATA$Species_syn[ATA$Species_syn == 'Arabidopsis petraea'] <- 'Arabidopsis lyrata'          # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Arctous alpinus'] <- 'Arctostaphylos alpinus'          # OBS! Needs validation --> 'Arctous alpina' found
ATA$Species_syn[ATA$Species_syn == 'Aristavena setacea'] <- 'Deschampsia setacea'
ATA$Species_syn[ATA$Species_syn == 'Atriplex hastata'] <- 'Atriplex prostrata calotheca'   # OBS! Subspecies not found
ATA$Species_syn[ATA$Species_syn == 'Atriplex lapponica'] <- 'Atriplex nudicaulis'          # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Cardamine dentata'] <- 'Cardamine pratensis'
ATA$Species_syn[ATA$Species_syn == 'Carex \xd7stenolepis'] <- 'Carex stenolepis'
ATA$Species_syn[ATA$Species_syn == 'Carex \xd7vacillans'] <- 'Carex vacillans'
ATA$Species_syn[ATA$Species_syn == 'Carex jemtlandica'] <- 'Carex lepidocarpa'             # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Centaurea x moncktonii'] <- 'Centaurea moncktonii'
ATA$Species_syn[ATA$Species_syn == 'Coptidium lapponicum'] <- 'Ranunculus lapponicus'      # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Crataegus kyrtostyla'] <- 'Crataegus kyrtostyla'       # Correct name in all sources
ATA$Species_syn[ATA$Species_syn == 'Deschampsia alpina'] <- 'Deschampsia cespitosa'        # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Diphasiastrum \xd7zeilleri'] <- 'Diphasiastrum zeilleri'
ATA$Species_syn[ATA$Species_syn == 'Draba dovrensis'] <- 'Draba glabella'                  # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Drymochloa sylvatica'] <- 'Festuca altissima'          # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Elatine orthosperma'] <- 'Elatine hydropiper'          # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Eleogiton fluitans'] <- 'Isolepis fluitans'            # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Eriophorum \xd7medium'] <- 'Eriophorum medium'
ATA$Species_syn[ATA$Species_syn == 'Hieracium argillaceum'] <- 'Hieracium lachenalii'      # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Hieracium atratum '] <- 'Hieracium atratum'            # 'space' in the end
ATA$Species_syn[ATA$Species_syn == 'Hieracium bifidum '] <- 'Hieracium bifidum'            
ATA$Species_syn[ATA$Species_syn == 'Hieracium caesium '] <- 'Hieracium caesium'
ATA$Species_syn[ATA$Species_syn == 'Hieracium crocatum '] <- 'Hieracium crocatum'
ATA$Species_syn[ATA$Species_syn == 'Hieracium diaphanum '] <- 'Hieracium diaphanum'
ATA$Species_syn[ATA$Species_syn == 'Hieracium dovrense '] <- 'Hieracium dovrense'
ATA$Species_syn[ATA$Species_syn == 'Hieracium epimedium '] <- 'Hieracium froelichianum'    # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Hieracium murorum '] <- 'Hieracium murorum'
ATA$Species_syn[ATA$Species_syn == 'Hieracium nigrescens '] <- 'Hieracium nigrescens'
ATA$Species_syn[ATA$Species_syn == 'Hieracium ramosum '] <- 'Hieracium ramosum'
ATA$Species_syn[ATA$Species_syn == 'Hierochloe hirta'] <- 'Anthoxanthum nitens'            # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Huperzia arctica'] <- 'Huperzia appressa'              # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Mentha \xd7verticillata'] <- 'Mentha verticillata'
ATA$Species_syn[ATA$Species_syn == 'Micranthes hieraciifolia'] <- 'Micranthes hieracifolia'
ATA$Species_syn[ATA$Species_syn == 'Mulgedium sibiricum'] <- 'Lactuca sibirica'            # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Nigritella nigra'] <- 'Gymnadenia nigra'               # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Papaver dahlianum'] <- 'Papaver radicatum'             # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Platanthera montana'] <- 'Platanthera chlorantha'      # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Poa \xd7jemtlandica'] <- 'Poa jemtlandica'
ATA$Species_syn[ATA$Species_syn == 'Polygonum raii'] <- 'Polygonum oxyspermum'             # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Pseudorchis straminea'] <- 'Pseudorchis albida'        # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Puccinellia capillaris'] <- 'Puccinellia distans'      # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Pyrola norvegica'] <- 'Pyrola rotundifolia'            # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Ranunculus auricomus '] <- 'Ranunculus auricomus'
ATA$Species_syn[ATA$Species_syn == 'Ranunculus subborealis'] <- 'Ranunculus propinquus'    # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Salicornia pojarkovae'] <- 'Salicornia procumbens'     # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Salix \xd7arctogena'] <- 'Salix arctogena'
ATA$Species_syn[ATA$Species_syn == 'Sedum rupestre'] <- 'Petrosedum rupestre'              # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Silene wahlbergella'] <- 'Silene uralensis'            # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Sorbus norvegica'] <- 'Sorbus obtusifolia'             # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Stellaria fennica'] <- 'Stellaria fennica'             # OBS! Multiple synonyms, both in GBIF data set (P. palustris and longipes)
ATA$Species_syn[ATA$Species_syn == 'Taraxacum officinale '] <- 'Taraxacum officinale'
ATA$Species_syn[ATA$Species_syn == 'Thalictrum kemense'] <- 'Thalictrum minus'             # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Valeriana sambucifolia'] <- 'Valeriana excelsa'        # OBS! Multiple synonyms, both in GBIF data set (V. excelsa and officinalis)
ATA$Species_syn[ATA$Species_syn == 'Carex concolor'] <- 'Carex aquatilis'                  # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Cirsium acaulon'] <- 'Cirsium acaule'
ATA$Species_syn[ATA$Species_syn == 'Eleocharis macrostachya'] <- 'Eleocharis macrostachya'   # Correct name in all databases
ATA$Species_syn[ATA$Species_syn == 'Stellaria alsine var. alsine'] <- 'Stellaria alsine'
ATA$Species_syn[ATA$Species_syn == 'Carex paupercula'] <- 'Carex magellanica'
ATA$Species_syn[ATA$Species_syn == 'Zostera noltii '] <- 'Zostera noltii'
ATA$Species_syn[ATA$Species_syn == 'Botrychium multifidum'] <- 'Sceptridium multifidum'
ATA$Species_syn[ATA$Species_syn == 'Hymenophyllum tunbrigense'] <- 'Hymenophyllum tunbrigense'   # Correct name in all databases
ATA$Species_syn[ATA$Species_syn == 'Lavatera arborea'] <- 'Malva arborea'
ATA$Species_syn[ATA$Species_syn == 'Mycelis muralis'] <- 'Lactuca muralis'
ATA$Species_syn[ATA$Species_syn == 'Listera cordata'] <- 'Neottia cordata'
ATA$Species_syn[ATA$Species_syn == 'Minuartia stricta'] <- 'Sabulina stricta'
ATA$Species_syn[ATA$Species_syn == 'Coeloglossum viride'] <- 'Dactylorhiza viridis'
ATA$Species_syn[ATA$Species_syn == 'Listera ovata'] <- 'Neottia ovata'
ATA$Species_syn[ATA$Species_syn == 'Minuartia rubella'] <- 'Sabulina rubella'
ATA$Species_syn[ATA$Species_syn == 'Ononis arvensis'] <- 'Ononis spinosa'                  # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Elytrigia juncea'] <- 'Thinopyrum junceum'             # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Atocion rupestre'] <- 'Heliosperma pusillum'           # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Acinos arvensis'] <- 'Clinopodium acinos'
ATA$Species_syn[ATA$Species_syn == 'Glaux maritima'] <- 'Lysimachia maritima'
ATA$Species_syn[ATA$Species_syn == 'Polystichum setiferum'] <- 'Polystichum setiferum'     # Correct name in all databases
ATA$Species_syn[ATA$Species_syn == 'Sagina subulata'] <- 'Sagina subulata'                 # No synonym found in dataset
ATA$Species_syn[ATA$Species_syn == 'Stellaria uliginosa'] <- 'Stellaria uliginosa'         # OBS! Stellaria alsine recognized synonym, but defined as another species too
ATA$Species_syn[ATA$Species_syn == 'Hierochloe alpina'] <- 'Anthoxanthum monticola'        # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Minuartia biflora'] <- 'Cherleria biflora'             # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Chamaepericlymenum suecicum'] <- 'Cornus suecica'
ATA$Species_syn[ATA$Species_syn == 'Schedonorus giganteus'] <- 'Lolium giganteum'          # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Blysmopsis rufa'] <- 'Blysmus rufus'
ATA$Species_syn[ATA$Species_syn == 'Chamerion angustifolium'] <- 'Epilobium angustifolium'
ATA$Species_syn[ATA$Species_syn == 'Kobresia simpliciuscula'] <- 'Carex simpliciuscula'
ATA$Species_syn[ATA$Species_syn == 'Salicornia dolichostachya'] <- 'Salicornia procumbens' # OBS! Multiple synonyms (S. oliveri)
ATA$Species_syn[ATA$Species_syn == 'Calamistrum globuliferum'] <- 'Pilularia globulifera'  # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Ligusticum scothicum'] <- 'Ligusticum scoticum'
ATA$Species_syn[ATA$Species_syn == 'Viscaria alpina'] <- 'Silene suecica'                  # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Monotropa hypopitys'] <- 'Hypopitys monotropa'
ATA$Species_syn[ATA$Species_syn == 'Ophioglossum vulgatum'] <- 'Ophioglossum vulgatum'     # Correct name in all databases
ATA$Species_syn[ATA$Species_syn == 'Potentilla tabernaemontani'] <- 'Potentilla verna'     # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Luzula nivalis'] <- 'Luzula arctica' 
ATA$Species_syn[ATA$Species_syn == 'Lycopodium annotinum'] <- 'Spinulum annotinum'
ATA$Species_syn[ATA$Species_syn == 'Bromopsis benekenii'] <- 'Bromus benekenii'
ATA$Species_syn[ATA$Species_syn == 'Carex viridula'] <- 'Carex oederi'                     # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Kobresia myosuroides'] <- 'Carex myosuroides'
ATA$Species_syn[ATA$Species_syn == 'Lappula deflexa'] <- 'Hackelia deflexa'
ATA$Species_syn[ATA$Species_syn == 'Seseli libanotis'] <- 'Libanotis pyrenaica'            # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Ranunculus confervoides'] <- 'Ranunculus trichophyllus'  # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Hierochloe odorata'] <- 'Hierochloe odorata'           # OBS!Anthoxanthum nitens is also synonym for Hierochloe hirta
ATA$Species_syn[ATA$Species_syn == 'Anagallis minima'] <- 'Lysimachia minima'
ATA$Species_syn[ATA$Species_syn == 'Callitriche hamulata'] <- 'Callitriche brutia'         # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Littorella uniflora'] <- 'Littorella uniflora'         # Correct name, no synonym in GBIF
ATA$Species_syn[ATA$Species_syn == 'Bromopsis ramosa'] <- 'Bromus ramosus'
ATA$Species_syn[ATA$Species_syn == 'Avenula pratensis'] <- 'Helictochloa pratensis'
ATA$Species_syn[ATA$Species_syn == 'Tetragonolobus maritimus'] <- 'Lotus maritimus'
ATA$Species_syn[ATA$Species_syn == 'Lychnis flos-cuculi'] <- 'Silene flos-cuculi'
ATA$Species_syn[ATA$Species_syn == 'Primula vulgaris'] <- 'Primula acaulis'
ATA$Species_syn[ATA$Species_syn == 'Ranunculus ficaria'] <- 'Ficaria verna'
ATA$Species_syn[ATA$Species_syn == 'Rumex alpestris'] <- 'Rumex scutatus'
ATA$Species_syn[ATA$Species_syn == 'Anisantha tectorum'] <- 'Bromus tectorum'
ATA$Species_syn[ATA$Species_syn == 'Elytrigia repens'] <- 'Elymus repens'
ATA$Species_syn[ATA$Species_syn == 'Ligustrum vulgare'] <- 'Syringa vulgaris'              # OBS! Synonym in databases not identical
ATA$Species_syn[ATA$Species_syn == 'Potentilla anserina'] <- 'Argentina anserina'
ATA$Species_syn[ATA$Species_syn == 'Schedonorus arundinaceus'] <- 'Scolochloa festucacea'  # OBS! Potentially doubtful
ATA$Species_syn[ATA$Species_syn == 'Oxycoccus palustris'] <- 'Vaccinium oxycoccos'
ATA$Species_syn[ATA$Species_syn == 'Helianthemum nummularium'] <- 'Helianthemum nummularium'   # OBS! No synonum found
}

ATA$Species_syn <- as.factor(ATA$Species_syn)
ATA$Species_syn <- droplevels(ATA$Species_syn)
ATA$Species_syn[!ATA$Species_syn %in% NorPlant_buff@data$species]

# Calculate the number of records for each of the species, so that this can be related to the availability of traits
View(table(NorPlant_buff@data$species))   # All species
View(table(droplevels(NorPlant_buff@data$species[NorPlant_buff@data$species %in% ATA$Species_syn])))   # Species with available traits

# Add the numbers to the ATA dataframe
ATA <- merge(ATA, table(droplevels(NorPlant_buff@data$species[NorPlant_buff@data$species %in% ATA$Species_syn])),
             by.x="Species_syn", by.y="Var1", all=TRUE)
ATA$Freq[is.na(ATA$Freq)] <- 0

# View the number of available traits and the number of occurrences
View(ATA[,c("Species_syn", "Species", "sum_all_traits", "sum_Grouped", "sum_Selected", "Freq")])

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
pairs(ATA[, c("sum_all_traits", "sum_Grouped", "sum_Selected", "Freq")],
      lower.panel=panel.cor)

# How many missing species in each trait category and frequency
nrow(ATA[ATA$Freq==0, ])
nrow(ATA[ATA$sum_all_traits==0, ])
nrow(ATA[ATA$sum_Grouped==0, ])
nrow(ATA[ATA$sum_Selected==0, ])
nrow(ATA[ATA$Height_Grouped==0, ])
nrow(ATA[ATA$Leaf_Grouped==0, ])
nrow(ATA[ATA$Seed_Grouped==0, ])

# Higlight the points/species with missing data
plot(x= ATA$Freq, y=ATA$sum_all_traits,
     xlim=c(min(ATA$Freq)-1, max(ATA$Freq)+1),
     ylim=c(min(ATA$sum_all_traits)-1, max(ATA$sum_all_traits)+1),
     cex=1, pch=17, col="gray")

plot(1, type="n",
     xlim=c(min(ATA$Freq)-1, max(ATA$Freq)+1), xlab="Freq",
     ylim=c(min(ATA$sum_all_traits)-1, max(ATA$sum_all_traits)+1), ylab="sum_all_traits") 
text(x=ATA$Freq,  y=ATA$sum_all_traits,
     labels=ATA$Species_syn,
     cex=0.5, adj=0.5, offset=0, col="gray40")  
  points(x= ATA[ATA$Seed_Grouped==0, "Freq"],
         y=ATA[ATA$Seed_Grouped==0, "sum_all_traits"],
         cex=1.25, pch=19, col="yellow")                         # No seed traits available
  points(x= ATA[ATA$Leaf_Grouped==0, "Freq"],
         y=ATA[ATA$Leaf_Grouped==0, "sum_all_traits"],
         cex=1.25, pch=1, col="forestgreen")                     # No leaf traits available
  points(x= ATA[ATA$Height_Grouped==0, "Freq"],
       y=ATA[ATA$Height_Grouped==0, "sum_all_traits"],
       cex=1, pch=4, col="blue")                                 # No height traits available
  points(x= ATA[ATA$Freq==0, "Freq"],
         y=ATA[ATA$Freq==0, "sum_all_traits"],
         cex=0.75, pch=20, col="red")                            # No observations for species name/synonym
legend("bottomright", legend=c("No seed traits", "No leaf traits", "No height traits", "No obs. for \nspecies name"),
       col=c("yellow", "forestgreen", "blue", "red"),
       pch=c(19, 1, 4, 20),
       cex=0.75)


## 3. RASTERIZE THE DATASETS       ####
## OBS! Should be redone, when we  ####
## agree on the synonym issue      ####
##---------------------------------####
# Rasterize the species occurrence data sets, both in terms of presence absence (fun='last') or number of occurrences (fun='count')
# Loop through and save files individually
# Use only the native species, for which we have data
NorPlant_ATA <- NorPlant_buff[NorPlant_buff@data$species %in% ATA$Species_syn,]
NorPlant_ATA@data <- droplevels(NorPlant_ATA@data)

for (i in 1:length(levels(as.factor(NorPlant_ATA$species)))){
  print(i)
  r20_occ<-rasterize(NorPlant_ATA[NorPlant_ATA$species==levels(as.factor(NorPlant_ATA$species))[i],],r20,field=1,fun='count')
  r20_pa<-rasterize(NorPlant_ATA[NorPlant_ATA$species==levels(as.factor(NorPlant_ATA$species))[i],],r20,field=1,fun='last')
  
  namevec_20o<-paste('rasters/occ20k/',levels(as.factor(NorPlant_ATA$species))[i],sep='_')
  namevec_20pa<-paste('rasters/pa20k/',levels(as.factor(NorPlant_ATA$species))[i],sep='_')
  
  writeRaster(r20_occ,filename=namevec_20o,format='GTiff',overwrite=T)
  writeRaster(r20_pa,filename=namevec_20pa,format='GTiff',overwrite=T)
}

# Import the rasters
# List and download files (If there are other files in the folder, need to speficiy those with string e.g. '.tif')
          # Important note: when working with the rasterstack-objects, R fetches them from the indicated location everytime
          # So do not move or rename anything while you're working!
occ20k_files<-list.files('rasters/occ20k/',full.names=T)
occ20k_stack<-stack(occ20k_files)

pa20k_files<-list.files('rasters/pa20k/',full.names=T)
pa20k_stack<-stack(pa20k_files)

rm(occ20k_files)  # Remove files we do not need anymore
rm(pa20k_files)

# Plot the rasters - if you want it all, just use e.g. plot(occ20k_stack). Here I have shown an example for a single species
# Just change the species name (but keep the format)
par(mfrow=c(1,2))
plot(occ20k_stack[["X_Potentilla_erecta"]], main="Potentilla erecta, \n# of occurrence records")
plot(pa20k_stack[["X_Potentilla_erecta"]], main="\nPresence/absence")

# Make a raster with number of species and number of observations in each grid cell
    # OBS! Takes quite a while to run, even on the server
ras_nspec <- calc(pa20k_stack, fun=sum, na.rm=TRUE)
ras_noccur <- calc(occ20k_stack, fun=sum, na.rm=TRUE)

plot(ras_nspec, main="Number of species in grid cell")
plot(ras_noccur, main="Number of occurrences in grid cell")

## 4. (VERY) PERLIMINARY MAPPING OF THE AVAILABLE TRAIT DATA ####  NO WORKING YET!
##-----------------------------------------------------------####
# The preliminary trait data is taken from the file 'TRAITSavaiable&lacking.csv'
# This is not the final analysis, it is simply done to test the code etc.
TRAITS <- read.csv("TRAITSavaiable&lacking.csv", header=TRUE, sep = ",")
TRAITS$original_name <- as.character(TRAITS$original_name)

# We have some issues with the coding of the names not being loaded properly, and I cannot figure out why that is
    # Unfortunately, 'gsub' doesn't recognize '?' as a valid character, and using it messes things up even more
    # Thus, I have replaced the original names in the problematic rows manually, to at least make the errors in the dataframes alike
    # This should changed/updated as we advance
{TRAITS[1071, "original_name"] <- 'Carex \xd7stenolepis'
  TRAITS[1072, "original_name"] <- 'Carex \xd7vacillans'
  TRAITS[1109, "original_name"] <- 'Diphasiastrum \xd7zeilleri'
  TRAITS[1132, "original_name"] <- 'Eriophorum \xd7medium'
  TRAITS[1136, "original_name"] <- 'Euphrasia foulaensis'
  TRAITS[1167, "original_name"] <- 'Mentha \xd7verticillata'
  TRAITS[1193, "original_name"] <- 'Poa \xd7jemtlandica'
  TRAITS[1219, "original_name"] <- 'Salix \xd7arctogena'}

# Add the GBIF synonyms to make it merge-able with the GBIF data set
TRAITS <- merge(TRAITS, ATA[,c("Species_syn", "Species")], by.x="original_name", by.y="Species", all=TRUE)
NorPlant_ATA@data <- merge(NorPlant_ATA@data, TRAITS, by.x="species", by.y="Species_syn", all.x=TRUE)

# Make rasters of a selected trait - one for each species - NOT WORKING YET!
for (i in 1:length(levels(as.factor(NorPlant_ATA$species)))){
  print(i)
  r20_pa_height <- rasterize(NorPlant_ATA[NorPlant_ATA$species==levels(as.factor(NorPlant_ATA$species))[i],],
                             r20, field=NorPlant_ATA[i, "canopy_height"], fun='last')
  
  namevec_20pa_height <- paste('rasters/pa20k_height/',levels(as.factor(NorPlant_ATA$species))[i],sep='_')
  
  writeRaster(r20_pa_height, filename=namevec_20pa_height, format='GTiff',overwrite=T)
}

# list and import
height_files<-list.files('rasters/pa20k_height/',full.names=T)
height_stack<-stack(height_files)



