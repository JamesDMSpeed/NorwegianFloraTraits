#### INTEGRATE GBIF OCCURRENCES AND FUNCTIONAL TRAITS ###

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

# Load the gbif occurrences from the .txt-file
NorPlant_occur <- fread("occurrence_10_15468_dl_sxzezh.txt")  # UPDATE THIS
# or
NorPlant <- fread("occurrence_stripped.txt")

# Keep only the vascular plants
NorPlant_vasc <- NorPlant[NorPlant$phylum=="Tracheophyta",]

# Remove "absent" records
table(NorPlant_vasc$occurrenceStatus)
NorPlant_vasc <- droplevels(NorPlant_vasc[!NorPlant_vasc$occurrenceStatus=="absent",])

### For the spatial analyses, I suggest doing most things with the 'sf'-package, as that is a lot faster than 'sp'
norway <- raster::getData('GADM', country='Norway', level=0)    # Get the Global Administrative border of Norway
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

# Only keep records within the Norwegian buffer-border (making sure that we do not have any "marine strays")
# OBS Even with 'sf' and on the server, this is a very time-consuming task!
# It might seem intuitive to use the 'st_intersection'-function - don't! That one takes a long time to run. Instead, a work-around is to use
# 'st_join' instead, as that one uses the faster 'st_intersects'  (this one takes a couple of minutes vs. hours). 'left=F' is needed to get an 
# 'inner_join', otherwise we'll get a 'left_join'
NorPlant_buff <- st_join(NorPlant_vasc, norway_UTM_buff, join=st_intersects, left=FALSE)

# Species names are not always constructed properly/have strange synonyms. Construct species names manually
NorPlant_buff$species_construct <- paste(NorPlant_buff$genus, NorPlant_buff$specificEpithet,
                                         sep = " ", collapse = NULL)
length(unique(NorPlant_buff$species_construct))
nrow(NorPlant_buff[is.na(NorPlant_buff$species_construct),])  # All have names now

# Further analyses showed some issues in the cases where "species" and "species_construct" do not match.
# Check those manually, and change those causing problems:
nrow(st_drop_geometry(NorPlant_buff[!NorPlant_buff$species == NorPlant_buff$species_construct,]))
View(unique(st_drop_geometry(NorPlant_buff[!NorPlant_buff$species == NorPlant_buff$species_construct, c("species","species_construct")])))
# Rename when necessary:
{
  NorPlant_buff$species_construct[NorPlant_buff$species_construct == "Thinopyrum juncea"] <- "Thinopyrum junceum"
  NorPlant_buff$species_construct[NorPlant_buff$species_construct == "Scolochloa arundinaceus"] <- "Scolochloa festucacea"
  NorPlant_buff$species_construct[NorPlant_buff$species_construct == "Ranunculus lapponicum"] <- "Ranunculus lapponicus"
  NorPlant_buff$species_construct[NorPlant_buff$species_construct == "Lolium giganteus"] <- "Lolium giganteum"
  NorPlant_buff$species_construct[NorPlant_buff$species_construct == "Hypopitys hypopitys"] <- "Hypopitys monotropa"
  NorPlant_buff$species_construct[NorPlant_buff$species_construct == "Heliosperma rupestre"] <- "Heliosperma pusillum"
  NorPlant_buff$species_construct[NorPlant_buff$species_construct == "Heliosperma rupestris"] <- "Heliosperma pusillum"
  NorPlant_buff$species_construct[NorPlant_buff$species_construct == "Bromus ramosa"] <- "Bromus ramosus"
  NorPlant_buff$species_construct[NorPlant_buff$species_construct == "Anthoxanthum odorata"] <- "Anthoxanthum nitens"
  NorPlant_buff$species_construct[NorPlant_buff$species_construct == "Anthoxanthum hirta"] <- "Anthoxanthum nitens"
  NorPlant_buff$species_construct[NorPlant_buff$species_construct == "Anthoxanthum alpina"] <- "Anthoxanthum monticola"
}

# Load the final trait data:
traits <- read.csv("/home/ahomez/t/tanjakp/export/NorwFlorTrait/FinalTraitTable2021_withpredictions.csv")
# There are some "broken" names in here that needs to be fixed before continuing
traits$Species.clean <- str_trim(gsub("√´", "e",
                                      gsub("\xd7","x",
                                           gsub("[.]", " ", as.character(traits$Species)))),
                                 side = "both")

# Check which of the names in the trait-data are not found within the occurrence data, and save it as a list
    # Having it as a list allows for unequal number of columns, and thus avoids mismatching
not.found <- vector(mode="list",
                    length=length(traits[!traits$Species.clean %in% unique(NorPlant_buff$species_construct), "Species.clean"]))
names(not.found) <- traits[!traits$Species.clean %in% unique(NorPlant_buff$species_construct), "Species.clean"]

# Run the names of the traits through GBIF's (updated) backbone taxonomy, to get the right synonyms (again)
for(i in 1:length(not.found)){
  print(i)
  print(names(not.found[i]))
  not.found[[i]] <- rgbif::name_backbone(name = names(not.found[i]), rank = "species")
}

# Convert to a dataframe
not.found_df <- do.call(plyr::rbind.fill, not.found)
not.found_df$traits.name <- names(not.found)

# We have 68 species names with potential synonyms or other issues. Look through them to find and fix the issues, and add
# the correct (GBIF appropriate) names to the trait-dataframe - this has been done manually to make sure that all species
# have been checked and re-named properly
traits$Species.gbif <- traits$Species.clean
# Rename where needed:
{
traits$Species.gbif[traits$Species.gbif == "Ligusticum scothicum"] <- "Ligusticum scoticum"     # FUZZY
traits$Species.gbif[traits$Species.gbif == "Seseli libanotis"] <- "Seseli libanotis"            # Name correct, no records
traits$Species.gbif[traits$Species.gbif == "Centaurea x moncktonii"] <- "Centaurea moncktonii"  # Accepted, remove 'x'
traits$Species.gbif[traits$Species.gbif == "Mulgedium sibiricum"] <- "Lactuca sibirica"         # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Mycelis muralis"] <- "Lactuca muralis"              # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Lappula deflexa"] <- "Hackelia deflexa"             # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Atocion rupestre"] <- "Heliosperma pusillum"        # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Lychnis flos-cuculi"] <- "Silene flos-cuculi"       # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Minuartia biflora"] <- "Cherleria biflora"          # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Minuartia rubella"] <- "Sabulina rubella"           # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Minuartia stricta"] <- "Sabulina stricta"           # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Viscaria alpina"] <- "Silene suecica"               # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Chamaepericlymenum suecicum"] <- "Cornus suecica"   # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Sedum rupestre"] <- "Petrosedum rupestre"           # Not recognised at all, SYNONYM
traits$Species.gbif[traits$Species.gbif == "Arctous alpinus"] <- "Arctostaphylos alpinus"       # FUZZY and SYNONYM
traits$Species.gbif[traits$Species.gbif == "Monotropa hypopitys"] <- "Hypopitys monotropa"      # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Oxycoccus palustris"] <- "Vaccinium oxycoccos"      # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Tetragonolobus maritimus"] <- "Lotus maritimus"     # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Acinos arvensis"] <- "Clinopodium acinos"           # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Diphasiastrum xzeilleri"] <- "Diphasiastrum zeilleri"  # Accepted, remove 'x'
traits$Species.gbif[traits$Species.gbif == "Lycopodium annotinum"] <- "Spinulum annotinum"      # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Lavatera arborea"] <- "Malva arborea"               # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Blysmopsis rufa"] <- "Blysmus rufus"                # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Carex xstenolepis"] <- "Carex stenolepis"           # Accepted, remove 'x'
traits$Species.gbif[traits$Species.gbif == "Eleogiton fluitans"] <- "Isolepis fluitans"         # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Kobresia myosuroides"] <- "Carex myosuroides"       # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Kobresia simpliciuscula"] <- "Carex simpliciuscula" # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Coeloglossum viride"] <- "Dactylorhiza viridis"     # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Listera cordata"] <- "Neottia cordata"              # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Listera ovata"] <- "Neottia ovata"                  # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Nigritella nigra"] <- "Gymnadenia nigra"            # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Agrostis arundinacea"] <- "Calamagrostis arundinacea"  # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Agrostis canescens"] <- "Calamagrostis canescens"   # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Agrostis chalybaea"] <- "Calamagrostis chalybaea"   # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Agrostis epigejos"] <- "Calamagrostis epigejos"     # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Agrostis lapponica"] <- "Calamagrostis lapponica"   # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Agrostis neglecta"] <- "Calamagrostis neglecta"     # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Agrostis phragmitoides"] <- "Calamagrostis phragmitoides"   # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Anisantha sterilis"] <- "Bromus sterilis"           # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Anisantha tectorum"] <- "Bromus tectorum"           # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Avenula pratensis"] <- "Helictochloa pratensis"     # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Bromopsis benekenii"] <- "Bromus benekenii"         # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Bromopsis ramosa"] <- "Bromus ramosus"              # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Drymochloa sylvatica"] <- "Festuca altissima"       # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Elytrigia juncea"] <- "Thinopyrum junceum"          # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Elytrigia repens"] <- "Elymus repens"               # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Hierochloe alpina"] <- "Anthoxanthum monticola"     # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Hierochloe hirta"] <- "Anthoxanthum nitens"                         ##### ASK KATA!
traits$Species.gbif[traits$Species.gbif == "Hierochloe odorata"] <- "Anthoxanthum nitens"                       ##### ASK KATA!
traits$Species.gbif[traits$Species.gbif == "Poa xjemtlandica"] <- "Poa jemtlandica"             # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Schedonorus arundinaceus"] <- "Scolochloa festucacea"  # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Schedonorus giganteus"] <- "Lolium giganteum"       # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Ligustrum vulgare"] <- "Syringa vulgaris"           # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Chamerion angustifolium"] <- "Epilobium angustifolium"  # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Littorella uniflora"] <- "Plantago uniflora"        # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Botrychium multifidum"] <- "Sceptridium multifidum" # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Anagallis minima"] <- "Lysimachia minima"                           ##### ASK KATA!
traits$Species.gbif[traits$Species.gbif == "Glaux maritima"] <- "Lysimachia maritima"           # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Coptidium lapponicum"] <- "Ranunculus lapponicus"   # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Potentilla anserina"] <- "Argentina anserina"       # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Salix xarctogena"] <- "Salix arctogena"             # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Antennaria lapponica"] <- "Antennaria alpina"                       #### ASK KATA!
traits$Species.gbif[traits$Species.gbif == "Aristavena setacea"] <- "Deschampsia setacea"                       #### ASK KATA!
traits$Species.gbif[traits$Species.gbif == "Calamistrum globuliferum"] <- "Pilularia globulifera" # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Hymenophyllum tunbrigense"] <- "Hymenophyllum tunbrigense"  # Accepted species, no records
traits$Species.gbif[traits$Species.gbif == "Mentha xverticillata"] <- "Mentha verticillata"       # SYNONYM
traits$Species.gbif[traits$Species.gbif == "Polystichum setiferum"] <- "Polystichum setiferum"          # Accepted, no records
traits$Species.gbif[traits$Species.gbif == "Ranunculus ficaria"] <- "Ficaria verna"            #### ASK KATA!
}

# Check if some are still missing
length(traits[!traits$Species.gbif %in% unique(NorPlant_buff$species_construct), "Species.gbif"])  # 3
View(traits[!traits$Species.gbif %in% unique(NorPlant_buff$species_construct),])

# Combine occurrence records and trait data - keep only records of species from the trait-list:
NorPlant_trait <- left_join(NorPlant_buff[NorPlant_buff$species_construct %in% traits$Species.gbif,],
                            traits,
                            by=c("species_construct"="Species.gbif"))

# Number of species in this filtered dataset:
length(unique(NorPlant_trait$species_construct))  # 1248

# Make rasters with the presence/absence of each species?
# Make a rasterStack for each trait, with a layer for each species?

