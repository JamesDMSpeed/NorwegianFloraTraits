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
library(data.table)
library(dplyr)
library(rgeos)
library(ggplot2)
library(picante) 
library(sf)
library(FD)

##--- INTEGRATING OCCURRENCE RECORDS AND TRAIT DATA ---####
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


##--- RASTERIZATION OF DATA ---####
# Make rasters with the presence/absence of each species?
# Make a rasterStack for each trait, with a layer for each species?

# Make a grid/raster of Norway (20km grid cells)
r20 <- raster(ext=extent(norway_UTM_buff), crs=crs(norway_UTM_buff), res=20000, vals=1)
r20 <- mask(r20, norway_UTM_buff)  # Cropping the raster
plot(r20)

# Convert raster to individual polygons
NorGrid <- st_as_sf(rasterToPolygons(r20))

# Find out which pixel each observation belongs to, and save that as 'Pixelnr' in the dataframe
NorGrid$Pixelnr <- 1:nrow(NorGrid)
NorPlant_trait <- st_join(NorPlant_trait, NorGrid, join=st_within)

length(unique(NorPlant_trait$species_construct))  # 1248 species

##--- FURTHER FILTERING OF DATA? ---####
# Before rasterizing, it is worth to assess the data a little further, potentially applying some more filters,
# e.g. only keeping species with a certain number of records and adding a temporal filter:
# Number of species in this filtered dataset:
length(unique(NorPlant_trait$species_construct))  # 1248

# Before rasterizing, it is worth to assess the data a little further, potentially applying some more filters,
# e.g. only keeping species with a certain number of records and adding a temporal filter

# Temporal distrubution
barplot(table(NorPlant_trait$year))
# Taxonomic distribution
sort(table(NorPlant_trait$species_construct))   # Some species have very few records. Some of these might need to be removed
# Taxonomic and temporal distribution
mat <- as.data.frame.matrix(table(NorPlant_trait$species_construct, NorPlant_trait$year))
temp <- vector(mode="list",length=1248)
for(i in 1:1248){
  print(i)
  temp[[i]] <- mat[i,]  # Keep only years with records
}
for(i in 1:length(temp)){
  temp[[i]] <- temp[[i]][,colSums(temp[[i]] != 0) > 0]
}
names(temp) <- rownames(mat)

# Check if any of the species only have records from before 1900
mat2 <- na.omit(mat[!(mat[,102:221]>0) ,] ) # Remove species if it has any records after year 1900
mat2 <- data.frame(sp = rownames(mat), old=rowSums(mat[,c(1:101)]), new=rowSums(mat,c(102:221)))
      # Only one, which does not seem to have any records?


##--- Presence/absence of individual species ---####

# Rasterize the species occurrence data sets in terms of presence absence (fun='last' or fun='first').
# As we cannot confidently evaluate sampling effort, abundances become too uncertain for further analyses.
# Loop through and save files individually (on for each species in the trait-list AND in the GBIF data)
for (i in 1:length(levels(as.factor(NorPlant_trait$species_construct)))){
  print(i)
  r20_pa<-rasterize(NorPlant_trait[NorPlant_trait$species_construct==levels(as.factor(NorPlant_trait$species_construct))[i],],r20,field=1,fun='first')
  r20_pa[is.na(r20_pa[])] <- 0
  r20_pa <- mask(r20_pa, as(norway_UTM_buff, 'Spatial'), updatevalue=NA)
  
  namevec_20pa<-paste('/home/ahomez/t/tanjakp/export/NorwFlorTrait/PA_raster_species/',levels(as.factor(NorPlant_trait$species_construct))[i],sep='_')
  
  writeRaster(r20_pa,filename=namevec_20pa,format='GTiff',overwrite=T)
}

# Import the rasters
# List and download files (If there are other files in the folder, need to speficiy those with string e.g. '.tif')
# Important note: when working with the rasterstack-objects, R fetches them from the indicated location everytime
# So do not move or rename anything while you're working!
pa20k_files<-list.files('/home/ahomez/t/tanjakp/export/NorwFlorTrait/PA_raster_species/',full.names=T, pattern = ".tif")
pa20k_stack<-stack(pa20k_files)

# Plot the rasters - if you want it all, just use e.g. plot(pa20k_stack). Here I have shown an example for a single species
# Just change the species name (but keep the format) - here is an example plot:
plot(pa20k_stack[["X_Hepatica_nobilis"]], main="\nPresence/absence")

# Make a raster with number of species and number of observations in each grid cell
# OBS! Takes quite a long time to run, even on the server
ras_nspec <- sum(pa20k_stack, na.rm=TRUE)
writeRaster(ras_nspec,filename='/home/ahomez/t/tanjakp/export/NorwFlorTrait/nspec',format='GTiff',overwrite=T)

#ras_nspec <- raster("PA_raster_species/nspec.tif")     # If we want to import the rasters from the folder
plot(ras_nspec, main="Number of species in grid cells")

# Have a closer look at the  values, to make an estimate of where to put a potential threshold; i.e. we need
# a minimum number of species within a grid cell for analyses to make sense. Most pixels have 0, of course
barplot(table(values(ras_nspec))[-1], las=2)  # Leave out the pixels with zero (effectively NA's)
head(table(values(ras_nspec))[-1])

##--- Rasters of traits ---####
### 1. Making/importing a presence/absence matrix for all species ------------

# (skip to the end of this section to import the matrix from the folder)

# To be able to calculate the CWM for all grid cells, we need a matrix from all the .tiff-files of the 'GBIF_download.R"
# Use the RasterStack of all the .tif-rasters, as made in the section above
# Make an empty dataframe, and fill it species-by-species (columns) in a loop
CWM_species <- as.data.frame(matrix(data=NA, ncol = nlayers(pa20k_stack), nrow = ncell(pa20k_stack)))
names(CWM_species) <- names(pa20k_stack)

for(i in 1:nlayers(pa20k_stack)){
  print(i)
  CWM_species[,i] <- as.data.frame(pa20k_stack[[i]])
}      # Some of the rows only have NA-values - those are the ones outside of the (terrestrial) border

# Fix the column-/species names
names(CWM_species) <- gsub("X_", "", names(CWM_species))  # Remove the prefix
names(CWM_species) <- gsub("_", " ", names(CWM_species))  # Replace the underscore with a space

# Get the centre-coordinates of each cell, in case we need that
grid_coord <- xyFromCell(pa20k_stack[[1]], cell=c(1:ncell(pa20k_stack)))
names(grid_coord) <- c("x","y")

# Export the P/A-matrix, so that we do not have to repeat it
write.table(CWM_species, file="/home/ahomez/t/tanjakp/export/NorwFlorTrait/Presence_absence_20210218.txt", row.names=TRUE, col.names=TRUE)

# Import P/A-matrix:  
CWM_species <- read.table("/home/ahomez/t/tanjakp/export/NorwFlorTrait/Presence_absence_20210218.txt", header = TRUE, row.names = 1)
names(CWM_species) <- gsub(".", " ", names(CWM_species))  # Replace the dot with a space

# It may seem excessive to have such a large matrix, but we do need the 'empty' pixels as well for the rasterization later

### 2. Calculate mean value of traits for each grid cell   ------------
# 'functcomp' from 'FD' returns the functional composition of a set of communities, as measured by the community- level
# weighted means of trait values (CWM). The needed objects are: a matrix/dataframe containing the functional traits.
# Here we use the 'traits' dataframe (a cleaned-up version though - some species have been shown to be synonymous)
# We also need a natrix containing abundances/PA (rows as sites, columns as species) - use 'CWM_species'.
# Number and names of species must be identical

traitsFD <- traits[!(traits$Species=="Antennaria.lapponica" | # synonym with no trait values
                       traits$Species.gbif=="Anthoxanthum nitens" |  # Two species/synonyms - values should be averaged and added
                       traits$Species=="Aristavena.setacea" | # synonym with no trait values
                       traits$Species=="Ranunculus.ficaria" | # synonym with no records
                       traits$Species=="Hieracium.bifidum." | # typo, no trait values
                       traits$Species=="Hieracium.murorum." | # typo, no trait values
                       traits$Species=="Anagallis.minima" |  # synonym woth identical trait values
                       traits$Species=="Ranunculus.auricomus." |  # typo, no trait values
                       traits$Species.gbif=="Hymenophyllum tunbrigense" |  # Species with no records
                       traits$Species.gbif=="Seseli libanotis" |  # Species with no records
                       traits$Species.gbif=="Polystichum setiferum"        # Species with no records
),c(2:10,12)]
traitsFD <- rbind(traitsFD, c(mean(traits[traits$Species.gbif=="Anthoxanthum nitens","Height.gen"]),mean(traits[traits$Species.gbif=="Anthoxanthum nitens","Height.veg"]),
                              mean(traits[traits$Species.gbif=="Anthoxanthum nitens","SLA"]),mean(traits[traits$Species.gbif=="Anthoxanthum nitens","Seed.number.per.plant"]),
                              mean(traits[traits$Species.gbif=="Anthoxanthum nitens","Seed.dry.mass"]),mean(traits[traits$Species.gbif=="Anthoxanthum nitens","Leaf.dry.mass"]),
                              mean(traits[traits$Species.gbif=="Anthoxanthum nitens","Leaf.area"]),mean(traits[traits$Species.gbif=="Anthoxanthum nitens","LDMC"]),
                              mean(traits[traits$Species.gbif=="Anthoxanthum nitens","Woodiness"]), "Anthoxanthum nitens"))
rownames(traitsFD) <- traitsFD$Species.gbif
traitsFD$Species.gbif <- NULL
# Convert trait values back to numeric:
{
  traitsFD$Height.gen <- as.numeric(traitsFD$Height.gen)
  traitsFD$Height.veg <- as.numeric(traitsFD$Height.veg)
  traitsFD$SLA <- as.numeric(traitsFD$SLA)
  traitsFD$Seed.number.per.plant <- as.numeric(traitsFD$Seed.number.per.plant)
  traitsFD$Seed.dry.mass <- as.numeric(traitsFD$Seed.dry.mass)
  traitsFD$Leaf.dry.mass <- as.numeric(traitsFD$Leaf.dry.mass)
  traitsFD$Leaf.area <- as.numeric(traitsFD$Leaf.area)
  traitsFD$LDMC <- as.numeric(traitsFD$LDMC)
  traitsFD$Woodiness <- as.numeric(traitsFD$Woodiness)
}
# To make the names match perfectly, we have to rename some which includes '-':
rownames(traitsFD) <- gsub("-", ".", rownames(traitsFD))
# The species names of the objects must be in the same order - ensure this:
traitsFD <- traitsFD[order(rownames(traitsFD)),]

# Calculate the CWM of all the traits/communities using the 'FD' package - this is mostly for testing it out:
functcomp.norway <- functcomp(traitsFD, as.matrix(CWM_species), bin.num = c("Woodiness"))
functcomp.norway$Pixelnr <- as.integer(rownames(functcomp.norway))

# Calculate Functional Diversity Indices - we cannot have empty grid cells here, so remove the rows with no species observations.
# Likewise, species with no records (I am not sure how they've snuck in?) must be removed
CWM_speciesFD <- CWM_species[rowSums(is.na(CWM_species)) != ncol(CWM_species), ]
CWM_speciesFD <- CWM_speciesFD[, which(colSums(CWM_speciesFD) != 0)]
traitsFD2 <- traitsFD[rownames(traitsFD) %in% names(CWM_speciesFD),]

FD.norway <- dbFD(traitsFD2, as.matrix(CWM_speciesFD),
                  w.abun = FALSE, stand.x = TRUE,  # OBS on these ones
                  corr = c("cailliez"),            # OBS on this ones
                  calc.FRic = TRUE, m = "max", stand.FRic = FALSE,
                  scale.RaoQ = FALSE,
                  calc.FGR = FALSE,
                  clust.type = "ward",
                  calc.CWM = TRUE, CWM.type = c("dom", "all"),
                  calc.FDiv = TRUE, 
                  print.pco = FALSE, messages = TRUE)
# As I forgot to add the argument "bin.num = c("Woodiness")" somewhere, the calculations for Woodiness are not quite right here - 
# instead, ise the ones from 'functcomp.norway'

# Histogram of number of species to assess which pixels to leave out
ggplot(as.data.frame(FD.norway$nbsp), aes(x=FD.norway$nbsp)) +
  geom_histogram(binwidth = 10) +
  geom_vline(xintercept = 50, linetype="dashed", color="red") +
  geom_text(aes(x=40, label="<50 species = 19 pixels", y=20), colour="red", angle=90, check_overlap = TRUE) +
  geom_vline(xintercept = 100, linetype="dashed", color="blue") +
  geom_text(aes(x=90, label="<100 species = 39 pixels", y=20), colour="blue", angle=90, check_overlap = TRUE) +
  labs(title = "Bin size = 10", x="Number of species in pixel")

# To  add these calculations as layers in the raster, we need the 'full' dataframe, including the empty pixels.
# Convert the list elements to a useful dataframe, and take out the pixels with fewer than 50 species before joining:
FD.norway.df <- cbind(as.integer(rownames(FD.norway$CWM)),
                      FD.norway$nbsp,
                      FD.norway$sing.sp,
                      FD.norway$FRic, FD.norway$FEve,
                      FD.norway$FDiv, FD.norway$FDis,
                      FD.norway$RaoQ, FD.norway$CWM[,c(1:8)] # Remove 'Woodiness'
                      )
names(FD.norway.df) <- c("Pixelnr","nbsp", "sing.sp","FRic","FEve","FDiv","FDis","RaoQ",names(FD.norway$CWM[,c(1:8)]))
FD.norway.df <- FD.norway.df[!FD.norway.df$nbsp<50,]
FD.norway.df <- left_join(functcomp.norway[,c("Pixelnr","Woodiness")], FD.norway.df, by="Pixelnr")

# Make a raster with CWM trait values, number of species and Functional Diversity Indices
CWM_raster <- dropLayer(pa20k_stack, c(17:1248))  # OBS HERE!
CWM_raster <- setValues(CWM_raster, FD.norway.df$Height.gen, layer=1)
CWM_raster <- setValues(CWM_raster, FD.norway.df$Height.veg, layer=2)
CWM_raster <- setValues(CWM_raster, FD.norway.df$LDMC, layer=3)
CWM_raster <- setValues(CWM_raster, FD.norway.df$Leaf.area, layer=4)
CWM_raster <- setValues(CWM_raster, FD.norway.df$Leaf.dry.mass, layer=5)
CWM_raster <- setValues(CWM_raster, FD.norway.df$Seed.dry.mass, layer=6)
CWM_raster <- setValues(CWM_raster, FD.norway.df$Seed.number.per.plant, layer=7)
CWM_raster <- setValues(CWM_raster, FD.norway.df$SLA, layer=8)
CWM_raster <- setValues(CWM_raster, FD.norway.df$Woodiness, layer=9)
CWM_raster <- setValues(CWM_raster, FD.norway.df$nbsp, layer=10)
CWM_raster <- setValues(CWM_raster, FD.norway.df$sing.sp, layer=11)
CWM_raster <- setValues(CWM_raster, FD.norway.df$FRic, layer=12)
CWM_raster <- setValues(CWM_raster, FD.norway.df$FEve, layer=13)
CWM_raster <- setValues(CWM_raster, FD.norway.df$FDiv, layer=14)
CWM_raster <- setValues(CWM_raster, FD.norway.df$FDis, layer=15)
CWM_raster <- setValues(CWM_raster, FD.norway.df$RaoQ, layer=16)

names(CWM_raster) <- c("Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass","Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness",
                       "nbsp", "sing.sp","FRic","FEve","FDiv","FDis","RaoQ")
plot(CWM_raster)
plot(CWM_raster[[1:9]])
plot(CWM_raster[[10:16]])

# Export the CWM rasters
for (i in 1:nlayers(CWM_raster)){
  print(i)
  namevec_CWM <-paste('/home/ahomez/t/tanjakp/export/NorwFlorTrait/rasters_2021/', names(CWM_raster)[[i]],sep='')  # OBS on the name/path - this is in my Franklin-export folder
  writeRaster(CWM_raster[[i]],filename=namevec_CWM,format='GTiff',overwrite=T)
}

# Import the CWM-rasters
# List and download files (If there are other files in the folder, need to speficiy those with string e.g. '.tif')
# Important note: when working with the rasterstack-objects, R fetches them from the indicated location everytime
# So do not move or rename anything while you're working!
CWM_files<-list.files('/home/ahomez/t/tanjakp/export/NorwFlorTrait/rasters/',full.names=T, pattern = ".tif")
CWM_raster<-stack(CWM_files)





