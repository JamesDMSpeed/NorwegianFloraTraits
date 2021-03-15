#### INTEGRATE GBIF OCCURRENCES AND FUNCTIONAL TRAITS ###

# Load packages
library(rgbif, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(stringr, lib.loc = "/home/ahomez/t/tanjakp/export/library") # string manipulations (not needed, may also be done by base R)
library(rio, lib.loc = "/home/ahomez/t/tanjakp/export/library")     # data import (not needed, may also be done by base R)
library(dplyr, lib.loc = "/home/ahomez/t/tanjakp/export/library")   # for data-wrangling
library(wicket, lib.loc = "/home/ahomez/t/tanjakp/export/library")  # check WKT strings
library(sp, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(maptools, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(raster, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(rgdal, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(data.table, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(rgeos, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(ggplot2, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(picante, lib.loc = "/home/ahomez/t/tanjakp/export/library") 
library(sf, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(FD, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(dismo, lib.loc = "/home/ahomez/t/tanjakp/export/library")

##--- 1. INTEGRATING OCCURRENCE RECORDS AND TRAIT DATA ---####
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

##-----------------------------------------------------####
##--- 2. RASTERIZATION OF DATA ---####
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

##--- 2.1 FURTHER FILTERING OF DATA? ---####
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


##--- 2.2 Presence/absence of individual species ---####

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
plot(pa20k_stack[["X_Anemone_nemorosa"]], main="\nPresence/absence (Anemone_nemorosa)")

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

##--- 2.3 Rasters of traits ---####
##--- 2.3.1 Making/importing a presence/absence matrix for all species ------------

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

##--- 2.3.2 Calculate mean value of traits for each grid cell   ------------
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

##--- 2.3.2.1 Number of PCoA axes ---####
# To evaluate if we are having issues due to the number of retained PCoA axes in the FD calculations, explore the
# PCoA manually ('FD' calls 'dudi.pco', so use the same function here). From previous tries, we need to add a correction
# to the Euclidian distance matrix (either cailliez or lingoes)
distmat.traits <- cailliez(dist(traitsFD2, method = "euclidean"))
PCoA.9 <- dudi.pco(distmat.traits, scannf = FALSE, nf = 9, full = FALSE)

# Check for outliers in any of the axes in either of the ordinations - OBS This does not reveal species, only the position
Mydotplot(PCoA.9$li[,c("A1","A2","A3","A4","A5","A6","A7","A8","A9")])

# Outliers in the traits in general:
Mydotplot(traitsFD2)

# Some of the species are outliers in the raw trait data as well.
# It seems that some of our issues are either in species which are outliers in the traits and/or the PCoA.
# Try and re-do the ordination with after removing the (traitbased) outliers
traitsFD3 <- traitsFD2[!(traitsFD2$Leaf.dry.mass>10000 |
                                          traitsFD2$Seed.dry.mass>1000 |
                                          traitsFD2$Seed.number.per.plant>45000000),]  
traitsFD3 <- traitsFD3[complete.cases(traitsFD3),]  # To avoid weirdly appearing NA's
Mydotplot(traitsFD3)
distmat.traits2 <- (dist(traitsFD3, method = "euclidean"))
PCoA.9.2 <- dudi.pco(distmat.traits2, scannf = FALSE, full = TRUE)

# Check for outliers in any of the axes in either of the ordinations - OBS This does not reveal species, only the position
Mydotplot(PCoA.9.2$li[,c("A1","A2")])

### Remove species with only woodiness data - these might cause issues. We don't want to remove the outliers
traitsFD4 <- traitsFD2[!(is.na(traitsFD2$Height.gen)),]
distmat.traits <- dist(traitsFD4, method = "euclidean")
PCoA.test1 <- dudi.pco(distmat.traits, scannf = FALSE, nf = 9, full = FALSE)
# Check for outliers in any of the axes in either of the ordinations - OBS This does not reveal species, only the position
Mydotplot(PCoA.test1$li)

### Remove species with any kind of missing data:
traitsFD5 <- traitsFD2[complete.cases(traitsFD2),]
distmat.traits <- dist(traitsFD5, method = "euclidean")
PCoA.test2 <- dudi.pco(distmat.traits, scannf = FALSE, nf = 9, full = FALSE)
# Check for outliers in any of the axes in either of the ordinations - OBS This does not reveal species, only the position
Mydotplot(PCoA.test2$li)


##--- 2.3.2.2 Calculation of Functional Diversity ---####
## INCL. OUTLIERS, NON-STANDARDISED FRic
{
  FD.norway <- dbFD(traitsFD2, as.matrix(CWM_speciesFD),
                  w.abun = FALSE, stand.x = TRUE,  # OBS on these ones
                  corr = c("cailliez"),            # OBS on this ones
                  calc.FRic = TRUE, m = "max", stand.FRic = FALSE,
                  scale.RaoQ = FALSE,
                  calc.FGR = FALSE,
                  clust.type = "ward",
                  calc.CWM = TRUE, CWM.type = c("dom", "all"),
                  calc.FDiv = TRUE, 
                  print.pco = TRUE, messages = TRUE)
# As I forgot to add the argument "bin.num = c("Woodiness")" somewhere, the calculations for Woodiness are not quite right here - 
# instead, use the ones from 'functcomp.norway'

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
                      FD.norway$RaoQ, FD.norway$CWM[,c(1:8)] # Remove 'Woodiness' - a mistake has happened in these calculations
                      )
names(FD.norway.df) <- c("Pixelnr","nbsp", "sing.sp","FRic","FEve","FDiv","FDis","RaoQ",names(FD.norway$CWM[,c(1:8)]))
FD.norway.df <- FD.norway.df[!FD.norway.df$nbsp<100,]
FD.norway.df <- left_join(functcomp.norway[,c("Pixelnr","Woodiness")], FD.norway.df, by="Pixelnr")  # Add the correct calculations of 'Woodiness'
      # Remove 'Woodiness' if the Pixel was not in FD.norway.df:
        FD.norway.df[is.na(FD.norway.df$nbsp),"Woodiness"] <- NA

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

# Check correlation of the traits and FD metrics
pairs(FD.norway.df[,-1], lower.panel = panel.cor)  # The function making the lower panel is from the code we got from the Zuur GLMM-course

# There is quite a split in FRic - try and see what happens if we either standardise ('stand.FRic=T') or log-transform the output:
CWM_raster$FRic.log <- log(CWM_raster$FRic)
par(mfrow=c(1,2))
plot(CWM_raster$FRic, main="FRic")
plot(CWM_raster$FRic.log, main="log(FRic)")
}
## INCL. OUTLIERS, STANDARDISED FRic
{
# Calculate with a standardised FRic  # OBS! 'Woodiness will still be wrong here!
FD.norway.Fric <- dbFD(traitsFD2, as.matrix(CWM_speciesFD),
                  w.abun = FALSE, stand.x = TRUE,  # OBS on these ones
                  corr = c("cailliez"),            # OBS on this ones
                  calc.FRic = TRUE, m = "max", stand.FRic = TRUE,
                  scale.RaoQ = FALSE,
                  calc.FGR = FALSE,
                  clust.type = "ward",
                  calc.CWM = TRUE, CWM.type = c("dom", "all"),
                  calc.FDiv = TRUE, 
                  print.pco = TRUE, messages = TRUE)

# As I could not add the argument "bin.num = c("Woodiness")" somewhere, the calculations for Woodiness are not quite right here - 
# instead, use the ones from 'functcomp.norway'

# Histogram of number of species to assess which pixels to leave out
ggplot(as.data.frame(FD.norway.Fric$nbsp), aes(x=FD.norway.Fric$nbsp)) +
  geom_histogram(binwidth = 10) +
  geom_vline(xintercept = 50, linetype="dashed", color="red") +
  geom_text(aes(x=40, label="<50 species = 19 pixels", y=20), colour="red", angle=90, check_overlap = TRUE) +
  geom_vline(xintercept = 100, linetype="dashed", color="blue") +
  geom_text(aes(x=90, label="<100 species = 39 pixels", y=20), colour="blue", angle=90, check_overlap = TRUE) +
  labs(title = "Bin size = 10", x="Number of species in pixel")

# To  add these calculations as layers in the raster, we need the 'full' dataframe, including the empty pixels.
# Convert the list elements to a useful dataframe, and take out the pixels with fewer than 50 species before joining:
FD.norway.df <- cbind(as.integer(rownames(FD.norway.Fric$CWM)),
                      FD.norway.Fric$nbsp,
                      FD.norway.Fric$sing.sp,
                      FD.norway.Fric$FRic, FD.norway.Fric$FEve,
                      FD.norway.Fric$FDiv, FD.norway.Fric$FDis,
                      FD.norway.Fric$RaoQ, FD.norway.Fric$CWM[,c(1:8)] # Remove 'Woodiness' - a mistake has happened in these calculations
)
names(FD.norway.df) <- c("Pixelnr","nbsp", "sing.sp","FRic","FEve","FDiv","FDis","RaoQ",names(FD.norway$CWM[,c(1:8)]))
FD.norway.df <- FD.norway.df[!FD.norway.df$nbsp<100,]
FD.norway.df <- left_join(functcomp.norway[,c("Pixelnr","Woodiness")], FD.norway.df, by="Pixelnr")  # Add the correct calculations of 'Woodiness'
# Remove 'Woodiness' if the Pixel was not in FD.norway.df:
FD.norway.df[is.na(FD.norway.df$nbsp),"Woodiness"] <- NA
FD.norway.df$FRic.log <- log(FD.norway.df$FRic)   # Add log-trandformed FRic

# Make a raster with CWM trait values, number of species and Functional Diversity Indices
CWM_raster <- dropLayer(pa20k_stack, c(18:1248))  # OBS HERE!
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
CWM_raster <- setValues(CWM_raster, FD.norway.df$FRic.log, layer=13)
CWM_raster <- setValues(CWM_raster, FD.norway.df$FEve, layer=14)
CWM_raster <- setValues(CWM_raster, FD.norway.df$FDiv, layer=15)
CWM_raster <- setValues(CWM_raster, FD.norway.df$FDis, layer=16)
CWM_raster <- setValues(CWM_raster, FD.norway.df$RaoQ, layer=17)

names(CWM_raster) <- c("Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass","Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness",
                       "nbsp", "sing.sp","FRic", "FRic.log","FEve","FDiv","FDis","RaoQ")
plot(CWM_raster)
plot(CWM_raster[[1:9]])
plot(CWM_raster[[10:17]])

# Export the CWM rasters
for (i in 1:nlayers(CWM_raster)){
  print(i)
  namevec_CWM <-paste('/home/ahomez/t/tanjakp/export/NorwFlorTrait/rasters_20210225/', names(CWM_raster)[[i]],sep='')  # OBS on the name/path - this is in my Franklin-export folder
  writeRaster(CWM_raster[[i]],filename=namevec_CWM,format='GTiff',overwrite=T)
}

# Import the CWM-rasters
# List and download files (If there are other files in the folder, need to speficiy those with string e.g. '.tif')
# Important note: when working with the rasterstack-objects, R fetches them from the indicated location everytime
# So do not move or rename anything while you're working!
CWM_files<-list.files('/home/ahomez/t/tanjakp/export/NorwFlorTrait/rasters/',full.names=T, pattern = ".tif")
CWM_raster<-stack(CWM_files)

# Check correlation of the traits and FD metrics
pairs(FD.norway.df[,-1], lower.panel = panel.cor)  # The function making the lower panel is from the code we got from the Zuur GLMM-course

# There is quite a split in FRic 
par(mfrow=c(1,2))
plot(CWM_raster$FRic, main="FRic")
plot(CWM_raster$FRic.log, main="log(FRic)")

# Unfortunately, standardising FRic does not seem to have made any kind of difference, unfortunately.
# Histogram of FRic
ggarrange(ggplot(FD.norway.df, aes(x=FRic)) +
  geom_histogram(binwidth = 0.0001, alpha=0.5) +
  labs(title = "Bin size = 0.0001", x="FRic"),
ggplot(FD.norway.df, aes(x=FRic.log)) +
  geom_histogram(binwidth = 0.25, fill="red", alpha=0.5) +
  labs(title = "Bin size = 0.25", x="log(FRic)"),
nrow = 1, ncol=2)

# Point plot
ggarrange(ggplot(data=FD.norway.df[!is.na(FD.norway.df$nbsp),], aes(x=nbsp, y=FRic)) +
            geom_point() +
            xlab("#Species") + ylab("log(FRic)"),
          ggplot(data=FD.norway.df[!is.na(FD.norway.df$nbsp),], aes(x=nbsp, y=FRic.log)) +
            geom_point() +
            xlab("#Species") + ylab("log(FRic)"),
          ncol=2, nrow=1)

# Does this correlate with species richness in any way:
plot(x=FD.norway.df$nbsp, y=FD.norway.df$FRic)
plot(x=FD.norway.df$nbsp, y=FD.norway.df$FRic.log)

# Can we fit an asymptotic function to either and look at the residuals?
asymp1 <- nls(FRic.log ~ SSasymp(as.numeric(nbsp), Asym, R0, lrc), data = FD.norway.df[!is.na(FD.norway.df$nbsp),])
asymp2 <- nls(FRic ~ SSasymp(as.numeric(nbsp), Asym, R0, lrc), data = FD.norway.df[!is.na(FD.norway.df$nbsp),],
              start = list(Asym=0.015, R0=0.001, lrc=100))
# Cannot be fitted using the raw data

ggplot(data=FD.norway.df[!is.na(FD.norway.df$nbsp),], aes(x=nbsp, y=FRic.log)) +
  geom_point() +
  #geom_smooth(color="Blue", se=F) +
  geom_smooth(method="nls", formula=y~SSasymp(x, Asym, R0, lrc), color="red", se=F, fullrange=T) +
  geom_hline(color="green", yintercept=-9.649) +
  #scale_x_continuous(limits=c(50,400)) +
  xlab("#Species") + ylab("log(FRic")
# Seems odd?

pred.asymp1 <- predict(asymp1, newdata=FD.norway.df, se.fit=T)
res.asymp1 <- FD.norway.df$FRic.log - pred.asymp1

# Create a raster and plot:
res_raster <- dropLayer(pa20k_stack, c(2:1248))  # OBS HERE!
res_raster <- setValues(res_raster, c(res.asymp1), layer=1)
plot(res_raster)

# Histograms of all other indices and species richness
ggarrange(ggplot(FD.norway.df, aes(x=nbsp)) +
            geom_histogram() +
            labs(x="Number of species"),
          ggplot(FD.norway.df, aes(x=FRic)) +
            geom_histogram() +
            labs(x="FRic"),
          ggplot(FD.norway.df, aes(x=FRic.log)) +
            geom_histogram() +
            labs(x="log(FRic)"),
          ggplot(FD.norway.df, aes(x=FEve)) +
            geom_histogram() +
            labs(x="FEve"),
          ggplot(FD.norway.df, aes(x=FDiv)) +
            geom_histogram() +
            labs(x="FDiv"),
          ggplot(FD.norway.df, aes(x=FDis)) +
            geom_histogram() +
            labs(x="FDis"),
          nrow = 2, ncol=3)
}
## EXCL. OUTLIERS, standardised FRic
{
  # Subset the CWM_speciesFD to match the new trait data frame
  CWM_speciesFD2 <- CWM_speciesFD[which(rownames(CWM_speciesFD) %in% rownames(CWM.df)),
                                  which(colnames(CWM_speciesFD) %in% rownames(traitsFD3))]
  
    # Calculate with a standardised FRic  # OBS! 'Woodiness will still be wrong here!
  # I have here tried to reduce the number of dimensions in the PCoA to m=2
  FD.norway.no <- dbFD(traitsFD3, as.matrix(CWM_speciesFD2),
                         w.abun = FALSE, stand.x = TRUE,  # OBS on these ones
                         #corr = c("cailliez"),            # OBS on this ones
                         calc.FRic = TRUE, m = 2, stand.FRic = TRUE,
                         scale.RaoQ = FALSE,
                         calc.FGR = FALSE,
                         clust.type = "ward",
                         calc.CWM = TRUE, CWM.type = c("dom", "all"),
                         calc.FDiv = TRUE, 
                         print.pco = TRUE, messages = TRUE)
  
  # As I could not add the argument "bin.num = c("Woodiness")" somewhere, the calculations for Woodiness are not quite right here - 
  # instead, use the ones from 'functcomp.norway'
  # To  add these calculations as layers in the raster, we need the 'full' dataframe, including the empty pixels.
  # Convert the list elements to a useful dataframe, and take out the pixels with fewer than 50 species before joining:
  FD.norway.df.no <- cbind(as.integer(rownames(FD.norway.no$CWM)),
                           FD.norway.no$nbsp,
                           FD.norway.no$sing.sp,
                           FD.norway.no$FRic, FD.norway.no$FEve,
                           FD.norway.no$FDiv, FD.norway.no$FDis,
                           FD.norway.no$RaoQ, FD.norway.no$CWM[,c(1:8)] # Remove 'Woodiness' - a mistake has happened in these calculations
  )
  names(FD.norway.df.no) <- c("Pixelnr","nbsp", "sing.sp","FRic","FEve","FDiv","FDis","RaoQ",names(FD.norway$CWM[,c(1:8)]))
  FD.norway.df.no <- left_join(functcomp.norway[,c("Pixelnr","Woodiness")], FD.norway.df.no, by="Pixelnr")  # Add the correct calculations of 'Woodiness'
  # Remove 'Woodiness' if the Pixel was not in FD.norway.df:
  FD.norway.df.no[is.na(FD.norway.df.no$nbsp),"Woodiness"] <- NA
  FD.norway.df.no$FRic.log <- log(FD.norway.df.no$FRic)   # Add log-trandformed FRic
  
  # Make a raster with CWM trait values, number of species and Functional Diversity Indices
  CWM_raster_no <- dropLayer(pa20k_stack, c(18:1248))  # OBS HERE!
  CWM_raster_no <- setValues(CWM_raster_no, FD.norway.df.no$Height.gen, layer=1)
  CWM_raster_no <- setValues(CWM_raster_no, FD.norway.df.no$Height.veg, layer=2)
  CWM_raster_no <- setValues(CWM_raster_no, FD.norway.df.no$LDMC, layer=3)
  CWM_raster_no <- setValues(CWM_raster_no, FD.norway.df.no$Leaf.area, layer=4)
  CWM_raster_no <- setValues(CWM_raster_no, FD.norway.df.no$Leaf.dry.mass, layer=5)
  CWM_raster_no <- setValues(CWM_raster_no, FD.norway.df.no$Seed.dry.mass, layer=6)
  CWM_raster_no <- setValues(CWM_raster_no, FD.norway.df.no$Seed.number.per.plant, layer=7)
  CWM_raster_no <- setValues(CWM_raster_no, FD.norway.df.no$SLA, layer=8)
  CWM_raster_no <- setValues(CWM_raster_no, FD.norway.df.no$Woodiness, layer=9)
  CWM_raster_no <- setValues(CWM_raster_no, FD.norway.df.no$nbsp, layer=10)
  CWM_raster_no <- setValues(CWM_raster_no, FD.norway.df.no$sing.sp, layer=11)
  CWM_raster_no <- setValues(CWM_raster_no, FD.norway.df.no$FRic, layer=12)
  CWM_raster_no <- setValues(CWM_raster_no, FD.norway.df.no$FRic.log, layer=13)
  CWM_raster_no <- setValues(CWM_raster_no, FD.norway.df.no$FEve, layer=14)
  CWM_raster_no <- setValues(CWM_raster_no, FD.norway.df.no$FDiv, layer=15)
  CWM_raster_no <- setValues(CWM_raster_no, FD.norway.df.no$FDis, layer=16)
  CWM_raster_no <- setValues(CWM_raster_no, FD.norway.df.no$RaoQ, layer=17)
  
  names(CWM_raster_no) <- c("Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass","Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness",
                         "nbsp", "sing.sp","FRic", "FRic.log","FEve","FDiv","FDis","RaoQ")
  plot(CWM_raster_no)
  plot(CWM_raster_no[[1:9]])
  plot(CWM_raster_no[[10:17]]) 
  
  # Histogram of FRic
  ggplot(FD.norway.df.no, aes(x=FRic)) +
              geom_histogram(binwidth = 0.05) +
              labs(title = "Bin size = 0.05", x="FRic")
  }
## EXCL SPECIES WITH ONLY WOODINESS DATA
{
# Remove species with only woodiness data - these might cause issues. We don't want to remove the outliers
traitsFD4 <- traitsFD2[!(is.na(traitsFD2$Height.gen)),]

# Subset the CWM_speciesFD2 to match traitsFD4
CWM_speciesFD4 <- CWM_speciesFD[which(rownames(CWM_speciesFD) %in% rownames(CWM.df)),
                                which(colnames(CWM_speciesFD) %in% rownames(traitsFD4))]

# As I cannot not add the argument "bin.num = c("Woodiness")" somewhere, the calculations for Woodiness a bit obscure.
# A solution seems to be setting  CWM.type=c("all"), and then use
# Woodiness_1; that gives the abundance of '1', which is the same as the mean value.
### OBS! R craches when doing the calculations with 'm="max"'; this is noted in the vignette as a common error:
### "Dimensionality reduction was required. The last 1103 PCoA axes (out of 1202 in total) were removed"
### The package vignette suggests to reduce the dimensionality - try setting this to 'min' a (somewhat) arbitrary number
### I here try to base it on the warning above: and keep only 6 axes (1202-1196)

# Calculate with a standardised FRic
{
FD.n.o <- dbFD(traitsFD4, as.matrix(CWM_speciesFD4),
                     w.abun = FALSE, stand.x = TRUE,  # OBS on these ones
                     corr = c("cailliez"),            # OBS on this one
                     calc.FRic = TRUE, m = 6, stand.FRic = TRUE,
                     scale.RaoQ = FALSE,
                     calc.FGR = FALSE,
                     clust.type = "ward",
                     calc.CWM = TRUE, CWM.type = c("all"), 
                     calc.FDiv = TRUE, 
                     print.pco = TRUE, messages = TRUE)
  }

# Try another number of dimensions (the correct number: 1202-1103): It crashes!
{FD.n.o_99 <- dbFD(traitsFD4, as.matrix(CWM_speciesFD4),
               w.abun = FALSE, stand.x = TRUE,  # OBS on these ones
               corr = c("cailliez"),            # OBS on this one
               calc.FRic = TRUE, m = 99, stand.FRic = TRUE,
               scale.RaoQ = FALSE,
               calc.FGR = FALSE,
               clust.type = "ward",
               calc.CWM = TRUE, CWM.type = c("all"), 
               calc.FDiv = TRUE, 
               print.pco = TRUE, messages = TRUE)}

# Try with "min" dimensions:
FD.n.o_min <- dbFD(traitsFD4, as.matrix(CWM_speciesFD4),
                  w.abun = FALSE, stand.x = TRUE,  # OBS on these ones
                  corr = c("cailliez"),            # OBS on this one
                  calc.FRic = TRUE, m = "min", stand.FRic = TRUE,
                  scale.RaoQ = FALSE,
                  calc.FGR = FALSE,
                  clust.type = "ward",
                  calc.CWM = TRUE, CWM.type = c("all"), 
                  calc.FDiv = TRUE, 
                  print.pco = TRUE, messages = TRUE)

# To  add these calculations as layers in the raster, we need the 'full' dataframe, including the empty pixels.
# Convert the list elements to a useful dataframe, and take out the pixels with fewer than 50 species before joining:
FD.no.df <- cbind(as.integer(rownames(FD.n.o_min$CWM)),
                  FD.n.o_min$nbsp,
                  FD.n.o_min$sing.sp,
                  FD.n.o_min$FRic, FD.n.o_min$FEve,
                  FD.n.o_min$FDiv, FD.n.o_min$FDis,
                  FD.n.o_min$RaoQ, FD.n.o_min$CWM[,c(1:8,10)] # Remember to use 'Woodiness_1' as the right variable!
)
names(FD.no.df) <- c("Pixelnr","nbsp", "sing.sp","FRic","FEve","FDiv","FDis","RaoQ",names(FD.n.o_min$CWM[,c(1:8,10)]))
FD.no.df <- left_join(data.frame(Pixelnr = functcomp.norway[,c("Pixelnr")]), FD.no.df, by="Pixelnr")

# Make a raster with CWM trait values, number of species and Functional Diversity Indices
CWM_raster.no <- dropLayer(pa20k_stack, c(17:1248))  # OBS HERE!
CWM_raster.no <- setValues(CWM_raster.no, FD.no.df$Height.gen, layer=1)
CWM_raster.no <- setValues(CWM_raster.no, FD.no.df$Height.veg, layer=2)
CWM_raster.no <- setValues(CWM_raster.no, FD.no.df$LDMC, layer=3)
CWM_raster.no <- setValues(CWM_raster.no, FD.no.df$Leaf.area, layer=4)
CWM_raster.no <- setValues(CWM_raster.no, FD.no.df$Leaf.dry.mass, layer=5)
CWM_raster.no <- setValues(CWM_raster.no, FD.no.df$Seed.dry.mass, layer=6)
CWM_raster.no <- setValues(CWM_raster.no, FD.no.df$Seed.number.per.plant, layer=7)
CWM_raster.no <- setValues(CWM_raster.no, FD.no.df$SLA, layer=8)
CWM_raster.no <- setValues(CWM_raster.no, FD.no.df$Woodiness, layer=9)
CWM_raster.no <- setValues(CWM_raster.no, FD.no.df$nbsp, layer=10)
CWM_raster.no <- setValues(CWM_raster.no, FD.no.df$sing.sp, layer=11)
CWM_raster.no <- setValues(CWM_raster.no, FD.no.df$FRic, layer=12)
CWM_raster.no <- setValues(CWM_raster.no, FD.no.df$FEve, layer=13)
CWM_raster.no <- setValues(CWM_raster.no, FD.no.df$FDiv, layer=14)
CWM_raster.no <- setValues(CWM_raster.no, FD.no.df$FDis, layer=15)
CWM_raster.no <- setValues(CWM_raster.no, FD.no.df$RaoQ, layer=16)

names(CWM_raster.no) <- c("Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass","Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness",
                       "nbsp", "sing.sp","FRic","FEve","FDiv","FDis","RaoQ")
plot(CWM_raster.no)
plot(CWM_raster.no[[1:9]])
plot(CWM_raster.no[[10:16]])

# Histograms of all indices and species richness
ggarrange(ggplot(FD.no.df, aes(x=nbsp)) +
            geom_histogram() +
            labs(x="Number of species"),
          ggplot(FD.no.df, aes(x=FRic)) +
            geom_histogram() +
            labs(x="FRic"),
          ggplot(FD.no.df, aes(x=FEve)) +
            geom_histogram() +
            labs(x="FEve"),
          ggplot(FD.no.df, aes(x=FDiv)) +
            geom_histogram() +
            labs(x="FDiv"),
          ggplot(FD.no.df, aes(x=FDis)) +
            geom_histogram() +
            labs(x="FDis"),
          nrow = 2, ncol=3)
}
## ONLY COMPLETE CASES
{
  # Remove species with incomplete trait data - these might cause issues. We don't want to remove the outliers
  traitsFD5 <- traitsFD2[(complete.cases(traitsFD2)),]
  
  # Subset the CWM_speciesFD2 to match traitsFD4
  CWM_speciesFD5 <- CWM_speciesFD[which(rownames(CWM_speciesFD) %in% rownames(CWM.df)),
                                  which(colnames(CWM_speciesFD) %in% rownames(traitsFD5))]
  
  # As I cannot not add the argument "bin.num = c("Woodiness")" somewhere, the calculations for Woodiness a bit obscure.
  # A solution seems to be setting  CWM.type=c("all"), and then use
  # Woodiness_1; that gives the abundance of '1', which is the same as the mean value.
  ### OBS! R craches when doing the calculations with 'm="max"'; this is noted in the vignette as a common error:
  ### The package vignette suggests to reduce the dimensionality - try setting this to 'min' 
  
  # Calculate with a standardised FRic
  {
    FD.cc <- dbFD(traitsFD5, as.matrix(CWM_speciesFD5),
                   w.abun = FALSE, stand.x = TRUE,  # OBS on these ones
                   corr = c("cailliez"),            # OBS on this one
                   calc.FRic = TRUE, m = 'min', stand.FRic = TRUE,
                   scale.RaoQ = FALSE,
                   calc.FGR = FALSE,
                   clust.type = "ward",
                   calc.CWM = TRUE, CWM.type = c("all"), 
                   calc.FDiv = TRUE, 
                   print.pco = TRUE, messages = TRUE)
  }
  
  # To  add these calculations as layers in the raster, we need the 'full' dataframe, including the empty pixels.
  # Convert the list elements to a useful dataframe, and take out the pixels with fewer than 50 species before joining:
  FD.cc.df <- cbind(as.integer(rownames(FD.cc$CWM)),
                    FD.cc$nbsp,
                    FD.cc$sing.sp,
                    FD.cc$FRic, FD.cc$FEve,
                    FD.cc$FDiv, FD.cc$FDis,
                    FD.cc$RaoQ, FD.cc$CWM[,c(1:8,10)] # Remember to use 'Woodiness_1' as the right variable!
  )
  names(FD.cc.df) <- c("Pixelnr","nbsp", "sing.sp","FRic","FEve","FDiv","FDis","RaoQ",names(FD.cc$CWM[,c(1:8,10)]))
  FD.cc.df <- left_join(data.frame(Pixelnr = functcomp.norway[,c("Pixelnr")]), FD.cc.df, by="Pixelnr")
  
  # Make a raster with CWM trait values, number of species and Functional Diversity Indices
  CWM_raster.cc <- dropLayer(pa20k_stack, c(17:1248))  # OBS HERE!
  CWM_raster.cc <- setValues(CWM_raster.cc, FD.cc.df$Height.gen, layer=1)
  CWM_raster.cc <- setValues(CWM_raster.cc, FD.cc.df$Height.veg, layer=2)
  CWM_raster.cc <- setValues(CWM_raster.cc, FD.cc.df$LDMC, layer=3)
  CWM_raster.cc <- setValues(CWM_raster.cc, FD.cc.df$Leaf.area, layer=4)
  CWM_raster.cc <- setValues(CWM_raster.cc, FD.cc.df$Leaf.dry.mass, layer=5)
  CWM_raster.cc <- setValues(CWM_raster.cc, FD.cc.df$Seed.dry.mass, layer=6)
  CWM_raster.cc <- setValues(CWM_raster.cc, FD.cc.df$Seed.number.per.plant, layer=7)
  CWM_raster.cc <- setValues(CWM_raster.cc, FD.cc.df$SLA, layer=8)
  CWM_raster.cc <- setValues(CWM_raster.cc, FD.cc.df$Woodiness, layer=9)
  CWM_raster.cc <- setValues(CWM_raster.cc, FD.cc.df$nbsp, layer=10)
  CWM_raster.cc <- setValues(CWM_raster.cc, FD.cc.df$sing.sp, layer=11)
  CWM_raster.cc <- setValues(CWM_raster.cc, FD.cc.df$FRic, layer=12)
  CWM_raster.cc <- setValues(CWM_raster.cc, FD.cc.df$FEve, layer=13)
  CWM_raster.cc <- setValues(CWM_raster.cc, FD.cc.df$FDiv, layer=14)
  CWM_raster.cc <- setValues(CWM_raster.cc, FD.cc.df$FDis, layer=15)
  CWM_raster.cc <- setValues(CWM_raster.cc, FD.cc.df$RaoQ, layer=16)
  
  names(CWM_raster.cc) <- c("Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass","Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness",
                            "nbsp", "sing.sp","FRic","FEve","FDiv","FDis","RaoQ")
  plot(CWM_raster.cc)
  plot(CWM_raster.cc[[1:9]])
  plot(CWM_raster.cc[[10:16]])
  plot(CWM_raster.cc[[c(10,12:15)]], box=F, axes=F)
  
  # Histograms of all indices and species richness
  ggarrange(ggplot(FD.cc.df, aes(x=nbsp)) +
              geom_histogram() +
              labs(x="Number of species"),
            ggplot(FD.cc.df, aes(x=FRic)) +
              geom_histogram() +
              labs(x="FRic"),
            ggplot(FD.cc.df, aes(x=FEve)) +
              geom_histogram() +
              labs(x="FEve"),
            ggplot(FD.cc.df, aes(x=FDiv)) +
              geom_histogram() +
              labs(x="FDiv"),
            ggplot(FD.cc.df, aes(x=FDis)) +
              geom_histogram() +
              labs(x="FDis"),
            nrow = 2, ncol=3)
}

##--- 2.3.3 Ordination of the CWM's ---####
CWM.pca <- prcomp(FD.norway.df[!is.na(FD.norway.df$Woodiness),  # Remove NA-rows
                               c("Woodiness","Height.gen","Height.veg","SLA","Seed.number.per.plant","Seed.dry.mass",
                                  "Leaf.dry.mass","Leaf.area","LDMC")], scale=T)
fviz_eig(CWM.pca)
fviz_pca_var(CWM.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(CWM.pca,
             axes = c(3,4),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

pca.coord <- as.data.frame(get_pca_ind(CWM.pca)$coord)
pca.coord$Pixelnr <- as.integer(rownames(pca.coord))

# Add to full dataframe and rasterise:
pca.coord <- left_join(data.frame(Pixelnr=functcomp.norway[,c("Pixelnr")]), pca.coord, by="Pixelnr")
pca_raster <- dropLayer(pa20k_stack, c(5:1248))  # OBS HERE!
pca_raster <- setValues(pca_raster, pca.coord$Dim.1, layer=1)
pca_raster <- setValues(pca_raster, pca.coord$Dim.2, layer=2)
pca_raster <- setValues(pca_raster, pca.coord$Dim.3, layer=3)
pca_raster <- setValues(pca_raster, pca.coord$Dim.4, layer=4)
names(pca_raster) <- c("PC1","PC2","PC3","PC4")

plot(pca_raster)

pairs(cbind(as.data.frame(pca_raster), 
            FD.norway.df[,c("nbsp","FRic","FRic.log","FEve","FDiv","FDis")]), lower.panel = panel.cor)

##--------------------------------------------------------------####
##--- 3. EXPLANATORY/ENVIRONMENTAL VARIABLES ---####
# As a first step, I will try and retrieve the same variables as were used in Ida's paper:
# Three bioclimatic variables (annual precipitation, mean temperature of warmest quarter and precipitation seasonality),
# habitat heterogeneity, soil pH, time since last glaciation cover and topographic heterogeneity.

##--- 3.1 Bioclimatic variables ---####
# As potential predictors of the FD patterns, we can download bioclimatic variables from WorldClim through 'dismo'
# A single tile does not cover all of Norway, so we need to merge two tiles:
# Downloading, stacking and cropping all bioclimatic variables causes problems - this seemingly has to do with memory/space and temporary files
# I will try and get around this; this might not be the most elegant approach. Rather than keeping all of the bands,
# I will only keep a select few (the same as in Ida's paper; this can be changed as we please)
bioclimdat1 <- raster::getData('worldclim', var='bio', res=0.5, lon=10, lat=70) # Details here  http://www.worldclim.org/bioclim
bioclimdat2 <- raster::getData('worldclim', var='bio', res=0.5, lon=10, lat=60)

# Merge the data and transform the CRS to match that of the norway_UTM_buff:
ann.prep <- projectRaster(merge(bioclimdat1$bio12_06, bioclimdat2$bio12_16), crs = crs(norway_UTM_buff))
summer.temp <- projectRaster(merge(bioclimdat1$bio10_06, bioclimdat2$bio10_16), crs = crs(norway_UTM_buff))  # OBS! the unit is degress celcius*10
prec.seas <- projectRaster(merge(bioclimdat1$bio15_06, bioclimdat2$bio15_16), crs = crs(norway_UTM_buff))  

# Stack the variables and crop to the extent of Norway:
ext <- extent(norway_UTM_buff)
bioclimdat_crop <- stack(crop(ann.prep, ext), crop(summer.temp, ext), crop(prec.seas, ext))
names(bioclimdat_crop)<-c("Annual Precipitation","Mean Temperature of Warmest Quarter","Precipitation Seasonality (Coefficient of Variation)")
bioclimdat_crop <- mask(bioclimdat_crop, norway_UTM_buff)   # Crop/mask even further
#plot(bioclimdat_crop)
# Rescale/resample to match the resolution of the 'FD'-raster:
bioclimdat_crop2 <- resample(bioclimdat_crop, CWM_raster, method="bilinear")
#plot(bioclimdat_crop2)

##--- 3.2 Topography ---####
# Elevation data; download, crop, resample and add as layer
Noralt <- raster::getData('alt', country='NOR', mask=TRUE)
Noralt <- crop(projectRaster(Noralt, crs = crs(norway_UTM_buff)), ext)
Noralt2 <- resample(Noralt, CWM_raster, method="bilinear")
bioclimdat_crop2 <- raster::addLayer(bioclimdat_crop2, Noralt2)
plot(bioclimdat_crop2)

# We need a layer of topographic heterogeneity as well; the difference between highest and lowest point in the grid cell
# This seems a little tricky; this is my best solution as to how to do it.
    # 1. Convert a raster to a SpatialPolygon to get a 'mold':
    r.poly <- rasterToPolygons(CWM_raster, fun=NULL, na.rm = F)  # Do not remove NA-values; all pixels are needed
    # 2. Extract values from the altitude raster based on the polygons - one for max and one for min
    maxalt <- extract(Noralt, r.poly, fun=max, na.rm=TRUE)
    minalt <- extract(Noralt, r.poly, fun=min, na.rm=TRUE)
    maxalt[is.infinite(maxalt)] <- NA   # Replace Inf values with NA
    minalt[is.infinite(minalt)] <- NA
    # 3. Subtract the two rasters two get the differences in max and min altitude
    hetalt <- maxalt - minalt
    # 4. Add as a layer to the rasterStack
    hetalt <- setValues(r20, hetalt, layer=1)
    bioclimdat_crop2 <- raster::addLayer(bioclimdat_crop2, hetalt)
    names(bioclimdat_crop2) <-c("Annual.Precipitation",
                              "Mean.Temperature.of.Warmest.Quarter",
                              "Precipitation.Seasonality.",
                              "Mean.altitude",
                              "Topographic.heterogeneity")
    plot(bioclimdat_crop2)
    
# Check correlation of the environmental variables and FD metrics
pairs(data.frame(nbsp = values(CWM_raster$nbsp),
                 sing.sp = values(CWM_raster$sing.sp),
                 FRic = values(CWM_raster$FRic),
                 FEve = values(CWM_raster$FEve),
                 FDiv = values(CWM_raster$FDiv),
                 FDis = values(CWM_raster$FDis),
                 Annual.Precipitation = values(bioclimdat_crop2$Annual.Precipitation),
                 Mean.Temperature.of.Warmest.Quarter = values(bioclimdat_crop2$Mean.Temperature.of.Warmest.Quarter),
                 Precipitation.Seasonality = values(bioclimdat_crop2$Precipitation.Seasonality.),
                 Mean.altitude = values(bioclimdat_crop2$Mean.altitude),
                 Topographic.heterogeneity = values(bioclimdat_crop2$Topographic.heterogeneity)),
      lower.panel = panel.cor)
    
##--- 3.3 Land-cover and habitat heterogeneity ---####
# As Ida did, calculate the number of different AR50 habitat classes found within pixels
# Load map - Try loading directly from gdf with 'sf':
AR50 <- sf::st_read("/home/ahomez/t/tanjakp/NorwegianFloraTraits/0000_ar50_25833_gdb.gdb", layer = "ar50_flate")
AR50 <- st_transform(AR50, crs(CWM_raster))
table(AR50$artype)

# Calculate cover of each land-cover category within the pixels; use the r.poly for intersecting the data
r.poly@data$Pixelnr <- rownames(r.poly@data)
r.poly2 <- st_as_sf(r.poly[,-c(1:16)])
AR50_grid <- st_intersection(r.poly2, st_make_valid(AR50))
AR50_grid$area_m2 <- st_area(AR50_grid)  # Calculate the area of polygons

# Group by Pixelnr and calculate area of each land-cover category
AR50_grpixel <- st_drop_geometry(AR50_grid) %>%
  group_by(Pixelnr) %>%
  summarize(sum_10 = sum(area_m2[artype=="10"]),
            sum_20 = sum(area_m2[artype=="20"]),
            sum_30 = sum(area_m2[artype=="30"]),
            sum_50 = sum(area_m2[artype=="50"]),
            sum_60 = sum(area_m2[artype=="60"]),
            sum_70 = sum(area_m2[artype=="70"]),
            sum_81 = sum(area_m2[artype=="81"]),
            sum_82 = sum(area_m2[artype=="82"]),
            sum_99 = sum(area_m2[artype=="99"]))
AR50_grpixel$total_area <- rowSums(AR50_grpixel[,c(2:10)])
# Make it proportional
for(i in 1:nrow(AR50_grpixel)){
  for(j in 2:10){
    AR50_grpixel[i,j] <- AR50_grpixel[i,j]/AR50_grpixel[i,"total_area"]
  }
}

# Calculate habitat heterogeneity (number of habtitat classes). OBS! Not including '99', which is unclassified
AR50_grpixel$hab.het <- rowSums(units::drop_units(AR50_grpixel[,c(2:9)])>0)  # We need to drop units here

# Make a raster with the proportional cover and the habitat heterogeneity
AR50_grpixel <- left_join(st_drop_geometry(r.poly2), AR50_grpixel, by="Pixelnr")

# Make a raster with CWM trait values, number of species and Functional Diversity Indices
landcover <- dropLayer(pa20k_stack, c(10:1248))  # OBS HERE!
landcover <- setValues(landcover, units::drop_units((AR50_grpixel$sum_10)), layer=1)
landcover <- setValues(landcover, units::drop_units((AR50_grpixel$sum_20)), layer=2)
landcover <- setValues(landcover, units::drop_units((AR50_grpixel$sum_30)), layer=3)
landcover <- setValues(landcover, units::drop_units((AR50_grpixel$sum_50)), layer=4)
landcover <- setValues(landcover, units::drop_units((AR50_grpixel$sum_60)), layer=5)
landcover <- setValues(landcover, units::drop_units((AR50_grpixel$sum_70)), layer=6)
landcover <- setValues(landcover, units::drop_units((AR50_grpixel$sum_81)), layer=7)
landcover <- setValues(landcover, units::drop_units((AR50_grpixel$sum_82)), layer=8)
landcover <- setValues(landcover, AR50_grpixel$hab.het, layer=9)
names(landcover) <- c("Developed","Agriculture","Forest","Open","Mire","Snow.Ice","Freshwater","Sea","Habitat.heterogeneity")
plot(landcover)

##--- 3.4 Soil pH ---####
# Download data from SoilGrids250m (https://www.soilgrids.org/)
# Follow the instructions from https://git.wur.nl/isric/soilgrids/soilgrids.notebooks/-/blob/master/markdown/webdav_from_R.md
library(rgdal, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(gdalUtils, lib.loc = "/home/ahomez/t/tanjakp/export/library")

bb=c(1598439,7609270,3443367,6263477) # bounding box, format c(ulx,uly,lrx,lry)     
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for the Homolosine projection usen in the SoilGrids250m data
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"
# download to local VRT
gdal_translate(paste0(sg_url,'phh2o/phh2o_0-5cm_mean.vrt'),  # https://files.isric.org/soilgrids/latest/data/  (names/directories of the data)
               "./crop_roi_igh_r.vrt",
               of="VRT",tr=c(250,250),
               projwin=bb,
               projwin_srs =igh,
               verbose=TRUE)
# Convert to a VRT in another projection
gdalwarp("./crop_roi_igh_r.vrt",
         "./crop_roi_ll_r.vrt", 
         s_src=igh, 
         t_srs="EPSG:32632", 
         of="VRT")
# To a local Geotiff
gdal_translate("./crop_roi_ll_r.vrt",  
               "./crop_roi_ll_r.tif", 
               co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))
# Read in R
soil=readGDAL("./crop_roi_ll_r.vrt")
# Convert to raster
soil <- as(soil, 'SpatialPixelsDataFrame')   # promote to SpatialPixelsDataFrame
soil_raster <- raster(soil)
plot(soil_raster)  # OBS! Unit is pH*10

# Crop, resample and add as layer
soilph_raster <- crop(soil_raster, ext)
soilph_raster <- resample(soilph_raster, CWM_raster, method="bilinear")
soilph_raster <- mask(soilph_raster, norway_UTM_buff)   # Crop/mask even further
names(soilph_raster) <- c("soil.pH")
plot(soilph_raster)

##--- 3.5 Deglaciation age ---####
# Data from supplementary shapefiles from Stroeven et al. (2016)
glaciation <- raster("Glaciation.tif")
#  Crop, resample and add as layer
glaciation <- crop(projectRaster(glaciation, crs = crs(norway_UTM_buff)), ext)
glaciation_raster <- resample(glaciation, CWM_raster, method="bilinear")
glaciation_raster <- mask(glaciation_raster, norway_UTM_buff)   # Crop/mask even further
plot(glaciation_raster)

##--- 3.6 Gather everything in one raster ---####
# Gather all the raster layers of interest in a single rasterStack
env_raster <- raster::addLayer(
  raster::addLayer(
    raster::addLayer(
      bioclimdat_crop2[[c("Annual.Precipitation","Mean.Temperature.of.Warmest.Quarter",
                          "Precipitation.Seasonality.","Topographic.heterogeneity")]],
      soilph_raster),
      #landcover[[c("Forest","Habitat.heterogeneity")]]),
    glaciation_raster),
  landcover)

plot(env_raster, axes=F, box=F)

##--------------------------------------------####
##--- 4. CLUSTER ANALYSIS OF FUNCTIONAL TRAITS ---####
# As discussed it might be interesting to see if we have any kind of functional clustering, in addition to the FD metrics.
# Perform cluster analysis on the CWM trait data (a trait matrix),and evaluate number of clusters:
library(vegan, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(NbClust, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(factoextra, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(ggpubr, lib.loc = "/home/ahomez/t/tanjakp/export/library")

# Perform cluster analysis on the CWM trait data (a trait matrix),and evaluate number of clusters:
CWM.df <- data.frame(Height.gen=as.data.frame(CWM_raster$Height.gen),
                     Height.veg=as.data.frame(CWM_raster$Height.veg),
                     LDMC=as.data.frame(CWM_raster$LDMC),
                     Leaf.area=as.data.frame(CWM_raster$Leaf.area),
                     Leaf.dry.mass=as.data.frame(CWM_raster$Leaf.dry.mass),
                     Seed.dry.mass=as.data.frame(CWM_raster$Seed.dry.mass),
                     Seed.number.per.plant=as.data.frame(CWM_raster$Seed.number.per.plant),
                     SLA=as.data.frame(CWM_raster$SLA),
                     Woodiness=as.data.frame(CWM_raster$Woodiness))
# Remove NA-rows
CWM.df <- CWM.df[rowSums(is.na(CWM.df)) != ncol(CWM.df), ]

# Make the needed distance matrix. I here use Mahalanobis distance, as that should standardise the variables;
CWM.dist <- vegdist(CWM.df, method="mahalanobis", na.rm=T)

# Identify the optimal number of clusters (we need to use the numeric data rather than the ordered factor):
## Elbow method
    fviz_nbclust(as.matrix(CWM.dist), hcut, method="wss", print.summary = TRUE) +
      labs(subtitle = "Elbow method")                # Suggested no. clusters not immediately clear
## Silhouette
    fviz_nbclust(as.matrix(CWM.dist), hcut, method="silhouette") +
      labs(subtitle = "Silhouette method")           # Suggests 2 clusters as the best option
## Gap statistic - OBS! Time-consuming
    set.seed(123)
    fviz_nbclust(as.matrix(CWM.dist), hcut, nstart=25, method="gap_stat", nboot=50) +
      labs(subtitle = "Gap statistic method")        # Suggests 1 cluster as the best option
## NbClust, majority rule
    nclust <- NbClust(CWM.df, diss=CWM.dist, distance=NULL,
                      min.nc = 2, max.nc = 25, method="ward.D")   # Cannot be computed with method='complete'
    nclust$Best.nc   # Best number of clusters: 6\3 or 4, according to the majority rule

# Make the cluster-dendrogram based on a distance matrix with Mahalanobis similarity 
clust <- hclust(CWM.dist, method = "ward.D")
par(mfrow=c(1,1), mar=c(4.1,4.1,5.1,2.1))
plot(clust, cex=0.5)
abline(h=200, col="red", lty=2) + text(x=55, y=200, labels=c("3 clusters"), col="red", pos=3, cex=0.75)   # table(as.data.frame(cutree(clust, h=200)))
abline(h=110, col="red", lty=2) + text(x=55, y=110, labels=c("4 clusters"), col="red", pos=3, cex=0.75)   # table(as.data.frame(cutree(clust, h=110)))

# Cut dendrogram and add clusters to a rasterLayer for plotting; make multiple categorisations, based on different cut-offs
clusterCut_3 <- as.data.frame(cutree(clust, h=200))
clusterCut_3$Pixelnr <- as.integer(rownames(clusterCut_3))
names(clusterCut_3) <- c("cluster3", "Pixelnr")
clusterCut_4 <- as.data.frame(cutree(clust, h=110))
clusterCut_4$Pixelnr <- as.integer(rownames(clusterCut_4))
names(clusterCut_4) <- c("cluster4", "Pixelnr")
CWM_traits$Pixelnr <- as.integer(rownames(CWM_traits))
CWM_cluster <- merge(merge(data.frame(Pixelnr=CWM_traits[,"Pixelnr"]),
                           clusterCut_3, by="Pixelnr", all.x=T),
                     clusterCut_4, by="Pixelnr", all.x=T)
CWM_cluster <- CWM_cluster[order(CWM_cluster$Pixelnr),]

CWM_clust_raster <- dropLayer(pa20k_stack, c(3:1248))  # OBS HERE!
    CWM_clust_raster <- setValues(CWM_clust_raster, CWM_cluster$cluster3, layer=1)
    CWM_clust_raster <- setValues(CWM_clust_raster, CWM_cluster$cluster4, layer=2)
    names(CWM_clust_raster) <- c("clust_3","clust_4")
par(mfrow=c(1,2))
plot(CWM_clust_raster[["clust_3"]], col=topo.colors(3), main="3 clusters", box=F, axes=F)
plot(CWM_clust_raster[["clust_4"]], col=topo.colors(4), main="4 clusters", box=F, axes=F)

CWM_clust_raster <- raster::addLayer(CWM_clust_raster, CWM_raster)

# See how the clusters relates to the previous variables, particularly the FRic
ggarrange(ggplot(data=as.data.frame(CWM_clust_raster), aes(x=as.factor(clust_4), y=FRic, color=as.factor(clust_4))) +
            geom_point() +
            xlab("Cluster (4)") + ylab("FRic"),
          ggplot(data=as.data.frame(CWM_clust_raster), aes(x=as.factor(clust_4), y=FRic.log, color=as.factor(clust_4))) +
            geom_point() +
            xlab("Cluster (4)") + ylab("log(FRic)"),
          ncol=2, nrow=1)

##--- 4.1 CLUSTER ANALYSIS OF LAND-COVER ---####
# Perform cluster analysis on the landcover data (an area matrix),and evaluate number of clusters:
landcover.df <- as.data.frame(landcover[[c(1:8)]])
# Remove NA-rows
landcover.df <- landcover.df[rowSums(is.na(landcover.df)) != ncol(landcover.df), ]
# We get an undue effect of marine grid cells - keep only grid cells for which we have other envirnomental variables;
# OBS! We don't have the same amount of grid cells for each variable - use Topographic Heterogeneity
env <- rownames_to_column(as.data.frame(env_raster[[c(4)]]))
landcover.df <- landcover.df[rownames(landcover.df) %in% env[!is.na(env$Topographic.heterogeneity),"rowname"],]

# Make the needed distance matrix - we need to make sure that the columns are centered and scaled first, then use
# Euclidean distance
landcover.df <- as.data.frame(scale(landcover.df, center = T, scale = T))
landcover.dist <- vegdist(landcover.df, method="euclidean", na.rm=T)

# Identify the optimal number of clusters: NbClust, majority rule. Use 'complete' or 'ward.D2' clustering
nclust.lc <- NbClust(landcover.df, diss=landcover.dist, distance=NULL,
                  min.nc = 2, max.nc = 25, method="ward.D2")   
nclust$Best.nc   # Best number of clusters: 3  (ward.D2)

# Make the cluster-dendrogram based on a distance matrix with Euclidean distances
clust.lc <- hclust(landcover.dist, method = "ward.D2")
par(mfrow=c(1,1), mar=c(4.1,4.1,5.1,2.1))
plot(clust.lc, cex=0.5)
# Plot different cut-offs
abline(h=50, col="red", lty=2) + text(x=55, y=50, labels=c("3 clusters"), col="red", pos=3, cex=0.75)

# Cut dendrogram and add clusters to a rasterLayer for plotting
  CC <- as.data.frame(cutree(clust.lc, h=50))
  CC$Pixelnr <- as.integer(rownames(CC))
  names(CC) <- c("cluster", "Pixelnr")

landcover.cluster.df <- rownames_to_column(as.data.frame(landcover))
      names(landcover.cluster.df) <- c("Pixelnr","Developed","Agriculture","Forest","Open","Mire","Snow.Ice","Freshwater","Sea","Habitat.heterogeneity")
      landcover.cluster.df$Pixelnr <- as.integer(landcover.cluster.df$Pixelnr)
      landcover.cluster.df <- merge(landcover.cluster.df,
                                                CC, by="Pixelnr", all.x=T)

landcover <- raster::addLayer(landcover, setValues(dropLayer(pa20k_stack, c(2:1248)),
                                                                       landcover.cluster.df$cluster,
                                                   layer=1))
names(landcover) <- c("Developed","Agriculture","Forest","Open","Mire","Snow.Ice","Freshwater","Sea","Habitat.heterogeneity","cluster")

plot(landcover[["cluster"]], box=F, axes=F, col=topo.colors(3))

# Spineplot of clusters
# Make a dataframe with cluster as column and habitat as rows.
# Each entry is then the average of that habitat type for all cells within that cluster.
spine <- matrix(nrow=3, ncol = 8)
colnames(spine) <- c("Developed","Agriculture","Forest","Open","Mire","Snow.Ice","Freshwater","Sea")
rownames(spine) <- c(1:3)
# Calculate the mean of habitat in the grid cells included in each cluster:
for(i in 1:dim(spine)[1]) {
  for(j in 1:dim(spine)[2]) {
    spine[i,j] = mean(landcover.cluster.df[landcover.cluster.df$cluster==i, colnames(spine)[j]], na.rm=T)
  }
}

layout(t(1:2),widths=c(2,1))
spineplot(spine, main="",
          col = c("hotpink", "darkorange", "forestgreen", "gray", "cyan",
                  "gray90", "dodgerblue", "navy"),
          xlab="", ylab="Mean cover of habitat in grid cells",
          xaxlabels = c("1","2","3"), yaxlabels = "", border=NA)

par(mar=c(0.5,0.5,0.5,0.5))
plot(0,type='n',axes=FALSE,ann=FALSE)
legend("center", legend=c("Developed area", "Agriculture","Forest","Open","Mire","Snow.ice","Freshwater","Sea"),
       fill=c("hotpink", "darkorange", "forestgreen", "gray", "cyan",
              "gray90", "dodgerblue", "navy"), border=NA, cex=1)

##--- 4.2 Add to raster ---####
### Add the cluster as a layer to the modelling data
env_raster <- raster::addLayer(env_raster,
                               landcover[["cluster"]])

plot(env_raster, axes=F, box=F)

# Gather both predictors and relevant responses in a single raster:
raster_data <- raster::addLayer(
  CWM_raster.cc[[c("nbsp","FRic","FEve","FDiv","FDis")]],
  env_raster
)


##--- 4.3 Ordination of landcover ---####
# As suggested by James on 03.03.21, we should not include 'Sea' at all. Instead, set all 'Sea' values to 'NA', and calculate 
# the proportion of landcover based on ONLY the terrestial area. Thus, reconstruct the land-cover raster and dataframe:
# Group by Pixelnr and calculat area of each land-cover category
AR50_terr <- st_drop_geometry(AR50_grid) %>%
  group_by(Pixelnr) %>%
  summarize(sum_10 = sum(area_m2[artype=="10"]),
            sum_20 = sum(area_m2[artype=="20"]),
            sum_30 = sum(area_m2[artype=="30"]),
            sum_50 = sum(area_m2[artype=="50"]),
            sum_60 = sum(area_m2[artype=="60"]),
            sum_70 = sum(area_m2[artype=="70"]),
            sum_81 = sum(area_m2[artype=="81"]),
            sum_82 = sum(area_m2[artype=="82"]),
            sum_99 = sum(area_m2[artype=="99"]))
AR50_terr$sum_82 <- NA  # Set 'Sea' to NA
AR50_terr$sum_99 <- NA  # Set '99' to NA - these are only found along the border and should not be included in the calculations
AR50_terr$total_area <- rowSums(AR50_terr[,c(2:8)])  # Include only the relevant columns - total_area is thus terrestrial area

# Make it proportional; proportion of MAPPED TERRESTRIAL area
for(i in 1:nrow(AR50_terr)){
  for(j in 2:8){
    AR50_terr[i,j] <- AR50_terr[i,j]/AR50_terr[i,"total_area"]  # Some grid cells are now 'NA' - the purely marine ones
  }
}
# Calculate habitat heterogeneity (number of habtitat classes). OBS! Not including '99', which is unclassified
AR50_terr$hab.het <- rowSums(units::drop_units(AR50_terr[,c(2:8)])>0)  # We need to drop units here
# Make a raster with the proportional cover and the habitat heterogeneity
AR50_terr <- left_join(st_drop_geometry(r.poly2), AR50_terr, by="Pixelnr")
# Make a raster with CWM trait values, number of species and Functional Diversity Indices
landcover_terr <- dropLayer(pa20k_stack, c(9:1248))  # OBS HERE!
landcover_terr <- setValues(landcover_terr, units::drop_units((AR50_terr$sum_10)), layer=1)
landcover_terr <- setValues(landcover_terr, units::drop_units((AR50_terr$sum_20)), layer=2)
landcover_terr <- setValues(landcover_terr, units::drop_units((AR50_terr$sum_30)), layer=3)
landcover_terr <- setValues(landcover_terr, units::drop_units((AR50_terr$sum_50)), layer=4)
landcover_terr <- setValues(landcover_terr, units::drop_units((AR50_terr$sum_60)), layer=5)
landcover_terr <- setValues(landcover_terr, units::drop_units((AR50_terr$sum_70)), layer=6)
landcover_terr <- setValues(landcover_terr, units::drop_units((AR50_terr$sum_81)), layer=7)
landcover_terr <- setValues(landcover_terr, AR50_terr$hab.het, layer=8)
names(landcover_terr) <- c("Developed","Agriculture","Forest","Open","Mire","Snow.Ice","Freshwater","Habitat.heterogeneity")
plot(landcover_terr)

# Make a regular dataframe for PCA
landcover_terr.df <- as.data.frame(landcover_terr[[c(1:7)]])
# Remove NA-rows
landcover_terr.df <- landcover_terr.df[rowSums(is.na(landcover_terr.df)) != ncol(landcover_terr.df), ]

# Do a PCA on the landcover
landcover_terr.pca <- prcomp(landcover_terr.df, scale=T)
fviz_eig(landcover_terr.pca)
fviz_pca_var(landcover_terr.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(landcover_terr.pca,
             axes = c(2,3),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

pca.coord <- as.data.frame(get_pca_ind(landcover_terr.pca)$coord)
pca.coord$Pixelnr <- as.integer(rownames(pca.coord))

# Add to full dataframe and rasterise/add to environmental raster:
pca.coord <- left_join(data.frame(Pixelnr=functcomp.norway[,c("Pixelnr")]), pca.coord, by="Pixelnr")
landcover_terr <- raster::addLayer(landcover_terr, setValues(dropLayer(pa20k_stack, c(2:1248)),
                                                   pca.coord$Dim.1, layer=1))
landcover_terr <- raster::addLayer(landcover_terr, setValues(dropLayer(pa20k_stack, c(2:1248)),
                                                   pca.coord$Dim.2, layer=1))
names(landcover_terr) <- c("Developed","Agriculture","Forest","Open","Mire","Snow.Ice","Freshwater","Habitat.heterogeneity","PC1","PC2")

env_raster <- addLayer(env_raster, landcover_terr)

plot(env_raster, axes=F, box=F)
par(mfrow=c(1,2), mar=c(1,1,3,1))
plot(env_raster[["PC1"]], axes=F, box=F, main="PC1")
plot(env_raster[["PC2"]], axes=F, box=F, main="PC2")

# Gather both predictors and relevant responses in a single raster:
raster_data <- raster::addLayer(
  CWM_raster.cc[[c("nbsp","FRic","FEve","FDiv","FDis")]],
  env_raster)

##--------------------------------------------####

##--- 5. SPATIALLY EXPLICIT MODELLING ---####
# As we want to use model averaging, we have to options for including spatial effects: either a correlation structure in a gls(),
# or use spatial eigenvector mapping. The latter is what Ida used, so try that first.
library(MuMIn, lib.loc = "/home/ahomez/t/tanjakp/export/library")
require(nlme, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(MASS, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(spData, lib.loc = "/home/ahomez/t/tanjakp/export/library")  # Most functions have moved to the 'spatialreg' package
library(spdep, lib.loc = "/home/ahomez/t/tanjakp/export/library")  # Most functions have moved to the 'spatialreg' package
library(spatialreg, lib.loc = "/home/ahomez/t/tanjakp/export/library")  
library(tibble, lib.loc = "/home/ahomez/t/tanjakp/export/library")  
library(data.table, lib.loc = "/home/ahomez/t/tanjakp/export/library")  
library(forcats, lib.loc = "/home/ahomez/t/tanjakp/export/library")  

##--- 5.1 Data exploration ---####
# Before initiating the models, check the data to see if we have colinear variables etc:
source("/home/ahomez/t/tanjakp/HighstatLibV10.R")
# Colinearity
pairs(cbind(FD.cc.df[,c("nbsp","FRic","FEve","FDiv","FDis")], as.data.frame(env_raster)), lower.panel = panel.cor)
pairs(cbind(FD.cc.df[,c("nbsp","FRic","FEve","FDiv","FDis")],
            as.data.frame(env_raster)[,c("Annual.Precipitation","Mean.Temperature.of.Warmest.Quarter",
                                         "Precipitation.Seasonality.","Topographic.heterogeneity","soil.pH",
                                         "Habitat.heterogeneity","PC1","PC2")]), lower.panel = panel.cor)

corvif(as.data.frame(env_raster)[,c("Annual.Precipitation","Mean.Temperature.of.Warmest.Quarter",
                                    "Precipitation.Seasonality.","Topographic.heterogeneity","soil.pH",
                                    "Habitat.heterogeneity","PC1","PC2")])
# From earlier tries, we already knew that soil-pH should be excluded. Additionally, PC1 is highly correlated with temperature,
# so we should leave that out, only keeping PC2 (the anthropogenic axis)

corvif(as.data.frame(env_raster)[,c("Annual.Precipitation","Mean.Temperature.of.Warmest.Quarter",
                                    "Precipitation.Seasonality.","Topographic.heterogeneity",
                                    "Habitat.heterogeneity","PC2")])
pairs(cbind(FD.cc.df[,c("nbsp","FRic","FEve","FDiv","FDis")],
            as.data.frame(env_raster)[,c("Annual.Precipitation","Mean.Temperature.of.Warmest.Quarter","Precipitation.Seasonality.",
                                         "Topographic.heterogeneity","Habitat.heterogeneity","PC2")]), lower.panel = panel.cor)

# Outliers:
Mydotplot(cbind(FD.cc.df[,c("nbsp","FRic","FEve","FDiv","FDis")],
                as.data.frame(env_raster)[,c("Annual.Precipitation","Mean.Temperature.of.Warmest.Quarter",
                                             "Precipitation.Seasonality.","Topographic.heterogeneity",
                                             "Habitat.heterogeneity","PC2")]))   # No immediate outliers

# Relationships 
{
### Species richness
Myxyplot(cbind(FD.cc.df[,c("nbsp","FRic","FEve","FDiv","FDis")], as.data.frame(env_raster)),
               c("Annual.Precipitation","Mean.Temperature.of.Warmest.Quarter","Precipitation.Seasonality.",
                 "Topographic.heterogeneity","Habitat.heterogeneity","Glaciation", "PC1", "PC2"),
               "nbsp", MyYlab = "#Species")
### Functional richness
  Myxyplot(cbind(FD.cc.df[,c("nbsp","FRic","FEve","FDiv","FDis")], as.data.frame(env_raster)),
           c("Annual.Precipitation","Mean.Temperature.of.Warmest.Quarter","Precipitation.Seasonality.",
             "Topographic.heterogeneity","Habitat.heterogeneity","Glaciation", "PC1", "PC2"),
           "FRic", MyYlab = "FRic")
### Functional Evenness
  Myxyplot(cbind(FD.cc.df[,c("nbsp","FRic","FEve","FDiv","FDis")], as.data.frame(env_raster)),
           c("Annual.Precipitation","Mean.Temperature.of.Warmest.Quarter","Precipitation.Seasonality.",
             "Topographic.heterogeneity","Habitat.heterogeneity","Glaciation", "PC1", "PC2"),
           "FEve", MyYlab = "Functional Evenness")
### Functional Divergence
  Myxyplot(cbind(FD.cc.df[,c("nbsp","FRic","FEve","FDiv","FDis")], as.data.frame(env_raster)),
           c("Annual.Precipitation","Mean.Temperature.of.Warmest.Quarter","Precipitation.Seasonality.",
             "Topographic.heterogeneity","Habitat.heterogeneity","Glaciation", "PC1", "PC2"),
         "FDiv", MyYlab = "Functional Divergence")
### Functional Dispersion
  Myxyplot(cbind(FD.cc.df[,c("nbsp","FRic","FEve","FDiv","FDis")], as.data.frame(env_raster)),
           c("Annual.Precipitation","Mean.Temperature.of.Warmest.Quarter","Precipitation.Seasonality.",
             "Topographic.heterogeneity","Habitat.heterogeneity","Glaciation", "PC1", "PC2"),
         "FDis", MyYlab = "Functional Dispersion")
}

##--- 5.2 Formulating the models and model averaging ---####
# To try and align with Ida's methods, we will do spatial eigenvector mapping, as described in the Dormann et al. (2007)

# Convert the raster to a SpatialPixelsDataFrame and a regular dataframe (to make it 'lighter'), and remove 'incomplete' pixels
data.pxl <- as(raster_data, "SpatialPixelsDataFrame")
    data.pxl <- data.pxl[!is.na(data.pxl$nbsp),]
    data.pxl <- data.pxl[complete.cases(data.pxl@data), ]  # Incomplete cases in bioclim and topography
data.pxl.df <- as.data.frame(data.pxl)
data.pxl.df$nbsp.prop <- data.pxl$nbsp/1232  # proportion of total no. species
# Find neighbours - The classes are in euclidian distance (m), thus we need a reasonable distance to define a neighbouring grid cell
      # Using 20km (centre to centre) gives max. 4 neighbours. To include all first-order neighbours (linear and diagonal), increase      
      # the upper distance to length(diagonal) = sqrt((20000^2)+(20000^2))
nb.pxl <- dnearneigh(coordinates(data.pxl), 0, 28284.3)
nb.pxl_dists <- nbdists(nb.pxl, coordinates(data.pxl))  # List of Euclidian distances along the neighbourhood links
nb.pxl_sims <- lapply(nb.pxl_dists, function(x) (1-((x/4)^2)) )  # List of general weights corresponding to the neighbours
ME.listw <- nb2listw(nb.pxl, glist=nb.pxl_sims, style="B", zero.policy = TRUE)  # Spatial weights for neighbours 

# Preliminary tries regarding model selection showed that multiple models have deltaAIC just around 2, but there seems to
# be a 'jump' after 3.5 - setting the threshold to <3 seems justifiable.

##--- 5.2.1 Number of species ---####
{

# Get the eigenvectors from the full, suggested model - these should then later be included in the model
# formulation (takes a little while to run) - OBS! I am uncertain whether to keep the eigenvectors of the 'global'
# model, or if these should be updated/reduced according to the averaged model?
# For better visualisation later, use the proportional species richness
ME.nbsp <- ME(nbsp.prop ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
               Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
             data = data.pxl.df,
             family = gaussian, listw = ME.listw)
m.nbsp <- glm(nbsp.prop ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.nbsp),
              data = data.pxl.df,
              family = gaussian,     # OBS! 'nbsp' is a count, not gaussian - I attempted Poisson first, changed to NB due to overdispersion
              na.action="na.fail")  # Needed, otherwise the dredging fails

# Model averaging - I need to figure out if we have to further redo the ME once the model has been reduced
d.nbsp <- dredge(m.nbsp)              # Model construction
d.nbsp2 <- subset(d.nbsp, delta<3)    # Subset based on deltaAIC
output_nbsp <- model.sel(d.nbsp2)     # Model selection
importance(output_nbsp)               # Importance of the models 

m.nbsp.avg <- model.avg(output_nbsp, fit=TRUE)     # Model averaging
m.nbsp.avg$msTable
m.nbsp.avg$coefficients
m.nbsp.avg$formula

summary(m.nbsp.avg)$coefmat.full  # When retrieving the results, I have chosen to use the zero-method (full model)
                                  # rather than the conditional, as this is better when we are interested in the
                                  # importance of variables (Grueber et al. 2011). However, this is changed to 'subset' in the plots
}
##--- 5.2.2 Functional richness ---####
{
  # Get the eigenvectors from the full, suggested model - these should then later be included in the model
  # formulation (takes a little while to run) - OBS! I am uncertain whether to keep the eigenvectors of the 'global'
  # model, or if these should be updated/reduced according to the averaged model?
  ME.FRic <- ME(FRic ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                  Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                data = data.pxl.df,
                family = gaussian, listw = ME.listw)
  m.FRic <- glm(FRic ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                  Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.FRic),
                data = data.pxl.df,
                family = gaussian,     
                na.action="na.fail")  # Needed, otherwise the dredging fails
  
  # Model averaging - I need to figure out if we have to further redo the ME once the model has been reduced
  d.FRic <- dredge(m.FRic)              # Model construction
  d.FRic2 <- subset(d.FRic, delta<3)    # Subset based on deltaAIC
  output_FRic <- model.sel(d.FRic2)     # Model selection
  importance(output_FRic)               # Importance of the models 
  
  m.FRic.avg <- model.avg(output_FRic, fit=TRUE)     # Model averaging
  m.FRic.avg$msTable
  m.FRic.avg$coefficients
  m.FRic.avg$formula
  
  summary(m.FRic.avg)$coefmat.full  # When retrieving the results, I have chosen to use the zero-method (full model)
  confint(m.FRic.avg)
  # rather than the conditional, as this is better when we are interested in the
  # importance of variables (Grueber et al. 2011)
}
##--- 5.2.3 Functional evenness ---####
{
  # Get the eigenvectors from the full, suggested model - these should then later be included in the model
  # formulation (takes a little while to run) - OBS! I am uncertain whether to keep the eigenvectors of the 'global'
  # model, or if these should be updated/reduced according to the averaged model?
  ME.FEve <- ME(FEve ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                  Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                data = data.pxl.df,
                family = gaussian, listw = ME.listw)
  m.FEve <- glm(FEve ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                  Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.FEve),
                data = data.pxl.df,
                family = gaussian,     
                na.action="na.fail")  # Needed, otherwise the dredging fails
  
  # Model averaging - I need to figure out if we have to further redo the ME once the model has been reduced
  d.FEve <- dredge(m.FEve)              # Model construction
  d.FEve2 <- subset(d.FEve, delta<3)    # Subset based on deltaAIC
  output_FEve <- model.sel(d.FEve2)     # Model selection
  importance(output_FEve)               # Importance of the models 
  
  m.FEve.avg <- model.avg(output_FEve, fit=TRUE)     # Model averaging
  m.FEve.avg$msTable
  m.FEve.avg$coefficients
  m.FEve.avg$formula
  
  summary(m.FEve.avg)$coefmat.full  # When retrieving the results, I have chosen to use the zero-method (full model)
  confint(m.FEve.avg)
  # rather than the conditional, as this is better when we are interested in the
  # importance of variables (Grueber et al. 2011)
 }
##--- 5.2.4 Functional divergence ---####
{
  # Get the eigenvectors from the full, suggested model - these should then later be included in the model
  # formulation (takes a little while to run) - OBS! I am uncertain whether to keep the eigenvectors of the 'global'
  # model, or if these should be updated/reduced according to the averaged model?
  ME.FDiv <- ME(FDiv ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                  Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                data = data.pxl.df,
                family = gaussian, listw = ME.listw)
  m.FDiv <- glm(FDiv ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                  Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.FDiv),
                data = data.pxl.df,
                family = gaussian,     
                na.action="na.fail")  # Needed, otherwise the dredging fails
  
  # Model averaging - I need to figure out if we have to further redo the ME once the model has been reduced
  d.FDiv <- dredge(m.FDiv)              # Model construction
  d.FDiv2 <- subset(d.FDiv, delta<3)    # Subset based on deltaAIC
  output_FDiv <- model.sel(d.FDiv2)     # Model selection
  importance(output_FDiv)               # Importance of the models - as we only hae one, all are equally important!
  
  #m.FDiv.avg <- model.avg(output_FDiv, fit=TRUE)     # Model averaging - not working for a singular model
  # As all variables are retained in the optimal model: m.FDiv.avg <- m.FDiv
  m.FDiv.avg$msTable
  m.FDiv.avg$coefficients
  m.FDiv.avg$formula
  
  summary(m.FDiv.avg)$coefmat.full  # When retrieving the results, I have chosen to use the zero-method (full model)
  confint(m.FDiv.avg)
  # rather than the conditional, as this is better when we are interested in the
  # importance of variables (Grueber et al. 2011)
 }
##--- 5.2.5 Functional dispersion ---####
{
  # Get the eigenvectors from the full, suggested model - these should then later be included in the model
  # formulation (takes a little while to run) - OBS! I am uncertain whether to keep the eigenvectors of the 'global'
  # model, or if these should be updated/reduced according to the averaged model?
  ME.FDis <- ME(FDis ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                  Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                data = data.pxl.df,
                family = gaussian, listw = ME.listw)
  m.FDis <- glm(FDis ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                  Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.FDis),
                data = data.pxl.df,
                family = gaussian,     
                na.action="na.fail")  # Needed, otherwise the dredging fails
  
  # Model averaging - I need to figure out if we have to further redo the ME once the model has been reduced
  d.FDis <- dredge(m.FDis)              # Model construction
  d.FDis2 <- subset(d.FDis, delta<3)    # Subset based on deltaAIC
  output_FDis <- model.sel(d.FDis2)     # Model selection
  importance(output_FDis)               # Importance of the models 
  
  m.FDis.avg <- model.avg(output_FDis, fit=TRUE)     # Model averaging
  m.FDis.avg$msTable
  m.FDis.avg$coefficients
  m.FDis.avg$formula
  
  summary(m.FDis.avg)$coefmat.full  # When retrieving the results, I have chosen to use the zero-method (full model)
  confint(m.FDis.avg)
  # rather than the conditional, as this is better when we are interested in the
  # importance of variables (Grueber et al. 2011)
 }

##--- 5.2.6 Individual functional traits ---####
# We need a new dataframe of the individual (CWM) traits 
raster_data2 <- raster::addLayer(
  CWM_raster.cc[[c("Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass",
                   "Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness")]],
  env_raster)

# Convert the raster to a SpatialPixelsDataFrame and a regular dataframe (to make it 'lighter'), and remove 'incomplete' pixels
data.pxl2 <- as(raster_data2, "SpatialPixelsDataFrame")
data.pxl2 <- data.pxl2[!is.na(data.pxl2$Height.gen),]
data.pxl2 <- data.pxl2[complete.cases(data.pxl2@data), ]  # Incomplete cases in bioclim and topography
data.pxl.df2 <- as.data.frame(data.pxl2)
# We can use the neighbour lists from the previous models

## Height.gen 
{
  # Eigenvectors
  ME.Height.gen <- ME(Height.gen ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                  Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                data = data.pxl.df2,
                family = gaussian, listw = ME.listw)
  m.Height.gen <- glm(Height.gen ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                  Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.Height.gen),
                data = data.pxl.df2,
                family = gaussian,     # OBS! 'nbsp' is a count, not gaussian - I attempted Poisson first, changed to NB due to overdispersion
                na.action="na.fail")  # Needed, otherwise the dredging fails
  
  # Model averaging - I need to figure out if we have to further redo the ME once the model has been reduced
  d.Height.gen <- dredge(m.Height.gen)              # Model construction
  d.Height.gen2 <- subset(d.Height.gen, delta<3)    # Subset based on deltaAIC
  output_Height.gen <- model.sel(d.Height.gen2)     # Model selection
  importance(output_Height.gen)               # Importance of the models 
  
  m.Height.gen.avg <- model.avg(output_Height.gen, fit=TRUE)     # Model averaging
}
## Height.veg 
{
  # Eigenvectors
  ME.Height.veg <- ME(Height.veg ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                      data = data.pxl.df2,
                      family = gaussian, listw = ME.listw)
  m.Height.veg <- glm(Height.veg ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.Height.veg),
                      data = data.pxl.df2,
                      family = gaussian,     # OBS! 'nbsp' is a count, not gaussian - I attempted Poisson first, changed to NB due to overdispersion
                      na.action="na.fail")  # Needed, otherwise the dredging fails
  
  # Model averaging - I need to figure out if we have to further redo the ME once the model has been reduced
  d.Height.veg <- dredge(m.Height.veg)              # Model construction
  d.Height.veg2 <- subset(d.Height.veg, delta<3)    # Subset based on deltaAIC
  output_Height.veg <- model.sel(d.Height.veg2)     # Model selection
  importance(output_Height.veg)               # Importance of the models 
  
  m.Height.veg.avg <- model.avg(output_Height.veg, fit=TRUE)     # Model averaging
}
## LDMC 
{
  # Eigenvectors
  ME.LDMC <- ME(LDMC ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                      data = data.pxl.df2,
                      family = gaussian, listw = ME.listw)
  m.LDMC <- glm(LDMC ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.LDMC),
                      data = data.pxl.df2,
                      family = gaussian,     # OBS! 'nbsp' is a count, not gaussian - I attempted Poisson first, changed to NB due to overdispersion
                      na.action="na.fail")  # Needed, otherwise the dredging fails
  
  # Model averaging - I need to figure out if we have to further redo the ME once the model has been reduced
  d.LDMC <- dredge(m.LDMC)              # Model construction
  d.LDMC2 <- subset(d.LDMC, delta<3)    # Subset based on deltaAIC
  output_LDMC <- model.sel(d.LDMC2)     # Model selection
  importance(output_LDMC)               # Importance of the models 
  
  m.LDMC.avg <- model.avg(output_LDMC, fit=TRUE)     # Model averaging
}
## Leaf.area 
{
  # Eigenvectors
  ME.Leaf.area <- ME(Leaf.area ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                      data = data.pxl.df2,
                      family = gaussian, listw = ME.listw)
  m.Leaf.area <- glm(Leaf.area ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.Leaf.area),
                      data = data.pxl.df2,
                      family = gaussian,     # OBS! 'nbsp' is a count, not gaussian - I attempted Poisson first, changed to NB due to overdispersion
                      na.action="na.fail")  # Needed, otherwise the dredging fails
  
  # Model averaging - I need to figure out if we have to further redo the ME once the model has been reduced
  d.Leaf.area <- dredge(m.Leaf.area)              # Model construction
  d.Leaf.area2 <- subset(d.Leaf.area, delta<3)    # Subset based on deltaAIC
  output_Leaf.area <- model.sel(d.Leaf.area2)     # Model selection
  importance(output_Leaf.area)               # Importance of the models 
  
  m.Leaf.area.avg <- model.avg(output_Leaf.area, fit=TRUE)     # Model averaging
}
## Leaf.dry.mass 
{
  # Eigenvectors
  ME.Leaf.dry.mass <- ME(Leaf.dry.mass ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                      data = data.pxl.df2,
                      family = gaussian, listw = ME.listw)
  m.Leaf.dry.mass <- glm(Leaf.dry.mass ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.Leaf.dry.mass),
                      data = data.pxl.df2,
                      family = gaussian,     # OBS! 'nbsp' is a count, not gaussian - I attempted Poisson first, changed to NB due to overdispersion
                      na.action="na.fail")  # Needed, otherwise the dredging fails
  
  # Model averaging - I need to figure out if we have to further redo the ME once the model has been reduced
  d.Leaf.dry.mass <- dredge(m.Leaf.dry.mass)              # Model construction
  d.Leaf.dry.mass2 <- subset(d.Leaf.dry.mass, delta<3)    # Subset based on deltaAIC
  output_Leaf.dry.mass <- model.sel(d.Leaf.dry.mass2)     # Model selection
  importance(output_Leaf.dry.mass)               # Importance of the models 
  
  m.Leaf.dry.mass.avg <- model.avg(output_Leaf.dry.mass, fit=TRUE)     # Model averaging
}
## Seed.dry.mass 
{
  # Eigenvectors
  ME.Seed.dry.mass <- ME(Seed.dry.mass ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                      data = data.pxl.df2,
                      family = gaussian, listw = ME.listw)
  m.Seed.dry.mass <- glm(Seed.dry.mass ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.Seed.dry.mass),
                      data = data.pxl.df2,
                      family = gaussian,     # OBS! 'nbsp' is a count, not gaussian - I attempted Poisson first, changed to NB due to overdispersion
                      na.action="na.fail")  # Needed, otherwise the dredging fails
  
  # Model averaging - I need to figure out if we have to further redo the ME once the model has been reduced
  d.Seed.dry.mass <- dredge(m.Seed.dry.mass)              # Model construction
  d.Seed.dry.mass2 <- subset(d.Seed.dry.mass, delta<3)    # Subset based on deltaAIC
  output_Seed.dry.mass <- model.sel(d.Seed.dry.mass2)     # Model selection
  importance(output_Seed.dry.mass)               # Importance of the models 
  
  m.Seed.dry.mass.avg <- model.avg(output_Seed.dry.mass, fit=TRUE)     # Model averaging
}
## Seed.number.per.plant 
{
  # Eigenvectors
  ME.Seed.number.per.plant <- ME(Seed.number.per.plant ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                      data = data.pxl.df2,
                      family = gaussian, listw = ME.listw)
  m.Seed.number.per.plant <- glm(Seed.number.per.plant ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.Seed.number.per.plant),
                      data = data.pxl.df2,
                      family = gaussian,     # OBS! 'nbsp' is a count, not gaussian - I attempted Poisson first, changed to NB due to overdispersion
                      na.action="na.fail")  # Needed, otherwise the dredging fails
  
  # Model averaging - I need to figure out if we have to further redo the ME once the model has been reduced
  d.Seed.number.per.plant <- dredge(m.Seed.number.per.plant)              # Model construction
  d.Seed.number.per.plant2 <- subset(d.Seed.number.per.plant, delta<3)    # Subset based on deltaAIC
  output_Seed.number.per.plant <- model.sel(d.Seed.number.per.plant2)     # Model selection
  importance(output_Seed.number.per.plant)               # Importance of the models 
  
  m.Seed.number.per.plant.avg <- model.avg(output_Seed.number.per.plant, fit=TRUE)     # Model averaging
}
## SLA 
{
  # Eigenvectors
  ME.SLA <- ME(SLA ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                      data = data.pxl.df2,
                      family = gaussian, listw = ME.listw)
  m.SLA <- glm(SLA ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.SLA),
                      data = data.pxl.df2,
                      family = gaussian,     # OBS! 'nbsp' is a count, not gaussian - I attempted Poisson first, changed to NB due to overdispersion
                      na.action="na.fail")  # Needed, otherwise the dredging fails
  
  # Model averaging - I need to figure out if we have to further redo the ME once the model has been reduced
  d.SLA <- dredge(m.SLA)              # Model construction
  d.SLA2 <- subset(d.SLA, delta<3)    # Subset based on deltaAIC
  output_SLA <- model.sel(d.SLA2)     # Model selection
  importance(output_SLA)               # Importance of the models 
  
  m.SLA.avg <- model.avg(output_SLA, fit=TRUE)     # Model averaging
}
## Woodiness 
{
  # Eigenvectors
  ME.Woodiness <- ME(Woodiness ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                      data = data.pxl.df2,
                      family = gaussian, listw = ME.listw)
  m.Woodiness <- glm(Woodiness ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.Woodiness),
                      data = data.pxl.df2,
                      family = gaussian,     # OBS! 'nbsp' is a count, not gaussian - I attempted Poisson first, changed to NB due to overdispersion
                      na.action="na.fail")  # Needed, otherwise the dredging fails
  
  # Model averaging - I need to figure out if we have to further redo the ME once the model has been reduced
  d.Woodiness <- dredge(m.Woodiness)              # Model construction
  d.Woodiness2 <- subset(d.Woodiness, delta<3)    # Subset based on deltaAIC
  output_Woodiness <- model.sel(d.Woodiness2)     # Model selection
  importance(output_Woodiness)               # Importance of the models 
  
  m.Woodiness.avg <- model.avg(output_Woodiness, fit=TRUE)     # Model averaging
}

##--- 5.3 Plots of coefficients/variables ---####
# Following a similar approach as Ida, we plot the relative variable importance for each of the variables, and the model-averaged
# coefficients next to each other for comparisons.
# To do this nicely, gather the needed values in appropriate dataframes and plot

## Variable importance
{
  var.imp <- full_join(full_join(full_join(tibble::rownames_to_column(as.data.frame(importance(output_nbsp))),
                                                     tibble::rownames_to_column(as.data.frame(importance(output_FRic))),
                                                   by = "rowname"),
                                           tibble::rownames_to_column(as.data.frame(importance(output_FEve))),
                                         by="rowname"),
                                 #tibble::rownames_to_column(as.data.frame(importance(output_FDiv))),   # No model average - all are '1'; add manually later
                               #by="rowname"),
                       tibble::rownames_to_column(as.data.frame(importance(output_FDis))),
                     by="rowname")
names(var.imp) <- c("var","nbsp","FRic","FEve","FDis")
var.imp$FDiv <- 1
var.imp <- var.imp[c(1,3:8),]  # OBS! Take out the spatial eigenvectors
var.imp <- reshape2::melt(var.imp, id=c("var"))
#var.imp$variable <- factor(var.imp$variable, levels=c("nbsp","FRic","FEve","FDiv","FDis"), ordered = T)  # fix order for better plotting
names(var.imp) <- c("var", "metric","importance")

p.imp <- var.imp %>%
  mutate(metric = fct_relevel(metric, "nbsp","FRic","FEve","FDiv","FDis")) %>%
  ggplot( aes(x=var, y=importance, shape=metric, fill=metric, width=0.5)) +
  geom_bar(position = position_dodge(width = 0.75), stat="identity") +
  labs(x="",y="Relative variable importance") +
  scale_fill_manual(values=c("gold","darkorange2","firebrick","navy","forestgreen")) + #,
                    #name="FD metrics", labels=c("No. species","Funct richness","Funct. evenness","Funct. Divergence","Funct. Dispersion")) +
  #scale_shape_manual(values=rev(c(0,1,2,5,6)),  # To make a common legend with the coefficients
  #                   name="FD metrics", labels=rev(c("No. species","Funct richness","Funct. evenness","Funct. Divergence","Funct. Dispersion"))) +
  coord_flip() +
  theme_minimal() 
}
## Model-averaged coefficients
{
coef.df.FDis <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.FDis.avg)$coefmat.subset))[,c(1,2)],
                          tibble::rownames_to_column(as.data.frame(confint(m.FDis.avg))))
      names(coef.df.FDis) <- c("var","est.FDis","lwr.FDis","upr.FDis")
coef.df.FDiv <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.FDiv.avg)$coefficients))[,c(1,2)],  # OBS!
                          tibble::rownames_to_column(as.data.frame(confint(m.FDiv.avg))))
      names(coef.df.FDiv) <- c("var","est.FDiv","lwr.FDiv","upr.FDiv")
coef.df.FEve <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.FEve.avg)$coefmat.subset))[,c(1,2)],
                          tibble::rownames_to_column(as.data.frame(confint(m.FEve.avg))))
      names(coef.df.FEve) <- c("var","est.FEve","lwr.FEve","upr.FEve")
coef.df.FRic <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.FRic.avg)$coefmat.subset))[,c(1,2)],
                          tibble::rownames_to_column(as.data.frame(confint(m.FRic.avg))))
      names(coef.df.FRic) <- c("var","est.FRic","lwr.FRic","upr.FRic")
coef.df.nbsp <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.nbsp.avg)$coefmat.subset))[,c(1,2)],
                          tibble::rownames_to_column(as.data.frame(confint(m.nbsp.avg))))
      names(coef.df.nbsp) <- c("var","est.nbsp","lwr.nbsp","upr.nbsp")
      
coef.df <- full_join(full_join(full_join(full_join(coef.df.FDis,
                                                   coef.df.FDiv, by="var"),
                                         coef.df.FEve, by="var"),
                               coef.df.FRic, by="var"),
                     coef.df.nbsp, byr="var")
coef.df <- coef.df[coef.df$var %in% c("Annual.Precipitation","PC2","Glaciation","Habitat.heterogeneity",
                                      "Precipitation.Seasonality.","Mean.Temperature.of.Warmest.Quarter",
                                      "Topographic.heterogeneity"),]
coef.df <- data.frame(var = c(rep(coef.df$var, 5)),
                      metric = c(rep(c("FDis","FDiv","FEve","FRic","nbsp"), each=7)),
                      est = c(coef.df$est.FDis, coef.df$est.FDiv, coef.df$est.FEve, coef.df$est.FRic, coef.df$est.nbsp),
                      lwr = c(coef.df$lwr.FDis, coef.df$lwr.FDiv, coef.df$lwr.FEve, coef.df$lwr.FRic, coef.df$lwr.nbsp),
                      upr = c(coef.df$upr.FDis, coef.df$upr.FDiv, coef.df$upr.FEve, coef.df$upr.FRic, coef.df$upr.nbsp))
#coef.df$metric <- factor(coef.df$metric, levels=c("nbsp","FRic","FEve","FDiv","FDis"), ordered = T)  # fix order for better plotting


p.coef <- coef.df %>%
  mutate(metric = fct_relevel(metric, "nbsp","FRic","FEve","FDiv","FDis")) %>%
  ggplot( aes(x=var, y=est, color=metric, shape=metric)) +
  geom_hline(yintercept = 0, linetype="dashed", color="gray") +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.5, position = position_dodge(width = 0.75)) +
  labs(x="",y="Model-averaged coefficient estimates") +
  scale_color_manual(values=(c("gold","darkorange2","firebrick","navy","forestgreen")) ) +#,
                    #name="FD metrics", labels=(c("No. species","Funct richness","Funct. evenness","Funct. Divergence","Funct. Dispersion"))) +
  #scale_shape_manual(values=(c(0,1,2,5,6)),
  #                   name="FD metrics", labels=(c("No. species","Funct richness","Funct. evenness","Funct. Divergence","Funct. Dispersion"))) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text = element_blank())

}

ggarrange(p.imp, p.coef, ncol=2, nrow=1, common.legend = T, legend="bottom")

##--- 5.3.1 Individual model-averaged coefficient estimates for the different trait metrics---####

# To assess the individual trait metrics
coef.df %>%
  mutate(metric = fct_relevel(metric, "nbsp","FRic","FEve","FDiv","FDis")) %>%
  ggplot( aes(x=var, y=est, color=metric, shape=metric)) +
  geom_hline(yintercept = 0, linetype="dashed", color="gray") +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.5, position = position_dodge(width = 0.75)) +
  labs(x="",y="Model-averaged coefficient estimates") +
  scale_color_manual(values=(c("gold","darkorange2","firebrick","navy","forestgreen")) ) +#,
  #name="FD metrics", labels=(c("No. species","Funct richness","Funct. evenness","Funct. Divergence","Funct. Dispersion"))) +
  #scale_shape_manual(values=(c(0,1,2,5,6)),
  #                   name="FD metrics", labels=(c("No. species","Funct richness","Funct. evenness","Funct. Divergence","Funct. Dispersion"))) +
  coord_flip() +
  facet_wrap(~ metric, scales = "free", nrow=2) +
  theme_bw() +
  theme(legend.position = "none")

##--- 5.3.2 Coefficients/variables, individual traits ---####
# For the individual traits rather than trait metrics:
## Variable importance
{
  var.imp2 <- full_join(full_join(full_join(full_join(full_join(full_join(full_join(full_join(tibble::rownames_to_column(as.data.frame(importance(output_Height.gen))),
                        tibble::rownames_to_column(as.data.frame(importance(output_Height.veg))), by = "rowname"),
                        tibble::rownames_to_column(as.data.frame(importance(output_LDMC))), by="rowname"),
                        tibble::rownames_to_column(as.data.frame(importance(output_Leaf.area))), by="rowname"),
                        tibble::rownames_to_column(as.data.frame(importance(output_Leaf.dry.mass))), by="rowname"),
                        tibble::rownames_to_column(as.data.frame(importance(output_Seed.dry.mass))), by="rowname"),
                        tibble::rownames_to_column(as.data.frame(importance(output_Seed.number.per.plant))), by="rowname"),
                        tibble::rownames_to_column(as.data.frame(importance(output_SLA))), by="rowname"),
                        tibble::rownames_to_column(as.data.frame(importance(output_Woodiness))), by="rowname")
  
  names(var.imp2) <- c("var","Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass","Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness")
  var.imp2 <- var.imp2[c(1,3:8),]  # OBS! Take out the spatial eigenvectors
  var.imp2 <- reshape2::melt(var.imp2, id=c("var"))
  names(var.imp2) <- c("var", "metric","importance")
  
  p.imp2 <- var.imp2 %>%
    mutate(metric = fct_relevel(metric, "Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass","Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness")) %>%
    ggplot( aes(x=var, y=importance, fill=metric, width=0.5)) +
    geom_bar(position = position_dodge(width = 0.75), stat="identity") +
    labs(x="",y="Relative variable importance") +
    #scale_fill_manual(values=c()) + 
    coord_flip() +
    theme_minimal() 
}

## Model-averaged coefficients
{
  {
    coef.df.Height.gen <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.Height.gen.avg)$coefmat.subset))[,c(1,2)],
                            tibble::rownames_to_column(as.data.frame(confint(m.Height.gen.avg))))
    names(coef.df.Height.gen) <- c("var","est.Height.gen","lwr.Height.gen","upr.Height.gen")
    coef.df.Height.veg <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.Height.veg.avg)$coefmat.subset))[,c(1,2)],
                              tibble::rownames_to_column(as.data.frame(confint(m.Height.veg.avg))))
    names(coef.df.Height.veg) <- c("var","est.Height.veg","lwr.Height.veg","upr.Height.veg")
    coef.df.LDMC <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.LDMC.avg)$coefmat.subset))[,c(1,2)],
                              tibble::rownames_to_column(as.data.frame(confint(m.LDMC.avg))))
    names(coef.df.LDMC) <- c("var","est.LDMC","lwr.LDMC","upr.LDMC")
    coef.df.Leaf.area <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.Leaf.area.avg)$coefmat.subset))[,c(1,2)],
                              tibble::rownames_to_column(as.data.frame(confint(m.Leaf.area.avg))))
    names(coef.df.Leaf.area) <- c("var","est.Leaf.area","lwr.Leaf.area","upr.Leaf.area")
    coef.df.Leaf.dry.mass <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.Leaf.dry.mass.avg)$coefmat.subset))[,c(1,2)],
                              tibble::rownames_to_column(as.data.frame(confint(m.Leaf.dry.mass.avg))))
    names(coef.df.Leaf.dry.mass) <- c("var","est.Leaf.dry.mass","lwr.Leaf.dry.mass","upr.Leaf.dry.mass")
    coef.df.Seed.dry.mass <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.Seed.dry.mass.avg)$coefmat.subset))[,c(1,2)],
                              tibble::rownames_to_column(as.data.frame(confint(m.Seed.dry.mass.avg))))
    names(coef.df.Seed.dry.mass) <- c("var","est.Seed.dry.mass","lwr.Seed.dry.mass","upr.Seed.dry.mass")
    coef.df.Seed.number.per.plant <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.Seed.number.per.plant.avg)$coefmat.subset))[,c(1,2)],
                              tibble::rownames_to_column(as.data.frame(confint(m.Seed.number.per.plant.avg))))
    names(coef.df.Seed.number.per.plant) <- c("var","est.Seed.number.per.plant","lwr.Seed.number.per.plant","upr.Seed.number.per.plant")
    coef.df.SLA <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.SLA.avg)$coefmat.subset))[,c(1,2)],
                              tibble::rownames_to_column(as.data.frame(confint(m.SLA.avg))))
    names(coef.df.SLA) <- c("var","est.SLA","lwr.SLA","upr.SLA")
    coef.df.Woodiness <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.Woodiness.avg)$coefmat.subset))[,c(1,2)],
                              tibble::rownames_to_column(as.data.frame(confint(m.Woodiness.avg))))
    names(coef.df.Woodiness) <- c("var","est.Woodiness","lwr.Woodiness","upr.Woodiness")
  
  }
  
  coef.df2 <- full_join(full_join(full_join(full_join(full_join(full_join(full_join(full_join(coef.df.Height.gen,
                        coef.df.Height.veg, by="var"),
                        coef.df.LDMC, by="var"),
                        coef.df.Leaf.area, by="var"),
                        coef.df.Leaf.dry.mass, by="var"),
                        coef.df.Seed.dry.mass, by="var"),
                        coef.df.Seed.number.per.plant, by="var"),
                        coef.df.SLA, by="var"),
                        coef.df.Woodiness, by="var")
  
  coef.df2 <- coef.df2[coef.df2$var %in% c("Annual.Precipitation","PC2","Glaciation","Habitat.heterogeneity",
                                        "Precipitation.Seasonality.","Mean.Temperature.of.Warmest.Quarter",
                                        "Topographic.heterogeneity"),]
  coef.df2 <- data.frame(var = c(rep(coef.df2$var, 9)),
                        metric = c(rep(c("Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass",
                                         "Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness"), each=7)),
                        est = c(coef.df2$est.Height.gen, coef.df2$est.Height.veg, coef.df2$est.LDMC, coef.df2$est.Leaf.area, coef.df2$est.Leaf.dry.mass, coef.df2$est.Seed.dry.mass, coef.df2$est.Seed.number.per.plant, coef.df2$est.SLA, coef.df2$est.Woodiness),
                        lwr = c(coef.df2$lwr.Height.gen, coef.df2$lwr.Height.veg, coef.df2$lwr.LDMC, coef.df2$lwr.Leaf.area, coef.df2$lwr.Leaf.dry.mass, coef.df2$lwr.Seed.dry.mass, coef.df2$lwr.Seed.number.per.plant, coef.df2$lwr.SLA, coef.df2$lwr.Woodiness),
                        upr = c(coef.df2$upr.Height.gen, coef.df2$upr.Height.veg, coef.df2$upr.LDMC, coef.df2$upr.Leaf.area, coef.df2$upr.Leaf.dry.mass, coef.df2$upr.Seed.dry.mass, coef.df2$upr.Seed.number.per.plant, coef.df2$upr.SLA, coef.df2$upr.Woodiness))

  
  p.coef2 <- coef.df2 %>%
    mutate(metric = fct_relevel(metric, "Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass",
                                "Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness")) %>%
    ggplot( aes(x=var, y=est, color=metric)) +
    geom_hline(yintercept = 0, linetype="dashed", color="gray") +
    geom_point(position = position_dodge(width = 0.75)) +
    geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.5, position = position_dodge(width = 0.75)) +
    labs(x="",y="Model-averaged coefficient estimates") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(~ metric, scales = "free")
  
}

##----------------------------------------------------####
##--- 6. PHYLOGENETIC DIVERSITY ---####
library(ape, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(picante, lib.loc = "/home/ahomez/t/tanjakp/export/library") 
library(rasterVis, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(hexbin, lib.loc = "/home/ahomez/t/tanjakp/export/library")
library(nlme, lib.loc = "/home/ahomez/t/tanjakp/export/library")

# We have to recreate the phylogenies and calculate phylogenetic diversity of the grid cells/communities
# The phylogeny is available as a newick file here https://datadryad.org/stash/dataset/doi:10.5061/dryad.kkwh70s1n 
phylogeny<-read.tree('/home/ahomez/t/tanjakp/NorwegianFloraTraits/Phylogeny/NorwVascPlantPhylogeny.nwk')
plot(phylogeny)
phylogeny$tip.label

# We here have an issue in that the names are not of the same format as our names - fix this
# with 'cleaned up' names  
{
  # Remove strings before genus and species epithet (specify the strings manually)
  phylogeny$tip.label <- {str_remove_all(phylogeny$tip.label, paste(c("Asteraceae_","Menyanthaceae_","Campanulaceae_","Apiaceae_",
                                                                   "Araliaceae_","Caprifoliaceae_","Adoxaceae_","Aquifoliaceae_",
                                                                   "Orobanchaceae_","Scrophulariaceae_","Lamiaceae_","Lentibulariaceae_",
                                                                   "Plantaginaceae_","Oleaceae_","Rubiaceae_","Gentianaceae_",
                                                                   "Boraginaceae_","Convolvulaceae_","Solanaceae_","Ericaceae_",
                                                                   "Diapensiaceae_","Primulaceae_","Polemoniaceae_","Balsaminaceae_",
                                                                   "Cornaceae_","Caryophyllaceae_","Portulacaceae_","Amaranthaceae_",
                                                                   "Polygonaceae_","Plumbaginaceae_","Droseraceae_","Tamaricaceae_",
                                                                   "Santalaceae_","Rosaceae_","Urticaceae_","Cannabaceae_","Ulmaceae_",
                                                                   "Rhamnaceae_","Elaeagnaceae_","Betulaceae_","Myricaceae_","Fagaceae_",
                                                                   "Fabaceae_","Polygalaceae_","Salicaceae_","Violaceae_","Hypericaceae_",
                                                                   "Linaceae_","Elatinaceae_","Euphorbiaceae_","Oxalidaceae_","Celastraceae_",
                                                                   "Brassicaceae_","Malvaceae_","Cistaceae_","Thymelaeaceae_","Sapindaceae_",
                                                                   "Onagraceae_","Lythraceae_","Geraniaceae_","Saxifragaceae_","Grossulariaceae_",
                                                                   "Crassulaceae_","Haloragaceae_","Ranunculaceae_","Papaveraceae_",
                                                                   "Ceratophyllaceae_","MONO_Cyperaceae_","MONO_Juncaceae_","MONO_Poaceae_",
                                                                   "MONO_Typhaceae_","MONO_Orchidaceae_","MONO_Amaryllidaceae_",
                                                                   "MONO_Asparagaceae_","MONO_Iridaceae_","MONO_Melanthiaceae_",
                                                                   "MONO_Liliaceae_","MONO_Potamogetonaceae_","MONO_Zosteraceae_",
                                                                   "MONO_Ruppiaceae_","MONO_Juncaginaceae_","MONO_Scheuchzeriaceae_",
                                                                   "MONO_Hydrocharitaceae_","MONO_Butomaceae_","MONO_Alismataceae_",
                                                                   "MONO_Araceae_","MONO_Tofieldiaceae_","Nymphaeaceae_","PINE_Pinaceae_",
                                                                   "PINE_Taxaceae_","PINE_Cupressaceae_","POLYPOD_Thelypteridaceae_",
                                                                   "POLYPOD_Woodsiaceae_","POLYPOD_Dryopteridaceae_","POLYPOD_Onocleaceae_",
                                                                   "POLYPOD_Cystopteridaceae_","POLYPOD_Aspleniaceae_","POLYPOD_Polypodiaceae_",
                                                                   "POLYPOD_Pteridaceae_","POLYPOD_Dennstaedtiaceae_","POLYPOD_Marsileaceae_",
                                                                   "POLYPOD_Hymenophyllaceae_","POLYPOD_Ophioglossaceae_","POLYPOD_Osmundaceae_",
                                                                   "LYCO_Equisetaceae_","LYCO_Lycopodiaceae_","LYCO_Isoetaceae_",
                                                                   "LYCO_Selaginellaceae_","POLYPOD_Athyriaceae","POLYPOD_Blechnaceae"),
                                                                   collapse="|"))}
  # Fix "broken" names 
  phylogeny$tip.label <- str_trim(gsub("√´", "e", gsub("\xd7","x", gsub("[.]", " ", as.character(phylogeny$tip.label)))), side = "both")
  # Replace '_' with space
  phylogeny$tip.label <- gsub("_", " ", phylogeny$tip.label)
  # Check for species not in the traits dataframe
  phylogeny$tip.label[!(phylogeny$tip.label %in% traits$Species.gbif)]
  # We need to do some manual fixes of species names to make them match:
  {
    phylogeny$tip.label[phylogeny$tip.label=="Mycelis muralis"] <- "Lactuca muralis"
    phylogeny$tip.label[phylogeny$tip.label=="Mulgedium sibiricum"] <- "Lactuca sibirica"
    phylogeny$tip.label[phylogeny$tip.label=="Hieracium diphanum"] <- "Hieracium diaphanum"
    phylogeny$tip.label[phylogeny$tip.label=="Cirsium acaule"] <- "Cirsium acaulon"
    phylogeny$tip.label[phylogeny$tip.label=="Peucedanum palustre"] <- "Thysselinum palustre"
    phylogeny$tip.label[phylogeny$tip.label=="Conioselinum vaginatum"] <- "Conioselinum vaginatum"  # NOT FOUND
    phylogeny$tip.label[phylogeny$tip.label=="Crithmum maritimum"] <- "Crithmum maritimum"     # NOT FOUND
    phylogeny$tip.label[phylogeny$tip.label=="Ligusticum scothicum"] <- "Ligusticum scoticum"
    phylogeny$tip.label[phylogeny$tip.label=="Acinos arvensis"] <- "Clinopodium acinos"
    phylogeny$tip.label[phylogeny$tip.label=="Littorella uniflora"] <- "Plantago uniflora"
    phylogeny$tip.label[phylogeny$tip.label=="Ligustrum vulgare"] <- "Syringa vulgaris"
    phylogeny$tip.label[phylogeny$tip.label=="Lappula deflexa"] <- "Hackelia deflexa"
    phylogeny$tip.label[phylogeny$tip.label=="Oxycoccus palustris"] <- "Vaccinium oxycoccos"
    phylogeny$tip.label[phylogeny$tip.label=="Arctous alpinus"] <- "Arctostaphylos alpinus"
    phylogeny$tip.label[phylogeny$tip.label=="Monotropa hypopitys"] <- "Hypopitys monotropa"
    phylogeny$tip.label[phylogeny$tip.label=="Anagallis minima"] <- "Lysimachia minima"
    phylogeny$tip.label[phylogeny$tip.label=="Glaux maritima"] <- "Lysimachia maritima"
    phylogeny$tip.label[phylogeny$tip.label=="Chamaepericlymenum suecicum"] <- "Cornus suecica"
    phylogeny$tip.label[phylogeny$tip.label=="Lychnis flos-cuculi"] <- "Silene flos-cuculi"
    phylogeny$tip.label[phylogeny$tip.label=="Atocion rupestre"] <- "Heliosperma pusillum"
    phylogeny$tip.label[phylogeny$tip.label=="Viscaria alpina"] <- "Silene suecica"
    phylogeny$tip.label[phylogeny$tip.label=="Minuartia stricta"] <- "Sabulina stricta"
    phylogeny$tip.label[phylogeny$tip.label=="Minuartia rubella"] <- "Sabulina rubella"
    phylogeny$tip.label[phylogeny$tip.label=="Minuartia biflora"] <- "Cherleria biflora"
    phylogeny$tip.label[phylogeny$tip.label=="Cerastium nigrescens"] <- "Cerastium arcticum"  
    phylogeny$tip.label[phylogeny$tip.label=="Salsola kali"] <- "Kali turgida"   
    phylogeny$tip.label[phylogeny$tip.label=="Alchemilla acutiloba"] <- "Alchemilla vulgaris"
    phylogeny$tip.label[phylogeny$tip.label=="Alchemilla taernansis"] <- "Alchemilla taernaensis"   
    phylogeny$tip.label[phylogeny$tip.label=="Alchemilla semidevisa"] <- "Alchemilla semidivisa"   
    phylogeny$tip.label[phylogeny$tip.label=="Potentilla anserina"] <- "Argentina anserina"
    phylogeny$tip.label[phylogeny$tip.label=="Potentilla anserina"] <- "Argentina anserina"
    phylogeny$tip.label[phylogeny$tip.label=="Rosa pimpinellifolia"] <- "Rosa spinosissima"
    phylogeny$tip.label[phylogeny$tip.label=="Rubus lindleyanus"] <- "Rubus lindleianus"  
    phylogeny$tip.label[phylogeny$tip.label=="Cotoneaster laxiflorus"] <- "Cotoneaster niger"    
    phylogeny$tip.label[phylogeny$tip.label=="Tetragonolobus maritimus"] <- "Lotus maritimus"
    phylogeny$tip.label[phylogeny$tip.label=="Salix xarctogena"] <- "Salix arctogena"
    phylogeny$tip.label[phylogeny$tip.label=="Elatine hydropiper"] <- "Elatine hydropiper"  # NOT FOUND
    phylogeny$tip.label[phylogeny$tip.label=="Lavatera arborea"] <- "Malva arborea"
    phylogeny$tip.label[phylogeny$tip.label=="Chamerion angustifolium"] <- "Epilobium angustifolium"
    phylogeny$tip.label[phylogeny$tip.label=="Sedum rupestre"] <- "Petrosedum rupestre"
    phylogeny$tip.label[phylogeny$tip.label=="Coptidium lapponicum"] <- "Ranunculus lapponicus"
    phylogeny$tip.label[phylogeny$tip.label=="Carex vacillans"] <- "Carex xvacillans"
    phylogeny$tip.label[phylogeny$tip.label=="Kobresia myosuroides"] <- "Carex myosuroides"
    phylogeny$tip.label[phylogeny$tip.label=="Kobresia simpliciuscula"] <- "Carex simpliciuscula"
    phylogeny$tip.label[phylogeny$tip.label=="Blysmopsis rufa"] <- "Blysmus rufus"
    phylogeny$tip.label[phylogeny$tip.label=="Eleogiton fluitans"] <- "Isolepis fluitans"
    phylogeny$tip.label[phylogeny$tip.label=="Schedonorus giganteus"] <- "Lolium giganteum"
    phylogeny$tip.label[phylogeny$tip.label=="Drymochloa sylvatica"] <- "Festuca altissima"
    phylogeny$tip.label[phylogeny$tip.label=="Avenula pratensis"] <- "Helictochloa pratensis"
    phylogeny$tip.label[phylogeny$tip.label=="Agrostis canescens"] <- "Calamagrostis canescens"
    phylogeny$tip.label[phylogeny$tip.label=="Agrostis neglecta"] <- "Calamagrostis neglecta"
    phylogeny$tip.label[phylogeny$tip.label=="Agrostis chalybaea"] <- "Calamagrostis chalybaea"
    phylogeny$tip.label[phylogeny$tip.label=="Agrostis phragmitoides"] <- "Calamagrostis phragmitoides"
    phylogeny$tip.label[phylogeny$tip.label=="Agrostis arundinacea"] <- "Calamagrostis arundinacea"
    phylogeny$tip.label[phylogeny$tip.label=="Agrostis epigejos"] <- "Calamagrostis epigejos"
    phylogeny$tip.label[phylogeny$tip.label=="Agrostis lapponica"] <- "Calamagrostis lapponica"
    phylogeny$tip.label[phylogeny$tip.label=="Hierochloe hirta"] <- "Anthoxanthum nitens"
    phylogeny$tip.label[phylogeny$tip.label=="Hierochloe odorata"] <- "Anthoxanthum nitens"
    phylogeny$tip.label[phylogeny$tip.label=="Hierochloe alpina"] <- "Anthoxanthum monticola"
    phylogeny$tip.label[phylogeny$tip.label=="Elytrigia juncea"] <- "Thinopyrum junceum"
    phylogeny$tip.label[phylogeny$tip.label=="Elytrigia repens"] <- "Elymus repens"
    phylogeny$tip.label[phylogeny$tip.label=="Anisantha sterilis"] <- "Bromus sterilis"
    phylogeny$tip.label[phylogeny$tip.label=="Anisantha tectorum"] <- "Bromus tectorum"
    phylogeny$tip.label[phylogeny$tip.label=="Bromopsis benekenii"] <- "Bromus benekenii"
    phylogeny$tip.label[phylogeny$tip.label=="Bromopsis ramosa"] <- "Bromus ramosus"
    phylogeny$tip.label[phylogeny$tip.label=="Coeloglossum viride"] <- "Dactylorhiza viridis"
    phylogeny$tip.label[phylogeny$tip.label=="Nigritella nigra"] <- "Gymnadenia nigra"
    phylogeny$tip.label[phylogeny$tip.label=="Platanthera chlorantha"] <- "Platanthera montana"
    phylogeny$tip.label[phylogeny$tip.label=="Listera ovata"] <- "Neottia ovata"
    phylogeny$tip.label[phylogeny$tip.label=="Listera cordata"] <- "Neottia cordata"
    phylogeny$tip.label[phylogeny$tip.label=="Zostera noltei"] <- "Zostera noltii"
    phylogeny$tip.label[phylogeny$tip.label==" Diplazium sibiricum"] <- "Diplazium sibiricum"
    phylogeny$tip.label[phylogeny$tip.label==" Blechnum spicant"] <- "Blechnum spicant"
    phylogeny$tip.label[phylogeny$tip.label=="Botrychium multifidum"] <- "Sceptridium multifidum"
    phylogeny$tip.label[phylogeny$tip.label=="Lycopodium annotinum"] <- "Spinulum annotinum"
    phylogeny$tip.label[phylogeny$tip.label=="Schedonorus arundinaceus"] <- "Scolochloa festucacea"
  }  
  
  phylogeny$tip.label[which(!phylogeny$tip.label%in%traits$Species.gbif)]   # Three species not found
}

# Replace '_' with space to match the names in the CMW_species community matrix
phylogeny$tip.label <- gsub("-", ".", phylogeny$tip.label)

# Standardise edge lengths if we want PD as a proportion of total PD
phylo2 <- phylogeny
phylo2$edge.length <- phylogeny$edge.length/sum(phylogeny$edge.length)

# Then you have to link it to the community data - the community data cannot have empty sites; therefore, use only the
# species used in all other analyses, CWM_speciesFD5
# Use picante to trim community and phylogenetic data
phydata <- match.phylo.comm(phylo2, CWM_speciesFD5)
      # Two species were dropped from the community data, as they were not found in the phylogeny - these seem to be genuinely missing:

# PD
phydiv <- pd(phydata$comm, phydata$phy, include.root=T)
# Which you can then rasterize
phydiv$Pixelnr <- (rownames(phydiv))
phydiv <- left_join(data.frame(Pixelnr=AR50_terr[,c("Pixelnr")]), phydiv)

raster_data <- addLayer(addLayer(raster_data,
                                  setValues(dropLayer(pa20k_stack, c(2:1248)),
                                            phydiv$PD, layer=1)),
                         setValues(dropLayer(pa20k_stack, c(2:1248)),
                                   phydiv$SR, layer=1))
names(raster_data) <- c("nbsp","FRic","FEve","FDiv","FDis","Annual.Precipitation","Mean.Temperature.of.Warmest.Quarter",
                        "Precipitation.Seasonality.","Topographic.heterogeneity","soil.pH","Glaciation","Developed",
                        "Agriculture","Forest","Open","Mire","Snow.Ice","Freshwater","Habitat.heterogeneity","PC1","PC2",
                        "PD","SR_phylo" )

plot(raster_data[["PD"]], axes=F, box=F, main="PD")


# Pair plots of all traits against each other and pair plots of the SR/FD incl PD:
pairs(as.data.frame(raster_data)[,c("nbsp","FRic","FEve","FDiv","FDis","PD","SR_phylo")], lower.panel = panel.cor)
pairs(as.data.frame(raster_data)[,c("Annual.Precipitation","Mean.Temperature.of.Warmest.Quarter",
                                    "Precipitation.Seasonality.","Topographic.heterogeneity","soil.pH","Glaciation","Developed",
                                    "Agriculture","Forest","Open","Mire","Snow.Ice","Freshwater","Habitat.heterogeneity","PC1","PC2")], lower.panel = panel.cor)
pairs(as.data.frame(raster_data2)[,c("Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass","Seed.dry.mass",
                                    "Seed.number.per.plant","SLA","Woodiness")], lower.panel = panel.cor)



##------------------------------####
##--- 7. SENSITIVITY TO OUTLIER ---####
# During the meeting on 08.03.21 we found that some if the patterns in FD are caused by outliers in the underlying PCoA,
# in particular Osmunda regalis. We therefore have to check how sensitive the results are to excluding this species:

##--- 7.1 Functional diversity metrics ---####
# Remove Osmunda regalis from trait- and communita data
traitsFD6 <- traitsFD5[!rownames(traitsFD5)=="Osmunda regalis",]
  
CWM_speciesFD6 <- CWM_speciesFD[which(rownames(CWM_speciesFD) %in% rownames(CWM.df)),
                                which(colnames(CWM_speciesFD) %in% rownames(traitsFD6))]
  
# Calculate with a standardised FRic
{
  FD.osreg <- dbFD(traitsFD6, as.matrix(CWM_speciesFD6),
                w.abun = FALSE, stand.x = TRUE,  # OBS on these ones
                corr = c("cailliez"),            # OBS on this one
                calc.FRic = TRUE, m = 'min', stand.FRic = TRUE,
                scale.RaoQ = FALSE,
                calc.FGR = FALSE,
                clust.type = "ward",
                calc.CWM = TRUE, CWM.type = c("all"), 
                calc.FDiv = TRUE, 
                print.pco = TRUE, messages = TRUE)
}
  
FD.osreg.df <- cbind(as.integer(rownames(FD.osreg$CWM)),
                  FD.osreg$nbsp,
                  FD.osreg$sing.sp,
                  FD.osreg$FRic, FD.osreg$FEve,
                  FD.osreg$FDiv, FD.osreg$FDis,
                  FD.osreg$RaoQ, FD.osreg$CWM[,c(1:8,10)] # Remember to use 'Woodiness_1' as the right variable!
)
  names(FD.osreg.df) <- c("Pixelnr","nbsp", "sing.sp","FRic","FEve","FDiv","FDis","RaoQ",names(FD.osreg$CWM[,c(1:8,10)]))
  FD.osreg.df <- left_join(data.frame(Pixelnr = functcomp.norway[,c("Pixelnr")]), FD.osreg.df, by="Pixelnr")
  
  # Make a raster with CWM trait values, number of species and Functional Diversity Indices
  CWM_raster.osreg <- dropLayer(pa20k_stack, c(17:1248))  # OBS HERE!
  CWM_raster.osreg <- setValues(CWM_raster.osreg, FD.osreg.df$Height.gen, layer=1)
  CWM_raster.osreg <- setValues(CWM_raster.osreg, FD.osreg.df$Height.veg, layer=2)
  CWM_raster.osreg <- setValues(CWM_raster.osreg, FD.osreg.df$LDMC, layer=3)
  CWM_raster.osreg <- setValues(CWM_raster.osreg, FD.osreg.df$Leaf.area, layer=4)
  CWM_raster.osreg <- setValues(CWM_raster.osreg, FD.osreg.df$Leaf.dry.mass, layer=5)
  CWM_raster.osreg <- setValues(CWM_raster.osreg, FD.osreg.df$Seed.dry.mass, layer=6)
  CWM_raster.osreg <- setValues(CWM_raster.osreg, FD.osreg.df$Seed.number.per.plant, layer=7)
  CWM_raster.osreg <- setValues(CWM_raster.osreg, FD.osreg.df$SLA, layer=8)
  CWM_raster.osreg <- setValues(CWM_raster.osreg, FD.osreg.df$Woodiness, layer=9)
  CWM_raster.osreg <- setValues(CWM_raster.osreg, FD.osreg.df$nbsp, layer=10)
  CWM_raster.osreg <- setValues(CWM_raster.osreg, FD.osreg.df$sing.sp, layer=11)
  CWM_raster.osreg <- setValues(CWM_raster.osreg, FD.osreg.df$FRic, layer=12)
  CWM_raster.osreg <- setValues(CWM_raster.osreg, FD.osreg.df$FEve, layer=13)
  CWM_raster.osreg <- setValues(CWM_raster.osreg, FD.osreg.df$FDiv, layer=14)
  CWM_raster.osreg <- setValues(CWM_raster.osreg, FD.osreg.df$FDis, layer=15)
  CWM_raster.osreg <- setValues(CWM_raster.osreg, FD.osreg.df$RaoQ, layer=16)
  
  names(CWM_raster.osreg) <- c("Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass","Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness",
                            "nbsp", "sing.sp","FRic","FEve","FDiv","FDis","RaoQ")
  
  plot(CWM_raster.osreg[[c(10,12:15)]], box=F, axes=F)

  # Histograms of all indices and species richness
  ggarrange(ggplot(FD.osreg.df, aes(x=nbsp)) +
              geom_histogram() +
              labs(x="Number of species"),
            ggplot(FD.osreg.df, aes(x=FRic)) +
              geom_histogram() +
              labs(x="FRic"),
            ggplot(FD.osreg.df, aes(x=FEve)) +
              geom_histogram() +
              labs(x="FEve"),
            ggplot(FD.osreg.df, aes(x=FDiv)) +
              geom_histogram() +
              labs(x="FDiv"),
            ggplot(FD.osreg.df, aes(x=FDis)) +
              geom_histogram() +
              labs(x="FDis"),
            nrow = 2, ncol=3)

##--- 7.2 Modelling, FD metrics ---####
# Gather both predictors and relevant responses in a single raster:
raster_data.osreg <- raster::addLayer(
  CWM_raster.osreg[[c("nbsp","FRic","FEve","FDiv","FDis")]],
  env_raster)  

# Check the response variables for outliers:
Mydotplot(FD.osreg.df[,c("nbsp","FRic","FEve","FDiv","FDis")])   
  
# Convert the raster to a SpatialPixelsDataFrame and a regular dataframe (to make it 'lighter'), and remove 'incomplete' pixels
data.pxl.osreg <- as(raster_data.osreg, "SpatialPixelsDataFrame")
data.pxl.osreg <- data.pxl.osreg[!is.na(data.pxl.osreg$nbsp),]
data.pxl.osreg <- data.pxl.osreg[complete.cases(data.pxl.osreg@data), ]  # Incomplete cases in bioclim and topography
data.pxl.df.osreg <- as.data.frame(data.pxl.osreg)
data.pxl.df.osreg$nbsp.prop <- data.pxl.osreg$nbsp/1232  # proportion of total no. species

nb.pxl.osreg <- dnearneigh(coordinates(data.pxl.osreg), 0, 28284.3)
nb.pxl_dists.osreg <- nbdists(nb.pxl.osreg, coordinates(data.pxl.osreg))  # List of Euclidian distances along the neighbourhood links
nb.pxl_sims.osreg <- lapply(nb.pxl_dists.osreg, function(x) (1-((x/4)^2)) )  # List of general weights corresponding to the neighbours
ME.listw.osreg <- nb2listw(nb.pxl.osreg, glist=nb.pxl_sims.osreg, style="B", zero.policy = TRUE)  # Spatial weights for neighbours 
  
### Number of species
{
  ME.nbsp.osreg <- ME(nbsp.prop ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                      data = data.pxl.df.osreg,
                      family = gaussian, listw = ME.listw)
  m.nbsp.osreg <- glm(nbsp.prop ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.nbsp.osreg),
                      data = data.pxl.df.osreg,
                      family = gaussian,     
                      na.action="na.fail")  
  
  d.nbsp.osreg <- dredge(m.nbsp.osreg)              # Model construction
  d.nbsp2.osreg <- subset(d.nbsp.osreg, delta<3)    # Subset based on deltaAIC
  output_nbsp.osreg <- model.sel(d.nbsp2.osreg)     # Model selection
  m.nbsp.avg.osreg <- model.avg(output_nbsp.osreg, fit=TRUE)     # Model averaging
}
### Functional richness
{
ME.FRic.osreg <- ME(FRic ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                      Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                    data = data.pxl.df.osreg,
                    family = gaussian, listw = ME.listw.osreg)
m.FRic.osreg <- glm(FRic ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                      Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.FRic.osreg),
                    data = data.pxl.df.osreg,
                    family = gaussian,     
                    na.action="na.fail")  # Needed, otherwise the dredging fails

d.FRic.osreg <- dredge(m.FRic.osreg)              # Model construction
d.FRic2.osreg <- subset(d.FRic.osreg, delta<3)    # Subset based on deltaAIC
output_FRic.osreg <- model.sel(d.FRic2.osreg)     # Model selection
m.FRic.avg.osreg <- model.avg(output_FRic.osreg, fit=TRUE)     # Model averaging
}
### Functional evenness 
{
ME.FEve.osreg <- ME(FEve ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                data = data.pxl.df.osreg,
                family = gaussian, listw = ME.listw.osreg)
m.FEve.osreg <- glm(FEve ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                      Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.FEve.osreg),
                    data = data.pxl.df.osreg,
                    family = gaussian,     
                    na.action="na.fail")  # Needed, otherwise the dredging fails
  
d.FEve.osreg <- dredge(m.FEve.osreg)              # Model construction
d.FEve2.osreg <- subset(d.FEve.osreg, delta<3)    # Subset based on deltaAIC
output_FEve.osreg <- model.sel(d.FEve2.osreg)     # Model selection
m.FEve.avg.osreg <- model.avg(output_FEve.osreg, fit=TRUE)     # Model averaging
}
### Functional divergence 
{
ME.FDiv.osreg <- ME(FDiv ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                  Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
              data = data.pxl.df.osreg,
                family = gaussian, listw = ME.listw.osreg)
m.FDiv.osreg <- glm(FDiv ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                  Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.FDiv.osreg),
                data = data.pxl.df.osreg,
                family = gaussian,     
                na.action="na.fail")  # Needed, otherwise the dredging fails
  
d.FDiv.osreg <- dredge(m.FDiv.osreg)              # Model construction
d.FDiv2.osreg <- subset(d.FDiv.osreg, delta<3)    # Subset based on deltaAIC
output_FDiv.osreg <- model.sel(d.FDiv2.osreg)     # Model selection

m.FDiv.avg.osreg <- model.avg(output_FDiv.osreg, fit=TRUE)     # Model averaging - not working for a singular model
  # As all variables are retained in the optimal model: m.FDiv.avg.osreg <- m.FDiv.osreg
}
### Functional dispersion
{
ME.FDis.osreg <- ME(FDis ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                  Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                data = data.pxl.df.osreg,
                family = gaussian, listw = ME.listw.osreg)
m.FDis.osreg <- glm(FDis ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                  Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.FDis.osreg),
                data = data.pxl.df.osreg,
                family = gaussian,     
                na.action="na.fail")  # Needed, otherwise the dredging fails
  
d.FDis.osreg <- dredge(m.FDis.osreg)              # Model construction
d.FDis2.osreg <- subset(d.FDis.osreg, delta<3)    # Subset based on deltaAIC
output_FDis.osreg <- model.sel(d.FDis2.osreg)     # Model selection
m.FDis.avg.osreg <- model.avg(output_FDis.osreg, fit=TRUE)     # Model averaging
}

##--- 7.3 Modelling, functional traits ---####
# We need a new dataframe of the individual (CWM) traits 
raster_data2.osreg <- raster::addLayer(
  CWM_raster.osreg[[c("Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass",
                   "Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness")]],
  env_raster)

data.pxl2.osreg <- as(raster_data2.osreg, "SpatialPixelsDataFrame")
data.pxl2.osreg <- data.pxl2.osreg[!is.na(data.pxl2.osreg$Height.gen),]
data.pxl2.osreg <- data.pxl2.osreg[complete.cases(data.pxl2.osreg@data), ]  # Incomplete cases in bioclim and topography
data.pxl.df2.osreg <- as.data.frame(data.pxl2.osreg)

## Height.gen 
{
  # Eigenvectors
  ME.Height.gen.osreg <- ME(Height.gen ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                      Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                      data = data.pxl.df2.osreg,
                      family = gaussian, listw = ME.listw.osreg)
  m.Height.gen.osreg <- glm(Height.gen ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.Height.gen.osreg),
                      data = data.pxl.df2.osreg,
                      family = gaussian,     
                      na.action="na.fail")  # Needed, otherwise the dredging fails
  
  d.Height.gen.osreg <- dredge(m.Height.gen.osreg)              # Model construction
  d.Height.gen2.osreg <- subset(d.Height.gen.osreg, delta<3)    # Subset based on deltaAIC
  output_Height.gen.osreg <- model.sel(d.Height.gen2.osreg)     # Model selection
  m.Height.gen.avg.osreg <- model.avg(output_Height.gen.osreg, fit=TRUE)     # Model averaging
}
## Height.veg 
{
  # Eigenvectors
  ME.Height.veg.osreg <- ME(Height.veg ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                      data = data.pxl.df2.osreg,
                      family = gaussian, listw = ME.listw.osreg)
  m.Height.veg.osreg <- glm(Height.veg ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                        Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.Height.veg.osreg),
                      data = data.pxl.df2.osreg,
                      family = gaussian,     # OBS! 'nbsp' is a count, not gaussian - I attempted Poisson first, changed to NB due to overdispersion
                      na.action="na.fail")  # Needed, otherwise the dredging fails
  
  d.Height.veg.osreg <- dredge(m.Height.veg.osreg)              # Model construction
  d.Height.veg2.osreg <- subset(d.Height.veg.osreg, delta<3)    # Subset based on deltaAIC
  output_Height.veg.osreg <- model.sel(d.Height.veg2.osreg)     # Model selection
  m.Height.veg.avg.osreg <- model.avg(output_Height.veg.osreg, fit=TRUE)     # Model averaging
}
## LDMC 
{
  # Eigenvectors
  ME.LDMC.osreg <- ME(LDMC ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                  Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                data = data.pxl.df2.osreg,
                family = gaussian, listw = ME.listw.osreg)
  m.LDMC.osreg <- glm(LDMC ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                  Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.LDMC.osreg),
                data = data.pxl.df2.osreg,
                family = gaussian,     # OBS! 'nbsp' is a count, not gaussian - I attempted Poisson first, changed to NB due to overdispersion
                na.action="na.fail")  # Needed, otherwise the dredging fails
  
  d.LDMC.osreg <- dredge(m.LDMC.osreg)              # Model construction
  d.LDMC2.osreg <- subset(d.LDMC.osreg, delta<3)    # Subset based on deltaAIC
  output_LDMC.osreg <- model.sel(d.LDMC2.osreg)     # Model selection
  m.LDMC.avg.osreg <- model.avg(output_LDMC.osreg, fit=TRUE)     # Model averaging
}
## Leaf.area 
{
  # Eigenvectors
  ME.Leaf.area.osreg <- ME(Leaf.area ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                       Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                     data = data.pxl.df2.osreg,
                     family = gaussian, listw = ME.listw.osreg)
  m.Leaf.area.osreg <- glm(Leaf.area ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                       Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.Leaf.area.osreg),
                     data = data.pxl.df2.osreg,
                     family = gaussian,     # OBS! 'nbsp' is a count, not gaussian - I attempted Poisson first, changed to NB due to overdispersion
                     na.action="na.fail")  # Needed, otherwise the dredging fails
  
  d.Leaf.area.osreg <- dredge(m.Leaf.area.osreg)              # Model construction
  d.Leaf.area2.osreg <- subset(d.Leaf.area.osreg, delta<3)    # Subset based on deltaAIC
  output_Leaf.area.osreg <- model.sel(d.Leaf.area2.osreg)     # Model selection
  m.Leaf.area.avg.osreg <- model.avg(output_Leaf.area.osreg, fit=TRUE)     # Model averaging
}
## Leaf.dry.mass 
{
  # Eigenvectors
  ME.Leaf.dry.mass.osreg <- ME(Leaf.dry.mass ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                           Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                         data = data.pxl.df2.osreg,
                         family = gaussian, listw = ME.listw.osreg)
  m.Leaf.dry.mass.osreg <- glm(Leaf.dry.mass ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                           Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.Leaf.dry.mass.osreg),
                         data = data.pxl.df2.osreg,
                         family = gaussian,     # OBS! 'nbsp' is a count, not gaussian - I attempted Poisson first, changed to NB due to overdispersion
                         na.action="na.fail")  # Needed, otherwise the dredging fails
  
  d.Leaf.dry.mass.osreg <- dredge(m.Leaf.dry.mass.osreg)              # Model construction
  d.Leaf.dry.mass2.osreg <- subset(d.Leaf.dry.mass.osreg, delta<3)    # Subset based on deltaAIC
  output_Leaf.dry.mass.osreg <- model.sel(d.Leaf.dry.mass2.osreg)     # Model selection
  m.Leaf.dry.mass.avg.osreg <- model.avg(output_Leaf.dry.mass.osreg, fit=TRUE)     # Model averaging
}
## Seed.dry.mass 
{
  # Eigenvectors
  ME.Seed.dry.mass.osreg <- ME(Seed.dry.mass ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                           Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                         data = data.pxl.df2.osreg,
                         family = gaussian, listw = ME.listw.osreg)
  m.Seed.dry.mass.osreg <- glm(Seed.dry.mass ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                           Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.Seed.dry.mass.osreg),
                         data = data.pxl.df2.osreg,
                         family = gaussian,     # OBS! 'nbsp' is a count, not gaussian - I attempted Poisson first, changed to NB due to overdispersion
                         na.action="na.fail")  # Needed, otherwise the dredging fails
  
  # Model averaging - I need to figure out if we have to further redo the ME once the model has been reduced
  d.Seed.dry.mass.osreg <- dredge(m.Seed.dry.mass.osreg)              # Model construction
  d.Seed.dry.mass2.osreg <- subset(d.Seed.dry.mass.osreg, delta<3)    # Subset based on deltaAIC
  output_Seed.dry.mass.osreg <- model.sel(d.Seed.dry.mass2.osreg)     # Model selection
  m.Seed.dry.mass.avg.osreg <- model.avg(output_Seed.dry.mass.osreg, fit=TRUE)     # Model averaging
}
## Seed.number.per.plant 
{
  # Eigenvectors
  ME.Seed.number.per.plant.osreg <- ME(Seed.number.per.plant ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                                   Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                                 data = data.pxl.df2.osreg,
                                 family = gaussian, listw = ME.listw.osreg)
  m.Seed.number.per.plant.osreg <- glm(Seed.number.per.plant ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                                   Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.Seed.number.per.plant.osreg),
                                 data = data.pxl.df2.osreg,
                                 family = gaussian,     # OBS! 'nbsp' is a count, not gaussian - I attempted Poisson first, changed to NB due to overdispersion
                                 na.action="na.fail")  # Needed, otherwise the dredging fails
  
  # Model averaging - I need to figure out if we have to further redo the ME once the model has been reduced
  d.Seed.number.per.plant.osreg <- dredge(m.Seed.number.per.plant.osreg)              # Model construction
  d.Seed.number.per.plant2.osreg <- subset(d.Seed.number.per.plant.osreg, delta<3)    # Subset based on deltaAIC
  output_Seed.number.per.plant.osreg <- model.sel(d.Seed.number.per.plant2.osreg)     # Model selection
  m.Seed.number.per.plant.avg.osreg <- model.avg(output_Seed.number.per.plant.osreg, fit=TRUE)     # Model averaging
}
## SLA 
{
  # Eigenvectors
  ME.SLA.osreg <- ME(SLA ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                 Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
               data = data.pxl.df2.osreg,
               family = gaussian, listw = ME.listw.osreg)
  m.SLA.osreg <- glm(SLA ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                 Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.SLA.osreg),
               data = data.pxl.df2.osreg,
               family = gaussian,     # OBS! 'nbsp' is a count, not gaussian - I attempted Poisson first, changed to NB due to overdispersion
               na.action="na.fail")  # Needed, otherwise the dredging fails
  
  # Model averaging - I need to figure out if we have to further redo the ME once the model has been reduced
  d.SLA.osreg <- dredge(m.SLA.osreg)              # Model construction
  d.SLA2.osreg <- subset(d.SLA.osreg, delta<3)    # Subset based on deltaAIC
  output_SLA.osreg <- model.sel(d.SLA2.osreg)     # Model selection
  m.SLA.avg.osreg <- model.avg(output_SLA.osreg, fit=TRUE)     # Model averaging
}
## Woodiness 
{
  # Eigenvectors
  ME.Woodiness.osreg <- ME(Woodiness ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                       Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation,
                     data = data.pxl.df2.osreg,
                     family = gaussian, listw = ME.listw.osreg)
  m.Woodiness.osreg <- glm(Woodiness ~ Annual.Precipitation + Mean.Temperature.of.Warmest.Quarter + Precipitation.Seasonality. +
                       Topographic.heterogeneity + PC2 + Habitat.heterogeneity + Glaciation + fitted(ME.Woodiness.osreg),
                     data = data.pxl.df2.osreg,
                     family = gaussian,     # OBS! 'nbsp' is a count, not gaussian - I attempted Poisson first, changed to NB due to overdispersion
                     na.action="na.fail")  # Needed, otherwise the dredging fails
  
  d.Woodiness.osreg <- dredge(m.Woodiness.osreg)              # Model construction
  d.Woodiness2.osreg <- subset(d.Woodiness.osreg, delta<3)    # Subset based on deltaAIC
  output_Woodiness.osreg <- model.sel(d.Woodiness2.osreg)     # Model selection
  m.Woodiness.avg.osreg <- model.avg(output_Woodiness.osreg, fit=TRUE)     # Model averaging
}

##--- 7.4 Plots of coefficients/variables ---####
## Variable importance, FD metrics
{
  var.imp.osreg <- full_join(full_join(full_join(tibble::rownames_to_column(as.data.frame(importance(output_nbsp.osreg))),
                                                           tibble::rownames_to_column(as.data.frame(importance(output_FRic.osreg))),
                                                           by = "rowname"),
                                                 tibble::rownames_to_column(as.data.frame(importance(output_FEve.osreg))),
                                                 by="rowname"),
                                       #tibble::rownames_to_column(as.data.frame(importance(output_FDiv))),  # Only a single model, add later
                                       #by="rowname"),
                             tibble::rownames_to_column(as.data.frame(importance(output_FDis))),
                             by="rowname")
  names(var.imp.osreg) <- c("var","nbsp","FRic","FEve","FDis")
  var.imp.osreg$FDiv <- 1
  var.imp.osreg <- var.imp.osreg[c(1,3:8),]  # OBS! Take out the spatial eigenvectors
  var.imp.osreg <- reshape2::melt(var.imp.osreg, id=c("var"))
  names(var.imp.osreg) <- c("var", "metric","importance")
  
var.imp.osreg %>%
  mutate(metric = fct_relevel(metric, "nbsp","FRic","FEve","FDiv","FDis")) %>%
  ggplot( aes(x=var, y=importance, shape=metric, fill=metric, width=0.5)) +
  geom_bar(position = position_dodge(width = 0.75), stat="identity") +
  labs(x="",y="Relative variable importance") +
  scale_fill_manual(values=c("gold","darkorange2","firebrick","navy","forestgreen")) + 
  coord_flip() +
  theme_minimal() 
}
## Model-averaged coefficients, FD metrics
{
  coef.df.FDis.osreg <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.FDis.avg.osreg)$coefmat.subset))[,c(1,2)],
                                  tibble::rownames_to_column(as.data.frame(confint(m.FDis.avg.osreg))))
  names(coef.df.FDis.osreg) <- c("var","est.FDis","lwr.FDis","upr.FDis")
  coef.df.FDiv.osreg <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.FDiv.avg.osreg)$coefficients))[,c(1,2)],  # OBS!
                                  tibble::rownames_to_column(as.data.frame(confint(m.FDiv.avg.osreg))))
  names(coef.df.FDiv.osreg) <- c("var","est.FDiv","lwr.FDiv","upr.FDiv")
  coef.df.FEve.osreg <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.FEve.avg.osreg)$coefmat.subset))[,c(1,2)],
                                  tibble::rownames_to_column(as.data.frame(confint(m.FEve.avg.osreg))))
  names(coef.df.FEve.osreg) <- c("var","est.FEve","lwr.FEve","upr.FEve")
  coef.df.FRic.osreg <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.FRic.avg.osreg)$coefmat.subset))[,c(1,2)],
                                  tibble::rownames_to_column(as.data.frame(confint(m.FRic.avg.osreg))))
  names(coef.df.FRic.osreg) <- c("var","est.FRic","lwr.FRic","upr.FRic")
  coef.df.nbsp.osreg <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.nbsp.avg.osreg)$coefmat.subset))[,c(1,2)],
                                  tibble::rownames_to_column(as.data.frame(confint(m.nbsp.avg.osreg))))
  names(coef.df.nbsp.osreg) <- c("var","est.nbsp","lwr.nbsp","upr.nbsp")
  
  coef.df.osreg <- full_join(full_join(full_join(full_join(coef.df.FDis.osreg,
                                                     coef.df.FDiv.osreg, by="var"),
                                           coef.df.FEve.osreg, by="var"),
                                 coef.df.FRic.osreg, by="var"),
                       coef.df.nbsp.osreg, byr="var")
  coef.df.osreg <- coef.df.osreg[coef.df.osreg$var %in% c("Annual.Precipitation","PC2","Glaciation","Habitat.heterogeneity",
                                                          "Precipitation.Seasonality.","Mean.Temperature.of.Warmest.Quarter",
                                                          "Topographic.heterogeneity"),]
  coef.df.osreg <- data.frame(var = c(rep(coef.df.osreg$var, 5)),
                        metric = c(rep(c("FDis","FDiv","FEve","FRic","nbsp"), each=7)),
                        est = c(coef.df.osreg$est.FDis, coef.df.osreg$est.FDiv, coef.df.osreg$est.FEve, coef.df.osreg$est.FRic, coef.df.osreg$est.nbsp),
                        lwr = c(coef.df.osreg$lwr.FDis, coef.df.osreg$lwr.FDiv, coef.df.osreg$lwr.FEve, coef.df.osreg$lwr.FRic, coef.df.osreg$lwr.nbsp),
                        upr = c(coef.df.osreg$upr.FDis, coef.df.osreg$upr.FDiv, coef.df.osreg$upr.FEve, coef.df.osreg$upr.FRic, coef.df.osreg$upr.nbsp))

coef.df.osreg %>%
    mutate(metric = fct_relevel(metric, "nbsp","FRic","FEve","FDiv","FDis")) %>%
    ggplot( aes(x=var, y=est, color=metric, shape=metric)) +
    geom_hline(yintercept = 0, linetype="dashed", color="gray") +
    geom_point(position = position_dodge(width = 0.75)) +
    geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.5, position = position_dodge(width = 0.75)) +
    labs(x="",y="Model-averaged coefficient estimates") +
    scale_color_manual(values=(c("gold","darkorange2","firebrick","navy","forestgreen")) ) +
    coord_flip() +
  facet_wrap(~ metric, scales = "free", nrow=2) +
  theme_bw() +
  theme(legend.position = "none")
  
}

## Variable importance, individual traits 
{
  var.imp2.osreg <- full_join(full_join(full_join(full_join(full_join(full_join(full_join(full_join(tibble::rownames_to_column(as.data.frame(importance(output_Height.gen.osreg))),
                                                                                              tibble::rownames_to_column(as.data.frame(importance(output_Height.veg.osreg))), by = "rowname"),
                                                                                    tibble::rownames_to_column(as.data.frame(importance(output_LDMC.osreg))), by="rowname"),
                                                                          tibble::rownames_to_column(as.data.frame(importance(output_Leaf.area.osreg))), by="rowname"),
                                                                tibble::rownames_to_column(as.data.frame(importance(output_Leaf.dry.mass.osreg))), by="rowname"),
                                                      tibble::rownames_to_column(as.data.frame(importance(output_Seed.dry.mass.osreg))), by="rowname"),
                                            tibble::rownames_to_column(as.data.frame(importance(output_Seed.number.per.plant.osreg))), by="rowname"),
                                  tibble::rownames_to_column(as.data.frame(importance(output_SLA.osreg))), by="rowname"),
                        tibble::rownames_to_column(as.data.frame(importance(output_Woodiness.osreg))), by="rowname")
  
  names(var.imp2.osreg) <- c("var","Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass","Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness")
  var.imp2.osreg <- var.imp2.osreg[c(1,3:8),]  # OBS! Take out the spatial eigenvectors
  var.imp2.osreg <- reshape2::melt(var.imp2.osreg, id=c("var"))
  names(var.imp2.osreg) <- c("var", "metric","importance")
  
var.imp2.osreg %>%
    mutate(metric = fct_relevel(metric, "Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass","Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness")) %>%
    ggplot( aes(x=var, y=importance, fill=metric, width=0.5)) +
    geom_bar(position = position_dodge(width = 0.75), stat="identity") +
    labs(x="",y="Relative variable importance") +
    coord_flip() +
    theme_minimal() 
}

## Model-averaged coefficients, individual traits 
{
  {
    coef.df.Height.gen.osreg <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.Height.gen.avg.osreg)$coefmat.subset))[,c(1,2)],
                                    tibble::rownames_to_column(as.data.frame(confint(m.Height.gen.avg.osreg))))
    names(coef.df.Height.gen.osreg) <- c("var","est.Height.gen","lwr.Height.gen","upr.Height.gen")
    coef.df.Height.veg.osreg <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.Height.veg.avg.osreg)$coefmat.subset))[,c(1,2)],
                                    tibble::rownames_to_column(as.data.frame(confint(m.Height.veg.avg.osreg))))
    names(coef.df.Height.veg.osreg) <- c("var","est.Height.veg","lwr.Height.veg","upr.Height.veg")
    coef.df.LDMC.osreg <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.LDMC.avg.osreg)$coefmat.subset))[,c(1,2)],
                              tibble::rownames_to_column(as.data.frame(confint(m.LDMC.avg.osreg))))
    names(coef.df.LDMC.osreg) <- c("var","est.LDMC","lwr.LDMC","upr.LDMC")
    coef.df.Leaf.area.osreg <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.Leaf.area.avg.osreg)$coefmat.subset))[,c(1,2)],
                                   tibble::rownames_to_column(as.data.frame(confint(m.Leaf.area.avg.osreg))))
    names(coef.df.Leaf.area.osreg) <- c("var","est.Leaf.area","lwr.Leaf.area","upr.Leaf.area")
    coef.df.Leaf.dry.mass.osreg <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.Leaf.dry.mass.avg.osreg)$coefmat.subset))[,c(1,2)],
                                       tibble::rownames_to_column(as.data.frame(confint(m.Leaf.dry.mass.avg.osreg))))
    names(coef.df.Leaf.dry.mass.osreg) <- c("var","est.Leaf.dry.mass","lwr.Leaf.dry.mass","upr.Leaf.dry.mass")
    coef.df.Seed.dry.mass.osreg <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.Seed.dry.mass.avg.osreg)$coefmat.subset))[,c(1,2)],
                                       tibble::rownames_to_column(as.data.frame(confint(m.Seed.dry.mass.avg.osreg))))
    names(coef.df.Seed.dry.mass.osreg) <- c("var","est.Seed.dry.mass","lwr.Seed.dry.mass","upr.Seed.dry.mass")
    coef.df.Seed.number.per.plant.osreg <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.Seed.number.per.plant.avg.osreg)$coefmat.subset))[,c(1,2)],
                                               tibble::rownames_to_column(as.data.frame(confint(m.Seed.number.per.plant.avg.osreg))))
    names(coef.df.Seed.number.per.plant.osreg) <- c("var","est.Seed.number.per.plant","lwr.Seed.number.per.plant","upr.Seed.number.per.plant")
    coef.df.SLA.osreg <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.SLA.avg.osreg)$coefmat.subset))[,c(1,2)],
                             tibble::rownames_to_column(as.data.frame(confint(m.SLA.avg.osreg))))
    names(coef.df.SLA.osreg) <- c("var","est.SLA","lwr.SLA","upr.SLA")
    coef.df.Woodiness.osreg <- full_join(tibble::rownames_to_column(as.data.frame(summary(m.Woodiness.avg.osreg)$coefmat.subset))[,c(1,2)],
                                   tibble::rownames_to_column(as.data.frame(confint(m.Woodiness.avg.osreg))))
    names(coef.df.Woodiness.osreg) <- c("var","est.Woodiness","lwr.Woodiness","upr.Woodiness")
    
  }
  
  coef.df2.osreg <- full_join(full_join(full_join(full_join(full_join(full_join(full_join(full_join(coef.df.Height.gen.osreg,
                                                                                              coef.df.Height.veg.osreg, by="var"),
                                                                                    coef.df.LDMC.osreg, by="var"),
                                                                          coef.df.Leaf.area.osreg, by="var"),
                                                                coef.df.Leaf.dry.mass.osreg, by="var"),
                                                      coef.df.Seed.dry.mass.osreg, by="var"),
                                            coef.df.Seed.number.per.plant.osreg, by="var"),
                                  coef.df.SLA.osreg, by="var"),
                        coef.df.Woodiness.osreg, by="var")
  
  coef.df2.osreg <- coef.df2.osreg[coef.df2.osreg$var %in% c("Annual.Precipitation","PC2","Glaciation","Habitat.heterogeneity",
                                           "Precipitation.Seasonality.","Mean.Temperature.of.Warmest.Quarter",
                                           "Topographic.heterogeneity"),]
  coef.df2.osreg <- data.frame(var = c(rep(coef.df2.osreg$var, 9)),
                         metric = c(rep(c("Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass",
                                          "Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness"), each=7)),
                         est = c(coef.df2.osreg$est.Height.gen, coef.df2.osreg$est.Height.veg, coef.df2.osreg$est.LDMC, coef.df2.osreg$est.Leaf.area, coef.df2.osreg$est.Leaf.dry.mass, coef.df2.osreg$est.Seed.dry.mass, coef.df2.osreg$est.Seed.number.per.plant, coef.df2.osreg$est.SLA, coef.df2.osreg$est.Woodiness),
                         lwr = c(coef.df2.osreg$lwr.Height.gen, coef.df2.osreg$lwr.Height.veg, coef.df2.osreg$lwr.LDMC, coef.df2.osreg$lwr.Leaf.area, coef.df2.osreg$lwr.Leaf.dry.mass, coef.df2.osreg$lwr.Seed.dry.mass, coef.df2.osreg$lwr.Seed.number.per.plant, coef.df2.osreg$lwr.SLA, coef.df2.osreg$lwr.Woodiness),
                         upr = c(coef.df2.osreg$upr.Height.gen, coef.df2.osreg$upr.Height.veg, coef.df2.osreg$upr.LDMC, coef.df2.osreg$upr.Leaf.area, coef.df2.osreg$upr.Leaf.dry.mass, coef.df2.osreg$upr.Seed.dry.mass, coef.df2.osreg$upr.Seed.number.per.plant, coef.df2.osreg$upr.SLA, coef.df2.osreg$upr.Woodiness))
  
  
coef.df2.osreg %>%
    mutate(metric = fct_relevel(metric, "Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass",
                                "Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness")) %>%
    ggplot( aes(x=var, y=est, color=metric)) +
    geom_hline(yintercept = 0, linetype="dashed", color="gray") +
    geom_point(position = position_dodge(width = 0.75)) +
    geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.5, position = position_dodge(width = 0.75)) +
    labs(x="",y="Model-averaged coefficient estimates") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(~ metric, scales = "free")
  
}

##----------------------------------------####

##--- 8. PREDICTIONS, FUTURE CLIMATE SCENARIOS ---####
##--- 8.1 Prepare environmental data ---####
# Get data for future climate scenarios
fut_SSP126 <- stack('Future_climate/wc2.1_2.5m_bioc_MIROC6_ssp126_2081-2100.tif') 
fut_SSP245 <- stack('Future_climate/wc2.1_2.5m_bioc_MRI-ESM2-0_ssp245_2081-2100.tif') 
fut_SSP585 <- stack('Future_climate/wc2.1_2.5m_bioc_CanESM5_ssp585_2081-2100.tif') 
names(fut_SSP126) <- {c('Annual Mean Temperature',
                     'Mean Diurnal Range',
                     'Isothermality',
                     'Temperature Seasonality',
                     'Max Temperature of Warmest Month',
                     'Min Temperature of Coldest Month',
                     'Temperature Annual Range',
                     'Mean Temperature of Wettest Quarter',
                     'Mean Temperature of Driest Quarter',
                     'Mean Temperature of Warmest Quarter',
                     'Mean Temperature of Coldest Quarter',
                     'Annual Precipitation',
                     'Precipitation of Wettest Month',
                     'Precipitation of Driest Month',
                     'Precipitation Seasonality.',
                     'Precipitation of Wettest Quarter',
                     'Precipitation of Driest Quarter',
                     'Precipitation of Warmest Quarter',
                     'Precipitation of Coldest Quarter')}
names(fut_SSP245) <- {c('Annual Mean Temperature',
                        'Mean Diurnal Range',
                        'Isothermality',
                        'Temperature Seasonality',
                        'Max Temperature of Warmest Month',
                        'Min Temperature of Coldest Month',
                        'Temperature Annual Range',
                        'Mean Temperature of Wettest Quarter',
                        'Mean Temperature of Driest Quarter',
                        'Mean Temperature of Warmest Quarter',
                        'Mean Temperature of Coldest Quarter',
                        'Annual Precipitation',
                        'Precipitation of Wettest Month',
                        'Precipitation of Driest Month',
                        'Precipitation Seasonality.',
                        'Precipitation of Wettest Quarter',
                        'Precipitation of Driest Quarter',
                        'Precipitation of Warmest Quarter',
                        'Precipitation of Coldest Quarter')}
names(fut_SSP585) <- {c('Annual Mean Temperature',
                        'Mean Diurnal Range',
                        'Isothermality',
                        'Temperature Seasonality',
                        'Max Temperature of Warmest Month',
                        'Min Temperature of Coldest Month',
                        'Temperature Annual Range',
                        'Mean Temperature of Wettest Quarter',
                        'Mean Temperature of Driest Quarter',
                        'Mean Temperature of Warmest Quarter',
                        'Mean Temperature of Coldest Quarter',
                        'Annual Precipitation',
                        'Precipitation of Wettest Month',
                        'Precipitation of Driest Month',
                        'Precipitation Seasonality.',
                        'Precipitation of Wettest Quarter',
                        'Precipitation of Driest Quarter',
                        'Precipitation of Warmest Quarter',
                        'Precipitation of Coldest Quarter')}

# Due to the difference in resolution, we need an even larger buffer to not lose data than the original one
norway_UTM_buff2 <- st_buffer(norway_UTM_buff, dist = 50000)

# Start with cropping a little to make the process a little faster - OBS! The extent needs to be slightly bigger, otherwise
# we get empty grid cells - this takes care of the extent at the same time:
ext2 <- extent(st_transform(norway_UTM_buff2, crs=st_crs(fut_SSP126)))

fut_SSP126 <- crop(fut_SSP126, ext2)
fut_SSP245 <- crop(fut_SSP245, ext2)
fut_SSP585 <- crop(fut_SSP585, ext2)

# Transform the CRS to match that of env_rast, crop and rescale/resample:
fut_SSP126.2 <- resample(mask(projectRaster(fut_SSP126, crs = crs(env_raster)),
                              norway_UTM_buff2),
                         env_raster, method="bilinear")
fut_SSP245.2 <- resample(mask(projectRaster(fut_SSP245, crs = crs(env_raster)),
                              norway_UTM_buff2),
                         env_raster, method="bilinear")
fut_SSP585.2 <- resample(mask(projectRaster(fut_SSP585, crs = crs(env_raster)),
                              norway_UTM_buff2),
                         env_raster, method="bilinear")

# Combine the relevant data for predictions in new rasters - only the bioclimatic variables are changing, for the remaining data,
# we'll be keeping the same values as were used in the original models:
fut1 <- addLayer(dropLayer(fut_SSP126.2, c(1:9,11,13:14,16:19)), env_raster[[c(4,6,14,16)]])
fut2 <- addLayer(dropLayer(fut_SSP245.2, c(1:9,11,13:14,16:19)), env_raster[[c(4,6,14,16)]])
fut3 <- addLayer(dropLayer(fut_SSP585.2, c(1:9,11,13:14,16:19)), env_raster[[c(4,6,14,16)]])

# Subset the data to only include the pixels for which we have spatial eigenvectors (otherwise 'predict()' fails)
# Get the right pixelnumbers
pxl <- tibble::rownames_to_column(as.data.frame(raster_data))
pxl <- pxl[complete.cases(pxl), ]  # Incomplete cases in bioclim and topography
pxl <- pxl$rowname

## OBS! The temperature data has another unit than the original data! (degree C vs. degree C *10) - multiply the data here
## by 10 to make it fit
fut1.df <- as.data.frame(fut1)[rownames(as.data.frame(fut1)) %in% pxl,]
      fut1.df$Mean.Temperature.of.Warmest.Quarter <- fut1.df$Mean.Temperature.of.Warmest.Quarter *10
fut2.df <- as.data.frame(fut2)[rownames(as.data.frame(fut2)) %in% pxl,]
      fut2.df$Mean.Temperature.of.Warmest.Quarter <- fut2.df$Mean.Temperature.of.Warmest.Quarter *10
fut3.df <- as.data.frame(fut3)[rownames(as.data.frame(fut3)) %in% pxl,]
      fut3.df$Mean.Temperature.of.Warmest.Quarter <- fut3.df$Mean.Temperature.of.Warmest.Quarter *10

##--- 8.2 Make predictions for the individual traits ---####
# Gather in one dataframes:
pred_fut1 <- data.frame(Pixelnr = as.integer(pxl),
                       Height.gen = predict(m.Height.gen.avg, newdata=fut1.df, full=T),
                       Height.veg = predict(m.Height.veg.avg, newdata=fut1.df, full=T),
                       LDMC = predict(m.LDMC.avg, newdata=fut1.df, full=T),
                       Leaf.area = predict(m.Leaf.area.avg, newdata=fut1.df, full=T),
                       Leaf.dry.mass = predict(m.Leaf.dry.mass.avg, newdata=fut1.df, full=T),
                       Seed.dry.mass = predict(m.Seed.dry.mass.avg, newdata=fut1.df, full=T),
                       Seed.number.per.plant = predict(m.Seed.number.per.plant.avg, newdata=fut1.df, full=T),
                       SLA = predict(m.SLA.avg, newdata=fut1.df, full=T),
                       Woodiness = predict(m.Woodiness.avg, newdata=fut1.df, full=T))

pred_fut2 <- data.frame(Pixelnr = as.integer(pxl),
                        Height.gen = predict(m.Height.gen.avg, newdata=fut2.df, full=T),
                        Height.veg = predict(m.Height.veg.avg, newdata=fut2.df, full=T),
                        LDMC = predict(m.LDMC.avg, newdata=fut2.df, full=T),
                        Leaf.area = predict(m.Leaf.area.avg, newdata=fut2.df, full=T),
                        Leaf.dry.mass = predict(m.Leaf.dry.mass.avg, newdata=fut2.df, full=T),
                        Seed.dry.mass = predict(m.Seed.dry.mass.avg, newdata=fut2.df, full=T),
                        Seed.number.per.plant = predict(m.Seed.number.per.plant.avg, newdata=fut2.df, full=T),
                        SLA = predict(m.SLA.avg, newdata=fut2.df, full=T),
                        Woodiness = predict(m.Woodiness.avg, newdata=fut2.df, full=T))

pred_fut3 <- data.frame(Pixelnr = as.integer(pxl),
                        Height.gen = predict(m.Height.gen.avg, newdata=fut3.df, full=T),
                        Height.veg = predict(m.Height.veg.avg, newdata=fut3.df, full=T),
                        LDMC = predict(m.LDMC.avg, newdata=fut3.df, full=T),
                        Leaf.area = predict(m.Leaf.area.avg, newdata=fut3.df, full=T),
                        Leaf.dry.mass = predict(m.Leaf.dry.mass.avg, newdata=fut3.df, full=T),
                        Seed.dry.mass = predict(m.Seed.dry.mass.avg, newdata=fut3.df, full=T),
                        Seed.number.per.plant = predict(m.Seed.number.per.plant.avg, newdata=fut3.df, full=T),
                        SLA = predict(m.SLA.avg, newdata=fut3.df, full=T),
                        Woodiness = predict(m.Woodiness.avg, newdata=fut3.df, full=T))

# Collect in full dataframe and rasterize
pred_fut1 <- left_join(data.frame(Pixelnr = FD.cc.df[,c("Pixelnr")]), pred_fut1, by="Pixelnr")
pred_fut2 <- left_join(data.frame(Pixelnr = FD.cc.df[,c("Pixelnr")]), pred_fut2, by="Pixelnr")
pred_fut3 <- left_join(data.frame(Pixelnr = FD.cc.df[,c("Pixelnr")]), pred_fut3, by="Pixelnr")

pred_fut1_raster <- dropLayer(pa20k_stack, c(10:1248))
{pred_fut1_raster <- setValues(pred_fut1_raster, pred_fut1$Height.gen, layer=1)
  pred_fut1_raster <- setValues(pred_fut1_raster, pred_fut1$Height.veg, layer=2)
  pred_fut1_raster <- setValues(pred_fut1_raster, pred_fut1$LDMC, layer=3)
  pred_fut1_raster <- setValues(pred_fut1_raster, pred_fut1$Leaf.area, layer=4)
  pred_fut1_raster <- setValues(pred_fut1_raster, pred_fut1$Leaf.dry.mass, layer=5)
  pred_fut1_raster <- setValues(pred_fut1_raster, pred_fut1$Seed.dry.mass, layer=6)
  pred_fut1_raster <- setValues(pred_fut1_raster, pred_fut1$Seed.number.per.plant, layer=7)
  pred_fut1_raster <- setValues(pred_fut1_raster, pred_fut1$SLA, layer=8)
  pred_fut1_raster <- setValues(pred_fut1_raster, pred_fut1$Woodiness, layer=9)
  names(pred_fut1_raster) <- c("Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass",
                               "Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness" )}
pred_fut2_raster <- dropLayer(pa20k_stack, c(10:1248))
{pred_fut2_raster <- setValues(pred_fut2_raster, pred_fut2$Height.gen, layer=1)
  pred_fut2_raster <- setValues(pred_fut2_raster, pred_fut2$Height.veg, layer=2)
  pred_fut2_raster <- setValues(pred_fut2_raster, pred_fut2$LDMC, layer=3)
  pred_fut2_raster <- setValues(pred_fut2_raster, pred_fut2$Leaf.area, layer=4)
  pred_fut2_raster <- setValues(pred_fut2_raster, pred_fut2$Leaf.dry.mass, layer=5)
  pred_fut2_raster <- setValues(pred_fut2_raster, pred_fut2$Seed.dry.mass, layer=6)
  pred_fut2_raster <- setValues(pred_fut2_raster, pred_fut2$Seed.number.per.plant, layer=7)
  pred_fut2_raster <- setValues(pred_fut2_raster, pred_fut2$SLA, layer=8)
  pred_fut2_raster <- setValues(pred_fut2_raster, pred_fut2$Woodiness, layer=9)
  names(pred_fut2_raster) <- c("Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass",
                               "Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness" )}
pred_fut3_raster <- dropLayer(pa20k_stack, c(10:1248))
{pred_fut3_raster <- setValues(pred_fut3_raster, pred_fut3$Height.gen, layer=1)
  pred_fut3_raster <- setValues(pred_fut3_raster, pred_fut3$Height.veg, layer=2)
  pred_fut3_raster <- setValues(pred_fut3_raster, pred_fut3$LDMC, layer=3)
  pred_fut3_raster <- setValues(pred_fut3_raster, pred_fut3$Leaf.area, layer=4)
  pred_fut3_raster <- setValues(pred_fut3_raster, pred_fut3$Leaf.dry.mass, layer=5)
  pred_fut3_raster <- setValues(pred_fut3_raster, pred_fut3$Seed.dry.mass, layer=6)
  pred_fut3_raster <- setValues(pred_fut3_raster, pred_fut3$Seed.number.per.plant, layer=7)
  pred_fut3_raster <- setValues(pred_fut3_raster, pred_fut3$SLA, layer=8)
  pred_fut3_raster <- setValues(pred_fut3_raster, pred_fut3$Woodiness, layer=9)
  names(pred_fut3_raster) <- c("Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass",
                               "Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness" )}

##--- 8.3 Calculate the difference between present day and the future scenarios ---####
diff_fut1 <- pred_fut1_raster - CWM_raster.cc[[c("Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass",
                                                 "Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness")]]
diff_fut2 <- pred_fut2_raster - CWM_raster.cc[[c("Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass",
                                                 "Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness")]]
diff_fut3 <- pred_fut3_raster - CWM_raster.cc[[c("Height.gen","Height.veg","LDMC","Leaf.area","Leaf.dry.mass",
                                                 "Seed.dry.mass","Seed.number.per.plant","SLA","Woodiness")]]

##--- 8.4 Plots with a diverging colour gradient ---####
# The original function was taken from https://stackoverflow.com/questions/33750235/plotting-a-raster-with-the-color-ramp-diverging-around-zero
# devtools::source_gist('306e4b7e69c87b1826db')
# I have added an extra 'rev()' to reverse the  RdBu color gradient (the negative values were originally red and positive ones
# were blue - I wanted it reversed). OBS! For now, it only works on the named palettes from 'brewer.pal'
library(gridExtra, lib.loc = "/home/ahomez/t/tanjakp/export/library")
Mydiverge0 <- function(p, ramp) {
  # p: a trellis object resulting from rasterVis::levelplot
  # ramp: the name of an RColorBrewer palette (as character), a character 
  #       vector of colour names to interpolate, or a colorRampPalette.
  require(RColorBrewer)
  require(rasterVis)
  if(length(ramp)==1 && is.character(ramp) && ramp %in% 
     row.names(brewer.pal.info)) {
    ramp <- suppressWarnings(colorRampPalette(rev(brewer.pal(11, ramp))))         # Here I added the 'rev()' function to reverse the palette
  } else if(length(ramp) > 1 && is.character(ramp) && all(ramp %in% colors())) {
    ramp <- colorRampPalette(ramp)      
  } else if(!is.function(ramp)) 
    stop('ramp should be either the name of a RColorBrewer palette, ', 
         'a vector of colours to be interpolated, or a colorRampPalette.')
  rng <- range(p$legend[[1]]$args$key$at)
  s <- seq(-max(abs(rng)), max(abs(rng)), len=1001)
  i <- findInterval(rng[which.min(abs(rng))], s)
  zlim <- switch(which.min(abs(rng)), `1`=i:(1000+1), `2`=1:(i+1))
  p$legend[[1]]$args$key$at <- s[zlim]
  p[[grep('^legend', names(p))]][[1]]$args$key$col <- ramp(1000)[zlim[-length(zlim)]]
  p$panel.args.common$col.regions <- ramp(1000)[zlim[-length(zlim)]]
  p
}

# As the function works through rasterVis and trellis/level plots, and combine them manually through grid.arrange():
# SSP126
{grid.arrange(Mydiverge0(levelplot(diff_fut1[["Height.gen"]],
                                  main="Height.gen",
                                  margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                  par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
               Mydiverge0(levelplot(diff_fut1[["Height.veg"]],
                                    main="Height.veg",
                                    margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                    par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
               Mydiverge0(levelplot(diff_fut1[["LDMC"]],
                                    main="LDMC",
                                    margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                    par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
               Mydiverge0(levelplot(diff_fut1[["Leaf.area"]],
                                    main="Leaf.area",
                                    margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                    par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
               Mydiverge0(levelplot(diff_fut1[["Leaf.dry.mass"]],
                                    main="Leaf.dry.mass",
                                    margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                    par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
               Mydiverge0(levelplot(diff_fut1[["Seed.dry.mass"]],
                                    main="Seed.dry.mass",
                                    margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                    par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
               Mydiverge0(levelplot(diff_fut1[["Seed.number.per.plant"]],
                                    main="Seed.number.per.plant",
                                    margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                    par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
               Mydiverge0(levelplot(diff_fut1[["SLA"]],
                                    main="SLA",
                                    margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                    par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
               Mydiverge0(levelplot(diff_fut1[["Woodiness"]],
                                    main="Woodiness",
                                    margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                    par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
               ncol=3, nrow=3)}
# SSP245
{grid.arrange(Mydiverge0(levelplot(diff_fut2[["Height.gen"]],
                                   main="Height.gen",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut2[["Height.veg"]],
                                   main="Height.veg",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut2[["LDMC"]],
                                   main="LDMC",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut2[["Leaf.area"]],
                                   main="Leaf.area",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut2[["Leaf.dry.mass"]],
                                   main="Leaf.dry.mass",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut2[["Seed.dry.mass"]],
                                   main="Seed.dry.mass",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut2[["Seed.number.per.plant"]],
                                   main="Seed.number.per.plant",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut2[["SLA"]],
                                   main="SLA",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut2[["Woodiness"]],
                                   main="Woodiness",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              ncol=3, nrow=3)}
# SSP585
{grid.arrange(Mydiverge0(levelplot(diff_fut3[["Height.gen"]],
                                   main="Height.gen",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut3[["Height.veg"]],
                                   main="Height.veg",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut3[["LDMC"]],
                                   main="LDMC",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut3[["Leaf.area"]],
                                   main="Leaf.area",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut3[["Leaf.dry.mass"]],
                                   main="Leaf.dry.mass",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut3[["Seed.dry.mass"]],
                                   main="Seed.dry.mass",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut3[["Seed.number.per.plant"]],
                                   main="Seed.number.per.plant",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut3[["SLA"]],
                                   main="SLA",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut3[["Woodiness"]],
                                   main="Woodiness",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              ncol=3, nrow=3)}


##--- 8.5 Make predictions for FD metrics ---####
# Gather in one dataframes:
pred_fut1.FD <- data.frame(Pixelnr = as.integer(pxl),
                           nbsp = predict(m.nbsp.avg, newdata=fut1.df, full=T),
                           FRic = predict(m.FRic.avg, newdata=fut1.df, full=T),
                           FEve = predict(m.FEve.avg, newdata=fut1.df, full=T),
                           FDiv = predict(m.FDiv.avg, newdata=fut1.df, full=T),
                           FDis = predict(m.FDis.avg, newdata=fut1.df, full=T))

pred_fut2.FD <- data.frame(Pixelnr = as.integer(pxl),
                           nbsp = predict(m.nbsp.avg, newdata=fut2.df, full=T),
                           FRic = predict(m.FRic.avg, newdata=fut2.df, full=T),
                           FEve = predict(m.FEve.avg, newdata=fut2.df, full=T),
                           FDiv = predict(m.FDiv.avg, newdata=fut2.df, full=T),
                           FDis = predict(m.FDis.avg, newdata=fut2.df, full=T))

pred_fut3.FD <- data.frame(Pixelnr = as.integer(pxl),
                           nbsp = predict(m.nbsp.avg, newdata=fut3.df, full=T),
                           FRic = predict(m.FRic.avg, newdata=fut3.df, full=T),
                           FEve = predict(m.FEve.avg, newdata=fut3.df, full=T),
                           FDiv = predict(m.FDiv.avg, newdata=fut3.df, full=T),
                           FDis = predict(m.FDis.avg, newdata=fut3.df, full=T))

# Collect in full dataframe and rasterize
pred_fut1.FD <- left_join(data.frame(Pixelnr = FD.cc.df[,c("Pixelnr")]), pred_fut1.FD, by="Pixelnr")
pred_fut2.FD <- left_join(data.frame(Pixelnr = FD.cc.df[,c("Pixelnr")]), pred_fut2.FD, by="Pixelnr")
pred_fut3.FD <- left_join(data.frame(Pixelnr = FD.cc.df[,c("Pixelnr")]), pred_fut3.FD, by="Pixelnr")

pred_fut1_raster.FD <- dropLayer(pa20k_stack, c(6:1248))
{pred_fut1_raster.FD <- setValues(pred_fut1_raster.FD, pred_fut1.FD$nbsp, layer=1)
  pred_fut1_raster.FD <- setValues(pred_fut1_raster.FD, pred_fut1.FD$FRic, layer=2)
  pred_fut1_raster.FD <- setValues(pred_fut1_raster.FD, pred_fut1.FD$FEve, layer=3)
  pred_fut1_raster.FD <- setValues(pred_fut1_raster.FD, pred_fut1.FD$FDiv, layer=4)
  pred_fut1_raster.FD <- setValues(pred_fut1_raster.FD, pred_fut1.FD$FDis, layer=5)
  names(pred_fut1_raster.FD) <- c("nbsp","FRic","FEve","FDiv","FDis")}
pred_fut2_raster.FD <- dropLayer(pa20k_stack, c(6:1248))
{pred_fut2_raster.FD <- setValues(pred_fut2_raster.FD, pred_fut2.FD$nbsp, layer=1)
  pred_fut2_raster.FD <- setValues(pred_fut2_raster.FD, pred_fut2.FD$FRic, layer=2)
  pred_fut2_raster.FD <- setValues(pred_fut2_raster.FD, pred_fut2.FD$FEve, layer=3)
  pred_fut2_raster.FD <- setValues(pred_fut2_raster.FD, pred_fut2.FD$FDiv, layer=4)
  pred_fut2_raster.FD <- setValues(pred_fut2_raster.FD, pred_fut2.FD$FDis, layer=5)
  names(pred_fut2_raster.FD) <- c("nbsp","FRic","FEve","FDiv","FDis")}
pred_fut3_raster.FD <- dropLayer(pa20k_stack, c(6:1248))
{pred_fut3_raster.FD <- setValues(pred_fut3_raster.FD, pred_fut3.FD$nbsp, layer=1)
  pred_fut3_raster.FD <- setValues(pred_fut3_raster.FD, pred_fut3.FD$FRic, layer=2)
  pred_fut3_raster.FD <- setValues(pred_fut3_raster.FD, pred_fut3.FD$FEve, layer=3)
  pred_fut3_raster.FD <- setValues(pred_fut3_raster.FD, pred_fut3.FD$FDiv, layer=4)
  pred_fut3_raster.FD <- setValues(pred_fut3_raster.FD, pred_fut3.FD$FDis, layer=5)
  names(pred_fut3_raster.FD) <- c("nbsp","FRic","FEve","FDiv","FDis")}

##--- 8.6 Calculate the difference between present day and the future scenarios ---####
# OBS! The nbsp-model predicts a proportional species richness - we this need to add a layer of tat to the raster
# to compare the right numbers:
CWM_raster.cc$nbsp.prop <- CWM_raster.cc$nbsp/1232

diff_fut1.FD <- pred_fut1_raster.FD - CWM_raster.cc[[c("nbsp.prop","FRic","FEve","FDiv","FDis")]]
diff_fut2.FD <- pred_fut2_raster.FD - CWM_raster.cc[[c("nbsp.prop","FRic","FEve","FDiv","FDis")]]
diff_fut3.FD <- pred_fut3_raster.FD - CWM_raster.cc[[c("nbsp.prop","FRic","FEve","FDiv","FDis")]]

##--- 8.7 Plots with a diverging colour gradient ---####
# As the function works through rasterVis and trellis/level plots, and combine them manually through grid.arrange():
# SSP126
{grid.arrange(Mydiverge0(levelplot(diff_fut1.FD[["nbsp"]],
                                   main="nbsp",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut1.FD[["FRic"]],
                                   main="FRic",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut1.FD[["FEve"]],
                                   main="FEve",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut1.FD[["FDiv"]],
                                   main="FDiv",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut1.FD[["FDis"]],
                                   main="FDis",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              ncol=3, nrow=2)}
# SSP245
{grid.arrange(Mydiverge0(levelplot(diff_fut2.FD[["nbsp"]],
                                   main="nbsp",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut2.FD[["FRic"]],
                                   main="FRic",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut2.FD[["FEve"]],
                                   main="FEve",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut2.FD[["FDiv"]],
                                   main="FDiv",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut2.FD[["FDis"]],
                                   main="FDis",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              ncol=3, nrow=2)}
# SSP585
{grid.arrange(Mydiverge0(levelplot(diff_fut3.FD[["nbsp"]],
                                   main="nbsp",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut3.FD[["FRic"]],
                                   main="FRic",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut3.FD[["FEve"]],
                                   main="FEve",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut3.FD[["FDiv"]],
                                   main="FDiv",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              Mydiverge0(levelplot(diff_fut3.FD[["FDis"]],
                                   main="FDis",
                                   margin=FALSE, xlab=NULL, ylab=NULL, scales=list(draw=FALSE),
                                   par.settings = list(axis.line = list(col = "transparent"))), ramp='RdBu'),
              ncol=3, nrow=2)}



