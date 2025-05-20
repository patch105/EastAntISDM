
#####################################################
########## 1. Biological Data Preparation ##########
#####################################################

# This code cleans the Biodiversity of Ice-Free Antarctica Database (Terauds et al., 2025) and the GBIF data and combines them for subsequent analysis.

# Steps for the GBIF approach are summarised as follows:
#   1. Download GBIF data online by selecting all occurrences with Antarctic as the continent
# 2. Drop records with no coordinates
# 3. Crop records to the ice-free areas of Antarctica (represented by the rock union layer of Toth et al.)
# 4. Drop records with various other criteria (e.g., genetic data)
# 5. Save as csv


# Load packages -----------------------------------------------------------


library(purrr)

packages <- c("here", "sf", "terra", "dplyr", "data.table", "tidyr", "bdc", "viridis", "ggplot2", "tidyterra", "stringr")

walk(packages, require, character.only = T)

here::here()


# Domain setup ------------------------------------------------------------


# Ice-Free Union layer sourced from: <http://dx.doi.org/doi:10.26179/7mnh-j215>

# ice_free_union <- rast(here("Data/Environmental_predictors/rock_union1.tif")) %>% 
#     project("EPSG:3031")
# 
# ice_free_union <- ifel(is.na(ice_free_union), NA, 1)

# # Save
# writeRaster(ice_free_union, here("Data/Environmental_predictors/ice_free_union_reproj_100m.tif"), overwrite = T)

# Load the ice-free areas
# ice_free <- rast(here("Data/Environmental_predictors/ice_free_union_reproj_100m.tif"))
ice_free <- rast(here("Data/Environmental_predictors/ice_free_upsamp_1km.tif"))

# Load the Antarctic Conservation Biogeographic Regions, filter to East Antarctica
ACBRS <- st_read(here("Data/Environmental_predictors/ACBRs_v2_2016.shp"), crs = 3031) %>% filter(ACBR_Name == "East Antarctica")
ACBRS_SPVE <- vect(ACBRS)


# Also trim ice-free land to just East Antarctica
ice_free.EastAnt <- terra::crop(ice_free, ext(ACBRS_SPVE))



#############################################
############ ICE-FREE DATABASE #############
#############################################


# Load data ---------------------------------------------------------------

# **NOTE** - When data was downloaded from Ecology supplementary materials <https://doi.org/10.1002/ecy.70000>, I first had to re-save the `.csv` file as UTF-8 encoding.

Ant_biodf <- read.csv(here("Data/Biological_records/Biodiversity_IceFree_Antarctica.csv"), 
                      header = T,
                      fileEncoding = "UTF-8")


# Cleaning  ------------------------------------------------------


# Filter out records < 1930 but not those with no date info.
Ant_biodf$year <- as.numeric(Ant_biodf$year)  
Ant_biodf <- Ant_biodf %>% filter(year > 1930 | is.na(year))

# Same for Publish_YEAR
# This function introduces NAs because some publish years have multiple in them
# This is ok because none of these cases are years < 1930
Ant_biodf$Publish_YEAR <- as.numeric(Ant_biodf$Publish_YEAR)
Ant_biodf <- Ant_biodf %>% filter(Publish_YEAR > 1930 | is.na(Publish_YEAR))

# Count the number of remaining records without a year or publish year, by dataset ID
Ant_biodf2 <- Ant_biodf %>% filter(is.na(year) & is.na(Publish_YEAR))

# DECIDED TO REMOVE THIS LINE, COULD BACK TRACK LATER
# Then remove records that have no year or publish year and aren't part of the ASPA database
# Ant_biodf2 <- Ant_biodf %>% filter(!(is.na(year) & is.na(Publish_YEAR)) | datasetID == "ASPA")

# Remove vagrant species or vagrant records of a species
Ant_biodf <- Ant_biodf %>% 
  filter(dynamicProperties != "Vagrant species")


# Remove those that have coord uncertainty > 10km
Ant_biodf <- Ant_biodf %>%
  filter(coordinateUncertaintyInMetres < 10001 | is.na(coordinateUncertaintyInMetres))


Ant_biodf <- Ant_biodf %>% 
  filter(!(Snap_IFA == "Y" & Dist_IFA > 10001) | is.na(Dist_IFA))

# The filter out records with up to 5km coord uncertainty and 5km Distance to IFA         
Ant_biodf <- Ant_biodf %>% 
  filter(!(coordinateUncertaintyInMetres > 5001 & Dist_IFA > 5001 & Snap_IFA == "Y") | is.na(Dist_IFA) | is.na(coordinateUncertaintyInMetres))

# Convert Ant_biodf to vector
Ant_bio <- st_as_sf(Ant_biodf,
                    coords = c("decimalLongitude", "decimalLatitude"),
                    crs = 4326) # set as WGS 1984

# Project to WGS_1984_Stereographic_South_Pole
Ant_bio <- st_transform(Ant_bio, 3031)

# Convert to a SpatVector for extract function below
Ant_bio_SPVE <- vect(Ant_bio)


# Remove records not on ice-free land ----------------------------------------

# Filter out records that do not intersect with ice-free areas (100m)
Ant_biodf.icefree <- terra::extract(ice_free, Ant_bio_SPVE)

Ant_biodf.icefree <- Ant_biodf.icefree[!is.na(Ant_biodf.icefree$rock_union1), ]

# Ensure ID column in Ant_biodf.icefree is treated as character
Ant_biodf.icefree$ID <- as.character(Ant_biodf.icefree$ID)

# Subset Ant_biodf using row names that match ID values in Ant_biodf.icefree
Ant_biodf.icefree <- Ant_biodf[rownames(Ant_biodf) %in% Ant_biodf.icefree$ID, ]

# Convert Ant_biodf to vector
Ant_biodf.icefree <- st_as_sf(Ant_biodf.icefree,
                              coords = c("decimalLongitude", "decimalLatitude"),
                              crs = 4326) # set as WGS 1984

# Project to WGS_1984_Stereographic_South_Pole
Ant_biodf.icefree <- st_transform(Ant_biodf.icefree, 3031)

# Add x y geometry as columns to the sf object
Ant_biodf.icefree.xy <- Ant_biodf.icefree %>% 
  mutate(x = st_coordinates(Ant_biodf.icefree)[,1], 
         y = st_coordinates(Ant_biodf.icefree)[,2]) %>% 
  st_drop_geometry()

write.csv(Ant_biodf.icefree.xy, here("Data/Biological_records/Ant_biodf.icefree.xy.csv"), row.names=FALSE)

################################
############ GBIF #############
################################

# Load GBIF dataset, we start with 1,021,296 observations.

GBIF <- data.table::fread(here("Data/Biological_records/GBIF_occurrence.txt"))

# glimpse(GBIF)

# Remove records with no coordinate information
GBIF <- GBIF %>% drop_na(decimalLongitude, decimalLatitude) 


# Trim records to ice-free areas ------------------------------------------


# Set as sf object with WGS84 crs
GBIF_sf <- st_as_sf(GBIF, 
                    coords = c("decimalLongitude", "decimalLatitude"), 
                    crs = 4326)

# Reproject to WGS_194_Stereographic_South_Pole
GBIF_sf <- st_transform(GBIF_sf, 3031) 

# Convert to SpatVector for extract function
GBIF_SPVE <- vect(GBIF_sf)

# Remove GBIF from environment
rm(GBIF)
rm(table)

# Now clear enviro.
gc()


# Load the ice-free areas (100m version and 1km version)
ice_free <- rast(here("Data/Environmental_predictors/ice_free_union_reproj_100m.tif"))

# Filter out records that do not intersect with ice-free areas (100m)
GBIF.icefree <- terra::extract(ice_free, GBIF_SPVE)

GBIF.icefree <- GBIF.icefree[!is.na(GBIF.icefree$rock_union1), ]

# Ensure ID column in GBIF.icefree is treated as character
GBIF.icefree$ID <- as.character(GBIF.icefree$ID)

# Subset GBIF data using row names that match ID values in Ant_biodf.icefree
GBIF.icefree <- GBIF_sf[rownames(GBIF_sf) %in% GBIF.icefree$ID, ]


# Perform data quality checks and remove records with errors --------------

# Remove GPS tracking data.
# Remove camera trap or satellite data.
# Remove fossils.

GBIF.icefree <- GBIF.icefree %>% filter(basisOfRecord != "MACHINE_OBSERVATION" & 
                                          basisOfRecord != "FOSSIL_SPECIMEN")

GBIF.icefree <- GBIF.icefree %>% filter(kingdom != "Bacteria" & 
                                          kingdom != "incertae sedis" &
                                          kingdom != "Viruses" &
                                          kingdom != "Archaea" &
                                          kingdom != "Protozoa")

# Remove records that are based on genetic sequences.
# Some datasets are from genetic sequences but have not consistent characteristics to identify and filter them out.
# We therefore filter out these specific datasets manually. We know these are genetic datasets based on the metadata of their Project.

GBIF.icefree <- GBIF.icefree %>% filter(organismQuantityType != "DNA sequence reads" &
                                          organismQuantityType != "Sequence Counts (%)" &
                                          associatedSequences == "")

GBIF.icefree <- GBIF.icefree %>% filter(datasetKey != "8531183a-86b5-459b-a93e-37198f38f8a6" &
                                          datasetKey != "f885d3e2-0310-43f5-a902-50dda62e6a68")

# Some other datasets we filter out due to their irrelevance:
#   
#   -pelagic fish one
# -diatoms from lakes
# -marine macrobenthic sampling
# -tracking data
# -marine benthic sampling
# -unknown dataset

GBIF.icefree <- GBIF.icefree %>% 
  filter(!datasetKey %in% c("c60d0b4f-4e51-4a5a-95dc-babc74ba3db0", 
                            "570489ac-e114-4a17-9340-2e01ee7d9ee3",
                            "34877684-bbd1-45f2-a205-8f1cc9ad58d7",
                            "b86fe411-8e62-4cd0-aab2-914d75401598",
                            "a267b6a7-91f9-457c-889a-481e7aa920b6",
                            "828a3d8c-f762-11e1-a439-00145eb45e9a",
                            "7b361b5a-f762-11e1-a439-00145eb45e9a",
                            "96419bea-f762-11e1-a439-00145eb45e9a",
                            "86b65c92-f762-11e1-a439-00145eb45e9a",
                            "bb54104f-3883-443b-b4fa-c43c6194d8d6",
                            "081daea5-fe6a-4303-bcc3-126b65d0adeb",
                            "7b67c51a-f762-11e1-a439-00145eb45e9a",
                            "0471f28e-b58b-4c02-89e4-d2a1d621a9d8",
                            "2952a69a-c159-4e83-849f-738428d5cea6",
                            "891e34fa-f762-11e1-a439-00145eb45e9a",
                            "7b43dfec-f762-11e1-a439-00145eb45e9a",
                            "933eeab0-5f75-4bd3-be88-e4e6e07eabd6",
                            "a1d3149a-899c-431b-b8f0-16df4eaaaa40",
                            "16a3533d-90cf-4402-95a4-42437e2f9660",
                            "ef2497c9-3b6b-4468-905d-430c87bd45ca",
                            "db9efd51-4f8b-4e58-b2a8-875a151ee00e",
                            "7a01619e-0e49-49fa-afcf-5a95428f0458",
                            "b1047888-ae52-4179-9dd5-5448ea342a24",
                            "ff3984d7-84bd-4f3a-b843-666faa4c1696",
                            "26098c25-8f7f-4c71-97ac-1d3db181c65e",
                            "cd023c5e-8729-41b2-b9df-1419289c0e40",
                            "e1d0d416-e430-4fb8-b3c1-6dfa66ffd778",
                            "f58922e2-93ed-4703-ba22-12a0674d1b54",
                            "3af84f96-376e-46f2-b701-cf5aa58463f1",
                            "2f7c33b3-ea7a-40ac-a3b7-bace96f978f7",
                            "7b461e60-f762-11e1-a439-00145eb45e9a",
                            "7b49ab2a-f762-11e1-a439-00145eb45e9a",
                            "07f969d7-4a0e-4e75-8f65-cf8bbc6cf44c",
                            "5d6c10bd-ea31-4363-8b79-58c96d859f5b",
                            "bc8bb608-aeb7-4fb5-b0e0-1c5c6795455f",
                            "df8e3fb8-3da7-4104-a866-748f6da20a3c",
                            "5e2c9b3a-9d7c-4871-af8c-144e4f40e9d2",
                            "890fb006-f762-11e1-a439-00145eb45e9a",
                            "8922dd20-f762-11e1-a439-00145eb45e9a",
                            "ec0ad39f-3370-4d51-a663-cb27abd2a6bb",
                            "b93edff8-66e2-43d5-9020-3f6a6379c3ed",
                            "0ed0a394-5276-433e-bf8d-09ceaaee45e9",
                            "c807f833-9fe4-45ec-b5d6-62c6b88979b3",
                            "39905320-6c8a-11de-8226-b8a03c50a862",
                            "07a54454-87c7-4e1e-b2ba-88a9d98d534b",
                            "62df6800-27f8-4f7a-bd8f-5ff82893b841",
                            "e1539f14-f749-4f73-8c18-3355277f94f4",
                            "01a6caea-b201-4495-b511-1adf1384e790",
                            "7b644c6e-f762-11e1-a439-00145eb45e9a",
                            "b52cb0c3-c024-46c4-b7b2-d635400ff6d1",
                            "2f58500b-b776-4fe6-b2b9-cbb32e8cd615",
                            "adb92d68-aaff-47ba-b197-d3d26f72f728",
                            "0214a6a7-898f-4ee8-b888-0be60ecde81f",
                            "7b6b50fe-f762-11e1-a439-00145eb45e9a",
                            "8a43c2b7-5976-457d-9f35-a232655efdcc",
                            "7b42aea6-f762-11e1-a439-00145eb45e9a",
                            "a4be4c6d-5ce7-47ba-982d-1deb05719133",
                            "b568aebb-b2c8-4dd2-8a59-ab74d50f8640",
                            "0ad1d61a-49ad-46a0-8b9f-6fc3f6d6af1d",
                            "7b6322ee-f762-11e1-a439-00145eb45e9a",
                            "c70094e2-7607-42da-8fb2-76669ac5c1ac",
                            "d8b06df0-81b3-41c9-bcf8-6ba5242e2b95",
                            "0d862dd2-12af-4445-92c9-d81d76598b19",
                            "426b4c7a-438a-4d23-97ce-3aae23459f0d",
                            "59825263-9f89-43a9-8f22-c22a6a53b8fc",
                            "7b488c54-f762-11e1-a439-00145eb45e9a",
                            "df3322f1-9247-4f7e-a173-0209cf155625",
                            "eccf4b09-f0c8-462d-a48c-41a7ce36815a",
                            "f11db245-3f9f-4fc6-a0cc-12b4124d081b",
                            "4e5552d1-5eaf-40a3-b48b-01f92da22f17",
                            "7b45011a-f762-11e1-a439-00145eb45e9a",
                            "7b4f3cc0-f762-11e1-a439-00145eb45e9a",
                            "7d8ff344-bf64-4f75-8aad-22a9d30f4fff",
                            "84b18cce-083a-4464-bee8-25b2083a17cd",
                            "040c5662-da76-4782-a48e-cdea1892d14c"
  ))



# Removing records with GEOSPATIAL issues ---------------------------------

remove <- paste(c("GEODETIC_DATUM_INVALID", "PRESUMED_SWAPPED_COORDINATE", "ZERO_COORDINATE"), 
                collapse = '|')

GBIF.icefree <- GBIF.icefree %>% 
  filter(!str_detect(issue, remove))


# Remove records based on cutoff date -------------------------------------

# Remove records older than 1930.
# Remove records published earlier than 1930.
# Keep records with no date information.


GBIF.icefree <- GBIF.icefree %>% 
  filter(!str_detect(issue, 'RECORDED_DATE_MISMATCH'))

# Count how many have no eventDate and no year
sum(GBIF.icefree$eventDate == "" & GBIF.icefree$year == "")

# Remove records with no eventDate or year

GBIF.icefree <- GBIF.icefree %>% 
  filter(eventDate != "" & year != "") 


# Record coordinate uncertainty -------------------------------------------

# 34,000 records have no coordinate uncertainty.
# Trim the records with >10 km uncertainty, removing 474.

# Count number of records with none
sum(is.na(GBIF.icefree$coordinateUncertaintyInMeters))

# Count number of records with > 10km uncertainty
sum(GBIF.icefree$coordinateUncertaintyInMeters > 10000, na.rm=T)

# Remove records with > 10km uncertainty

GBIF.icefree <- GBIF.icefree %>% 
  filter(coordinateUncertaintyInMeters < 10000 | is.na(coordinateUncertaintyInMeters))


# Removing overlap with projects from the Biodiversity of Ice-Free DB --------

# BAS Antarctic Plant Database records from 2004 and earlier

GBIF.icefree <- GBIF.icefree %>% 
  filter(!(datasetKey == "82d9ff5c-f762-11e1-a439-00145eb45e9a" & year <= 2004))

count <- count(GBIF.icefree, datasetKey)

# Australian Antarctic Division Herbarium

GBIF.icefree <- GBIF.icefree %>%
  filter(datasetKey != "82e45d26-f762-11e1-a439-00145eb45e9a" )

# First remove all penguin records, then add back in the relevant ones
save <- GBIF.icefree %>%
  filter(!species %in% c("Pygoscelis adeliae", "Pygoscelis antarcticus", "Pygoscelis papua"))

# All Antarctic Penguin Biogeography Project older than 2021

# Also remove those records that are of penguins that aren't in the Antarctic Penguin Biogeography Project (not likely to capture new breeding locations).

penguin <- GBIF.icefree %>%
  filter(species %in% c("Pygoscelis adeliae", "Pygoscelis antarcticus", "Pygoscelis papua") &
           datasetKey == "f7c30fac-cf80-471f-8343-4ec5d8594661" & year > 2021)

GBIF.icefree <- bind_rows(save, penguin)


# Check for duplicates with GBIF data -------------------------------------


# Almost none of them have taxonID.
# All have acceptedNameUsageID.
# scientificName
# 
# The below code removes duplicate records with the same values for several columns.
# 
# If we do the below code, we end up with 21,440 records remaining. 
# Therefore, we have 22,827 records with the same x, y, year and acceptedNameUsageID
# 
# If we do based on x, y, year, acceptedNameUsageID, taxonKey, acceptedScientificName, species we still get only 21,273 records


# Add x y geometry as columns to the sf object
GBIF.icefree_xy <- GBIF.icefree %>% 
  mutate(x = st_coordinates(GBIF.icefree)[,1], 
         y = st_coordinates(GBIF.icefree)[,2]
  )

# Checking if there's any rows with NA in these columns, there isn't
na_rows <- GBIF.icefree_xy %>% 
  drop_na(x, y, acceptedNameUsageID, taxonKey, acceptedScientificName, species)

GBIF.icefree_xy <- GBIF.icefree_xy %>% 
  distinct(x, y, year, acceptedNameUsageID, taxonKey, acceptedScientificName, species, .keep_all = T) %>% 
  st_drop_geometry()

# Save cleaned GBIF data 

write.csv(GBIF.icefree_xy, here("Data/Biological_records/GBIF.icefree_xy.csv"), row.names = FALSE)


#####################################################
############ COMBINE ICE-FREE DB AND GBIF  #############
#####################################################

ICEFREE_df <- read.csv(here("Data/Biological_records/Ant_biodf.icefree.xy.csv"), header = T) 

GBIF_df <- read.csv(here("Data/Biological_records/GBIF.icefree_xy.csv")) 


# Tidy ice-free DB names --------------------------------------------------

# Using the bdc package. It removes uncertainty terms.


# Clean scientific name
bdc_clean_names(ICEFREE_df$scientificName, save_outputs = TRUE)

# Load the cleaned data
clean_doc <- read.csv(here("Output/Check/02_parsed_names.csv"))

# Add cleaned scientific name to database
ICEFREE_df <- ICEFREE_df %>% mutate(scientificNameClean = clean_doc[,11])

# Clean genus name
bdc_clean_names(ICEFREE_df$genus, save_outputs = TRUE)

# Load the cleaned data
clean_doc <- read.csv(here("Output/Check/02_parsed_names.csv"))

# Add cleaned genus name to database
ICEFREE_df <- ICEFREE_df %>% mutate(genusClean = clean_doc[,11])

# Clean family name
bdc_clean_names(ICEFREE_df$family, save_outputs = TRUE)

# Load the cleaned data
clean_doc <- read.csv(here("Output/Check/02_parsed_names.csv"))

# Add cleaned family name to database
ICEFREE_df <- ICEFREE_df %>% mutate(familyClean = clean_doc[,11])

# Clean the original name usage for PTM checking
bdc_clean_names(ICEFREE_df$originalNameUsage, save_outputs = TRUE)

# Load the cleaned data
clean_doc <- read.csv(here("Output/Check/02_parsed_names.csv"))

# Add cleaned orig name to database
ICEFREE_df <- ICEFREE_df %>% mutate(originalNameUsageClean = clean_doc[,11])


# Check for duplicates between the two data sources -----------------------

# Check the two databases for x, y, year, and scientific name duplicates.

# Rename GBIF columns to match Ant bio DB
GBIF_df <- GBIF_df %>% 
  rename(scientificNameClean = species) %>% 
  rename(genusClean = genus) %>% 
  rename(familyClean = family)

GBIF_df$year <- as.numeric(GBIF_df$year)

# Check for duplicates between GBIF and SCAR Bio
unique_rows_GBIF <- anti_join(GBIF_df, ICEFREE_df, by = c("x", "y", "year", "scientificNameClean", "genusClean", "familyClean"))
unique_rows_SCAR <- anti_join(ICEFREE_df, GBIF_df, by = c("x", "y", "year", "scientificNameClean", "genusClean", "familyClean"))

GBIF_df <- unique_rows_GBIF

ICEFREE_df <- unique_rows_SCAR



# Trim to just East Antarctica --------------------------------------------

GBIF_vect <- vect(GBIF_df, geom = c("x","y"))

ICEFREE_vect <- vect(ICEFREE_df, geom = c("x","y"))

GBIF_east_ant.df <- terra::mask(GBIF_vect, ext(ice_free.EastAnt))  %>% as.data.frame(geom = "XY")

ICEFREE_east_ant.df <- terra::mask(ICEFREE_vect, ext(ice_free.EastAnt))  %>% 
  as.data.frame(geom = "XY") 


# Save just lichens and moss records --------------------------------------

count <- GBIF_east_ant.df %>% filter(!kingdom %in% c("Animalia", "Chromista", "Bacteria", "Protozoa")) %>%  count(kingdom, class, familyClean, genusClean)

GBIF_east_ant.df <- GBIF_east_ant.df %>% 
  filter(class %in% c("Lecanoromycetes", "Ascomycetes", "Bryopsida") | genusClean %in% c("Kuttlingeria", "Candelariella")) %>% 
  mutate(vegtype = ifelse(class == "Bryopsida", "Moss", "Lichen"))

# Removed functional groups that have been identified as non-lichenised fungi
ICEFREE_east_ant.df <- ICEFREE_east_ant.df %>% 
  filter(!PTM_ID %in% c("?", "29", "26 & 29", "75")) 

count2 <- ICEFREE_east_ant.df %>% filter(!kingdom %in% c("Animalia", "Chromista", "Bacteria", "Protozoa")) %>%  count(kingdom, class, familyClean, genusClean)

ICEFREE_east_ant.df <- ICEFREE_east_ant.df %>% 
  filter(class %in% c("Lecanoromycetes", "Ascomycetes", "Bryopsida") | genusClean %in% c("Kuttlingeria", "Candelariella")) %>% 
  mutate(vegtype = ifelse(class == "Bryopsida", "Moss", "Lichen"))


# Tidy and join the datasets ------------------------------------------

GBIF_east_ant.sf <- st_as_sf(GBIF_east_ant.df, 
                             coords = c("x", "y"), 
                             crs = 3031) %>% 
  dplyr::select(vegtype,coordinateUncertaintyInMeters) %>% 
  rename(coordinateUncertaintyInMetres = coordinateUncertaintyInMeters)

ICEFREE_east_ant.sf <- st_as_sf(ICEFREE_east_ant.df, 
                                coords = c("x", "y"), 
                                crs = 3031) %>% 
  dplyr::select(vegtype, coordinateUncertaintyInMetres)

veg.east.ant.sf <- rbind(GBIF_east_ant.sf, ICEFREE_east_ant.sf)

count(veg.east.ant.sf, vegtype)

veg.east.ant <- vect(rbind(GBIF_east_ant.sf, ICEFREE_east_ant.sf))


# Save the PO records -----------------------------------------------------

st_write(veg.east.ant.sf, here("Data/Biological_records", "PO_Veg_East_Ant.shp"))


#####################################################################
############ Presence-absence survey - Vestfold Hills #############
####################################################################

# Presence-absence data from Travers et al. (2024) http://dx.doi.org/doi:10.26179/wrss-ta40

vestfold <- read.csv(here("Data/Biological_records/Travers_Vestfold_PA_Survey.csv"))

vestfold_sf <- st_as_sf(vestfold,
                       coords = c("x", "y"),
                       crs = 4326) # WGS 84 geographic coordinates

vestfold_sf <- st_transform(vestfold_sf, 3031) #project to WGS_1984 Antarctic Polar Stereographic

st_write(vestfold_sf, here("Data/Biological_records", "PA_Veg_vestfold.shp"))


vestfold_df <- vestfold_sf %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  bind_cols(st_drop_geometry(vestfold_sf)) %>% 
  rename(x = X, y = Y) 

count(vestfold_df, surface_moss)
count(vestfold_df, surface_lichen)

#####################################################################
############ Presence-absence survey - Bunger Hills 23 #############
####################################################################

# Unpublished records from a 2023/24 field season

bunger23 <- read.csv(file.path(getwd(), "Data/Biological_records/Bunger_2023_Data.csv"), strip.white = T)

# Remove observations taken opportunistically, not at designated stratified sites
bunger23 <- bunger23 %>% filter(record_type != "Opportunistic obs. not at site")

bunger23 <- bunger23 %>% mutate(surface_moss = NA, 
                                surface_lichen = NA)


bunger23 <- bunger23 %>% 
  filter(PlotCentreGPSLocation1 != "") %>% # Remove subplots
  mutate(surface_moss = ifelse(!is.na(MossSpp_common) & MossSpp_common != "", 1, 0)) %>% 
  mutate(surface_lichen = ifelse(!is.na(LichenSpp_common) & LichenSpp_common != "", 1, 0)) %>% 
  rename(lat = plot_lat_dec1, lon = plot_lon_dec1) %>% 
  dplyr::select(site_id, lat, lon, surface_moss, surface_lichen)


bunger23_sf <- st_as_sf(bunger23,
                        coords = c("lon", "lat"),
                        crs = 4326) # WGS 84 geographic coordinates

bunger23_sf <- st_transform(bunger23_sf, 3031) #project to WGS_1984 Antarctic Polar Stereographic

st_write(bunger23_sf, here("Data/Biological_records", "PA_Veg_bunger23.shp"))

bunger23_df <- bunger23_sf %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  bind_cols(st_drop_geometry(bunger23_sf)) %>% 
  rename(x = X, y = Y) 


#####################################################################
############ Presence-absence survey - Bunger Hills Leishman #########
####################################################################

leishman <- read.csv("Data/Biological_records/leishman_data.csv")

leishman$easting <- as.character(leishman$easting)
leishman$northing_new <- as.character(leishman$northing_new)

leishman <- leishman %>% mutate(easting_final = paste0("5", easting, "00"))
leishman <- leishman %>% mutate(northing_final = paste0("26", northing_new, "00"))

leishman$easting_final <- as.numeric(leishman$easting_final)
leishman$northing_final <- as.numeric(leishman$northing_final)

leishman_sf <- st_as_sf(leishman,
                        coords = c("easting_final", "northing_final"),
                        crs = 32747) # UTM Zone 47S

# Project to WGS 84 Antarctic Polar Stereographic
leishman_sf <- st_transform(leishman_sf, 3031)

st_write(leishman_sf, here("Data/Biological_records", "PA_Veg_bunger_leishman.shp"))

leishman_df <- leishman_sf %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(st_drop_geometry(leishman_sf)) %>%
  rename(x = X, y = Y)



# 
# # Plot records across East Antarctica -------------------------------------
# 
# # Load the Antarctic coastline for plotting
# coast <- st_read(here("Data/Environmental_predictors/add_coastline_high_res_polygon_v7_10.shp"), crs = 3031)
# 
# # Plot distribution of presence-only lichen records across East Antarctica
# a <- ggplot() +
#   geom_sf(data = coast, color = "black", size = 0.05) +
#   geom_tile(data = as.data.frame(ice_free.EastAnt, xy = T)) +
#   geom_sf(data = veg.east.ant.sf, aes(color = vegtype)) +
#   coord_sf(
#     xlim = c(ext(ice_free.EastAnt)$xmin, ext(ice_free.EastAnt)$xmax), 
#     ylim = c(ext(ice_free.EastAnt)$ymin, ext(ice_free.EastAnt)$ymax)) +
#   # scale_fill_manual(name = "", 
#   #                   labels = element_blank(),
#   #                   values = c("white", "grey92","grey92", "grey92")) +
#   theme_bw() + 
#   theme(legend.title = element_blank(),
#         legend.key = element_blank(),
#         legend.background = element_blank())
# 
# # Now just in Vestfold Hills
# b <- ggplot() +
#   geom_sf(data = coast, color = "black", size = 0.05) +
#   geom_sf(data = ice_free, fill = "grey80", size = 0.05) +
#   geom_sf(data = bio_east_ant_sf, aes(color = class)) +
#   coord_sf(
#     xlim = c(st_bbox(bio_vestfold_sf)$xmin, st_bbox(bio_vestfold_sf)$xmax), 
#     ylim = c(st_bbox(bio_vestfold_sf)$ymin, st_bbox(bio_vestfold_sf)$ymax)) +
#   scale_fill_manual(name = "", 
#                     labels = element_blank(),
#                     values = c("white", "grey92","grey92", "grey92")) +
#   theme_bw() +
#   theme(legend.title = element_blank(),
#         legend.key = element_blank(),
#         legend.background = element_blank())
# 
# # And in Bunger Hills
# c <- ggplot() +
#   geom_sf(data = coast, color = "black", size = 0.05) +
#   geom_sf(data = ice_free, fill = "grey80", size = 0.05) +
#   geom_sf(data = bio_east_ant_sf, aes(color = class)) +
#   coord_sf(
#     xlim = c(st_bbox(bio_bunger_sf)$xmin, st_bbox(bio_bunger_sf)$xmax), 
#     ylim = c(st_bbox(bio_bunger_sf)$ymin, st_bbox(bio_bunger_sf)$ymax)) +
#   scale_fill_manual(name = "", 
#                     labels = element_blank(),
#                     values = c("white", "grey92","grey92", "grey92")) +
#   theme_bw() +
#   theme(legend.title = element_blank(),
#         legend.key = element_blank(),
#         legend.background = element_blank())
# 
# PO_plot <- ggarrange(a , 
#                      ggarrange(b, c, nrow = 2, labels = c("(b)", "(c)")),
#                      labels = c("(a)", ""), ncol = 2, 
#                      common.legend = T)
# 
# # ggsave(plot = PO , filename = here("output/Locs_of_PO_data_East_Ant_plot.png"), w = 21.5, h = 21.2, units = "cm", dpi = 800, device = "png" )


