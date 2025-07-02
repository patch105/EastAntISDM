
#####################################################
########## 2. Environmental Data Preparation ##########
#####################################################

# This code cleans and creates 500m resolution climate and topographic variables for the integrated species distribution model.
# Note that in a separate file we have downloaded and reformatted the 100m topographic variables


# Load packages -----------------------------------------------------------


library(purrr)

packages <- c("here", "sf", "terra", "dplyr", "tidyr", "viridis", "ggplot2", "tidyterra")

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
ice_free <- rast(here("Data/Environmental_predictors/ice_free_union_reproj_100m.tif"))

# Load the Antarctic Conservation Biogeographic Regions, filter to East Antarctica
ACBRS <- st_read(here("Data/Environmental_predictors/ACBRs_v2_2016.shp"), crs = 3031) %>% filter(ACBR_Name == "East Antarctica")
ACBRS_SPVE <- vect(ACBRS)

# Also trim ice-free land to just East Antarctica
ice_free.EastAnt <- terra::crop(ice_free, ext(ACBRS_SPVE))

# Now using our 100m topography as the template for our domain dimensions (because we don't want to resample these topographic layers if we don't need to), we'll resample our ice-free area layer to match the dimensions of the topography rasters

# Load one 100m topography as our template
template <- rast(here("Data/Environmental_predictors/TWI_100m_IceFree_EastAnt.tif"))
ice_free.EastAnt <- resample(ice_free.EastAnt, template, method = "bilinear")

# Save the ice-free land for East Antarctica with the right domain
writeRaster(ice_free.EastAnt, here("Data/Environmental_predictors/ice_free_EastAnt_100m.tif"), overwrite = T)

# ice_free.EastAnt <- rast(here("Data/Environmental_predictors/ice_free_EastAnt_100m.tif"))

# Make bounding boxes of Vestfold Hills & Bunger Hills --------------------

Vestfold <- vect(here("Data/Biological_records", "PA_Veg_vestfold.shp")) 
Bunger <- vect(here("Data/Biological_records", "PA_Veg_bunger23.shp"))

# Convert extent to polygon
vestfold_box <- as.polygons(ext(Vestfold))
bunger_box <- as.polygons(ext(Bunger))

# Ensure the CRS is set
crs(vestfold_box) <- crs(Vestfold)
crs(bunger_box) <- crs(Bunger)

# Buffer the bounding boxes by 100m
vestfold_buff <- buffer(vestfold_box, width = 20000)
bunger_buff <- buffer(bunger_box, width = 20000)

# Save
writeVector(vestfold_buff, here("Data/Environmental_predictors/vestfold_boundary.shp"), overwrite = T)
writeVector(bunger_buff, here("Data/Environmental_predictors/bunger_boundary.shp"), overwrite = T)

# Distance to vertebrates
# Distance to seasonal water
# Distance to station

# ###############################################
# ######## Distance to research station #########
# ################################################

# Data from COMNAP list of stations NOV 2024
# Downloaded from: https://www.comnap.aq/antarctic-facilities-information [Accessed 18 Feb 2025]
# NOTE: The `.csv` file was re-saved as UTF-8 encoding

stations_df <- read.csv(here("Data/Environmental_predictors/COMNAP_Antarctic_Facilities_Nov2024.csv"))
stations <- st_as_sf(stations_df,
                     coords = c("Longitude..DD.", "Latitude..DD."),
                     crs = 4326) # Set as WGS 1984

stations <- st_transform(stations, 3031) # Project to WGS_1984_Stereographic_South_Pole
stations_SPVE <- vect(stations)

dist_station <- terra::distance(ice_free.EastAnt, stations_SPVE)
dist_station <- mask(dist_station, ice_free.EastAnt, maskvalue = NA)

writeRaster(dist_station, here("Data/Environmental_predictors/distance_to_station_ICEFREE_100m.tif"), overwrite = T)


# ################################################
# ######### Distance to seasonal meltwater #######
# ################################################

# Extract just the seasonal meltwater within a buffer of ice-free land
# Using the add_rock_outcrop_high_res_polygon_v_7_3

seasonal_water <- st_read(here("Data/Environmental_predictors/GeoMap_seasonal_water_shp.shp"), crs = 3031)
seasonal_waterSPVE <- vect(seasonal_water)

# Make polygon version for mask
domainSPVE <- terra::as.polygons(ice_free.EastAnt)

# Mask seasonal water shapefile to ice-free land to avoid water melting on glaciers etc.
seasonal_waterSPVE <- mask(seasonal_waterSPVE, domainSPVE)

dist_seasonal_water <- terra::distance(ice_free.EastAnt, seasonal_waterSPVE)
dist_seasonal_water <- mask(dist_seasonal_water, ice_free.EastAnt, maskvalue = NA)

writeRaster(dist_seasonal_water, here("Data/Environmental_predictors/distance_to_seasonal_water_ICEFREE_100m.tif"), overwrite = T)
plot(dist_seasonal_water)


###################################################
########### Distance to vertebrates ##################
####################################################

# Load the xy records with just seals
seals_xy <- read.csv(here("Data/Environmental_predictors/ICEFREE_GBIF_SEAL_XY.csv"), header = T)

seals_xy <- st_as_sf(seals_xy,
                     coords = c("x", "y"),
                     crs = 3031) #set as WGS 1984

seals_SPVE <- vect(seals_xy) %>%
  as.polygons()

# Load the xy records with just seabirds
seabirds_xy <- read.csv(here("Data/Environmental_predictors/ICEFREE_GBIF_SEABIRD_XY.csv"), header = T)

seabirds_xy <- st_as_sf(seabirds_xy,
                        coords = c("x", "y"),
                        crs = 3031) #set as WGS 1984

seabirds_SPVE <- vect(seabirds_xy) %>%
  as.polygons()

# Load the penguin colony locations
penguins_xy <- st_read(here("Data/Environmental_predictors/penguin_rookeries.trim.shp"), crs = 3031)

penguin_colony_SPVE <- vect(penguins_xy)

# Combine them together

vertebrates <- rbind(penguin_colony_SPVE, seals_SPVE, seabirds_SPVE)

dist_vertebrates <- terra::distance(ice_free.EastAnt, vertebrates)
dist_vertebrates <- mask(dist_vertebrates, ice_free.EastAnt, maskvalue = NA)

writeRaster(dist_vertebrates, here("Data/Environmental_predictors/distance_to_vertebrates_ICEFREE_100m.tif"), overwrite = T)


###################################################
########### Mean Summer Temperature ##################
####################################################

# From 2004 to 2021. Have to remove 2003 because it only has records for January, February.

files <- list.files("C:/Users/n11222026/OneDrive - Queensland University of Technology/Data/raw/AntAirIce/Seasonal_mean", full.names = T, pattern = "DJF")

summer_temp <- files %>% 
  purrr::map(rast) %>% 
  purrr::map(project, "EPSG:3031")

# Remove 2003 (01)
summer_temp <- summer_temp[-1]

# Stack rasters
summer_temp <- rast(summer_temp)

# Calculate the mean across the years
mean_summer_temp <- terra::app(summer_temp, mean, na.rm = T)

# Save 1 km version for downscaling
writeRaster(mean_summer_temp, 
            here("Data/Environmental_predictors/mean_summer_temp_AntAirIce_1km.tif"), 
            overwrite = T)

### TEMPORARY: interpolate 1km temp to 100m

summer_temp <- rast(here("Data/Environmental_predictors/mean_summer_temp_AntAirIce_1km.tif"))
summer_temp <- crop(summer_temp, ext(ice_free.EastAnt))

summer_temp <- terra::project(summer_temp, ice_free.EastAnt, method = "near")
summer_temp <- mask(summer_temp, ice_free.EastAnt, maskvalue = NA)
writeRaster(summer_temp, here("Data/Environmental_predictors/mean_summer_temp_AntAirIce_100m.tif"), overwrite = T)


###################################################
########### Wind speed ##################
####################################################

wind <- rast(here("Data/Environmental_predictors/Mean_Annual_Wind_Speed_ALL_YEARS.tif"))


###################################################
########### Precipitation ##################
####################################################

precipitation_rain <- rast(here("Data/Environmental_predictors/Mean_Annual_Precipitation_RAIN_ALL_YEARS.tif"))
precipitation_snow <- rast(here("Data/Environmental_predictors/Mean_Annual_Precipitation_SNOW_ALL_YEARS.tif"))


####################
# Inspect covariate distributions and transform ---------------------------
####################

ice_free.EastAnt <- rast(here("Data/Environmental_predictors/ice_free_EastAnt_100m.tif"))

TWI <- rast(here("Data/Environmental_predictors/TWI_100m_IceFree_EastAnt.tif"))
names(TWI) <- "TWI"
slope <- rast(here("Data/Environmental_predictors/slope_100m_IceFree_EastAnt.tif"))
names(slope) <- "slope"
aspect <- rast(here("Data/Environmental_predictors/aspect_100m_IceFree_EastAnt.tif"))
names(aspect) <- "aspect"

dist_vertebrates <- rast(here("Data/Environmental_predictors/distance_to_vertebrates_ICEFREE_100m.tif"))
names(dist_vertebrates) <- "dist_vertebrates"

dist_seasonal_water <- rast(here("Data/Environmental_predictors/distance_to_seasonal_water_ICEFREE_100m.tif"))
names(dist_seasonal_water) <- "dist_seasonal_water"

# Bias covariate
dist_station <- rast(here("Data/Environmental_predictors/distance_to_station_ICEFREE_100m.tif"))
names(dist_station) <- "dist_station"

summer_temp <- here("Data/Environmental_predictors/mean_summer_temp_AntAirIce_100m.tif")
names(summer_temp) <- "summer_temp"

# Stack covariates
covs <- c(TWI, slope, aspect, dist_seasonal_water, dist_station, dist_vertebrates, summer_temp)

# Plot histograms
hist(covs, na.rm = T, col = "lightblue")
hist(log(covs), na.rm = T, col = "lightblue")
hist(sqrt(covs), na.rm = T, col = "lightblue")

covs_cube <- sign(covs) * abs(covs)^(1/3)
hist(covs_cube, na.rm = T, col = "lightblue")

# Apply some transformations
sqrt_slope <- sqrt(slope)
log_dist_seasonal_water <- log(dist_seasonal_water+1)
log_dist_station <- log(dist_station+1)
log_dist_vertebrates <- log(dist_vertebrates+1)


covs <- c(TWI, sqrt_slope, aspect, log_dist_seasonal_water, log_dist_station, log_dist_vertebrates, summer_temp)

####################
# Check for correlation among predictors ---------------------------------
####################

library(corrplot)

covs_df <- as.data.frame(covs, xy = F, na.rm = T)

ecospat::ecospat.cor.plot(covs_df)

# Nicer corr plot
corrplot.mixed(M, order = 'AOE')

usdm::vif(covs)








