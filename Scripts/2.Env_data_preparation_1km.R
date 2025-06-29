
#####################################################
########## 2. Environmental Data Preparation ##########
#####################################################

# This code cleans and creates 1 km resolution climate and topographic variables for the integrated species distribution model.


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
# ice_free <- rast(here("Data/Environmental_predictors/ice_free_union_reproj_100m.tif"))
ice_free <- rast(here("Data/Environmental_predictors/ice_free_upsamp_1km.tif"))

# Load the Antarctic Conservation Biogeographic Regions, filter to East Antarctica
ACBRS <- st_read(here("Data/Environmental_predictors/ACBRs_v2_2016.shp"), crs = 3031) %>% filter(ACBR_Name == "East Antarctica")
ACBRS_SPVE <- vect(ACBRS)


# Also trim ice-free land to just East Antarctica
ice_free.EastAnt <- terra::crop(ice_free, ext(ACBRS_SPVE))


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



# Load the 1 km topography layers & crop to East Antarctica ------------------

# rast(here("Data/Environmental_predictors/elev_upscaled_new_rockout_merged.tif")) %>%
#   crop(ext(ice_free.EastAnt)) %>% 
#   resample(ice_free.EastAnt, method = "bilinear") %>%
#   mask(ice_free.EastAnt, maskvalue = NA) %>% 
#   writeRaster(here("Data/Environmental_predictors/elevation_EAST_ANTARCTICA.tif"), overwrite = T)
# 
# rast(here("Data/Environmental_predictors/twi_upscaled_new_rockout_merged.tif")) %>%
#   crop(ext(ice_free.EastAnt)) %>% 
#   resample(ice_free.EastAnt, method = "bilinear") %>%
#   mask(ice_free.EastAnt, maskvalue = NA) %>%
#   writeRaster(here("Data/Environmental_predictors/topographic_wetness_index_EAST_ANTARCTICA.tif"), overwrite = T)
# 
# rast(here("Data/Environmental_predictors/tri_upscaled_new_rockout_merged.tif")) %>%
#   crop(ext(ice_free.EastAnt)) %>% 
#   resample(ice_free.EastAnt, method = "bilinear") %>%
#   mask(ice_free.EastAnt, maskvalue = NA) %>%
#   writeRaster(here("Data/Environmental_predictors/topographic_roughness_index_EAST_ANTARCTICA.tif"), overwrite = T)
# 
# rast(here("Data/Environmental_predictors/slope_upscaled_new_rockout_merged.tif")) %>%
#   crop(ext(ice_free.EastAnt)) %>%
#   resample(ice_free.EastAnt, method = "bilinear") %>%
#   mask(ice_free.EastAnt, maskvalue = NA) %>%
#   writeRaster(here("Data/Environmental_predictors/slope_EAST_ANTARCTICA.tif"), overwrite = T)

# rast(here("Data/Environmental_predictors/northness_upscaled_new_rockout_merged.tif")) %>%
#   crop(ext(ice_free.EastAnt)) %>% 
#   resample(ice_free.EastAnt, method = "bilinear") %>%
#   mask(ice_free.EastAnt, maskvalue = NA) %>%
#   writeRaster(here("Data/Environmental_predictors/northness_EAST_ANTARCTICA.tif"), overwrite = T)


# Load other layers & crop to East Antarctica -------------------------------

# rast(here("Data/Environmental_predictors/distance_to_vertebrates_ICEFREE.tif")) %>%
#   crop(ext(ice_free.EastAnt)) %>% 
#   writeRaster(here("Data/Environmental_predictors/distance_to_vertebrates_EAST_ANTARCTICA.tif"), overwrite = T)
# 
# rast(here("Data/Environmental_predictors/dist_to_coast_seamask_v7_10_ICEFREE.tif")) %>%
#   crop(ext(ice_free.EastAnt)) %>% 
#   writeRaster(here("Data/Environmental_predictors/dist_to_coast_seamask_v7_10_EAST_ANTARCTICA.tif"), overwrite = T)
# 
# rast(here("Data/Environmental_predictors/distance_to_seasonal_water_ICEFREE.tif")) %>%
#   crop(ext(ice_free.EastAnt)) %>% 
#   writeRaster(here("Data/Environmental_predictors/distance_to_seasonal_water_EAST_ANTARCTICA.tif"), overwrite = T)
# 
# rast(here("Data/Environmental_predictors/distance_to_station_ICEFREE.tif")) %>%
#   crop(ext(ice_free.EastAnt)) %>% 
#   writeRaster(here("Data/Environmental_predictors/distance_to_station_EAST_ANTARCTICA.tif"), overwrite = T)


# Conductivity ------------------------------------------------------------

# Summer temperature -----------------------------------------------------

rast(here("Data/Environmental_predictors/Mean_Summer_Temp_ICEFREE.tif")) %>%
  crop(ext(ice_free.EastAnt)) %>%
  writeRaster(here("Data/Environmental_predictors/Mean_Summer_Temp_EAST_ANTARCTICA.tif"), overwrite = T)


# Wind speed (resample 10km to 1km) --------------------------------------

rast(here("Data/Environmental_predictors/Mean_Annual_Wind_Speed_ALL_YEARS.tif")) %>%
  crop(ext(ice_free.EastAnt)) %>%
  terra::project(ice_free.EastAnt, method = "near") %>% 
  mask(ice_free.EastAnt, maskvalue = NA) %>% 
  writeRaster(here("Data/Environmental_predictors/Mean_Annual_Wind_Speed_ALL_YEARS_EAST_ANTARCTICA.tif"), overwrite = T)



####################
# Inspect covariate distributions and transform ---------------------------
####################

# For the gamba data, the accessibility and the DEM were squareroot transformed to reduce skewness of their distributions, which will also lead to reducing
# the influence in the analysis of very high values. This is to reduce the leverage of these
# observations and to try and obtain the most amount of reliable information from these data.


# Square root or cube root transformation

TWI <- rast(here("Data/Environmental_predictors/topographic_wetness_index_EAST_ANTARCTICA.tif"))
slope <- rast(here("Data/Environmental_predictors/slope_EAST_ANTARCTICA.tif"))
northness <- rast(here("Data/Environmental_predictors/northness_EAST_ANTARCTICA.tif"))
names(northness) <- "northness"

dist_vertebrates <- rast(here("Data/Environmental_predictors/distance_to_vertebrates_EAST_ANTARCTICA.tif"))
names(dist_vertebrates) <- "dist_vertebrates"

dist_seasonal_water <- rast(here("Data/Environmental_predictors/distance_to_seasonal_water_EAST_ANTARCTICA.tif"))
names(dist_seasonal_water) <- "dist_seasonal_water"

summer_temp <- rast(here("Data/Environmental_predictors/Mean_Summer_Temp_EAST_ANTARCTICA.tif"))
names(summer_temp) <- "summer_temp"

wind_speed <- rast(here("Data/Environmental_predictors/Mean_Annual_Wind_Speed_ALL_YEARS_EAST_ANTARCTICA.tif"))
names(wind_speed) <- "wind_speed"

# Bias covariate
dist_station <- rast(here("Data/Environmental_predictors/distance_to_station_EAST_ANTARCTICA.tif"))
names(dist_station) <- "dist_station"

# Stack covariates
covs <- c(TWI, slope, northness, dist_vertebrates, dist_seasonal_water, summer_temp, wind_speed, dist_station)

# Plot histograms
hist(covs, na.rm = T, col = "lightblue")
hist(log(covs+1), na.rm = T, col = "lightblue")
hist(sqrt(covs), na.rm = T, col = "lightblue")

covs_cube <- sign(covs) * abs(covs)^(1/3)
hist(covs_cube, na.rm = T, col = "lightblue")

# Apply some transformations
sqrt_slope <- sqrt(slope)
log_dist_seasonal_water <- log(dist_seasonal_water+1)
log_dist_station <- log(dist_station+1)


####################
# Check for correlation among predictors ---------------------------------
####################

library(corrplot)

covs_df <- as.data.frame(covs, xy = F, na.rm = T)

ecospat::ecospat.cor.plot(covs_df)

# Nicer corr plot
corrplot.mixed(cor(covs_df), order = 'AOE')

usdm::vif(covs)

