
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


# Aspect (northness)?
# Soil conductivity
# Wind 
# Slope
# Rugosity
# Elevation

# Mean summer precip
# Snow cover
# Dist. to seasonal meltwater
# Melt days
# Degree days -5
# Solar radiation
# Summer temp
# Dist to birds



# Load the 100 m topographic variables ------------------------------------

# Source: https://data.pgc.umn.edu/elev/dem/setsm/REMA/mosaic/latest/100m/
# Original file is not in the EPSG 3031

# # Unzip file
# untar("C:/Users/n11222026/rema_mosaic_100m_v2.0_filled_cop30.tar.gz", list = TRUE)
# untar("C:/Users/n11222026/rema_mosaic_100m_v2.0_filled_cop30.tar.gz", exdir = here("Data"))

# Reproject to EPSG 3031
# library(gdalUtilities)
# rema <- gdalwarp(srcfile = here("Data/rema_mosaic_100m_v2.0_filled_cop30_dem.tif"),
#                  dstfile = here("Data/rema_REPROJ_mosaic_100m_v2.0_filled_cop30_dem.tif"),
#                  r = "bilinear",
#                  t_srs = 'EPSG:3031',
#                  overwrite = T,
#                  tr = c(100, 100))

# Load reprojected file
# rema <- terra::rast(here("Data/rema_REPROJ_mosaic_100m_v2.0_filled_cop30_dem.tif"))

# Load original file
# rema <- terra::rast(here("Data/rema_mosaic_100m_v2.0_filled_cop30_dem.tif"))



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



# # Downscaling PolarRes variable (11 km to 1 km) ---------------------------
# 
# # Temperature is in K
# summer_temp <- rast(here("Data/Environmental_predictors/Mean_Summer_Temperature_2001_2018.tif"))
# summer_temp <- summer_temp - 273.15 # Convert to Celsius
# 
# names(summer_temp) <- "summer_temp"
# 
# # Make a buffered ice_free.EastAnt layer
# ice_free.EastAnt_buffer <- terra::buffer(ice_free.EastAnt, width = 10000) 
# ice_free.EastAnt_buffer <- terra::ifel(ice_free.EastAnt_buffer, 1, NA)
# 
# # Downscaling based on elevation, slope, and northness (relates to solar radiation)
# elev_buffered <- rast(here("Data/Environmental_predictors/elev_upscaled_new_rockout_merged.tif")) %>% 
#   crop(ext(ice_free.EastAnt_buffer)) 
#   # resample(ice_free.EastAnt_buffer, method = "bilinear") 
#   #mask(ice_free.EastAnt_buffer, maskvalue = NA)
#   
# slope_buffered <- rast(here("Data/Environmental_predictors/slope_upscaled_new_rockout_merged.tif")) %>% 
#   crop(ext(ice_free.EastAnt_buffer)) 
#   # resample(ice_free.EastAnt_buffer, method = "bilinear") 
#   #mask(ice_free.EastAnt_buffer, maskvalue = NA) 
#   
# northness_buffered <- rast(here("Data/Environmental_predictors/northness_upscaled_new_rockout_merged.tif")) %>%
#   crop(ext(ice_free.EastAnt_buffer)) 
#   # resample(ice_free.EastAnt_buffer, method = "bilinear")
#   #mask(ice_free.EastAnt_buffer, maskvalue = NA)
#  
# summer_temp <- crop(summer_temp, ext(ice_free.EastAnt_buffer))
# 
# # Combine predictors for spline into raster stack -------------------------
# 
# predictors <- c(elev_buffered, slope_buffered, northness_buffered)
# names(predictors) <- c("elev", "slope", "northness")
# 
# predictors_10km <- resample(predictors, summer_temp, method = "bilinear")
# 
# # Create single stack and propagate NA values
# stack <- c(summer_temp, predictors_10km)
# stack <- ENMTools::check.env(stack)
# 
# 
# # Probably need to go back and crop to coast or ice-free land -------------
# 
# ## Create data frame of values for GAM
# 
# coarse_field <- data.frame(long = crds(stack)[,1],
#                            lat = crds(stack)[,2],
#                            summer_temp = as.vector(values(stack["summer_temp"], na.rm = T)),
#                            elev = as.vector(values(stack["elev"], na.rm = T)),
#                            slope = as.vector(values(stack["slope"], na.rm = T)),
#                            northness = as.vector(values(stack["northness"], na.rm = T)))
# 
# 
# ## Plot histogram of summer temperature
# 
# hist(coarse_field$summer_temp)
# 
# ## Fit a thin plate regression spline model
# 
# mod_list <- list()
# # 
# # nms <- names(coarse_field[,3]) # Names of ERA5-Land variables
# 
# library(mgcv)
# 
# # Just for when running just wind
# name <- "summer_temp"
# 
# for(name in nms){
#   
#   kk <- 1000
#   runtime <- system.time(tps <- gam(coarse_field[[name]] ~ elev + northness + slope +
#                                       s(long, lat, bs="tp", k = kk),
#                                     method = "REML",
#                                     data = coarse_field,
#                                     family = gaussian()))
#   mod_list[[name]] <- tps
#   
# }
# 
# # NOW RUNNING WITH A GAMMA DISTRIBUTION
# runtime <- system.time(tps <- gam(coarse_field$w.speed ~ elev + aspect + slope +
#                                     s(long, lat, bs="tp", k = kk),
#                                   method = "REML",
#                                   data = coarse_field,
#                                   family = Gamma(link = "log")))
# 
# 




