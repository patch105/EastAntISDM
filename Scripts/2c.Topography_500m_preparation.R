
library(dplyr)
library(purrr)
library(here)
library(terra) 
library(sf) 

# Read in 100m variables
aspect <- rast("C:/Users/n11222026/Downloads/aspect_100m.tif")
slope <- rast("C:/Users/n11222026/Downloads/slope_100m.tif")
TWI <- rast("C:/Users/n11222026/Downloads/twi_100m.tif")

# Upsample to 500m resolution
aspect <- aggregate(aspect, fact = 5, fun = mean, na.rm = TRUE)
slope <- aggregate(slope, fact = 5, fun = mean, na.rm = TRUE)
TWI <- aggregate(TWI, fact = 5, fun = mean, na.rm = TRUE)

# Read in the ice-free extent to crop to and upsample to match the 500m resolution
ice_free_100m <- rast("C:/Users/n11222026/OneDrive - Queensland University of Technology/Code/Objective_3/AntarcticFutureHabitat/Data/Environmental_predictors/ice_free_union_reproj_100m.tif")

ice_free_500m <- aggregate(ice_free_100m, fact = 5, fun = mean, na.rm = TRUE)

# # Now using our 500m topography as the template for our domain dimensions (because we don't want to resample these topographic layers if we don't need to), we'll resample our ice-free area layer to match the dimensions of the topography rasters
ice_free_500m <- resample(ice_free_500m, TWI, method = "bilinear")

# writeRaster(ice_free_500m, here("Data/Environmental_predictors/ice_free_union_500m.tif"))

# Crop the ice-free area to just East Antarctica
ACBRS <- st_read(here("Data/Environmental_predictors/ACBRs_v2_2016.shp"), crs = 3031) %>% filter(ACBR_Name == "East Antarctica")
ACBRS_SPVE <- vect(ACBRS)

ice_free.EastAnt <- terra::crop(ice_free_500m, ext(ACBRS_SPVE))

# writeRaster(ice_free.EastAnt, "Data/Environmental_predictors/ice_free_union_EastAnt_500m.tif", overwrite = TRUE)

# TWI - Crop the topography to ice-free East Antarctica 
TWI <- terra::crop(TWI, ext(ACBRS_SPVE))
TWI <- terra::mask(TWI, ice_free.EastAnt)

vestfold_boundary <- vect(here("Data/Environmental_predictors/vestfold_boundary.shp"))
bunger_boundary <- vect(here("Data/Environmental_predictors/bunger_boundary.shp"))

plot(crop(TWI, ext(vestfold_boundary)))
plot(crop(TWI, ext(bunger_boundary))) 

writeRaster(TWI, here("Data/Environmental_predictors/TWI_500m_IceFree_EastAnt.tif"), overwrite = T)

rm(TWI)

# ASPECT - Crop the topography to ice-free East Antarctica
aspect <- terra::crop(aspect, ext(ACBRS_SPVE))
aspect <- terra::mask(aspect, ice_free.EastAnt)

plot(crop(aspect, ext(vestfold_boundary)))
plot(crop(aspect, ext(bunger_boundary))) 

writeRaster(aspect, here("Data/Environmental_predictors/aspect_500m_IceFree_EastAnt.tif"), overwrite = T)

# SLOPE - Crop the topography to ice-free East Antarctica
slope <- terra::crop(slope, ext(ACBRS_SPVE))
slope <- terra::mask(slope, ice_free.EastAnt)

plot(crop(slope, ext(vestfold_boundary)))
plot(crop(slope, ext(bunger_boundary))) 

writeRaster(slope, here("Data/Environmental_predictors/slope_500m_IceFree_EastAnt.tif"), overwrite = T)

# MAKE NORTHNESS
northness <- cos(slope) * sin(aspect)

plot(crop(northness, ext(vestfold_boundary)))
plot(crop(northness, ext(bunger_boundary))) 

writeRaster(northness, here("Data/Environmental_predictors/northness_500m_IceFree_EastAnt.tif"), overwrite = T)



