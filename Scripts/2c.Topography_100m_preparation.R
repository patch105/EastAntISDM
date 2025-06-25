# HPC version
lib_loc <- paste(getwd(),"/r_lib",sep="")

library(dplyr)
library(purrr)
library(here)
#library(rJava)
library(terra) # Note - I have an HPC terra earlier version not on lib_loc 
library(sf) # Also updated sf so don't need lib.loc=lib_loc

# Read in 100m variables
aspect <- rast("C:/Users/n11222026/Downloads/aspect_100m.tif")
slope <- rast("C:/Users/n11222026/Downloads/slope_100m.tif")
TWI <- rast("C:/Users/n11222026/Downloads/twi_100m.tif")

# Read in the ice-free extent to crop to
ice_free_100m <- rast("C:/Users/n11222026/OneDrive - Queensland University of Technology/Code/Objective_3/AntarcticFutureHabitat/Data/Environmental_predictors/ice_free_union_reproj_100m.tif")

# Match extents of the two rasters
ice_free_100m <- project(ice_free_100m, TWI, method = "bilinear")

# Crop the ice-free area to just East Antarctica
ACBRS <- st_read(here("Data/Environmental_predictors/ACBRs_v2_2016.shp"), crs = 3031) %>% filter(ACBR_Name == "East Antarctica")
ACBRS_SPVE <- vect(ACBRS)
ice_free.EastAnt <- terra::crop(ice_free_100m, ext(ACBRS_SPVE))

# TWI - Crop the topography to ice-free East Antarctica 
TWI <- terra::crop(TWI, ext(ACBRS_SPVE))
TWI <- terra::mask(TWI, ice_free.EastAnt)

vestfold_boundary <- vect(here("Data/Environmental_predictors/vestfold_boundary.shp"))
bunger_boundary <- vect(here("Data/Environmental_predictors/bunger_boundary.shp"))

plot(crop(TWI, ext(vestfold_boundary)))
plot(crop(TWI, ext(bunger_boundary))) 

writeRaster(TWI, here("Data/Environmental_predictors/TWI_100m_IceFree_EastAnt.tif"))

# ASPECT - Crop the topography to ice-free East Antarctica
aspect <- terra::crop(aspect, ext(ACBRS_SPVE))
aspect <- terra::mask(aspect, ice_free.EastAnt)

plot(crop(aspect, ext(vestfold_boundary)))
plot(crop(aspect, ext(bunger_boundary))) 

writeRaster(aspect, here("Data/Environmental_predictors/aspect_100m_IceFree_EastAnt.tif"))

# SLOPE - Crop the topography to ice-free East Antarctica
slope <- terra::crop(slope, ext(ACBRS_SPVE))
slope <- terra::mask(slope, ice_free.EastAnt)

plot(crop(slope, ext(vestfold_boundary)))
plot(crop(slope, ext(bunger_boundary))) 

writeRaster(aspect, here("Data/Environmental_predictors/slope_100m_IceFree_EastAnt.tif"))

# MAKE NORTHNESS
northness <- cos(slope) * sin(aspect)

plot(crop(northness, ext(vestfold_boundary)))
plot(crop(northness, ext(bunger_boundary))) 

writeRaster(aspect, here("Data/Environmental_predictors/northness_100m_IceFree_EastAnt.tif"))


