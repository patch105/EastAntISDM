
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

# Load the ice-free areas of East Antarctica
ice_free.EastAnt <- rast(here("Data/Environmental_predictors/ice_free_union_500m.tif"))

# Load the Antarctic Conservation Biogeographic Regions, filter to East Antarctica
ACBRS <- st_read(here("Data/Environmental_predictors/ACBRs_v2_2016.shp"), crs = 3031) %>% filter(ACBR_Name == "East Antarctica")
ACBRS_SPVE <- vect(ACBRS)


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

writeRaster(dist_station, here("Data/Environmental_predictors/distance_to_station_ICEFREE_500m.tif"), overwrite = T)


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

writeRaster(dist_seasonal_water, here("Data/Environmental_predictors/distance_to_seasonal_water_ICEFREE_500m.tif"), overwrite = T)
plot(dist_seasonal_water)


###################################################
########### Distance to vertebrates ##################
####################################################

# # Load the xy records with just seals
# seals_xy <- read.csv(here("Data/Environmental_predictors/ICEFREE_GBIF_SEAL_XY.csv"), header = T)
# 
# seals_xy <- st_as_sf(seals_xy,
#                      coords = c("x", "y"),
#                      crs = 3031) #set as WGS 1984
# 
# seals_SPVE <- vect(seals_xy) %>%
#   as.polygons()
# 
# # Load the xy records with just seabirds
# seabirds_xy <- read.csv(here("Data/Environmental_predictors/ICEFREE_GBIF_SEABIRD_XY.csv"), header = T)
# 
# seabirds_xy <- st_as_sf(seabirds_xy,
#                         coords = c("x", "y"),
#                         crs = 3031) #set as WGS 1984
# 
# seabirds_SPVE <- vect(seabirds_xy) %>%
#   as.polygons()
# 
# # Load the penguin colony locations
# penguins_xy <- st_read(here("Data/Environmental_predictors/penguin_rookeries.trim.shp"), crs = 3031)
# 
# penguin_colony_SPVE <- vect(penguins_xy)
# 
# # Combine them together
# 
# vertebrates <- rbind(penguin_colony_SPVE, seals_SPVE, seabirds_SPVE)
# 
# dist_vertebrates <- terra::distance(ice_free.EastAnt, vertebrates)
# dist_vertebrates <- mask(dist_vertebrates, ice_free.EastAnt, maskvalue = NA)
# 
# writeRaster(dist_vertebrates, here("Data/Environmental_predictors/distance_to_vertebrates_ICEFREE_100m.tif"), overwrite = T)


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
writeRaster(summer_temp, here("Data/Environmental_predictors/mean_summer_temp_AntAirIce_500m.tif"), overwrite = T)


###################################################
########### Wind speed ##################
####################################################

wind <- rast(here("Data/Environmental_predictors/Mean_Annual_Wind_Speed_ALL_YEARS.tif"))

wind <- crop(wind, ext(ice_free.EastAnt))

wind <- terra::project(wind, ice_free.EastAnt, method = "near")
wind <- mask(wind, ice_free.EastAnt, maskvalue = NA)
writeRaster(wind, here("Data/Environmental_predictors/Mean_Annual_Wind_Speed_ALL_YEARS_500m.tif"), overwrite = T)

wind2 <- rast(here("Data/Environmental_predictors/mean_wind_bm.tif"))
wind2 <- project(wind2, "EPSG:3031")
wind2 <- crop(wind2, ext(ice_free.EastAnt))

wind2 <- terra::project(wind2, ice_free.EastAnt, method = "near")
wind2 <- mask(wind2, ice_free.EastAnt, maskvalue = NA)
writeRaster(wind2, here("Data/Environmental_predictors/AMPS_Mean_Annual_Wind_Speed_500m.tif"), overwrite = T)



####################
# Inspect covariate distributions and transform ---------------------------
####################

# Load ice-free land 500m -------------------------------------------------

ice_free.EastAnt <- rast(here("Data/Environmental_predictors/ice_free_union_EastAnt_500m.tif"))


# Load covariates for all of East Antarctica ------------------------------

TWI <- rast(here("Data/Environmental_predictors/TWI_500m_IceFree_EastAnt.tif"))
names(TWI) <- "TWI"

slope <- rast(here("Data/Environmental_predictors/slope_500m_IceFree_EastAnt.tif"))
names(slope) <- "Slope"

northness <- rast(here("Data/Environmental_predictors/northness_500m_IceFree_EastAnt.tif"))
names(northness) <- "Northness"

# dist_seasonal_water <- rast(here("Data/Environmental_predictors/distance_to_seasonal_water_ICEFREE_500m.tif"))
# names(dist_seasonal_water) <- "dist_seasonal_water"

summer_temp <- rast(here("Data/Environmental_predictors/mean_summer_temp_AntAirIce_500m.tif"))
names(summer_temp) <- "Summer temp."

wind_speed <- rast(here("Data/Environmental_predictors/AMPS_Mean_Annual_Wind_Speed_500m.tif"))
names(wind_speed) <- "Wind speed"

# Bias covariate
dist_station <- rast(here("Data/Environmental_predictors/distance_to_station_ICEFREE_500m.tif"))
names(dist_station) <- "Dist. to Station"

# Stack covariates
covs <- c(TWI, slope, northness, summer_temp, wind_speed, dist_station)

# # Plot histograms
# hist(covs, na.rm = T, col = "lightblue")
# hist(log(covs), na.rm = T, col = "lightblue")
# hist(sqrt(covs), na.rm = T, col = "lightblue")
# 
# covs_cube <- sign(covs) * abs(covs)^(1/3)
# hist(covs_cube, na.rm = T, col = "lightblue")
# 
# # Apply some transformations
# sqrt_slope <- sqrt(slope)
# log_dist_seasonal_water <- log(dist_seasonal_water+1)
# log_dist_station <- log(dist_station+1)
# log_dist_vertebrates <- log(dist_vertebrates+1)
# 
# 

####################
# Check for correlation among predictors ---------------------------------
####################

library(corrplot)

covs_df <- as.data.frame(covs, xy = F, na.rm = T)

# Save the correlation plot

# plot <- ecospat::ecospat.cor.plot(covs_df)

M <- cor(covs_df, use = "pairwise.complete.obs")

# Nicer corr plot

png((here("Outputs/Results/Covariate_correlation_plot.png")), width = 900, height = 600)

print(corrplot.mixed(M, order = 'AOE'))

dev.off()

usdm::vif(covs)



# PLOTTING ENVIRONMENTAL COVARIATES ---------------------------------------

covs <- c(TWI, sqrt(slope), northness, summer_temp, wind_speed, log(dist_station+1))
names(covs) <- c("TWI", "Slope", "Northness", "Summer_temp", "Wind_speed", "Dist_to_station")


# Load boundaries
vestfold_boundary <- vect(here("Data/Environmental_predictors/vestfold_boundary.shp"))
bunger_boundary <- vect(here("Data/Environmental_predictors/bunger_boundary.shp"))

P1 <- covs %>%
  crop(ext(bunger_boundary)) %>% 
  as.data.frame(xy = TRUE, na.rm = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = TWI)) +
  scale_fill_viridis() +
  coord_fixed() +
  theme_bw() +
  labs(title = "Topographic Wetness Index") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank())

P2 <- covs %>%
  crop(ext(bunger_boundary)) %>% 
  as.data.frame(xy = TRUE, na.rm = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Slope)) +
  scale_fill_viridis() +
  coord_fixed() +
  theme_bw() +
  labs(title = "Slope (sqrt)") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank())


P3 <- covs %>%
  crop(ext(bunger_boundary)) %>% 
  as.data.frame(xy = TRUE, na.rm = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Northness)) +
  scale_fill_viridis() +
  coord_fixed() +
  theme_bw() +
  labs(title = "Northness") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank())


P4 <- covs %>%
  crop(ext(bunger_boundary)) %>% 
  as.data.frame(xy = TRUE, na.rm = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Summer_temp)) +
  scale_fill_viridis() +
  coord_fixed() +
  theme_bw() +
  labs(title = "Mean Summer Temperature") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank())


P5 <- covs %>%
  crop(ext(bunger_boundary)) %>% 
  as.data.frame(xy = TRUE, na.rm = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Wind_speed)) +
  scale_fill_viridis() +
  coord_fixed() +
  theme_bw() +
  labs(title = "Mean Wind Speed") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank())


P6 <- covs %>%
  crop(ext(bunger_boundary)) %>% 
  as.data.frame(xy = TRUE, na.rm = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Dist_to_station)) +
  scale_fill_viridis() +
  coord_fixed() +
  theme_bw() +
  labs(title = "Distance to station (log)") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank())


# Make the combined figure ------------------------------------------------

# Arrange Lichen row
plot <- ggarrange(P1, P2, P3, P4, P5, P6, 
                  ncol = 2, nrow = 3, align = "hv")

# Save the combined figure ------------------------------------------------

ggsave(here("Outputs/Figures", "Covariates_BUNGER.png"),
       plot = plot,
       width = 17, height = 17, units = "cm", dpi = 600)


# VESTFOLD ----------------------------------------------------------------

P1 <- covs %>%
  crop(ext(vestfold_boundary)) %>% 
  as.data.frame(xy = TRUE, na.rm = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = TWI)) +
  scale_fill_viridis() +
  coord_fixed() +
  theme_bw() +
  labs(title = "Topographic Wetness Index") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank())

P2 <- covs %>%
  crop(ext(vestfold_boundary)) %>% 
  as.data.frame(xy = TRUE, na.rm = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Slope)) +
  scale_fill_viridis() +
  coord_fixed() +
  theme_bw() +
  labs(title = "Slope (sqrt)") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank())


P3 <- covs %>%
  crop(ext(vestfold_boundary)) %>% 
  as.data.frame(xy = TRUE, na.rm = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Northness)) +
  scale_fill_viridis() +
  coord_fixed() +
  theme_bw() +
  labs(title = "Northness") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank())


P4 <- covs %>%
  crop(ext(vestfold_boundary)) %>% 
  as.data.frame(xy = TRUE, na.rm = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Summer_temp)) +
  scale_fill_viridis() +
  coord_fixed() +
  theme_bw() +
  labs(title = "Mean Summer Temperature") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank())


P5 <- covs %>%
  crop(ext(vestfold_boundary)) %>% 
  as.data.frame(xy = TRUE, na.rm = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Wind_speed)) +
  scale_fill_viridis() +
  coord_fixed() +
  theme_bw() +
  labs(title = "Mean Wind Speed") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank())


P6 <- covs %>%
  crop(ext(vestfold_boundary)) %>% 
  as.data.frame(xy = TRUE, na.rm = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Dist_to_station)) +
  scale_fill_viridis() +
  coord_fixed() +
  theme_bw() +
  labs(title = "Distance to station (log)") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank())


# Make the combined figure ------------------------------------------------

# Arrange Lichen row
plot <- ggarrange(P1, P2, P3, P4, P5, P6, 
                  ncol = 2, nrow = 3, align = "hv")

# Save the combined figure ------------------------------------------------

ggsave(here("Outputs/Figures", "Covariates_VESTFOLD.png"),
       plot = plot,
       width = 17, height = 17, units = "cm", dpi = 600)






