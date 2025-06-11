
#############################################################
########## 2b. Environmental differences between sites ##########
#############################################################

# This code tests the environmental coverage of the Vestfold and Bunger Hills sites and looks at how different types of data represent the environmental conditions at the sites.


# Load packages -----------------------------------------------------------

library(purrr)

packages <- c("here", "sf", "terra", "dplyr", "tidyr", "viridis", "ggplot2", "tidyterra", "flexsdm")

walk(packages, require, character.only = T)

here::here()


# Load covariates for all of East Antarctica ------------------------------

TWI <- rast(here("Data/Environmental_predictors/topographic_wetness_index_EAST_ANTARCTICA.tif"))
slope <- rast(here("Data/Environmental_predictors/slope_EAST_ANTARCTICA.tif"))
northness <- rast(here("Data/Environmental_predictors/northness_EAST_ANTARCTICA.tif"))
names(northness) <- "northness"

dist_vertebrates <- rast(here("Data/Environmental_predictors/distance_to_vertebrates_EAST_ANTARCTICA.tif"))
names(dist_vertebrates) <- "dist_vertebrates"

dist_seasonal_water <- rast(here("Data/Environmental_predictors/distance_to_seasonal_water_EAST_ANTARCTICA.tif"))
names(dist_seasonal_water) <- "dist_seasonal_water"

# Bias covariate
dist_station <- rast(here("Data/Environmental_predictors/distance_to_station_EAST_ANTARCTICA.tif"))
names(dist_station) <- "dist_station"


# TEMPORARY - TEMPERATURE & WIND ------------------------------------------

# ice_free <- rast(here("Data/Environmental_predictors/ice_free_union_reproj_100m.tif"))
ice_free <- rast(here("Data/Environmental_predictors/ice_free_upsamp_1km.tif"))

# Load the Antarctic Conservation Biogeographic Regions, filter to East Antarctica
ACBRS <- st_read(here("Data/Environmental_predictors/ACBRs_v2_2016.shp"), crs = 3031) %>% filter(ACBR_Name == "East Antarctica")
ACBRS_SPVE <- vect(ACBRS)


# Also trim ice-free land to just East Antarctica
ice_free.EastAnt <- terra::crop(ice_free, ext(ACBRS_SPVE))

ice_free.EastAnt <- ifel(not.na(ice_free.EastAnt), 1, NA)

summer_temp <- rast(here("Data/Environmental_predictors/Mean_Summer_Temp_ICEFREE.tif"))
summer_temp <- crop(summer_temp, ice_free.EastAnt) # Crop to ice-free land
names(summer_temp) <- "summer_temp"

wind <- rast(here("Data/Environmental_predictors/mean_wind_bm.tif"))
wind <- crop(wind, ice_free.EastAnt) 
names(wind) <- "wind"
wind <- resample(wind, ice_free.EastAnt, method = "bilinear") # Resample to match summer_temp resolution
wind <- mask(wind, ice_free.EastAnt) # Mask to ice-free land

# Stack covariates
EastAnt <- c(TWI, slope, northness, dist_vertebrates, dist_seasonal_water, dist_station, summer_temp, wind)


# Plotting environmental conditions at sites ------------------------------

vestfold_boundary <- vect(here("Data/Environmental_predictors/vestfold_boundary.shp"))
bunger_boundary <- vect(here("Data/Environmental_predictors/bunger_boundary.shp"))

bunger <- crop(EastAnt, bunger_boundary)
vestfold <- crop(EastAnt, vestfold_boundary)

EastAnt.df <- as.data.frame(EastAnt, xy = FALSE, na.rm = TRUE) %>% 
  mutate(Region = "East Antarctica")

bunger.df <- as.data.frame(bunger, xy = FALSE, na.rm = TRUE) %>% 
  mutate(Region = "Bunger Hills")

vestfold.df <- as.data.frame(vestfold, xy = FALSE, na.rm = TRUE) %>% 
  mutate(Region = "Vestfold Hills")

all <- rbind(EastAnt.df, bunger.df, vestfold.df)

all$Region <- factor(all$Region, levels = c("Bunger Hills", "Vestfold Hills", "East Antarctica"))


library(viridisLite)

cov_names <- names(EastAnt)

for(i in seq_along(cov_names)) {
  
  xmax <- quantile(all[, i], 0.95, na.rm = TRUE)
  
 plot <- ggplot(all, aes(x = .data[[names(all)[[i]]]], y = Region, fill = Region)) +
    geom_density_ridges(scale = 4, alpha = 0.8) +
    scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
    scale_x_continuous(expand = c(0, 0)) +   # for both axes to remove unneeded padding
      coord_cartesian(xlim = c(NA, xmax)) + # to avoid clipping of the very top of the top ridgeline
    scale_fill_manual(values = c(
      "East Antarctica" = "#e34a33",  # deep orange-red
      "Vestfold Hills" = "#fc8d59",   # warm coral-orange
      "Bunger Hills" = "#fdbb84"      # soft peachy-yellow
    )) +
    theme_classic()+
    labs(x = names(all)[[i]], y = NULL, title = paste0("Distribution of ", names(all)[[i]], " Across East Antarctic Regions"))
  
  ggsave(filename = here("Outputs/Figures", paste0("Distribution_of_", names(all)[[i]], "_Across_East_Antarctic_Regions.png")),
         plot = plot,
         width = 8, height = 4, dpi = 300, units = "in")
  
}



# Now adding in the information about where the records are found ---------





# Extrapolation (between SITES) ---------------------------------------------

# Adding presence column due to extra_eval requirements
# Trimming so just the covariates
training <- vestfold.xy.df %>% 
  mutate(Presence = 1) %>% 
  select(-Region)

projection <- bunger.xy.df %>% 
  select(-Region)

## NOTE - TAKES A WHILE
shape_extrap <- extra_eval(training_data = training,
                           pr_ab = "Presence",
                           projection_data = projection,
                           metric = "mahalanobis",
                           univar_comb = T,
                           n_cores = 3)

bunger.xy.df <- as.data.frame(bunger, xy = TRUE, na.rm = TRUE) 

vestfold.xy.df <- as.data.frame(vestfold, xy = TRUE, na.rm = TRUE)

shape_extrap <- cbind(shape_extrap, bunger.xy.df[, c("x", "y")])

shape_extrap %>% 
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = extrapolation)) + 
  scale_fill_viridis() +
  coord_fixed() + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('Extrapolation - Vestfold to Bunger')


### Plotting data in covariate space with extrapolation


ggplot() + 
  geom_point(data = bunger.xy.df, aes(x = twi, y = slope), color = "grey") +
  geom_point(data = shape_extrap, aes(x = twi, y = slope, color = extrapolation)) +
  scale_color_viridis(option = "magma", direction = -1) +
  theme_bw() +
  theme(legend.ticks = element_blank()) +
  ggtitle('Extrapolation Bunger to Vestfold - elev and TWI')

ggplot() + 
  geom_point(data = bunger.xy.df, aes(x = TWI, y = wind), color = "grey") +
  geom_point(data = shape_extrap, aes(x = TWI, y = wind, color = extrapolation)) +
  scale_color_viridis(option = "magma", direction = -1) +
  theme_bw() +
  theme(legend.ticks = element_blank()) +
  ggtitle('Extrapolation Bunger to Vestfold - TWI and wind')

ggplot() +
  geom_point(data = bunger.xy.df, aes(x = wind, y = summer_temp), color = "grey") +
  geom_point(data = shape_extrap, aes(x = wind, y = summer_temp, color = extrapolation)) +
  scale_color_viridis(option = "magma", direction = -1) +
  theme_bw() +
  theme(legend.ticks = element_blank()) +
  ggtitle('Extrapolation Bunger to Vestfold - wind and summer_temp')

ggplot() +
  geom_point(data = bunger.xy.df, aes(x = summer_temp, y = northness), color = "grey") +
  geom_point(data = shape_extrap, aes(x = summer_temp, y = northness, color = extrapolation)) +
  scale_color_viridis(option = "magma", direction = -1) +
  theme_bw() +
  theme(legend.ticks = element_blank()) +
  ggtitle('Extrapolation Bunger to Vestfold - summer_temp and northness')

ggplot() +
  geom_point(data = bunger.xy.df, aes(x = elev, y = wind), color = "grey") +
  geom_point(data = shape_extrap, aes(x = elev, y = wind, color = extrapolation)) +
  scale_color_viridis(option = "magma", direction = -1) +
  theme_bw() +
  theme(legend.ticks = element_blank()) +
  ggtitle('Extrapolation Bunger to Vestfold - elev and wind')



