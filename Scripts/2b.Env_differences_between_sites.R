
#############################################################
########## 2b. Environmental differences between sites ##########
#############################################################

# This code tests the environmental coverage of the Vestfold and Bunger Hills sites and looks at how different types of data represent the environmental conditions at the sites.


# Load packages -----------------------------------------------------------

library(purrr)

packages <- c("here", "sf", "terra", "dplyr", "tidyr", "viridis", "ggplot2", "tidyterra", "flexsdm", "viridisLite", "ggridges")

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

# Load the # Load the presence-only records 

PO_lichen_vect <- vect(here("Data/Biological_records", "PO_Veg_East_Ant.shp")) %>% 
  filter(vegtype == "Lichen")

PO_moss_vect <- vect(here("Data/Biological_records", "PO_Veg_East_Ant.shp")) %>% 
  filter(vegtype == "Moss")

PO_lichen_df <- terra::extract(EastAnt, PO_lichen_vect, xy = FALSE, na.rm = TRUE) %>% 
  select(-ID) %>% 
  mutate(Region = "PO Lichen")

PO_moss_df <- terra::extract(EastAnt, PO_moss_vect, xy = FALSE, na.rm = TRUE) %>% 
  select(-ID) %>% 
  mutate(Region = "PO Moss")


# Load the presence-absence records 

PA_Vestfold_vect <- st_read(here("Data/Biological_records", "PA_Veg_vestfold.shp"))

PA_Vestfold_df <- terra::extract(EastAnt, PA_Vestfold_vect, xy = FALSE, na.rm = TRUE) %>% 
  select(-ID) %>% 
  mutate(Region = "PA Vestfold")



# PLOTTING comparisong PO vs. PA vs. Bunger -------------------------------

all_lichen <- rbind(bunger.df, PO_lichen_df, PA_Vestfold_df)

all_lichen$Region <- factor(all_lichen$Region, levels = c("PO Lichen", "PA Vestfold", "Bunger Hills"))

cov_names <- names(EastAnt)

for(i in seq_along(cov_names)) {
  
  xmax <- quantile(all_lichen[, i], 0.95, na.rm = TRUE)
  
  plot <- ggplot(all_lichen, aes(x = .data[[names(all_lichen)[[i]]]], y = Region, fill = Region)) +
    geom_density_ridges(scale = 4, alpha = 0.8) +
    scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
    scale_x_continuous(expand = c(0, 0)) +   # for both axes to remove unneeded padding
    coord_cartesian(xlim = c(NA, xmax)) + # to avoid clipping of the very top of the top ridgeline
    scale_fill_manual(values = c(
      "Bunger Hills" = "orchid3",  # deep orange-red
      "PO Lichen" = "palegreen3",   # warm coral-orange
      "PA Vestfold" = "palegreen"      # soft peachy-yellow
    )) +
    theme_classic()+
    labs(x = names(all_lichen)[[i]], y = NULL, title = paste0("Distribution of ", names(all_lichen)[[i]], " Across Lichen PO & PA data vs. Bunger Hills"))
  
  ggsave(filename = here("Outputs/Figures", paste0("Distribution_of_", names(all_lichen)[[i]], "_Across_PO_PA_Lichen_vs_Bunger.png")),
         plot = plot,
         width = 8, height = 4, dpi = 300, units = "in")
  
}

all_moss <- rbind(bunger.df, PO_moss_df, PA_Vestfold_df)

all_moss$Region <- factor(all_moss$Region, levels = c("PO Moss", "PA Vestfold", "Bunger Hills"))

cov_names <- names(EastAnt)

for(i in seq_along(cov_names)) {
  
  xmax <- quantile(all_moss[, i], 0.97, na.rm = TRUE)
  
  plot <- ggplot(all_moss, aes(x = .data[[names(all_moss)[[i]]]], y = Region, fill = Region)) +
    geom_density_ridges(scale = 4, alpha = 0.8) +
    scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
    scale_x_continuous(expand = c(0, 0)) +   # for both axes to remove unneeded padding
    coord_cartesian(xlim = c(NA, xmax)) + # to avoid clipping of the very top of the top ridgeline
    scale_fill_manual(values = c(
      "Bunger Hills" = "orchid3",  # deep orange-red
      "PO Moss" = "palegreen3",   # warm coral-orange
      "PA Vestfold" = "palegreen"      # soft peachy-yellow
    )) +
    theme_classic()+
    labs(x = names(all_moss)[[i]], y = NULL, title = paste0("Distribution of ", names(all_moss)[[i]], " Across Moss PO & PA data vs. Bunger Hills"))
  
  ggsave(filename = here("Outputs/Figures", paste0("Distribution_of_", names(all_moss)[[i]], "_Across_PO_PA_Moss_vs_Bunger.png")),
         plot = plot,
         width = 8, height = 4, dpi = 300, units = "in")
  
}

# MULTI-PANEL PLOTS FOR ALL COVARIATES (to update later) ----------------------

plot_list <- list()

for (i in seq_along(cov_names)) {
  
  var_name <- names(all_moss)[[i]]
  xmax <- quantile(all_moss[[i]], 0.97, na.rm = TRUE)
  
  p <- ggplot(all_moss, aes(x = .data[[var_name]], y = Region, fill = Region)) +
    geom_density_ridges(scale = 4, alpha = 0.8) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    coord_cartesian(xlim = c(NA, xmax)) +
    scale_fill_manual(values = c(
      "Bunger Hills" = "orchid3",
      "PO Moss" = "palegreen3",
      "PA Vestfold" = "palegreen"
    )) +
    theme_classic() +
    labs(x = var_name, y = NULL,
         title = paste0("Distribution of ", var_name, " Across Moss PO & PA data vs. Bunger Hills"))
  
  plot_list[[i]] <- p
}

# Arrange all plots into a multi-panel figure
combined_plot <- ggarrange(plotlist = plot_list,
                           ncol = 2, nrow = ceiling(length(plot_list) / 2),
                           common.legend = TRUE, legend = "bottom")

# Save combined plot
ggsave(here("Outputs/Figures", "Combined_Distribution_Plots.png"),
       plot = combined_plot,
       width = 12, height = 6 + 3 * ceiling(length(plot_list) / 2), dpi = 300)

###########################################################
# Extrapolation (between PO / PA DATA & BUNGER) -------------------------------------
##########################################################

# Adding presence column due to extra_eval requirements
# Trimming so just the covariates
projection <- bunger.df %>% 
  select(-Region)

training_PO_lichen <- PO_lichen_df %>% 
  mutate(Presence = 1) %>% 
  select(-Region)

training_PO_moss <- PO_moss_df %>% 
  mutate(Presence = 1) %>% 
  select(-Region)

training_PA_Vestfold <- PA_Vestfold_df %>%
  mutate(Presence = 1) %>% 
  select(-Region)

training_integrated_lichen <- rbind(training_PO_lichen, training_PA_Vestfold)
training_integrated_moss <- rbind(training_PO_moss, training_PA_Vestfold)

## NOTE - TAKES A WHILE
shape_extrap_PO_lichen <- extra_eval(training_data = training_PO_lichen,
                                     pr_ab = "Presence",
                                     projection_data = projection,
                                     metric = "mahalanobis",
                                     univar_comb = T,
                                     n_cores = 3)

shape_extrap_PO_moss <- extra_eval(training_data = training_PO_moss,
                                     pr_ab = "Presence",
                                     projection_data = projection,
                                     metric = "mahalanobis",
                                     univar_comb = T,
                                     n_cores = 3)

shape_extrap_PA_Vestfold <- extra_eval(training_data = training_PA_Vestfold,
                                       pr_ab = "Presence",
                                       projection_data = projection,
                                       metric = "mahalanobis",
                                       univar_comb = T,
                                       n_cores = 3)

shape_extrap_PA_Vestfold <- extra_eval(training_data = training_PA_Vestfold,
                                       pr_ab = "Presence",
                                       projection_data = projection,
                                       metric = "mahalanobis",
                                       univar_comb = T,
                                       n_cores = 3)

shape_extrap_integrated_lichen <- extra_eval(training_data = training_integrated_lichen,
                                             pr_ab = "Presence",
                                             projection_data = projection,
                                             metric = "mahalanobis",
                                             univar_comb = T,
                                             n_cores = 3)

shape_extrap_integrated_moss <- extra_eval(training_data = training_integrated_moss,
                                           pr_ab = "Presence",
                                           projection_data = projection,
                                           metric = "mahalanobis",
                                           univar_comb = T,
                                           n_cores = 3)

# Summarising Extrapolation per data type --------------------------------

shape_extrap_PO_lichen_summary <- data.frame(data_type = "PO Lichen",
                                             mean = mean(shape_extrap_PO_lichen$extrapolation, na.rm = TRUE),
                                             median = median(shape_extrap_PO_lichen$extrapolation, na.rm = TRUE),
                                             min = min(shape_extrap_PO_lichen$extrapolation, na.rm = TRUE),
                                             max = max(shape_extrap_PO_lichen$extrapolation, na.rm = TRUE),
                                             proportion_combinatorial = sum(shape_extrap_PO_lichen$univar_comb == 2)/ length(shape_extrap_PO_lichen$univar_comb))

shape_extrap_PO_moss_summary <- data.frame(data_type = "PO Moss",
                                           mean = mean(shape_extrap_PO_moss$extrapolation, na.rm = TRUE),
                                           median = median(shape_extrap_PO_moss$extrapolation, na.rm = TRUE),
                                           min = min(shape_extrap_PO_moss$extrapolation, na.rm = TRUE),
                                           max = max(shape_extrap_PO_moss$extrapolation, na.rm = TRUE),
                                           proportion_combinatorial = sum(shape_extrap_PO_moss$univar_comb == 2)/ length(shape_extrap_PO_moss$univar_comb))
                                           

shape_extrap_PA_Vestfold_summary <- data.frame(data_type = "PA Vestfold", 
                                               mean = mean(shape_extrap_PA_Vestfold$extrapolation, na.rm = TRUE),
                                               median = median(shape_extrap_PA_Vestfold$extrapolation, na.rm = TRUE),
                                               min = min(shape_extrap_PA_Vestfold$extrapolation, na.rm = TRUE),
                                               max = max(shape_extrap_PA_Vestfold$extrapolation, na.rm = TRUE),
                                               proportion_combinatorial = sum(shape_extrap_PA_Vestfold$univar_comb == 2)/ length(shape_extrap_PA_Vestfold$univar_comb))
                                               

shape_extrap_integrated_lichen_summary <- data.frame(data_type = "Integrated Lichen", 
                                               mean = mean(shape_extrap_integrated_lichen$extrapolation, na.rm = TRUE),
                                               median = median(shape_extrap_integrated_lichen$extrapolation, na.rm = TRUE),
                                               min = min(shape_extrap_integrated_lichen$extrapolation, na.rm = TRUE),
                                               max = max(shape_extrap_integrated_lichen$extrapolation, na.rm = TRUE),
                                               proportion_combinatorial = sum(shape_extrap_integrated_lichen$univar_comb == 2)/ length(shape_extrap_integrated_lichen$univar_comb))


shape_extrap_integrated_moss_summary <- data.frame(data_type = "Integrated Moss",
                                               mean = mean(shape_extrap_integrated_moss$extrapolation, na.rm = TRUE),
                                               median = median(shape_extrap_integrated_moss$extrapolation, na.rm = TRUE),
                                               min = min(shape_extrap_integrated_moss$extrapolation, na.rm = TRUE),
                                               max = max(shape_extrap_integrated_moss$extrapolation, na.rm = TRUE),
                                               proportion_combinatorial = sum(shape_extrap_integrated_moss$univar_comb == 2)/ length(shape_extrap_integrated_moss$univar_comb))


extrap_summary <- rbind(shape_extrap_PO_lichen_summary, 
                        shape_extrap_PO_moss_summary, 
                        shape_extrap_PA_Vestfold_summary,
                        shape_extrap_integrated_lichen_summary,
                        shape_extrap_integrated_moss_summary)


# Plot the extrapolation across Bunger Hills

shape_extrap_PO_lichen <- cbind(shape_extrap_PO_lichen, bunger.xy.df[, c("x", "y")])
# extrap_rast <- rast(shape_extrap_PO_lichen[, c("x", "y", "extrapolation")], type = "xyz", crs = crs(bunger), ext = ext(bunger))

P1 <- shape_extrap_PO_lichen %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = extrapolation)) +
  scale_fill_viridis() +
  coord_fixed() +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('Extrapolation - PO Lichen records to Bunger Hills')

ggsave(filename = here("Outputs/Figures", paste0("Extrapolation - PO Lichen records to Bunger Hills.png")),
       plot = P1,
       width = 8, height = 4, dpi = 300, units = "in")

shape_extrap_PO_moss <- cbind(shape_extrap_PO_moss, bunger.xy.df[, c("x", "y")])
# extrap_rast <- rast(shape_extrap_PO_moss[, c("x", "y", "extrapolation")], type = "xyz", crs = crs(bunger), ext = ext(bunger))

P2 <- shape_extrap_PO_moss %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = extrapolation)) +
  scale_fill_viridis() +
  coord_fixed() +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('Extrapolation - Po Moss records to Bunger Hills')

ggsave(filename = here("Outputs/Figures", paste0("Extrapolation - PO Moss records to Bunger Hills.png")),
       plot = P2,
       width = 8, height = 4, dpi = 300, units = "in")

shape_extrap_PA_Vestfold <- cbind(shape_extrap_PA_Vestfold, bunger.xy.df[, c("x", "y")])
# extrap_rast <- rast(shape_extrap_PA_Vestfold[, c("x", "y", "extrapolation")], type = "xyz", crs = crs(bunger), ext = ext(bunger))

P3 <- shape_extrap_PA_Vestfold %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = extrapolation)) +
  scale_fill_viridis() +
  coord_fixed() +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.title = element_blank()) +
  ggtitle('Extrapolation - PA Vestfold records to Bunger Hills')

ggsave(filename = here("Outputs/Figures", paste0("Extrapolation - PA Vestfold records to Bunger Hills.png")),
       plot = P3,
       width = 8, height = 4, dpi = 300, units = "in")

# PO_lichen_df_xy = as.data.frame(PO_lichen_vect, geom = "XY")
# training_PO_lichen <- cbind(training_PO_lichen, PO_lichen_df_xy[, c("x", "y")])

# p_extra(training_data = training_PO_lichen,
#         x = "x",
#         y = "y",
#         pr_ab = "Presence",
#         extra_suit_data = extrap_rast,
#         projection_data = bunger)


### Plotting data in covariate space with extrapolation

# Interpreting: Here, grey is the environment that the data source covers
# Extrapolation is all the grid cells of Bunger Hills and how far they are from being covered by the data source

library(ggpubr)
library(ggnewscale)

P1 <- ggplot() + 
  geom_point(data = PO_lichen_df, aes(x = twi, y = slope, color = "Bunger Hills"), size = 2, alpha = 0.8) +
  scale_color_manual(values = c("Bunger Hills" = "grey"), name = NULL) +
  new_scale_color() +
  geom_point(data = shape_extrap_PO_lichen, aes(x = twi, y = slope, color = extrapolation), size = 2, alpha = 0.7) +
  scale_color_viridis(option = "magma", direction = -1, name = "PO Lichen\nExtrapolation") +
  theme_bw() +
  theme(legend.ticks = element_blank()) 

P2 <-  ggplot() + 
  geom_point(data = PA_Vestfold_df, aes(x = twi, y = slope), color = "grey", size = 2, alpha = 0.8) +
  geom_point(data = shape_extrap_PA_Vestfold, aes(x = twi, y = slope, color = extrapolation), size = 2, alpha = 0.7) +
  scale_color_viridis(option = "magma", direction = -1, "PA Vestfold\nExtrapolation") +
  theme_bw() +
  theme(legend.ticks = element_blank()) 

ggarrange(P1, P2, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom")


P1 <- ggplot() + 
  geom_point(data = PO_lichen_df, aes(x = northness, y = dist_vertebrates, color = "Bunger Hills"), size = 2, alpha = 0.8) +
  scale_color_manual(values = c("Bunger Hills" = "grey"), name = NULL) +
  new_scale_color() +
  geom_point(data = shape_extrap_PO_lichen, aes(x = northness, y = dist_vertebrates, color = extrapolation), size = 2, alpha = 0.7) +
  scale_color_viridis(option = "magma", direction = -1, name = "PO Lichen\nExtrapolation") +
  theme_bw() +
  theme(legend.ticks = element_blank()) 

P2 <-  ggplot() + 
  geom_point(data = PA_Vestfold_df, aes(x = northness, y = dist_vertebrates), color = "grey", size = 2, alpha = 0.8) +
  geom_point(data = shape_extrap_PA_Vestfold, aes(x = northness, y = dist_vertebrates, color = extrapolation), size = 2, alpha = 0.7) +
  scale_color_viridis(option = "magma", direction = -1, "PA Vestfold\nExtrapolation") +
  theme_bw() +
  theme(legend.ticks = element_blank()) 

ggarrange(P1, P2, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom")

P1 <- ggplot() + 
  geom_point(data = PO_lichen_df, aes(x = dist_seasonal_water, y = summer_temp, color = "Bunger Hills"), size = 2, alpha = 0.8) +
  scale_color_manual(values = c("Bunger Hills" = "grey"), name = NULL) +
  new_scale_color() +
  geom_point(data = shape_extrap_PO_lichen, aes(x = dist_seasonal_water, y = summer_temp, color = extrapolation), size = 2, alpha = 0.7) +
  scale_color_viridis(option = "magma", direction = -1, name = "PO Lichen\nExtrapolation") +
  theme_bw() +
  theme(legend.ticks = element_blank()) 

P2 <-  ggplot() + 
  geom_point(data = PA_Vestfold_df, aes(x = dist_seasonal_water, y = summer_temp), color = "grey", size = 2, alpha = 0.8) +
  geom_point(data = shape_extrap_PA_Vestfold, aes(x = dist_seasonal_water, y = summer_temp, color = extrapolation), size = 2, alpha = 0.7) +
  scale_color_viridis(option = "magma", direction = -1, "PA Vestfold\nExtrapolation") +
  theme_bw() +
  theme(legend.ticks = element_blank()) 

ggarrange(P1, P2, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom")


P1 <- ggplot() + 
  geom_point(data = PO_lichen_df, aes(x = wind, y = summer_temp, color = "Bunger Hills"), size = 2, alpha = 0.8) +
  scale_color_manual(values = c("Bunger Hills" = "grey"), name = NULL) +
  new_scale_color() +
  geom_point(data = shape_extrap_PO_lichen, aes(x = wind, y = summer_temp, color = extrapolation), size = 2, alpha = 0.7) +
  scale_color_viridis(option = "magma", direction = -1, name = "PO Lichen\nExtrapolation") +
  theme_bw() +
  theme(legend.ticks = element_blank()) 

P2 <-  ggplot() + 
  geom_point(data = PA_Vestfold_df, aes(x = wind, y = summer_temp), color = "grey", size = 2, alpha = 0.8) +
  geom_point(data = shape_extrap_PA_Vestfold, aes(x = wind, y = summer_temp, color = extrapolation), size = 2, alpha = 0.7) +
  scale_color_viridis(option = "magma", direction = -1, "PA Vestfold\nExtrapolation") +
  theme_bw() +
  theme(legend.ticks = element_blank()) 

ggarrange(P1, P2, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom")


# Key variable driving extrapolation --------------------------------------

ggplot(shape_extrap_PO_lichen, aes(x = twi, y = extrapolation)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_bw() +
  theme(legend.ticks = element_blank()) 

ggplot(shape_extrap_PO_lichen, aes(x = slope, y = extrapolation)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_bw() +
  theme(legend.ticks = element_blank()) 

ggplot(shape_extrap_PO_lichen, aes(x = northness, y = extrapolation)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_bw() +
  theme(legend.ticks = element_blank()) 

ggplot(shape_extrap_PO_lichen, aes(x = dist_vertebrates, y = extrapolation)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_bw() +
  theme(legend.ticks = element_blank()) 

ggplot(shape_extrap_PO_lichen, aes(x = dist_seasonal_water, y = extrapolation)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_bw() +
  theme(legend.ticks = element_blank()) 

ggplot(shape_extrap_PO_lichen, aes(x = summer_temp, y = extrapolation)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_bw() +
  theme(legend.ticks = element_blank()) 

ggplot(shape_extrap_PO_lichen, aes(x = wind, y = extrapolation)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_bw() +
  theme(legend.ticks = element_blank()) 

hist(shape_extrap_PO_lichen$extrapolation, breaks = 50, main = "Extrapolation - PO Lichen records to Bunger Hills", xlab = "Extrapolation")

hist(shape_extrap_PO_moss$extrapolation, breaks = 50, main = "Extrapolation - PO Moss records to Bunger Hills", xlab = "Extrapolation")

hist(shape_extrap_PA_Vestfold$extrapolation, breaks = 50, main = "Extrapolation - PA Vestfold records to Bunger Hills", xlab = "Extrapolation")

##########################################
# TO DO: ENVIRONMENTAL COVERAGE - FUTURE ENVIRONMENTS ---------------------
##########################################



