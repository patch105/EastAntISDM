
#############################################################
########## 2b. Environmental differences between sites ##########
#############################################################

# This code tests the environmental coverage of the Vestfold and Bunger Hills sites and looks at how different types of data represent the environmental conditions at the sites.


# Load packages -----------------------------------------------------------

library(purrr)

packages <- c("here", "sf", "terra", "dplyr", "tidyr", "viridis", "ggplot2", "tidyterra", "flexsdm", "viridisLite", "ggridges", "ggpubr")

walk(packages, require, character.only = T)

here::here()



# Load ice-free land 500m -------------------------------------------------

ice_free.EastAnt <- rast(here("Data/Environmental_predictors/ice_free_union_EastAnt_500m.tif"))


# Load covariates for all of East Antarctica ------------------------------

TWI <- rast(here("Data/Environmental_predictors/TWI_500m_IceFree_EastAnt.tif"))
names(TWI) <- "TWI"

slope <- rast(here("Data/Environmental_predictors/slope_500m_IceFree_EastAnt.tif"))
names(slope) <- "slope"

northness <- rast(here("Data/Environmental_predictors/northness_500m_IceFree_EastAnt.tif"))
names(northness) <- "northness"

aspect <- rast(here("Data/Environmental_predictors/aspect_500m_IceFree_EastAnt.tif"))
names(aspect) <- "aspect"

# dist_seasonal_water <- rast(here("Data/Environmental_predictors/distance_to_seasonal_water_ICEFREE_500m.tif"))
# names(dist_seasonal_water) <- "dist_seasonal_water"

summer_temp <- rast(here("Data/Environmental_predictors/mean_summer_temp_AntAirIce_500m.tif"))
names(summer_temp) <- "summer_temp"

wind_speed <- rast(here("Data/Environmental_predictors/Mean_Annual_Wind_Speed_ALL_YEARS_500m.tif"))
names(wind_speed) <- "wind_speed"

# Bias covariate
dist_station <- rast(here("Data/Environmental_predictors/distance_to_station_ICEFREE_500m.tif"))
names(dist_station) <- "dist_station"

# Transform some variables 
sqrt_slope <- sqrt(slope)
log_dist_station <- log(dist_station+1)

# Load the Antarctic Conservation Biogeographic Regions, filter to East Antarctica
ACBRS <- st_read(here("Data/Environmental_predictors/ACBRs_v2_2016.shp"), crs = 3031) %>% filter(ACBR_Name == "East Antarctica")
ACBRS_SPVE <- vect(ACBRS)


# Stack covariates
EastAnt <- c(TWI, sqrt_slope, northness, summer_temp, wind_speed)
EastAnt_w_bias <- c(c(TWI, sqrt_slope, northness, log_dist_station, summer_temp, wind_speed))


# Load boundaries
vestfold_boundary <- vect(here("Data/Environmental_predictors/vestfold_boundary.shp"))
bunger_boundary <- vect(here("Data/Environmental_predictors/bunger_boundary.shp"))


##########################################################################
## EXTRACT ENVIRO. CONDITIONS COVERED BY DATASETS AND AT BUNGER HILLS ##
##########################################################################

# BUNGER HILLS 

bunger_boundary <- vect(here("Data/Environmental_predictors/bunger_boundary.shp"))

bunger <- crop(EastAnt, bunger_boundary)

bunger.df <- as.data.frame(bunger, xy = FALSE, na.rm = TRUE) %>% 
  mutate(Region = "Bunger Hills")

# PO RECORDS

PO_lichen_vect <- vect(here("Data/Biological_records", "PO_Veg_East_Ant.shp")) %>% 
  filter(vegtype == "Lichen")

PO_moss_vect <- vect(here("Data/Biological_records", "PO_Veg_East_Ant.shp")) %>% 
  filter(vegtype == "Moss")

PO_lichen_df <- terra::extract(EastAnt, PO_lichen_vect, xy = FALSE, na.rm = TRUE) %>% 
  dplyr::select(-ID) %>% 
  mutate(Region = "PO Lichen")

PO_moss_df <- terra::extract(EastAnt, PO_moss_vect, xy = FALSE, na.rm = TRUE) %>% 
  dplyr::select(-ID) %>% 
  mutate(Region = "PO Moss")


# PA RECORDS

PA_Vestfold_vect <- st_read(here("Data/Biological_records", "PA_Veg_vestfold.shp"))

PA_Vestfold_df <- terra::extract(EastAnt, PA_Vestfold_vect, xy = FALSE, na.rm = TRUE) %>% 
  dplyr::select(-ID) %>% 
  mutate(Region = "PA Vestfold")


# SATELLITE-DERIVED RECORDS

Plantarctica_lichen_vect <- vect(here("Data/Biological_records", "PO_Plantarctica.shp")) %>% 
  filter(Vegetation == "Lichen") 

Plantarctica_moss_vect <- vect(here("Data/Biological_records", "PO_Plantarctica.shp")) %>% 
  filter(Vegetation == "Vegetation")

Plantarctica_lichen_df <- terra::extract(EastAnt, Plantarctica_lichen_vect, xy = F, na.rm = T) %>% 
  dplyr::select(-ID) %>% 
  mutate(Region = "Satellite lichen")

Plantarctica_moss_df <- terra::extract(EastAnt, Plantarctica_moss_vect, xy = F, na.rm = T) %>%
  dplyr::select(-ID) %>% 
  mutate(Region = "Satellite vegetation")


##########################################################################
###### PLOTTING ENVIRO. CONDITIONS BUNGER HILLS VS. DATA TYPES ############
##########################################################################


# LICHEN ------------------------------------------------------------------

all_lichen <- rbind(bunger.df, PO_lichen_df, PA_Vestfold_df, Plantarctica_lichen_df)

all_lichen$Region <- factor(all_lichen$Region, levels = c("PO Lichen", "Satellite lichen", "PA Vestfold", "Bunger Hills"))

all_lichen$Region <- factor(all_lichen$Region, levels = c("PA Vestfold", "Satellite lichen", "PO Lichen", "Bunger Hills"))

cov_names <- names(EastAnt) 


tidy_names <- c("Topographic wetness index", "Slope (sqrt)", "Northness", "Distance to station (log)", "Mean summer temperature", "Mean annual wind speed")


## Density plots

plot_list <- list()

for(i in seq_along(cov_names)) {
  
  # xmax <- quantile(all_lichen[, i], 0.99, na.rm = TRUE)
  
  # Conditional theme: show y-axis text only for plots in left column
  axis_theme <- if (i %% 2 == 1) {
    theme()  # Do nothing, keep y-axis labels
  } else {
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  }
  
  p <- ggplot(all_lichen, aes(x = .data[[names(all_lichen)[[i]]]], y = Region, fill = Region)) +
    geom_density_ridges(scale = 4, alpha = 0.8) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = c(
      "Bunger Hills" = "tan4",  
      "PO Lichen" = "darkorange",   
      "Satellite lichen" = "slateblue3",  
      "PA Vestfold" = "gray"
    )) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_bw(base_size = 16) +      
    axis_theme +                    
    theme(
      legend.position = "none"     
    ) +
    labs(
      x = tidy_names[[i]],
      y = NULL
    )
  
  
  plot_list[[i]] <- p
}

# Arrange all plots into a multi-panel figure
combined_plot <- ggarrange(plotlist = plot_list,
                           ncol = 2, nrow = ceiling(length(plot_list) / 2), 
                           legend = "none",
                           align = "hv")

# Save combined plot
ggsave(here("Outputs/Figures", "Combined_LICHEN_Distribution_Plots_500m_DENSITY.png"),
       plot = combined_plot,
       width = 13, height = 6 + 3 * ceiling(length(plot_list) / 2), dpi = 600)


# Histograms

plot_list <- list()

for(i in seq_along(cov_names)) {
  
  var_name <- names(all_lichen)[[i]]
  
  # Conditional theme: show y-axis text only for plots in left column
  axis_theme <- if (i %% 2 == 1) {
    theme()  # Do nothing, keep y-axis labels
  } else {
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  }
  
  p <- ggplot(all_lichen, aes(x = .data[[var_name]], y = Region, fill = Region)) +
    geom_density_ridges(
      stat = "binline",
      bins = 40,
      scale = 0.97,
      alpha = 0.8
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = c(
      "Bunger Hills" = "tan4",
      "PO Lichen" = "darkorange",
      "Satellite lichen" = "slateblue3",
      "PA Vestfold" = "gray"
    )) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_bw(base_size = 16) +
    axis_theme +  # Apply conditional theme
    theme(
      legend.position = element_blank()
      
    ) +
    labs(
      x = tidy_names[[i]],
      y = NULL
    )
  
  plot_list[[i]] <- p
}


# Arrange all plots into a multi-panel figure
combined_plot <- ggarrange(plotlist = plot_list,
                           ncol = 2, nrow = ceiling(length(plot_list) / 2), 
                           legend = "none",
                           align = "hv")

# Save combined plot
ggsave(here("Outputs/Figures", "Combined_LICHEN_Distribution_Plots_500m_HISTOGRAM.png"),
       plot = combined_plot,
       width = 13, height = 6 + 3 * ceiling(length(plot_list) / 2), dpi = 600)



# MOSS --------------------------------------------------------------------


all_moss <- rbind(bunger.df, PO_moss_df, PA_Vestfold_df, Plantarctica_moss_df)

# all_moss$Region <- factor(all_moss$Region, levels = c("PO Moss", "Satellite vegetation", "PA Vestfold", "Bunger Hills"))

all_moss$Region <- factor(all_moss$Region, levels = c("PA Vestfold", "Satellite vegetation", "PO Moss", "Bunger Hills"))

cov_names <- names(EastAnt) 


tidy_names <- c("Topographic wetness index", "Slope (sqrt)", "Northness", "Distance to station (log)", "Mean summer temperature", "Mean annual wind speed")


## Density plots

plot_list <- list()

for(i in seq_along(cov_names)) {
  
  # xmax <- quantile(all_moss[, i], 0.99, na.rm = TRUE)
  
  # Conditional theme: show y-axis text only for plots in left column
  axis_theme <- if (i %% 2 == 1) {
    theme()  # Do nothing, keep y-axis labels
  } else {
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  }
  
  p <- ggplot(all_moss, aes(x = .data[[names(all_moss)[[i]]]], y = Region, fill = Region)) +
    geom_density_ridges(scale = 4, alpha = 0.8) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = c(
      "Bunger Hills" = "tan4",  
      "PO Moss" = "darkorange",   
      "Satellite vegetation" = "slateblue3",  
      "PA Vestfold" = "gray"
    )) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_bw(base_size = 16) +      
    axis_theme +                    
    theme(
      legend.position = "none"     
    ) +
    labs(
      x = tidy_names[[i]],
      y = NULL
    )
  
  
  plot_list[[i]] <- p
}

# Arrange all plots into a multi-panel figure
combined_plot <- ggarrange(plotlist = plot_list,
                           ncol = 2, nrow = ceiling(length(plot_list) / 2), 
                           legend = "none",
                           align = "hv")

# Save combined plot
ggsave(here("Outputs/Figures", "Combined_MOSS_Distribution_Plots_500m_DENSITY.png"),
       plot = combined_plot,
       width = 13, height = 6 + 3 * ceiling(length(plot_list) / 2), dpi = 600)


# Histograms

plot_list <- list()

for(i in seq_along(cov_names)) {
  
  var_name <- names(all_moss)[[i]]
  
  # Conditional theme: show y-axis text only for plots in left column
  axis_theme <- if (i %% 2 == 1) {
    theme()  # Do nothing, keep y-axis labels
  } else {
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  }
  
  p <- ggplot(all_moss, aes(x = .data[[var_name]], y = Region, fill = Region)) +
    geom_density_ridges(
      stat = "binline",
      bins = 40,
      scale = 0.97,
      alpha = 0.8
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = c(
      "Bunger Hills" = "tan4",
      "PO Moss" = "darkorange",
      "Satellite vegetation" = "slateblue3",
      "PA Vestfold" = "gray"
    )) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_bw(base_size = 16) +
    axis_theme +  # Apply conditional theme
    theme(
      legend.position = element_blank()
      
    ) +
    labs(
      x = tidy_names[[i]],
      y = NULL
    )
  
  plot_list[[i]] <- p
}


# Arrange all plots into a multi-panel figure
combined_plot <- ggarrange(plotlist = plot_list,
                           ncol = 2, nrow = ceiling(length(plot_list) / 2), 
                           legend = "none",
                           align = "hv")

# Save combined plot
ggsave(here("Outputs/Figures", "Combined_MOSS_Distribution_Plots_500m_HISTOGRAM.png"),
       plot = combined_plot,
       width = 13, height = 6 + 3 * ceiling(length(plot_list) / 2), dpi = 600)



##########################################################################
################ QUANTIFYING ENVIRONMENTAL DISTANCE ##################
##########################################################################

# Adding presence column due to extra_eval requirements
# Trimming so just the covariates
projection <- bunger.df %>% 
  dplyr::select(-Region)

training_PO_lichen <- PO_lichen_df %>% 
  mutate(Presence = 1) %>% 
  dplyr::select(-Region)

training_PO_moss <- PO_moss_df %>% 
  mutate(Presence = 1) %>% 
  dplyr::select(-Region)

training_PA_Vestfold <- PA_Vestfold_df %>%
  mutate(Presence = 1) %>% 
  dplyr::select(-Region)

training_integrated_lichen <- rbind(training_PO_lichen, training_PA_Vestfold)
training_integrated_moss <- rbind(training_PO_moss, training_PA_Vestfold)

# Adding Plantarctica data to the training data
training_Plantarctica_lichen <- Plantarctica_lichen_df %>% 
  mutate(Presence = 1) %>% 
  dplyr::select(-Region)

training_Plantarctica_moss <- Plantarctica_moss_df %>%
  mutate(Presence = 1) %>% 
  dplyr::select(-Region)

training_PO_PA_Plantarctica_lichen <- rbind(training_PO_lichen, 
                                            training_PA_Vestfold, 
                                            training_Plantarctica_lichen)

training_PO_PA_Plantarctica_moss <- rbind(training_PO_moss, 
                                          training_PA_Vestfold, 
                                          training_Plantarctica_moss)

training_PO_Plantarctica_lichen <- rbind(training_PO_lichen, 
                                            training_Plantarctica_lichen)

training_PO_Plantarctica_moss <- rbind(training_PO_moss, 
                                          training_Plantarctica_moss)

training_PA_Plantarctica_lichen <- rbind(training_PA_Vestfold, 
                                            training_Plantarctica_lichen)

training_PA_Plantarctica_moss <- rbind(training_PA_Vestfold, 
                                          training_Plantarctica_moss)

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

shape_extrap_Plantarctica_lichen <- extra_eval(training_data = training_Plantarctica_lichen,
                                               pr_ab = "Presence",
                                               projection_data = projection,
                                               metric = "mahalanobis",
                                               univar_comb = T,
                                               n_cores = 3)

shape_extrap_Plantarctica_moss <- extra_eval(training_data = training_Plantarctica_moss,
                                             pr_ab = "Presence",
                                             projection_data = projection,
                                             metric = "mahalanobis",
                                             univar_comb = T,
                                             n_cores = 3)

shape_extrap_PO_PA_Plantarctica_lichen <- extra_eval(training_data = training_PO_PA_Plantarctica_lichen,
                                                     pr_ab = "Presence",
                                                     projection_data = projection,
                                                     metric = "mahalanobis",
                                                     univar_comb = T,
                                                     n_cores = 3)

shape_extrap_PO_PA_Plantarctica_moss <- extra_eval(training_data = training_PO_PA_Plantarctica_moss,
                                                   pr_ab = "Presence",
                                                   projection_data = projection,
                                                   metric = "mahalanobis",
                                                   univar_comb = T,
                                                   n_cores = 3)

shape_extrap_PO_Plantarctica_lichen <- extra_eval(training_data = training_PO_Plantarctica_lichen,
                                                   pr_ab = "Presence",
                                                   projection_data = projection,
                                                   metric = "mahalanobis",
                                                   univar_comb = T,
                                                   n_cores = 3)

shape_extrap_PO_Plantarctica_moss <- extra_eval(training_data = training_PO_Plantarctica_moss,
                                                   pr_ab = "Presence",
                                                   projection_data = projection,
                                                   metric = "mahalanobis",
                                                   univar_comb = T,
                                                   n_cores = 3)

shape_extrap_PA_Plantarctica_lichen <- extra_eval(training_data = training_PA_Plantarctica_lichen,
                                                   pr_ab = "Presence",
                                                   projection_data = projection,
                                                   metric = "mahalanobis",
                                                   univar_comb = T,
                                                   n_cores = 3)

shape_extrap_PA_Plantarctica_moss <- extra_eval(training_data = training_PA_Plantarctica_moss,
                                                   pr_ab = "Presence",
                                                   projection_data = projection,
                                                   metric = "mahalanobis",
                                                   univar_comb = T,
                                                   n_cores = 3)


##########################################################################
################ PLOTTING RESULTS - ENVIRONMENTAL DISTANCE ##################
##########################################################################


# LICHEN ------------------------------------------------------------------

# Combine all results into a single data frame

lichen_extrap <- rbind(
  shape_extrap_PO_lichen %>% mutate(Region = "PO"),
  shape_extrap_PA_Vestfold %>% mutate(Region = "PA"),
  shape_extrap_Plantarctica_lichen %>% mutate(Region = "Satellite"),
  shape_extrap_integrated_lichen %>% mutate(Region = "PO + PA"),
  shape_extrap_PO_Plantarctica_lichen %>% mutate(Region = "PO + Satellite"),
  shape_extrap_PA_Plantarctica_lichen %>% mutate(Region = "PA + Satellite"),
  shape_extrap_PO_PA_Plantarctica_lichen %>% mutate(Region = "PO + PA + Satellite")
)

# Convert Region to a factor with specified levels
lichen_extrap$Region <- factor(lichen_extrap$Region, 
                                levels = c("PO", 
                                           "Satellite", 
                                           "PA", 
                                           "PO + PA", 
                                           "PO + Satellite",
                                           "PA + Satellite",
                                           "PO + PA + Satellite"),
                               labels = c("PO", 
                                          "Satellite", 
                                          "PA", 
                                          "PO + PA", 
                                          "PO\n+\nSatellite", 
                                          "PA\n+\nSatellite", 
                                          "PO\n+\nPA\n+\nSatellite"))

# Create the violin plot
extrap_plot_lichen <- lichen_extrap %>% 
  ggplot(aes(x = Region, y = extrapolation)) +
  geom_violin(fill = "gray30", alpha = 0.2, trim = TRUE, width = 0.7, color = NA) +
  geom_boxplot(fill = "gray30", alpha = 0.6, width = 0.2, outlier.shape = NA) +
  #geom_jitter(width = 0.05, alpha = 0.01, size = 0.5, color = "purple") +
  labs(x = NULL, y = "Environmental distance", title = "Lichen") +
  ylim(0, 250) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(size = 12))


# MOSS ------------------------------------------------------------------

# Combine all results into a single data frame

moss_extrap <- rbind(
  shape_extrap_PO_moss %>% mutate(Region = "PO"),
  shape_extrap_PA_Vestfold %>% mutate(Region = "PA"),
  shape_extrap_Plantarctica_moss %>% mutate(Region = "Satellite"),
  shape_extrap_integrated_moss %>% mutate(Region = "PO + PA"),
  shape_extrap_PO_Plantarctica_moss %>% mutate(Region = "PO + Satellite"),
  shape_extrap_PA_Plantarctica_moss %>% mutate(Region = "PA + Satellite"),
  shape_extrap_PO_PA_Plantarctica_moss %>% mutate(Region = "PO + PA + Satellite")
)

# Convert Region to a factor with specified levels
moss_extrap$Region <- factor(moss_extrap$Region, 
                               levels = c("PO", 
                                          "Satellite", 
                                          "PA", 
                                          "PO + PA", 
                                          "PO + Satellite",
                                          "PA + Satellite",
                                          "PO + PA + Satellite"),
                               labels = c("PO", 
                                          "Satellite", 
                                          "PA", 
                                          "PO + PA", 
                                          "PO\n+\nSatellite", 
                                          "PA\n+\nSatellite", 
                                          "PO\n+\nPA\n+\nSatellite"))

# Create the violin plot
extrap_plot_moss <- moss_extrap %>% 
  ggplot(aes(x = Region, y = extrapolation)) +
  geom_violin(fill = "gray30", alpha = 0.2, trim = TRUE, width = 0.7, color = NA) +
  geom_boxplot(fill = "gray30", alpha = 0.6, width = 0.2, outlier.shape = NA) +
  #geom_jitter(width = 0.05, alpha = 0.01, size = 0.5, color = "purple") +
  labs(x = NULL, y = "Environmental distance", title = "Moss") +
  ylim(0, 250) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(size = 12))

# Combine the two plots into one figure
require(grid)
combined_extrap_plot <- ggarrange(extrap_plot_lichen + rremove("ylab"), 
                                  extrap_plot_moss + rremove("ylab"),
                                  labels = NULL,
                                   ncol = 1, nrow = 2, 
                                   align = "hv")

combined_extrap_plot <- annotate_figure(combined_extrap_plot, 
                                        left = textGrob("Environmental distance from Bunger Hills to training data", 
                                                        rot = 90, vjust = 1, gp = gpar(cex = 1.05)))

# Save the combined plot

ggsave(here("Outputs/Figures", "Combined_Extrapolation_Box_Plots_500m.png"),
       plot = combined_extrap_plot,
       w = 15, h = 17, units = "cm", dpi = 600, device = "png")



##########################################################################
################ SUMMARISING RESULTS - ENVIRO. DISTANCE ################
##########################################################################

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




shape_extrap_PA_Vestfold_summary <- data.frame(data_type = "PA", 
                                               mean = mean(shape_extrap_PA_Vestfold$extrapolation, na.rm = TRUE),
                                               median = median(shape_extrap_PA_Vestfold$extrapolation, na.rm = TRUE),
                                               min = min(shape_extrap_PA_Vestfold$extrapolation, na.rm = TRUE),
                                               max = max(shape_extrap_PA_Vestfold$extrapolation, na.rm = TRUE),
                                               proportion_combinatorial = sum(shape_extrap_PA_Vestfold$univar_comb == 2)/ length(shape_extrap_PA_Vestfold$univar_comb))




shape_extrap_integrated_lichen_summary <- data.frame(data_type = "PO + PA Lichen", 
                                                     mean = mean(shape_extrap_integrated_lichen$extrapolation, na.rm = TRUE),
                                                     median = median(shape_extrap_integrated_lichen$extrapolation, na.rm = TRUE),
                                                     min = min(shape_extrap_integrated_lichen$extrapolation, na.rm = TRUE),
                                                     max = max(shape_extrap_integrated_lichen$extrapolation, na.rm = TRUE),
                                                     proportion_combinatorial = sum(shape_extrap_integrated_lichen$univar_comb == 2)/ length(shape_extrap_integrated_lichen$univar_comb))




shape_extrap_integrated_moss_summary <- data.frame(data_type = "PO + PA Moss",
                                                   mean = mean(shape_extrap_integrated_moss$extrapolation, na.rm = TRUE),
                                                   median = median(shape_extrap_integrated_moss$extrapolation, na.rm = TRUE),
                                                   min = min(shape_extrap_integrated_moss$extrapolation, na.rm = TRUE),
                                                   max = max(shape_extrap_integrated_moss$extrapolation, na.rm = TRUE),
                                                   proportion_combinatorial = sum(shape_extrap_integrated_moss$univar_comb == 2)/ length(shape_extrap_integrated_moss$univar_comb))




shape_extrap_Plantarctica_lichen_summary <- data.frame(data_type = "Satellite Lichen",
                                                       mean = mean(shape_extrap_Plantarctica_lichen$extrapolation, na.rm = TRUE),
                                                       median = median(shape_extrap_Plantarctica_lichen$extrapolation, na.rm = TRUE),
                                                       min = min(shape_extrap_Plantarctica_lichen$extrapolation, na.rm = TRUE),
                                                       max = max(shape_extrap_Plantarctica_lichen$extrapolation, na.rm = TRUE),
                                                       proportion_combinatorial = sum(shape_extrap_Plantarctica_lichen$univar_comb == 2)/ length(shape_extrap_Plantarctica_lichen$univar_comb))




shape_extrap_Plantarctica_moss_summary <- data.frame(data_type = "Satellite Moss",
                                                     mean = mean(shape_extrap_Plantarctica_moss$extrapolation, na.rm = TRUE),
                                                     median = median(shape_extrap_Plantarctica_moss$extrapolation, na.rm = TRUE),
                                                     min = min(shape_extrap_Plantarctica_moss$extrapolation, na.rm = TRUE),
                                                     max = max(shape_extrap_Plantarctica_moss$extrapolation, na.rm = TRUE),
                                                     proportion_combinatorial = sum(shape_extrap_Plantarctica_moss$univar_comb == 2)/ length(shape_extrap_Plantarctica_moss$univar_comb))





shape_extrap_PO_Plantarctica_lichen_summary <- data.frame(data_type = "PO + Satellite Lichen",
                                                             mean = mean(shape_extrap_PO_Plantarctica_lichen$extrapolation, na.rm = TRUE),
                                                             median = median(shape_extrap_PO_Plantarctica_lichen$extrapolation, na.rm = TRUE),
                                                             min = min(shape_extrap_PO_Plantarctica_lichen$extrapolation, na.rm = TRUE),
                                                             max = max(shape_extrap_PO_Plantarctica_lichen$extrapolation, na.rm = TRUE),
                                                             proportion_combinatorial = sum(shape_extrap_PO_Plantarctica_lichen$univar_comb == 2)/ length(shape_extrap_PO_Plantarctica_lichen$univar_comb))




shape_extrap_PO_Plantarctica_moss_summary <- data.frame(data_type = "PO + Satellite Moss",
                                                           mean = mean(shape_extrap_PO_Plantarctica_moss$extrapolation, na.rm = TRUE),
                                                           median = median(shape_extrap_PO_Plantarctica_moss$extrapolation, na.rm = TRUE),
                                                           min = min(shape_extrap_PO_Plantarctica_moss$extrapolation, na.rm = TRUE),
                                                           max = max(shape_extrap_PO_Plantarctica_moss$extrapolation, na.rm = TRUE),
                                                           proportion_combinatorial = sum(shape_extrap_PO_Plantarctica_moss$univar_comb == 2)/ length(shape_extrap_PO_Plantarctica_moss$univar_comb))




shape_extrap_PA_Plantarctica_lichen_summary <- data.frame(data_type = "PA + Satellite Lichen",
                                                             mean = mean(shape_extrap_PA_Plantarctica_lichen$extrapolation, na.rm = TRUE),
                                                             median = median(shape_extrap_PA_Plantarctica_lichen$extrapolation, na.rm = TRUE),
                                                             min = min(shape_extrap_PA_Plantarctica_lichen$extrapolation, na.rm = TRUE),
                                                             max = max(shape_extrap_PA_Plantarctica_lichen$extrapolation, na.rm = TRUE),
                                                             proportion_combinatorial = sum(shape_extrap_PA_Plantarctica_lichen$univar_comb == 2)/ length(shape_extrap_PA_Plantarctica_lichen$univar_comb))




shape_extrap_PA_Plantarctica_moss_summary <- data.frame(data_type = "PA + Satellite Moss",
                                                           mean = mean(shape_extrap_PA_Plantarctica_moss$extrapolation, na.rm = TRUE),
                                                           median = median(shape_extrap_PA_Plantarctica_moss$extrapolation, na.rm = TRUE),
                                                           min = min(shape_extrap_PA_Plantarctica_moss$extrapolation, na.rm = TRUE),
                                                           max = max(shape_extrap_PA_Plantarctica_moss$extrapolation, na.rm = TRUE),
                                                           proportion_combinatorial = sum(shape_extrap_PA_Plantarctica_moss$univar_comb == 2)/ length(shape_extrap_PA_Plantarctica_moss$univar_comb))





shape_extrap_PO_PA_Plantarctica_lichen_summary <- data.frame(data_type = "PO + PA + Satellite Lichen",
                                                             mean = mean(shape_extrap_PO_PA_Plantarctica_lichen$extrapolation, na.rm = TRUE),
                                                             median = median(shape_extrap_PO_PA_Plantarctica_lichen$extrapolation, na.rm = TRUE),
                                                             min = min(shape_extrap_PO_PA_Plantarctica_lichen$extrapolation, na.rm = TRUE),
                                                             max = max(shape_extrap_PO_PA_Plantarctica_lichen$extrapolation, na.rm = TRUE),
                                                             proportion_combinatorial = sum(shape_extrap_PO_PA_Plantarctica_lichen$univar_comb == 2)/ length(shape_extrap_PO_PA_Plantarctica_lichen$univar_comb))





shape_extrap_PO_PA_Plantarctica_moss_summary <- data.frame(data_type = "PO + PA + Satellite Moss",
                                                           mean = mean(shape_extrap_PO_PA_Plantarctica_moss$extrapolation, na.rm = TRUE),
                                                           median = median(shape_extrap_PO_PA_Plantarctica_moss$extrapolation, na.rm = TRUE),
                                                           min = min(shape_extrap_PO_PA_Plantarctica_moss$extrapolation, na.rm = TRUE),
                                                           max = max(shape_extrap_PO_PA_Plantarctica_moss$extrapolation, na.rm = TRUE),
                                                           proportion_combinatorial = sum(shape_extrap_PO_PA_Plantarctica_moss$univar_comb == 2)/ length(shape_extrap_PO_PA_Plantarctica_moss$univar_comb))


# Combine all summaries into one data frame
extrap_summary <- rbind(shape_extrap_PO_lichen_summary, 
                        shape_extrap_PO_moss_summary, 
                        shape_extrap_PA_Vestfold_summary,
                        shape_extrap_integrated_lichen_summary,
                        shape_extrap_integrated_moss_summary,
                        shape_extrap_Plantarctica_lichen_summary,
                        shape_extrap_Plantarctica_moss_summary,
                        shape_extrap_PO_Plantarctica_lichen_summary,
                        shape_extrap_PO_Plantarctica_moss_summary,
                        shape_extrap_PA_Plantarctica_lichen_summary,
                        shape_extrap_PA_Plantarctica_moss_summary,
                        shape_extrap_PO_PA_Plantarctica_lichen_summary,
                        shape_extrap_PO_PA_Plantarctica_moss_summary)

write.csv(extrap_summary, 
          here("Outputs/Results", "Extrapolation_Summary_500m.csv"),
          row.names = FALSE)


##########################################################################
################ PLOTTING ENVIRO. DISTANCE IN SPACE ################
##########################################################################

# Plot the extrapolation across Bunger Hills
bunger.xy.df <- as.data.frame(bunger, xy = T)


# LICHEN ------------------------------------------------------------------


shape_extrap_PO_lichen <- cbind(shape_extrap_PO_lichen, bunger.xy.df[, c("x", "y")])

shape_extrap_Plantarctica_lichen <- cbind(shape_extrap_Plantarctica_lichen, bunger.xy.df[, c("x", "y")])

shape_extrap_PA_Vestfold <- cbind(shape_extrap_PA_Vestfold, bunger.xy.df[, c("x", "y")])



# MOSS --------------------------------------------------------------------

shape_extrap_PO_moss <- cbind(shape_extrap_PO_moss, bunger.xy.df[, c("x", "y")])

shape_extrap_Plantarctica_moss <- cbind(shape_extrap_Plantarctica_moss, bunger.xy.df[, c("x", "y")])

global_min <- min(c(
  shape_extrap_PO_lichen$extrapolation,
  shape_extrap_Plantarctica_lichen$extrapolation,
  shape_extrap_PA_Vestfold$extrapolation,
  shape_extrap_PO_moss$extrapolation,
  shape_extrap_Plantarctica_moss$extrapolation
), na.rm = TRUE)

global_max <- max(c(
  shape_extrap_PO_lichen$extrapolation,
  shape_extrap_Plantarctica_lichen$extrapolation,
  shape_extrap_PA_Vestfold$extrapolation,
  shape_extrap_PO_moss$extrapolation,
  shape_extrap_Plantarctica_moss$extrapolation
), na.rm = TRUE)

shared_scale <- scale_fill_viridis(limits = c(global_min, global_max),
                                   name = "Environmental\ndistance")

P1 <- shape_extrap_PO_lichen %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = extrapolation)) +
  shared_scale +
  coord_fixed() +
  theme_bw() +
  labs(title = "Presence-only") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.ticks = element_blank())

P2 <- shape_extrap_Plantarctica_lichen %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = extrapolation)) +
  shared_scale +
  coord_fixed() +
  theme_bw() +
  labs(title = "Satellite") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.ticks = element_blank())


P3 <- shape_extrap_PA_Vestfold %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = extrapolation)) +
  shared_scale +
  coord_fixed() +
  theme_bw() +
  labs(title = "Presence-absence") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.ticks = element_blank())

P4 <- shape_extrap_PO_moss %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = extrapolation)) +
  shared_scale +
  coord_fixed() +
  theme_bw() +
  labs(title = "Presence-only") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.ticks = element_blank())

P5 <- shape_extrap_Plantarctica_moss %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = extrapolation)) +
  shared_scale +
  coord_fixed() +
  theme_bw() +
  labs(title = "Satellite") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.ticks = element_blank())

P6 <- shape_extrap_PA_Vestfold %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = extrapolation)) +
  shared_scale +
  coord_fixed() +
  theme_bw() +
  labs(title = "Presence-absence") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.ticks = element_blank())


# Make the combined figure ------------------------------------------------

# Arrange Lichen row
lichen_row <- ggarrange(P1, P2, P3, ncol = 3, common.legend = TRUE, legend = "none")

# Arrange Moss row
moss_row <- ggarrange(P4, P5, P6, ncol = 3, common.legend = TRUE, legend = "bottom")  # no legend here

# Add titles
lichen_row <- annotate_figure(lichen_row, top = textGrob("Lichen", gp = gpar(fontsize = 14, fontface = "bold")))
moss_row   <- annotate_figure(moss_row, top = textGrob("Moss", gp = gpar(fontsize = 14, fontface = "bold")))

# Combine rows
combined_extrap_spatial <- ggarrange(lichen_row, moss_row, ncol = 1, heights = c(1, 1.35))


# Save the combined figure ------------------------------------------------

ggsave(here("Outputs/Figures", "Combined_extrap_Bunger_plot_500m.png"),
       plot = combined_extrap_spatial,
       width = 18, height = 18, units = "cm", dpi = 600)


