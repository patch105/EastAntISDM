# Set up for running on HPC

# HPC version
# lib_loc <- "/mnt/hpccs01/home/n11222026/ISDM/r_lib"

# Non-HPC version
lib_loc = .libPaths()

# Load packages -----------------------------------------------------------

library(here)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(viridis)
library(terra)
library(purrr)
library(DescTools)
# library(inlabru, lib.loc=lib_loc) # Only needed for plotting mesh 
library(sf) # NOTE SF MUST BE LOADED BEFORE RISDM
library(RISDM,lib.loc=lib_loc)
library(fmesher,lib.loc=lib_loc)
library(flexsdm, lib.loc=lib_loc)



# Load some helper functions -----------------------------------------------

source(here("Scripts/Helper_functions_ISDM.R"))


# Set group ---------------------------------------------------------------

group <- "Lichen"
# group <- "Moss"

# Set outpath -------------------------------------------------------------

outpath <- here("Outputs", "Integrated", group)

if(!dir.exists(outpath)) {
  dir.create(outpath, showWarnings = FALSE)
} 

# Domain setup 1 km ------------------------------------------------------------

# Load the ice-free areas
ice_free <- rast(here("Data/Environmental_predictors/ice_free_upsamp_1km.tif"))

# Load the Antarctic Conservation Biogeographic Regions, filter to East Antarctica
ACBRS <- st_read(here("Data/Environmental_predictors/ACBRs_v2_2016.shp"), crs = 3031) %>% filter(ACBR_Name == "East Antarctica")
ACBRS_SPVE <- vect(ACBRS)


# Also trim ice-free land to just East Antarctica
ice_free.EastAnt <- terra::crop(ice_free, ext(ACBRS_SPVE))

ice_free.EastAnt <- ifel(not.na(ice_free.EastAnt), 1, NA)

vestfold_boundary <- vect(here("Data/Environmental_predictors/vestfold_boundary.shp"))
bunger_boundary <- vect(here("Data/Environmental_predictors/bunger_boundary.shp"))


# # Domain setup 100m ------------------------------------------------------------
# 
# # Load the ice-free areas
# ice_free.EastAnt <- rast(here("Data/Environmental_predictors/ice_free_EastAnt_100m.tif"))
# 
# # Load the Antarctic Conservation Biogeographic Regions, filter to East Antarctica
# ACBRS <- st_read(here("Data/Environmental_predictors/ACBRs_v2_2016.shp"), crs = 3031) %>% filter(ACBR_Name == "East Antarctica")
# ACBRS_SPVE <- vect(ACBRS)
# 
# 
# vestfold_boundary <- vect(here("Data/Environmental_predictors/vestfold_boundary.shp"))
# bunger_boundary <- vect(here("Data/Environmental_predictors/bunger_boundary.shp"))


# Load the presence-only records ------------------------------------------

PO_East_Ant_Veg_sf <- st_read(here("Data/Biological_records", "PO_Veg_East_Ant.shp"))

PO_East_Ant_Veg_df <- PO_East_Ant_Veg_sf %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  bind_cols(st_drop_geometry(PO_East_Ant_Veg_sf)) %>% 
  rename(x = X, y = Y) 


# Load the presence-absence records ---------------------------------------

PA_Vestfold_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_vestfold.shp"))

PA_Vestfold_Veg_df <- PA_Vestfold_Veg_sf %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  bind_cols(st_drop_geometry(PA_Vestfold_Veg_sf)) %>% 
  rename(x = X, y = Y)


PA_Bunger23_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_bunger23.shp"))

PA_Bunger23_Veg_df <- PA_Bunger23_Veg_sf %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  bind_cols(st_drop_geometry(PA_Bunger23_Veg_sf)) %>% 
  rename(x = X, y = Y)




# Format for modelling  ----------------------------------------------------

if(group == "Moss") {
  
  
  PO <- PO_East_Ant_Veg_df %>% 
    filter(vegtype == "Moss") %>% 
    dplyr::select(x, y) 
  
  PO.sf <- PO_East_Ant_Veg_sf %>% 
    filter(vegtype == "Moss")
  
  PA_vestfold <- PA_Vestfold_Veg_df %>% 
    dplyr::select(x, y, srfc_ms) %>% 
    rename(presence = srfc_ms)
  
  PA_vestfold.sf <- PA_Vestfold_Veg_sf %>% 
    rename(presence = srfc_ms)
  
  PA_bunger23 <- PA_Bunger23_Veg_df %>% 
    dplyr::select(x, y, srfc_ms) %>% 
    rename(presence = srfc_ms)
  
  PA_bunger23.sf <- PA_Bunger23_Veg_sf %>% 
    rename(presence = srfc_ms)
  
}

if(group == "Lichen") {
  
  PO <- PO_East_Ant_Veg_df %>% 
    filter(vegtype == "Lichen") %>% 
    dplyr::select(x, y) 
  
  PO.sf <- PO_East_Ant_Veg_sf %>% 
    filter(vegtype == "Lichen")
  
  PA_vestfold <- PA_Vestfold_Veg_df %>% 
    dplyr::select(x, y, srfc_lc) %>% 
    rename(presence = srfc_lc)
  
  PA_vestfold.sf <- PA_Vestfold_Veg_sf %>% 
    rename(presence = srfc_lc)
  
  PA_bunger23 <- PA_Bunger23_Veg_df %>% 
    dplyr::select(x, y, srfc_lc) %>% 
    rename(presence = srfc_lc)
  
  PA_bunger23.sf <- PA_Bunger23_Veg_sf %>% 
    rename(presence = srfc_lc)
  
}


# Add search area information

PA_vestfold <- PA_vestfold %>% mutate(area = 80)
PA_bunger23 <- PA_bunger23 %>% mutate(area = 100)

# # Add together
# PA_both <- rbind(PA_vestfold, PA_bunger23)

# Load the 1km covariates -----------------------------------------------------

TWI <- rast(here("Data/Environmental_predictors/topographic_wetness_index_EAST_ANTARCTICA.tif"))
names(TWI) <- "TWI"

slope <- rast(here("Data/Environmental_predictors/slope_EAST_ANTARCTICA.tif"))
names(slope) <- "slope"

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

# Apply some transformations
sqrt_slope <- sqrt(slope)
names(sqrt_slope) <- "sqrt_slope"

log_dist_seasonal_water <- log(dist_seasonal_water+1)
names(log_dist_seasonal_water) <- "log_dist_seasonal_water"

log_dist_station <- log(dist_station+1)
names(log_dist_station) <- "log_dist_station"

log_dist_vertebrates <- log(dist_vertebrates+1)
names(log_dist_vertebrates) <- "log_dist_vertebrates"


# # Load the 100m covariates -----------------------------------------------------
# 
# TWI <- rast(here("Data/Environmental_predictors/TWI_100m_IceFree_EastAnt.tif"))
# names(TWI) <- "TWI"
# 
# slope <- rast(here("Data/Environmental_predictors/slope_100m_IceFree_EastAnt.tif"))
# names(slope) <- "slope"
# 
# aspect <- rast(here("Data/Environmental_predictors/aspect_100m_IceFree_EastAnt.tif"))
# names(aspect) <- "aspect"
# 
# dist_vertebrates <- rast(here("Data/Environmental_predictors/distance_to_vertebrates_ICEFREE_100m.tif"))
# names(dist_vertebrates) <- "dist_vertebrates"
# 
# dist_seasonal_water <- rast(here("Data/Environmental_predictors/distance_to_seasonal_water_ICEFREE_100m.tif"))
# names(dist_seasonal_water) <- "dist_seasonal_water"
# 
# summer_temp <- rast(here("Data/Environmental_predictors/mean_summer_temp_AntAirIce_100m.tif"))
# names(summer_temp) <- "summer_temp"
# 
# # Bias covariate
# dist_station <- rast(here("Data/Environmental_predictors/distance_to_station_ICEFREE_100m.tif"))
# names(dist_station) <- "dist_station"
# 
# # Apply some transformations
# sqrt_slope <- sqrt(slope)
# names(sqrt_slope) <- "sqrt_slope"
# 
# log_dist_seasonal_water <- log(dist_seasonal_water+1)
# names(log_dist_seasonal_water) <- "log_dist_seasonal_water"
# 
# log_dist_station <- log(dist_station+1)
# names(log_dist_station) <- "log_dist_station"
# 
# log_dist_vertebrates <- log(dist_vertebrates+1)
# names(log_dist_vertebrates) <- "log_dist_vertebrates"

# Stack covariates & save version w/o bias cov
covs_no_bias <- c(TWI, sqrt_slope, northness, log_dist_seasonal_water, summer_temp)
covs <- c(TWI, sqrt_slope, northness, log_dist_seasonal_water, summer_temp, log_dist_station)

# Make sure that if any predictors are NA, all become NA

# Here we're just exploiting the fact that sum will by default return NA when any layer has an NA
covs_no_bias <- terra::mask(covs_no_bias, sum(covs_no_bias))
covs <- terra::mask(covs, sum(covs))


# 1. MAKE THE MESH --------------------------------------------------------

# domain_poly <- st_read(here("Data/Environmental_predictors/rocks_Union_Land.shp"), crs = 3031)
# 
# domain_poly <- st_crop(domain_poly, ext(ice_free.EastAnt)) 
# 
# domain_poly_1km_buffer <- st_buffer(domain_poly, 1000)

# CROP ACBRS TO THE VESTFOLD BOUNDARY
ACBRS_buffer1km <- st_buffer(ACBRS, 1000) 
ACBRS_buffer1km <- st_union(ACBRS_buffer1km)


boundary <- fmesher::fm_as_segm(st_as_sf(ACBRS_buffer1km))

dep.range <- 10000 # 10km Set the range based on biology


mesh.range.10km.cutoff.50 <- fmesher::fm_mesh_2d_inla(loc = st_coordinates(PO.sf),
                                                      boundary = boundary,
                                                      max.edge = c(0.2, 0.5)*dep.range,
                                                      cutoff = 50,
                                                      crs=3031)
print(paste0("Time is: ", Sys.time()))
print("Mesh created")

# Save the mesh: 

# saveRDS(mesh.range.10km.cutoff.50, file = here("Outputs/Testing/VESTFOLD_mesh.range.10km.cutoff.50.rds"))
# mesh.range.10km.cutoff.50 <- readRDS(file = "mesh.range.10km.cutoff.50.rds")

# Plot the mesh:
# 
#  p1 <- ggplot() +
#    inlabru::gg(mesh.range.10km.cutoff.50) +
#    # gg(mesh.vrt, color = "red") +
#    geom_sf(data = PO.sf, color = "red")
# 
#  p2 <- ggplot() +
#    inlabru::gg(mesh.range.10km.cutoff.50) +
#    # gg(mesh.vrt, color = "red") +
#    coord_sf(
#      xlim = c(st_bbox(bunger_boundary)$xmin, st_bbox(bunger_boundary)$xmax),
#      ylim = c(st_bbox(bunger_boundary)$ymin, st_bbox(bunger_boundary)$ymax))
# 
#  p3 <- ggplot() +
#    inlabru::gg(mesh.range.10km.cutoff.50)
# 
#  ggsave(plot = p1, filename = here("Outputs/Testing/v2mesh_range_10km_cutoff_50_VESTFOLD.png"), width = 10, height = 10, dpi = 300)
# ggsave(plot = p2, filename = here("Outputs/Figures/mesh_range_10km_cutoff_50_BUNGER.png"), width = 10, height = 10, dpi = 300)
# ggsave(plot = p3, filename = here("Outputs/Figures/mesh_range_10km_cutoff_50_ALL.png"), width = 10, height = 10, dpi = 300)
# 


# 2. MODEL FITTING --------------------------------------------------------

# Priors
my.control <- list(coord.names = c("x", "y"),
                   prior.mean = 0,
                   int.sd = 1000, # Intercept standard deviation
                   other.sd = 10, # Covariate effect standard deviation
                   addRandom = FALSE) # No random effect

my.control.GRF <- list(coord.names = c("x", "y"),
                       prior.mean = 0,
                       int.sd = 1000, # Intercept standard deviation
                       other.sd = 10, # Covariate effect standard deviation
                       prior.range = c(1, 0.1), # Prior chance 10% that parameter falls below range of 1m
                       prior.space.sigma = c(5, 0.1), # Prior chance 10% that parameter falls above SD of 5
                       addRandom = TRUE) # With random effect

# Distribution formula
distributionFormula <- ~0 + poly(sqrt_slope, 2) + poly(TWI, 2) + poly(northness, 2) + poly(log_dist_seasonal_water, 2) + poly(summer_temp, 2)


# Presence-Absence Model Fitting ------------------------------------------

m.PA <- isdm(observationList = list(PAdat = PA_vestfold),
             covars = covs,
             mesh = mesh.range.10km.cutoff.50,
             responseNames = c(PA = "presence"),
             sampleAreaNames = c(PA = "area"),
             distributionFormula = distributionFormula, 
             biasFormula = NULL, #Intercept only
             artefactFormulas = list(PA = ~1), # Intercept only
             control = my.control)



# Presence-Only Model Fitting ----------------------------------------------


m.PO <- isdm(observationList = list(POdat = PO), 
             covars = covs,
             mesh = mesh.range.10km.cutoff.50,
             responseNames = NULL,
             sampleAreaNames = NULL,
             distributionFormula = distributionFormula, 
             biasFormula = ~1,
             artefactFormulas = NULL,
             control = my.control)

m.PO.bias <- isdm(observationList = list(POdat = PO), 
                  covars = covs,
                  mesh = mesh.range.10km.cutoff.50,
                  responseNames = NULL,
                  sampleAreaNames = NULL,
                  distributionFormula = distributionFormula, 
                  biasFormula = ~1 + log_dist_station,
                  artefactFormulas = NULL,
                  control = my.control)


# Integrated Model Fitting ------------------------------------------------

m.int <- isdm(observationList = list(POdat = PO,
                                     PAdat = PA_vestfold),
              covars = covs,
              mesh = mesh.range.10km.cutoff.50,
              responseNames = c(PO = NULL, PA = "presence"),
              sampleAreaNames = c(PO = NULL, PA = "area"),
              distributionFormula = distributionFormula, 
              biasFormula = ~1, 
              artefactFormulas = list(PA = ~1), # Intercept only
              control = my.control) 

m.int.bias <- isdm(observationList = list(POdat = PO,
                                          PAdat = PA_vestfold),
                   covars = covs,
                   mesh = mesh.range.10km.cutoff.50,
                   responseNames = c(PO = NULL, PA = "presence"),
                   sampleAreaNames = c(PO = NULL, PA = "area"),
                   distributionFormula = distributionFormula, 
                   biasFormula = ~1 + log_dist_station, 
                   artefactFormulas = list(PA = ~1), # Intercept only
                   control = my.control) 



# Save all models to a list -----------------------------------------------

mod.list <- list(m.PA = m.PA,
                 m.PO = m.PO,
                 m.PO.bias = m.PO.bias,
                 m.int = m.int,
                 m.int.bias = m.int.bias)


# 3. EXTRACT MODEL RESULTS ------------------------------------------------

extrap.scenario.df <- extract_model_results_func(mod.list = mod.list)

write.csv(extrap.scenario.df,
          paste0(outpath, "/Model_results_table.csv"))



# 4. PREDICT FROM FITTED  -------------------------------------------------

# Presence-absence models -------------------------------------------------

# Add sampling area for prediction back to Vestfold
covs$sampAreaPA_Vestfold <- mask(rast(covs[[1]], vals=80), covs[[1]])

# Add sampling area for prediction to Bunger
covs$sampAreaPA_Bunger <- mask(rast(covs[[1]], vals=100), covs[[1]])

m.PA$preds.INT.Vestfold <- predict(m.PA, 
                                   covars = covs,
                                   habitatArea="sampAreaPA_Vestfold",
                                   S = 500,
                                   intercept.terms = "PA_Intercept",
                                   type = "intensity",
                                   includeRandom = F,
                                   includeFixed = T)

m.PA$preds.INT.Bunger <- predict(m.PA, 
                                 covars = covs,
                                 habitatArea="sampAreaPA_Bunger",
                                 S = 500,
                                 intercept.terms = "PA_Intercept",
                                 type = "intensity",
                                 includeRandom = F,
                                 includeFixed = T)

m.PA$preds.prob.Vestfold <- predict(m.PA, 
                                    covars = covs,
                                    habitatArea="sampAreaPA_Vestfold",
                                    S = 500,
                                    intercept.terms = "PA_Intercept",
                                    type = "probability",
                                    includeRandom = F,
                                    includeFixed = T)

m.PA$preds.prob.Bunger <- predict(m.PA, 
                                  covars = covs,
                                  habitatArea="sampAreaPA_Bunger",
                                  S = 500,
                                  intercept.terms = "PA_Intercept",
                                  type = "probability",
                                  includeRandom = F,
                                  includeFixed = T)

# Presence-only models -------------------------------------------------

m.PO$preds.INT <- predict(m.PO, 
                          covars = covs,
                          S = 500,
                          intercept.terms = "PO_Intercept",
                          type = "intensity",
                          includeRandom = F,
                          includeFixed = T)

m.PO$preds.prob <- predict(m.PO, 
                           covars = covs,
                           S = 500,
                           intercept.terms = "PO_Intercept",
                           type = "probability",
                           includeRandom = F,
                           includeFixed = T)

m.PO.bias$preds.INT <- predict(m.PO.bias, 
                               covars = covs,
                               S = 500,
                               intercept.terms = "PO_Intercept",
                               type = "intensity",
                               includeRandom = F,
                               includeFixed = T)

m.PO.bias$preds.prob <- predict(m.PO.bias, 
                                covars = covs,
                                S = 500,
                                intercept.terms = "PO_Intercept",
                                type = "probability",
                                includeRandom = F,
                                includeFixed = T)


# Integrated models -------------------------------------------------------

m.int$preds.INT.Vestfold <- predict(m.int, 
                                    covars = covs,
                                    habitatArea = "sampAreaPA_Vestfold",
                                    S = 500,
                                    intercept.terms = "PA_Intercept",
                                    type = "intensity",
                                    includeRandom = F,
                                    includeFixed = T)

m.int$preds.INT.Bunger <- predict(m.int, 
                                  covars = covs,
                                  habitatArea = "sampAreaPA_Bunger",
                                  S = 500,
                                  intercept.terms = "PA_Intercept",
                                  type = "intensity",
                                  includeRandom = F,
                                  includeFixed = T)

m.int$preds.prob.Vestfold <- predict(m.int, 
                                     covars = covs,
                                     habitatArea = "sampAreaPA_Vestfold",
                                     S = 500,
                                     intercept.terms = "PA_Intercept",
                                     type = "probability",
                                     includeRandom = F,
                                     includeFixed = T)

m.int$preds.prob.Bunger <- predict(m.int, 
                                   covars = covs,
                                   habitatArea = "sampAreaPA_Bunger",
                                   S = 500,
                                   intercept.terms = "PA_Intercept",
                                   type = "probability",
                                   includeRandom = F,
                                   includeFixed = T)

m.int.bias$preds.INT.Vestfold <- predict(m.int.bias, 
                                         covars = covs,
                                         habitatArea = "sampAreaPA_Vestfold",
                                         S = 500,
                                         intercept.terms = "PA_Intercept",
                                         type = "intensity",
                                         includeRandom = F,
                                         includeFixed = T)

m.int.bias$preds.INT.Bunger <- predict(m.int.bias, 
                                       covars = covs,
                                       habitatArea = "sampAreaPA_Bunger",
                                       S = 500,
                                       intercept.terms = "PA_Intercept",
                                       type = "intensity",
                                       includeRandom = F,
                                       includeFixed = T)

m.int.bias$preds.prob.Vestfold <- predict(m.int.bias, 
                                          covars = covs,
                                          habitatArea = "sampAreaPA_Vestfold",
                                          S = 500,
                                          intercept.terms = "PA_Intercept",
                                          type = "probability",
                                          includeRandom = F,
                                          includeFixed = T)

m.int.bias$preds.prob.Bunger <- predict(m.int.bias, 
                                        covars = covs,
                                        habitatArea = "sampAreaPA_Bunger",
                                        S = 500,
                                        intercept.terms = "PA_Intercept",
                                        type = "probability",
                                        includeRandom = F,
                                        includeFixed = T)

# Save all models to a list again-----------------------------------------

mod.list <- list(m.PA = m.PA,
                 m.PO = m.PO,
                 m.PO.bias = m.PO.bias,
                 m.int = m.int,
                 m.int.bias = m.int.bias)


# Plotting predictions ----------------------------------------------------

plot_predictions_func(mod.list = mod.list, 
                      outpath = outpath,
                      vestfold_boundary = vestfold_boundary,
                      bunger_boundary = bunger_boundary)


# Save output rasters -----------------------------------------------------

save_output_rasters_func(mod.list = mod.list,
                         outpath = outpath)


# Partial dependence plots -------------------------------------------------

# STOP & THINK: Do I want to run all models for this?

partial_dependence_func(mod.list = mod.list,
                        covs_no_bias = covs_no_bias,
                        outpath = outpath,
                        ice_free.EastAnt = ice_free.EastAnt)


