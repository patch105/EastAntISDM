### NOTE - I HAD ONLY GONE THROUGH HALF OF THE PLOTS HERE TO MAKE A BUNGER AND VESTFOLD PLOT, MORE TO DO



# Set up for running on HPC

# Set the library for packages
lib_loc <- paste(dirname(getwd()),"/r_lib",sep="")

# Non-HPC version
# lib_loc = .libPaths()

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

# packages <- c("here", "sf", "terra", "dplyr", "data.table", "tidyr", "bdc", "viridis", "ggplot2", "tidyterra", "stringr")


# Set group ---------------------------------------------------------------

group <- "Lichen"
# group <- "Moss"

# Set outpath -------------------------------------------------------------

outpath <- here("Outputs", "Integrated", group)

if(!dir.exists(outpath)) {
  dir.create(outpath, showWarnings = FALSE)
} 

# Domain setup ------------------------------------------------------------

# Load the ice-free areas
# ice_free <- rast(here("Data/Environmental_predictors/ice_free_union_reproj_100m.tif"))
ice_free <- rast(here("Data/Environmental_predictors/ice_free_upsamp_1km.tif"))

# Load the Antarctic Conservation Biogeographic Regions, filter to East Antarctica
ACBRS <- st_read(here("Data/Environmental_predictors/ACBRs_v2_2016.shp"), crs = 3031) %>% filter(ACBR_Name == "East Antarctica")
ACBRS_SPVE <- vect(ACBRS)


# Also trim ice-free land to just East Antarctica
ice_free.EastAnt <- terra::crop(ice_free, ext(ACBRS_SPVE))

ice_free.EastAnt <- ifel(not.na(ice_free.EastAnt), 1, NA)


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


# Load Vestfold and Bunger boundary for plotting --------------------------

vestfold_boundary <- vect(here("Data/Environmental_predictors/vestfold_boundary.shp"))
bunger_boundary <- vect(here("Data/Environmental_predictors/bunger_boundary.shp"))

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
PA_bunger23 <- PA_bunger23 %>% mutate(area = 1)

# Add together
PA_both <- rbind(PA_vestfold, PA_bunger23)


# Load the covariates -----------------------------------------------------

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

# Apply some transformations
sqrt_slope <- sqrt(slope)
log_dist_seasonal_water <- log(dist_seasonal_water+1)
log_dist_station <- log(dist_station+1)

# Stack covariates
covs <- c(TWI, sqrt_slope, northness, dist_vertebrates, log_dist_seasonal_water, log_dist_station)
  
# Make sure that if any predictors are NA, all become NA

# Here we're just exploiting the fact that sum will by default return NA when any layer has an NA
covs <- terra::mask(covs, sum(covs))

# # Check covariate correlations --------------------------------------------
# 
# library(ecospat)
# library(usdm)
# 
# ecospat.cor.plot(covs)
# 
# usdm::vifstep(covs)



# Plot environmental variables where there's PO, PA data ------------------

PO_covs <- terra::extract(covs, PO[, c("x", "y")], xy = T)

PA_vestfold_covs <- terra::extract(covs, PA_vestfold[, c("x", "y")], xy = T)
PA_vestfold_covs <- cbind(PA_vestfold_covs, PA_vestfold["presence"])

PA_bunger23_covs <- terra::extract(covs, PA_bunger23[, c("x", "y")], xy = T)
PA_bunger23_covs <- cbind(PA_bunger23_covs, PA_bunger23["presence"])

PA_both_covs <- terra::extract(covs, PA_both[, c("x", "y")], xy = T)
PA_both_covs <- cbind(PA_both_covs, PA_both["presence"])

for(i in 1:ncol(PA_vestfold_covs)) {
  
  print(ggplot() +
          geom_density(data = PA_vestfold_covs, 
                       aes(x = .data[[names(PA_vestfold_covs)[i]]], fill = as.factor(presence)), 
                       alpha = 0.5) +
          theme_bw() +
          labs(title = names(PA_vestfold_covs)[i]))
  
}

for(i in 1:ncol(PA_bunger23_covs)) {
  
  print(ggplot() +
          geom_density(data = PA_bunger23_covs, 
                       aes(x = .data[[names(PA_bunger23_covs)[i]]], fill = as.factor(presence)), 
                       alpha = 0.5) +
          theme_bw() +
          labs(title = names(PA_bunger23_covs)[i]))
  
}

for(i in 1:ncol(PA_both_covs)) {
  
  print(ggplot() +
          geom_density(data = PA_both_covs, 
                       aes(x = .data[[names(PA_both_covs)[i]]], fill = as.factor(presence)), 
                       alpha = 0.5) +
          theme_bw() +
          labs(title = names(PA_both_covs)[i]))
  
}

# 1. MAKE THE MESH --------------------------------------------------------

# domain_poly <- st_read(here("Data/Environmental_predictors/rocks_Union_Land.shp"), crs = 3031)
# 
# domain_poly <- st_crop(domain_poly, ext(ice_free.EastAnt)) 
# 
# domain_poly_1km_buffer <- st_buffer(domain_poly, 1000)

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

#saveRDS(mesh.range.10km.cutoff.50, file = "mesh.range.10km.cutoff.50.rds")
mesh.range.10km.cutoff.50 <- readRDS(file = "mesh.range.10km.cutoff.50.rds")

# # Plot the mesh:
# 
#  p1 <- ggplot() +
#    inlabru::gg(mesh.range.10km.cutoff.50) +
#    # gg(mesh.vrt, color = "red") +
#    coord_sf(
#      xlim = c(st_bbox(vestfold_boundary)$xmin, st_bbox(vestfold_boundary)$xmax),
#      ylim = c(st_bbox(vestfold_boundary)$ymin, st_bbox(vestfold_boundary)$ymax))
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
#  ggsave(plot = p1, filename = here("Outputs/Figures/mesh_range_10km_cutoff_50_VESTFOLD.png"), width = 10, height = 10, dpi = 300)
# ggsave(plot = p2, filename = here("Outputs/Figures/mesh_range_10km_cutoff_50_BUNGER.png"), width = 10, height = 10, dpi = 300)
# ggsave(plot = p3, filename = here("Outputs/Figures/mesh_range_10km_cutoff_50_ALL.png"), width = 10, height = 10, dpi = 300)


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


# Integrated Model Fitting ------------------------------------------------

# m.int.Intercept <- isdm(observationList = list(POdat = PO,
#                                                PAdat = PA_bunger23),
#                         covars = covs,
#                         mesh = mesh.range.10km.cutoff.50,
#                         responseNames = c(PO = NULL, PA = "presence"),
#                         sampleAreaNames = c(PO = NULL, PA = "area"),
#                         distributionFormula = ~0, # Intercept only
#                         biasFormula = ~1, # Intercept only
#                         artefactFormulas = list(PA = ~1), # Intercept only
#                         control = my.control)


m.int.no.GRF <- isdm(observationList = list(POdat = PO,
                                            PAdat = PA_both),
                     covars = covs,
                     mesh = mesh.range.10km.cutoff.50,
                     responseNames = c(PO = NULL, PA = "presence"),
                     sampleAreaNames = c(PO = NULL, PA = "area"),
                     distributionFormula = ~0 + poly(slope, 2), # Linear w covs
                     biasFormula = ~1 + dist_station, 
                     artefactFormulas = list(PA = ~1), # Intercept only
                     control = my.control)    

print(paste0("Time is: ", Sys.time()))
Print("Fitted m.int.no.GRF")

m.int.GRF <- isdm(observationList = list(POdat = PO,
                                         PAdat = PA_both),
                  covars = covs,
                  mesh = mesh.range.10km.cutoff.50,
                  responseNames = c(PO = NULL, PA = "presence"),
                  sampleAreaNames = c(PO = NULL, PA = "area"),
                  distributionFormula = ~0 + dist_seasonal_water, # Linear w covs
                  biasFormula = ~1 + dist_station, 
                  artefactFormulas = list(PA = ~1), # Intercept only
                  control = my.control.GRF)    

print(paste0("Time is: ", Sys.time()))
Print("Fitted m.int.GRF")

# Presence-Only Model Fitting ---------------------------------------------


m.PO.no.GRF <- isdm(observationList = list(POdat = PO), 
                    covars = covs,
                    mesh = mesh.range.10km.cutoff.50,
                    responseNames = NULL,
                    sampleAreaNames = NULL,
                    distributionFormula = ~0 + slope, # Linear w covs
                    biasFormula = ~1 + dist_station,
                    artefactFormulas = NULL,
                    control = my.control)

print(paste0("Time is: ", Sys.time()))
Print("Fitted m.PO.no.GRF")

m.PO.GRF <- isdm(observationList = list(POdat = PO), 
                 covars = covs,
                 mesh = mesh.range.10km.cutoff.50,
                 responseNames = NULL,
                 sampleAreaNames = NULL,
                 distributionFormula = ~0 + slope + northness + twi + dist_seasonal_water + dist_vertebrates, # Linear w covs
                 biasFormula = ~1 + dist_station,
                 artefactFormulas = NULL,
                 control = my.control.GRF)

print(paste0("Time is: ", Sys.time()))
Print("Fitted m.PO.GRF")

# Presence-Absence Model Fitting ------------------------------------------

#Intercept only

m.PA.no.GRF <- isdm(observationList = list(PAdat = PA_both),
                    covars = covs,
                    mesh = mesh.range.10km.cutoff.50,
                    responseNames = c(PA = "presence"),
                    sampleAreaNames = c(PA = "area"),
                    distributionFormula = ~0 + slope, # Linear w covs
                    biasFormula = NULL, #Intercept only
                    artefactFormulas = list(PA = ~1), # Intercept only
                    control = my.control)

print(paste0("Time is: ", Sys.time()))
Print("Fitted m.PA.no.GRF")

m.PA.GRF <- isdm(observationList = list(PAdat = PA_bunger23),
                 covars = covs,
                 mesh = mesh.range.10km.cutoff.50,
                 responseNames = c(PA = "presence"),
                 sampleAreaNames = c(PA = "area"),
                 distributionFormula = ~0 + slope + northness + twi + dist_seasonal_water + dist_vertebrates, # Linear w covs
                 biasFormula = NULL, #Intercept only
                 artefactFormulas = list(PA = ~1), # Intercept only
                 control = my.control.GRF)

print(paste0("Time is: ", Sys.time()))
Print("Fitted m.PA.GRF")

# Stack models as a list
# mod.list <- list(integrated.no.GRF = m.int.no.GRF,
#                  integrated.GRF = m.int.GRF,
#                  PO.no.GRF = m.PO.no.GRF,
#                  PO.GRF = m.PO.GRF,
#                  PA.no.GRF = m.PA.no.GRF,
#                  PA.GRF = m.PA.GRF)

mod.list <- list(integrated.no.GRF = m.int.no.GRF, 
                 PO.no.GRF = m.PO.no.GRF, 
                 PA.no.GRF = m.PA.no.GRF)


# 3. MODEL EVALUATION -----------------------------------------------------

output.path <- here("Outputs/Figures")

# Summary
map(mod.list, function(x) { summary(x)})

## Residual plots
for(i in seq_along(mod.list)) {
  
  png(paste0(output.path, "/plot.isdm_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
  
  plot(mod.list[[i]], nFigRow = 2, ask = FALSE)
  
  dev.off()
  
}


# 4. Partial effects plots ------------------------------------------------

# 1) create a raster layer with a constant value for the habitatArea offset, 
# 2) predict specifying which terms should be included, and 
# 3) plot

#adding a temporary cell area layer
covarsForInter <- c( covs, covs[[1]])
names( covarsForInter) <- c( names( covs), "tmp.habiArea")

# area is now constant with log(1)=0 contribution
values( covarsForInter$tmp.habiArea) <- 1

covarsForInter <- covarsForInter[[c("slope", "tmp.habiArea")]]

covarsForInter$tmp.habiArea <- mask(covarsForInter$tmp.habiArea, ice_free.EastAnt)

#You could use a much(!) larger value of S.
# * NOTE - shouldn't use anything other than 'link' here
interpPreds <- predict( m.int.no.GRF, 
                        covars=covarsForInter,
                        habitatArea="tmp.habiArea", 
                        S=500,
                        includeFixed="slope", 
                        includeRandom=FALSE, 
                        type="link")

#compile covariate and prediction
pred.df <- as.data.frame( cbind( slope=values( covs$slope),
                                 values( interpPreds$field[[c("Median","Lower","Upper")]])))
#plot
pred.df <- pred.df[!is.na( pred.df$slope),]
pred.df <- pred.df[order( pred.df$slope),]
matplot( pred.df[,1], pred.df[,2:4], pch="", xlab="slope", ylab="Effect",
         main="Effect plot for slope")
polygon( x=c( pred.df$slope, rev( pred.df$slope)),
         c(pred.df$Upper, rev(pred.df$Lower)),
         col=grey(0.95), bor=NA)
lines( pred.df[,c("slope","Median")], type='l', lwd=2)





# 4. PREDICTION EAST ANT  -----------------------------------------------------

# Run for intensity predictions first

for(i in seq_along(mod.list)) {
  
  if(names(mod.list)[i] == "PA.no.GRF" || names(mod.list)[i] == "PA.GRF") {
    
    mod.list[[i]]$preds.INT.EASTANT <- predict(mod.list[[i]],
                                                covars = covs,
                                                S = 500, 
                                                intercept.terms = "PA_Intercept",
                                                type = "intensity",
                                                includeRandom = T)
    
    # Write prediction 
    writeRaster(mod.list[[i]]$preds.INT.EASTANT$field[[2]], 
                filename = paste0(output.path, "/pred.int.EastAnt_", names(mod.list)[i], ".tif"), 
                overwrite = T)
    
    # Crop raster to extent of smaller areas
    pred_crop_vestfold <- crop(mod.list[[i]]$preds.INT.EASTANT$field[[2]], ext(vestfold_boundary))
    pred_crop_bunger <- crop(mod.list[[i]]$preds.INT.EASTANT$field[[2]], ext(bunger_boundary))
    
    png(paste0(output.path, "/plot.pred.int.VESTFOLD_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(pred_crop_vestfold)                           
    
    dev.off()
    
    png(paste0(output.path, "/plot.pred.int.BUNGER_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(pred_crop_bunger)                          
    
    dev.off()
    
    # Run for probability next 

    mod.list[[i]]$preds.PROB.EASTANT <- predict(mod.list[[i]],
                                                 covars = covs,
                                                 S = 500, 
                                                 intercept.terms = "PA_Intercept",
                                                 type = "probability",
                                                 includeRandom = T)
    
    # Crop raster to extent of smaller areas
    pred_crop_vestfold <- crop(mod.list[[i]]$preds.PROB.EASTANT$field[[2]], ext(vestfold_boundary))
    pred_crop_bunger <- crop(mod.list[[i]]$preds.PROB.EASTANT$field[[2]], ext(bunger_boundary))
    
    png(paste0(output.path, "/plot.pred.prob.VESTFOLD_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(pred_crop_vestfold)                           
    
    dev.off()
    
    png(paste0(output.path, "/plot.pred.prob.BUNGER_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(pred_crop_bunger)                           
    
    dev.off()
    
    
  } else {
    
    mod.list[[i]]$preds.INT.EASTANT <- predict(mod.list[[i]],
                                                covars = covs,
                                                S = 500, 
                                                intercept.terms = "PO_Intercept",
                                                type = "intensity",
                                                includeRandom = T)
    
    png(paste0(output.path, "/plot.pred.int.EastAnt_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(mod.list[[i]]$preds.INT.EASTANT$field[[2]])                           
    
    dev.off()
    
    
    # Run for probability next 
    mod.list[[i]]$preds.PROB.EASTANT <- predict(mod.list[[i]],
                                                 covars = covs,
                                                 S = 500, 
                                                 intercept.terms = "PO_Intercept",
                                                 type = "probability",
                                                 includeRandom = T)
    
    png(paste0(output.path, "/plot.pred.prob.EastAnt_", names(mod.list)[i], ".png"), width = 10, height = 10, units = "in", res = 300)
    
    plot(mod.list[[i]]$preds.PROB.EASTANT$field[[2]])                           
    
    dev.off()
  }}


