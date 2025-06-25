
# Set up for running on HPC

# Set the library for packages
# lib_loc <- paste(dirname(getwd()),"/r_lib",sep="")

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

# Trim to Vestfold:

vestfold_boundary <- vect(here("Data/Environmental_predictors/vestfold_boundary.shp"))
# bunger_boundary <- vect(here("Data/Environmental_predictors/bunger_boundary.shp"))

ice_free.EastAnt <- crop(ice_free.EastAnt, ext(vestfold_boundary))

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
PA_bunger23 <- PA_bunger23 %>% mutate(area = 1)

# # Add together
# PA_both <- rbind(PA_vestfold, PA_bunger23)

# Load the covariates -----------------------------------------------------

# Bias covariate
dist_station <- rast(here("Data/Environmental_predictors/distance_to_station_EAST_ANTARCTICA.tif"))
names(dist_station) <- "dist_station"

log_dist_station <- log(dist_station+1)
names(log_dist_station) <- "log_dist_station"

# Stack covariates
covs <- log_dist_station

# Make sure that if any predictors are NA, all become NA

# # Here we're just exploiting the fact that sum will by default return NA when any layer has an NA
# covs <- terra::mask(covs, sum(covs))

# CROP COVARIATES TO THE VESTFOLD BOUNDARY
covs <- crop(covs, ext(vestfold_boundary))

# 1. MAKE THE MESH --------------------------------------------------------

# domain_poly <- st_read(here("Data/Environmental_predictors/rocks_Union_Land.shp"), crs = 3031)
# 
# domain_poly <- st_crop(domain_poly, ext(ice_free.EastAnt)) 
# 
# domain_poly_1km_buffer <- st_buffer(domain_poly, 1000)

# CROP ACBRS TO THE VESTFOLD BOUNDARY
ACBRS_buffer1km <- st_buffer(ACBRS, 1000) 
ACBRS_buffer1km <- st_union(ACBRS_buffer1km)

ACBRS_buffer1km <- crop(vect(ACBRS_buffer1km), ext(vestfold_boundary))

boundary <- fmesher::fm_as_segm(st_as_sf(ACBRS_buffer1km))

dep.range <- 10000 # 10km Set the range based on biology

# Crop PO to Vestfold
PO.sf <- crop(vect(PO.sf), ext(vestfold_boundary)) %>% 
  st_as_sf()

mesh.range.10km.cutoff.502 <- fmesher::fm_mesh_2d_inla(loc = st_coordinates(PO.sf),
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

 p1 <- ggplot() +
   inlabru::gg(mesh.range.10km.cutoff.50) +
   # gg(mesh.vrt, color = "red") +
   geom_sf(data = PO.sf, color = "red")

 p2 <- ggplot() +
   inlabru::gg(mesh.range.10km.cutoff.50) +
   # gg(mesh.vrt, color = "red") +
   coord_sf(
     xlim = c(st_bbox(bunger_boundary)$xmin, st_bbox(bunger_boundary)$xmax),
     ylim = c(st_bbox(bunger_boundary)$ymin, st_bbox(bunger_boundary)$ymax))

 p3 <- ggplot() +
   inlabru::gg(mesh.range.10km.cutoff.50)

 ggsave(plot = p1, filename = here("Outputs/Testing/v2mesh_range_10km_cutoff_50_VESTFOLD.png"), width = 10, height = 10, dpi = 300)
ggsave(plot = p2, filename = here("Outputs/Figures/mesh_range_10km_cutoff_50_BUNGER.png"), width = 10, height = 10, dpi = 300)
ggsave(plot = p3, filename = here("Outputs/Figures/mesh_range_10km_cutoff_50_ALL.png"), width = 10, height = 10, dpi = 300)



# 2. MODEL FITTING --------------------------------------------------------

# Priors

my.control <- list(coord.names = c("x", "y"),
                       prior.mean = 0,
                       int.sd = 1000, # Intercept standard deviation
                       other.sd = 10, # Covariate effect standard deviation
                       prior.range = c(1, 0.1), # Prior chance 10% that parameter falls below range of 1m
                       prior.space.sigma = c(5, 0.1), # Prior chance 10% that parameter falls above SD of 5
                       addRandom = FALSE) # Without random effect

my.control.GRF <- list(coord.names = c("x", "y"),
                       prior.mean = 0,
                       int.sd = 1000, # Intercept standard deviation
                       other.sd = 10, # Covariate effect standard deviation
                       prior.range = c(1, 0.1), # Prior chance 10% that parameter falls below range of 1m
                       prior.space.sigma = c(5, 0.1), # Prior chance 10% that parameter falls above SD of 5
                       addRandom = TRUE) # With random effect


# Presence-Absence Model Fitting ------------------------------------------

m.PA.GRF <- isdm(observationList = list(PAdat = PA_vestfold),
                    covars = covs,
                    mesh = mesh.range.10km.cutoff.50,
                    responseNames = c(PA = "presence"),
                    sampleAreaNames = c(PA = "area"),
                    distributionFormula = ~0 + log_dist_station, # Linear w covs
                    biasFormula = NULL, #Intercept only
                    artefactFormulas = list(PA = ~1), # Intercept only
                    control = my.control.GRF)

print(paste0("Time is: ", Sys.time()))
Print("Fitted m.PA.no.GRF")


m.PA.no.GRF <- isdm(observationList = list(PAdat = PA_vestfold),
                 covars = covs,
                 mesh = mesh.range.10km.cutoff.50,
                 responseNames = c(PA = "presence"),
                 sampleAreaNames = c(PA = "area"),
                 distributionFormula = ~0 + log_dist_station, # Linear w covs
                 biasFormula = NULL, #Intercept only
                 artefactFormulas = list(PA = ~1), # Intercept only
                 control = my.control)


# Print the coefficient
summary(m.PA.GRF)

summary(m.PA.no.GRF)


# Predict & plot the GRF ----------------------------------------------------

# Prediction shouldn't include an intercept
m.PA.GRF$preds.GRF <- predict(m.PA.GRF,
                             covars = covs,
                             S = 500,
                             intercept.terms = NULL,
                             type = "link",
                             includeRandom = T,
                             includeFixed = F)


plot(m.PA.GRF$preds.GRF$field[[1]], nc = 3)
plot(vect(PA_vestfold.sf), add = T)

# Predict & plot the log intensity ----------------------------------------

m.PA.no.GRF$preds.link <- predict(m.PA.no.GRF, 
                                  covars = covs,
                                  S = 500,
                                  intercept.terms = "PA_Intercept",
                                  type = "link",
                                  includeRandom = F,
                                  includeFixed = T)


m.PA.GRF$preds.link <- predict(m.PA.GRF, 
                               covars = covs,
                               S = 500,
                               intercept.terms = "PA_Intercept",
                               type = "link",
                               includeRandom = T,
                               includeFixed = T)


plot(m.PA.no.GRF$preds.link$field[[1]], nc = 3)
plot(m.PA.GRF$preds.link$field[[1]], nc = 3)


#####################################
# Now trying the same for PO data -----------------------------------------
######################################

m.PO.GRF <- isdm(observationList = list(POdat = PO), 
                 covars = covs,
                 mesh = mesh.range.10km.cutoff.50,
                 responseNames = NULL,
                 sampleAreaNames = NULL,
                 distributionFormula = ~0 + log_dist_station, # Linear w covs
                 biasFormula = ~1,
                 artefactFormulas = NULL,
                 control = my.control.GRF)

print(paste0("Time is: ", Sys.time()))
Print("Fitted m.PA.no.GRF")


m.PO.no.GRF <- isdm(observationList = list(POdat = PO), 
                    covars = covs,
                    mesh = mesh.range.10km.cutoff.50,
                    responseNames = NULL,
                    sampleAreaNames = NULL,
                    distributionFormula = ~0 + log_dist_station, # Linear w covs
                    biasFormula = ~1,
                    artefactFormulas = NULL,
                    control = my.control)


# Print the coefficient
summary(m.PO.GRF)

summary(m.PO.no.GRF)


# Predict & plot the GRF ----------------------------------------------------

# Prediction shouldn't include an intercept
m.PO.GRF$preds.GRF <- predict(m.PO.GRF,
                              covars = covs,
                              S = 500,
                              intercept.terms = NULL,
                              type = "link",
                              includeRandom = T,
                              includeFixed = F)


plot(m.PO.GRF$preds.GRF$field[[1]], nc = 3)
plot(vect(PO.sf), add = T)

# Predict & plot the log intensity ----------------------------------------

m.PO.no.GRF$preds.link <- predict(m.PO.GRF, 
                                  covars = covs,
                                  S = 500,
                                  intercept.terms = "PO_Intercept",
                                  type = "link",
                                  includeRandom = F,
                                  includeFixed = T)


m.PO.GRF$preds.link <- predict(m.PO.GRF, 
                               covars = covs,
                               S = 500,
                               intercept.terms = "PO_Intercept",
                               type = "intensity",
                               includeRandom = T,
                               includeFixed = T)


plot(m.PO.no.GRF$preds.link$field[[1]], nc = 3)
plot(m.PO.GRF$preds.link$field[[1]], nc = 3)















# Trying model with smaller sample area (makes it a lot larger the intensity values)
# PA_vestfold$area <- 1
# m.PA.GRF.1km <- isdm(observationList = list(PAdat = PA_vestfold),
#                      covars = covs,
#                      mesh = mesh.range.10km.cutoff.50,
#                      responseNames = c(PA = "presence"),
#                      sampleAreaNames = c(PA = "area"),
#                      distributionFormula = ~0 + sqrt_slope, # Linear w covs
#                      biasFormula = NULL, #Intercept only
#                      artefactFormulas = list(PA = ~1), # Intercept only
#                      control = my.control)


m.PA.no.GRF$preds.INT <- predict(m.PA.no.GRF, 
                                          covars = covs,
                                          S = 500,
                                          intercept.terms = "PA_Intercept",
                                          type = "intensity",
                                          includeRandom = F,
                                          includeFixed = T)

m.PA.no.GRF$preds.INT.nohabitat <- predict(m.PA.no.GRF, 
                                 covars = covarsForInter,
                                 S = 500,
                                 intercept.terms = "PA_Intercept",
                                 habitatArea = "tmp.habiArea",
                                 type = "intensity",
                                 includeRandom = F,
                                 includeFixed = T)


plot(m.PA.no.GRF$preds.INT$field[[1]], nc = 3)
plot(m.PA.no.GRF$preds.INT.nohabitat$field[[1]], nc = 3)

m.PA.no.GRF$preds.link <- predict(m.PA.no.GRF, 
                                 covars = covs,
                                 S = 500,
                                 intercept.terms = "PA_Intercept",
                                 type = "link",
                                 includeRandom = F,
                                 includeFixed = T)

plot(m.PA.no.GRF$preds.link$field[[1]], nc = 3)


m.PA.no.GRF$preds.prob <- predict(m.PA.no.GRF, 
                                  covars = covs,
                                  S = 500,
                                  intercept.terms = "PA_Intercept",
                                  type = "probability",
                                  includeRandom = F,
                                  includeFixed = T)

plot(m.PA.no.GRF$preds.prob$field[[1]], nc = 3)



# 1) create a raster layer with a constant value for the habitatArea offset, 
# 2) predict specifying which terms should be included, and 
# 3) plot

#adding a temporary cell area layer
covarsForInter <- c( covs, covs[[1]])
names( covarsForInter) <- c( names( covs), "tmp.habiArea")

# area is now constant with log(1)=0 contribution
values( covarsForInter$tmp.habiArea) <- 1

covarsForInter <- covarsForInter[[c("sqrt_slope", "tmp.habiArea")]]

covarsForInter$tmp.habiArea <- mask(covarsForInter$tmp.habiArea, ice_free.EastAnt)

#You could use a much(!) larger value of S.
interpPreds <- predict( m.PA.no.GRF, 
                        covars=covarsForInter,
                        habitatArea="tmp.habiArea", 
                        S=500,
                        includeFixed="sqrt_slope", 
                        includeRandom=FALSE, 
                        type="link")

#compile covariate and prediction
pred.df <- as.data.frame( cbind( slope=values( covs$sqrt_slope),
                                 values( interpPreds$field[[c("Median","Lower","Upper")]])))
#plot
pred.df <- pred.df[!is.na( pred.df$sqrt_slope),]
pred.df <- pred.df[order( pred.df$sqrt_slope),]
matplot( pred.df[,1], pred.df[,2:4], pch="", xlab="sqrt slope", ylab="Effect",
         main="Effect plot for sqrt slope")
polygon( x=c( pred.df$sqrt_slope, rev( pred.df$sqrt_slope)),
         c(pred.df$Upper, rev(pred.df$Lower)),
         col=grey(0.95), bor=NA)
lines( pred.df[,c("sqrt_slope","Median")], type='l', lwd=2)


###############
# PO only model -----------------------------------------------------------
###############

PO <- PO.sf %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  bind_cols(st_drop_geometry(PO.sf)) %>% 
  rename(x = X, y = Y) %>% 
  select(x, y)


m.PO.no.GRF <- isdm(observationList = list(POdat = PO), 
                    covars = covs,
                    mesh = mesh.range.10km.cutoff.50,
                    responseNames = NULL,
                    sampleAreaNames = NULL,
                    distributionFormula = ~0 + sqrt_slope, # Linear w covs
                    biasFormula = ~1,
                    artefactFormulas = NULL,
                    control = my.control)

m.PO.no.GRF$preds.INT <- predict(m.PO.no.GRF, 
                                 covars = covs,
                                 S = 500,
                                 intercept.terms = "PO_Intercept",
                                 type = "intensity",
                                 includeRandom = F,
                                 includeFixed = T)


plot(m.PO.no.GRF$preds.INT$field[[1]], nc = 3)

m.PO.no.GRF$preds.prob <- predict(m.PO.no.GRF, 
                                 covars = covs,
                                 S = 500,
                                 intercept.terms = "PO_Intercept",
                                 type = "probability",
                                 includeRandom = F,
                                 includeFixed = T)


plot(m.PO.no.GRF$preds.prob$field[[1]], nc = 3)


# Adding bias field -------------------------------------------------------



#############################################
# RANDOM FIELD TESTING ----------------------------------------------------
##############################################

m.PA.GRF$preds.GRF <- predict(m.PA.GRF, 
                              covars = covs,
                              S = 500,
                              intercept.terms = "PA_Intercept",
                              type = "intensity",
                              includeRandom = T,
                              includeFixed = F)

png(paste0(output.path, "/GRF.plot_m.PA.GRF.png"),width = 10, height = 10, units = "in", res = 300)
plot(m.PA.GRF$preds.GRF$field[[1:3]], nc = 3)
dev.off()

plot(m.PA.GRF$preds.GRF$field[[1]])

summary(m.PA.GRF)

plot(m.PA.no.GRF, nFigRow = 2, ask = FALSE)

# Fixed effect 
m.PA.GRF$preds.INT <- predict(m.PA.GRF, 
                              covars = covs,
                              S = 500,
                              intercept.terms = "PA_Intercept",
                              type = "link",
                              includeRandom = F,
                              includeFixed = T)

plot(covs[["sqrt_slope"]])
plot(m.PA.GRF$preds.INT$field[[1]])

PA_vestfold_covs <- terra::extract(covs, PA_vestfold[, c("x", "y")], xy = T)
PA_vestfold_covs <- cbind(PA_vestfold_covs, PA_vestfold["presence"])

ggplot() +
        geom_density(data = PA_vestfold_covs, 
                     aes(x = .data[[names(PA_vestfold_covs)[5]]], fill = as.factor(presence)), 
                     alpha = 0.5) +
        theme_bw() +
        labs(title = names(PA_vestfold_covs)[5])


m.PA.GRF$preds.randomfixed <- predict(m.PA.GRF, 
                              covars = covs,
                              S = 500,
                              intercept.terms = "PA_Intercept",
                              type = "link",
                              includeRandom = T,
                              includeFixed = T)

plot(m.PA.GRF$preds.randomfixed$field[[1]])

m.PA.GRF$preds.randomfixed.INT <- predict(m.PA.GRF, 
                                      covars = covs,
                                      S = 500,
                                      intercept.terms = "PA_Intercept",
                                      type = "intensity",
                                      includeRandom = T,
                                      includeFixed = T)

plot(m.PA.GRF$preds.randomfixed.INT$field[[1]])

m.PA.GRF$preds.randomfixed.PROB <- predict(m.PA.GRF, 
                                          covars = covs,
                                          S = 500,
                                          intercept.terms = "PA_Intercept",
                                          type = "probability",
                                          includeRandom = T,
                                          includeFixed = T)

plot(m.PA.GRF$preds.randomfixed.PROB$field[[1]])


#############################################
# PO now ----------------------------------------------------
#############################################
m.PO.GRF <- isdm(observationList = list(POdat = PO), 
                    covars = covs,
                    mesh = mesh.range.10km.cutoff.50,
                    responseNames = NULL,
                    sampleAreaNames = NULL,
                    distributionFormula = ~0 + sqrt_slope, # Linear w covs
                    biasFormula = ~1,
                    artefactFormulas = NULL,
                    control = my.control.GRF)

m.PO.no.GRF$preds.INT <- predict(m.PO.no.GRF, 
                                 covars = covs,
                                 S = 500,
                                 intercept.terms = "PO_Intercept",
                                 type = "intensity",
                                 includeRandom = F,
                                 includeFixed = T)

#############################################
# Integrated now ----------------------------------------------------
#############################################

m.int.GRF <- isdm(observationList = list(POdat = PO,
                                         PAdat = PA_vestfold),
                  covars = covs,
                  mesh = mesh.range.10km.cutoff.50,
                  responseNames = c(PO = NULL, PA = "presence"),
                  sampleAreaNames = c(PO = NULL, PA = "area"),
                  distributionFormula = ~0 + sqrt_slope, # Linear w covs
                  biasFormula = ~1, 
                  artefactFormulas = list(PA = ~1), # Intercept only
                  control = my.control.GRF) 

m.int.no.GRF <- isdm(observationList = list(POdat = PO,
                                         PAdat = PA_vestfold),
                  covars = covs,
                  mesh = mesh.range.10km.cutoff.50,
                  responseNames = c(PO = NULL, PA = "presence"),
                  sampleAreaNames = c(PO = NULL, PA = "area"),
                  distributionFormula = ~0 + sqrt_slope, # Linear w covs
                  biasFormula = ~1, 
                  artefactFormulas = list(PA = ~1), # Intercept only
                  control = my.control) 

############ PREDICT THE GRF ################

m.PA.GRF$preds.GRF <- predict(m.PA.GRF, 
                              covars = covs,
                              S = 500,
                              intercept.terms = "PA_Intercept",
                              type = "intensity",
                              includeRandom = T,
                              includeFixed = F)

plot(m.PA.GRF$preds.GRF$field[[1]])

m.PO.GRF$preds.GRF <- predict(m.PO.GRF, 
                              covars = covs,
                              S = 500,
                              intercept.terms = "PO_Intercept",
                              type = "intensity",
                              includeRandom = T,
                              includeFixed = F)

plot(m.PO.GRF$preds.GRF$field[[1]])

m.int.GRF$preds.GRF <- predict(m.int.GRF, 
                                 covars = covs,
                                 S = 500,
                                 intercept.terms = "PA_Intercept",
                                 type = "intensity",
                                 includeRandom = T,
                                 includeFixed = F)

plot(m.int.GRF$preds.GRF$field[[1]])
