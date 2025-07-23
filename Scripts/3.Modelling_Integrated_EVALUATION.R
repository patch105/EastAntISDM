
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
library(DescTools, lib.loc = lib_loc)
library(precrec, lib.loc = lib_loc)
library(ecospat, lib.loc = lib_loc)
library(kuenm, lib.loc = lib_loc)
library(prg, lib.loc = lib_loc)

# Load some helper functions -----------------------------------------------

source(here("Scripts/Helper_functions_ISDM.R"))


# Set group ---------------------------------------------------------------

# group <- "Lichen"

 group <- "Moss"


# Set scenario ---------------------------------------------------------------

scenario = "500m_ALL_DATASETS_SEASON19"


# Load model predictions (no GRF) --------------------------------------------------

# inpath <- paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated/", group, "/", scenario )
# outpath <-  paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated/", group, "/", scenario )

inpath <- paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/", group, "/", scenario )
outpath <-  paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/", group, "/", scenario )

m.PA.Bunger <- list(preds.prob.Bunger = rast(paste0(inpath, "/BUNGER_Probability_prediction_m.PA_median.tif")))
m.PA.Vestfold <- list(preds.prob.Vestfold = rast(paste0(inpath, "/VESTFOLD_Probability_prediction_m.PA_median.tif")))

m.PO <- list(preds.prob = rast(paste0(inpath, "/Probability_prediction_m.PO_median.tif")))

m.PO.bias <- list(preds.prob = rast(paste0(inpath, "/Probability_prediction_m.PO.bias_median.tif")))

m.PO.Plantarctica <- list(preds.prob = rast(paste0(inpath, "/Probability_prediction_m.PO.Plantarctica_median.tif")))

m.int.occ.VH.Bunger <- list(preds.prob.Bunger = rast(paste0(inpath, "/BUNGER_Probability_prediction_m.int.occ.VH_median.tif")))
m.int.occ.VH.Vestfold <- list(preds.prob.Vestfold = rast(paste0(inpath, "/VESTFOLD_Probability_prediction_m.int.occ.VH_median.tif")))

m.int.occ.VH.bias.Bunger <- list(preds.prob.Bunger = rast(paste0(inpath, "/BUNGER_Probability_prediction_m.int.occ.VH.bias_median.tif")))
m.int.occ.VH.bias.Vestfold <- list(preds.prob.Vestfold = rast(paste0(inpath, "/VESTFOLD_Probability_prediction_m.int.occ.VH.bias_median.tif")))

m.int.Plantarctica.VH.Bunger <- list(preds.prob.Bunger = rast(paste0(inpath, "/BUNGER_Probability_prediction_m.int.Plantarctica.VH_median.tif")))
m.int.Plantarctica.VH.Vestfold <- list(preds.prob.Vestfold = rast(paste0(inpath, "/VESTFOLD_Probability_prediction_m.int.Plantarctica.VH_median.tif")))

m.int.occ.Plantarctica.VH.Bunger <- list(preds.prob.Bunger = rast(paste0(inpath, "/BUNGER_Probability_prediction_m.int.occ.Plantarctica.VH_median.tif")))
m.int.occ.Plantarctica.VH.Vestfold <- list(preds.prob.Vestfold = rast(paste0(inpath, "/VESTFOLD_Probability_prediction_m.int.occ.Plantarctica.VH_median.tif")))

# List them all
mod.list <- list(m.PA = m.PA.Bunger,
                 m.PO = m.PO,
                 m.PO.bias = m.PO.bias,
                 m.PO.Plantarctica = m.PO.Plantarctica,
                 m.int.occ.VH = m.int.occ.VH.Bunger,
                 m.int.occ.VH.bias = m.int.occ.VH.bias.Bunger,
                 m.int.Plantarctica.VH = m.int.Plantarctica.VH.Bunger,
                 m.int.occ.Plantarctica.VH = m.int.occ.Plantarctica.VH.Bunger)

# Load model predictions (w GRF) --------------------------------------------------

# inpath <- paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated/", group, "/", scenario )
# outpath <-  paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated/", group, "/", scenario )
# 
# m.PA.GRF <- list(preds.prob.Bunger = rast(paste0(inpath, "/BUNGER_Probability_prediction_m.PA.GRF_median.tif")))
# m.PO.GRF <- list(preds.prob = rast(paste0(inpath, "/Probability_prediction_m.PO.GRF_median.tif")))
# m.PO.bias <- list(preds.prob = rast(paste0(inpath, "/Probability_prediction_m.PO.bias_median.tif")))
# m.int <- list(preds.prob.Bunger = rast(paste0(inpath, "/BUNGER_Probability_prediction_m.int_median.tif")))
# m.int.bias <- list(preds.prob.Bunger = rast(paste0(inpath, "/BUNGER_Probability_prediction_m.int.bias_median.tif")))
# 
# # List them all
# mod.list <- list(m.PA = m.PA,
#                  m.PO = m.PO,
#                  m.PO.bias = m.PO.bias,
#                  m.int = m.int,
#                  m.int.bias = m.int.bias)




############################################
# Evaluate the predictions on BUNGER PA dataset ----------------
############################################

# Load the presence-absence records ---------------------------------------

PA_Bunger23_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_bunger23.shp"))

PA_Bunger23_Veg_df <- PA_Bunger23_Veg_sf %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(st_drop_geometry(PA_Bunger23_Veg_sf)) %>%
  rename(x = X, y = Y)

if(group == "Moss") {
  
  PA_bunger23 <- PA_Bunger23_Veg_df %>%
    dplyr::select(x, y, srfc_ms) %>%
    rename(presence = srfc_ms)
  
}

if(group == "Lichen") {
  
  PA_bunger23 <- PA_Bunger23_Veg_df %>%
    dplyr::select(x, y, srfc_lc) %>%
    rename(presence = srfc_lc)
}


# Doing this to get the xy coordinates for PA data to match predictions
# PA_bunger23_covs <- terra::extract(covs, PA_bunger23[, c("x", "y")], xy = T)
# PA_bunger23_covs <- cbind(PA_bunger23_covs, PA_bunger23["presence"])
PA_bunger23 <- PA_bunger23 %>%
  rename(Presence = presence)


# Evaluate the predictions on the Bunger PA dataset ----------------------

eval_df <- evaluate_prediction_raster_isdm(mod.list = mod.list,
                                           outpath = outpath,
                                           eval_dataset = PA_bunger23)

write.csv(eval_df, file = paste0(outpath, "/RISDM_eval_df.csv"))




############################################
# Evaluate the presence-only predictions on VESTFOLD PA dataset ----------------
############################################

mod.list <- list(m.PO = m.PO,
                 m.PO.bias = m.PO.bias,
                 m.PO.Plantarctica = m.PO.Plantarctica)

# Load the presence-absence records ---------------------------------------

PA_Vestfold_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_vestfold_19.shp"))
#PA_Vestfold_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_vestfold.shp"))


PA_Vestfold_Veg_df <- PA_Vestfold_Veg_sf %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  bind_cols(st_drop_geometry(PA_Vestfold_Veg_sf)) %>% 
  rename(x = X, y = Y)


# Format for modelling  ----------------------------------------------------

if(group == "Moss") {
  
  PA_Vestfold <- PA_Vestfold_Veg_df %>% 
    dplyr::select(x, y, srfc_ms) %>% 
    rename(Presence = srfc_ms)
  
}

if(group == "Lichen") {
  
  PA_Vestfold <- PA_Vestfold_Veg_df %>% 
    dplyr::select(x, y, srfc_lc) %>% 
    rename(Presence = srfc_lc)
  
}


eval_df <- evaluate_prediction_raster_isdm(mod.list = mod.list,
                                           outpath = outpath,
                                           eval_dataset = PA_Vestfold)

write.csv(eval_df, file = paste0(outpath, "/RISDM_PO_models_VESTFOLD_eval_df.csv"))


############################################
# Evaluate the MODEL FIT (PA & INTEGRATED) on VESTFOLD PA dataset  ---------
############################################

mod.list <- list(m.PA = m.PA.Vestfold,
                 m.int.occ.VH = m.int.occ.VH.Vestfold,
                 m.int.occ.VH.bias = m.int.occ.VH.bias.Vestfold,
                 m.int.Plantarctica.VH = m.int.Plantarctica.VH.Vestfold,
                 m.int.occ.Plantarctica.VH = m.int.occ.Plantarctica.VH.Vestfold)

# Load the presence-absence records ---------------------------------------

PA_Vestfold_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_vestfold_19.shp"))
#PA_Vestfold_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_vestfold.shp"))


PA_Vestfold_Veg_df <- PA_Vestfold_Veg_sf %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  bind_cols(st_drop_geometry(PA_Vestfold_Veg_sf)) %>% 
  rename(x = X, y = Y)


# Format for modelling  ----------------------------------------------------

if(group == "Moss") {
  
  PA_Vestfold <- PA_Vestfold_Veg_df %>% 
    dplyr::select(x, y, srfc_ms) %>% 
    rename(Presence = srfc_ms)
  
}

if(group == "Lichen") {
  
  PA_Vestfold <- PA_Vestfold_Veg_df %>% 
    dplyr::select(x, y, srfc_lc) %>% 
    rename(Presence = srfc_lc)
  
}



# Evaluate the predictions on the Vestfold PA dataset  -------------------

eval_df <- evaluate_fit_PA_raster_isdm(mod.list = mod.list,
                                       outpath = outpath,
                                       eval_dataset = PA_Vestfold)

write.csv(eval_df, file = paste0(outpath, "/RISDM_eval_PA_fit_df.csv"))




############################################
# Evaluate the MODEL FIT (PO) on PO dataset  ----------------------------
############################################

mod.list <- list(m.PO = m.PO,
                 m.PO.bias = m.PO.bias)

# Load the presence records -------------------------------------

PO_East_Ant_Veg_sf <- st_read(here("Data/Biological_records", "PO_Veg_East_Ant.shp"))

PO_East_Ant_Veg_df <- PO_East_Ant_Veg_sf %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  bind_cols(st_drop_geometry(PO_East_Ant_Veg_sf)) %>% 
  rename(x = X, y = Y) 

if(group == "Moss") {
  
  PO <- PO_East_Ant_Veg_df %>% 
    filter(vegtype == "Moss") %>% 
    dplyr::select(x, y) %>% 
    mutate(Presence = 1)
  
}

if(group == "Lichen") {
  
  PO <- PO_East_Ant_Veg_df %>% 
    filter(vegtype == "Lichen") %>% 
    dplyr::select(x, y) %>% 
    mutate(Presence = 1)
  
}


# Evaluate the predictions on the PO records ---------

eval_df <- evaluate_fit_PO_raster_isdm(mod.list = mod.list,
                                       outpath = outpath,
                                       PO = PO,
                                       type = "PO")

write.csv(eval_df, file = paste0(outpath, "/RISDM_eval_PO_fit_df.csv"))




############################################
# Evaluate the MODEL FIT (Plantarctica) on Plantarctica dataset  -------------
############################################

mod.list <- list(m.PO.Plantarctica = m.PO.Plantarctica)

# Load the presence-only records ------------------------------------------

ACBRS <- st_read(here("Data/Environmental_predictors/ACBRs_v2_2016.shp"), crs = 3031) %>% filter(ACBR_Name == "East Antarctica")
ACBRS_SPVE <- vect(ACBRS)

veg_map <- st_read(here("Data/Biological_records/PlantarcticaVegetationMap.shp")) %>%
  vect()

vestfold_boundary <- vect(here("Data/Environmental_predictors/vestfold_boundary.shp"))
bunger_boundary <- vect(here("Data/Environmental_predictors/bunger_boundary.shp"))

# Crop to East Antarctica
veg_map <- terra::crop(veg_map, ext(ACBRS_SPVE))

veg_map <- centroids(veg_map)

PO_East_Ant_Veg_sf <- st_as_sf(veg_map)

PO_East_Ant_Veg_df <- PO_East_Ant_Veg_sf %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  bind_cols(st_drop_geometry(PO_East_Ant_Veg_sf)) %>% 
  rename(x = X, y = Y) 


if(group == "Moss") {
  
  Plantarctica <- PO_East_Ant_Veg_df %>% 
    filter(Vegetation == "Vegetation") %>% 
    dplyr::select(x, y) %>% 
    mutate(Presence = 1)
  
}

if(group == "Lichen") {
  
  Plantarctica <- PO_East_Ant_Veg_df %>% 
    filter(Vegetation == "Lichen") %>%
    dplyr::select(x, y) %>% 
    mutate(Presence = 1)

}

# Evaluate the predictions on the PO records ---------

eval_df <- evaluate_fit_PO_raster_isdm(mod.list = mod.list,
                                       outpath = outpath,
                                       PO = Plantarctica,
                                       type = "Plantarctica")

write.csv(eval_df, file = paste0(outpath, "/RISDM_eval_Plantarctica_fit_df.csv"))

