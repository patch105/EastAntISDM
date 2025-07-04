
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

group <- "Lichen"
# group <- "Moss"


# Set scenario ---------------------------------------------------------------

scenario = "TEST"


# Set outpath -------------------------------------------------------------

outpath <- here("Outputs", "Integrated", group, scenario)

if(!dir.exists(outpath)) {
  dir.create(outpath, showWarnings = FALSE)
} 

############################################
# Evaluate the ensemble predictions on BUNGER PA dataset ----------------
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


# Load model predictions --------------------------------------------------

inpath <- paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated/", group)
outpath <-  paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated/", group)

m.PA <- list(preds.prob.Bunger = rast(paste0(inpath, "/BUNGER_Probability_prediction_m.PA_median.tif")))
m.PO <- list(preds.prob = rast(paste0(inpath, "/Probability_prediction_m.PO_median.tif")))
m.PO.bias <- list(preds.prob = rast(paste0(inpath, "/Probability_prediction_m.PO.bias_median.tif")))
m.int <- list(preds.prob.Bunger = rast(paste0(inpath, "/BUNGER_Probability_prediction_m.int_median.tif")))
m.int.bias <- list(preds.prob.Bunger = rast(paste0(inpath, "/BUNGER_Probability_prediction_m.int.bias_median.tif")))

# List them all
mod.list <- list(m.PA = m.PA,
                 m.PO = m.PO,
                 m.PO.bias = m.PO.bias,
                 m.int = m.int,
                 m.int.bias = m.int.bias)


# Evaluate the predictions on the Bunger PA dataset ----------------------

eval_df <- evaluate_prediction_raster_isdm(mod.list = mod.list,
                                           outpath = outpath)

write.csv(eval_df, file = paste0(outpath, "/RISDM_eval_df.csv"))
