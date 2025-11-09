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
library(flexsdm, lib.loc=lib_loc)
library(ecospat, lib.loc = lib_loc)
library(usdm, lib.loc = lib_loc)

library(tidyterra, lib.loc = lib_loc)
library(dismo, lib.loc = lib_loc)
library(predicts, lib.loc = lib_loc)
library(blockCV, lib.loc = lib_loc)
library(scales)
library(mgcv)
library(randomForest, lib.loc = lib_loc)
library(precrec, lib.loc = lib_loc)
library(glmnet, lib.loc = lib_loc)
library(flexsdm, lib.loc = lib_loc)
# NOTE * - Also required to have rJava  installed

# # Installing and loading packages
# if(!require(devtools)){
#   install.packages("devtools")
# }
# 
# if(!require(kuenm)){
#   devtools::install_github("marlonecobos/kuenm")
# }

library(kuenm, lib.loc = lib_loc)

# library(devtools)
# install_github('meeliskull/prg/R_package/prg')
library(prg, lib.loc = lib_loc)

# remotes::install_github("rvalavi/myspatial")
library(myspatial, lib.loc = lib_loc)



# Source helper functions -------------------------------------------------

source(here("C:/Users/n11222026/OneDrive - Queensland University of Technology/Code/Objective_3/AntarcticFutureHabitat/Scripts/Additional_functions.R"))


# Set model types to run --------------------------------------------------

model_types <- list("LASSO", "GAM", "RF", "BRT")



# Set group ---------------------------------------------------------------

# group <- "Lichen"
 group <- "Moss"


# Set scenario ---------------------------------------------------------------

scenario = "PA_Ensemble_VESTFOLD_Nov_5"


# Set outpath -------------------------------------------------------------

outpath <- here("Outputs", "Ensemble", group, scenario)

if(!dir.exists(outpath)) {
  dir.create(outpath, showWarnings = FALSE)
} 


# Domain setup ------------------------------------------------------------

# Load the ice-free areas
# ice_free <- rast(here("Data/Environmental_predictors/ice_free_union_reproj_100m.tif"))
#ice_free <- rast(here("Data/Environmental_predictors/ice_free_upsamp_1km.tif"))


# Load the Antarctic Conservation Biogeographic Regions, filter to East Antarctica
ACBRS <- st_read(here("Data/Environmental_predictors/ACBRs_v2_2016.shp"), crs = 3031) %>% filter(ACBR_Name == "East Antarctica")
ACBRS_SPVE <- vect(ACBRS)

# 
# # Also trim ice-free land to just East Antarctica
# ice_free.EastAnt <- terra::crop(ice_free, ext(ACBRS_SPVE))
# 
# ice_free.EastAnt <- ifel(not.na(ice_free.EastAnt), 1, NA)
ice_free.EastAnt <- rast(here("Data/Environmental_predictors/ice_free_union_EastAnt_500m.tif"))

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
  
  PA <- PA_Vestfold_Veg_df %>% 
    dplyr::select(x, y, srfc_ms) %>% 
    rename(Presence = srfc_ms)
  
  PA.sf <- PA_Vestfold_Veg_sf %>% 
    rename(Presence = srfc_ms)
  
}

if(group == "Lichen") {
  
  PA <- PA_Vestfold_Veg_df %>% 
    dplyr::select(x, y, srfc_lc) %>% 
    rename(Presence = srfc_lc)
  
  PA.sf <- PA_Vestfold_Veg_sf %>% 
    rename(Presence = srfc_lc)
  
}


# Load the covariates -----------------------------------------------------

# Load the 1km covariates -----------------------------------------------------

# TWI <- rast(here("Data/Environmental_predictors/topographic_wetness_index_EAST_ANTARCTICA.tif"))
# names(TWI) <- "TWI"
# 
# slope <- rast(here("Data/Environmental_predictors/slope_EAST_ANTARCTICA.tif"))
# names(slope) <- "slope"
# 
# northness <- rast(here("Data/Environmental_predictors/northness_EAST_ANTARCTICA.tif"))
# names(northness) <- "northness"
# 
# dist_vertebrates <- rast(here("Data/Environmental_predictors/distance_to_vertebrates_EAST_ANTARCTICA.tif"))
# names(dist_vertebrates) <- "dist_vertebrates"
# 
# dist_seasonal_water <- rast(here("Data/Environmental_predictors/distance_to_seasonal_water_EAST_ANTARCTICA.tif"))
# names(dist_seasonal_water) <- "dist_seasonal_water"
# 
# summer_temp <- rast(here("Data/Environmental_predictors/Mean_Summer_Temp_EAST_ANTARCTICA.tif"))
# names(summer_temp) <- "summer_temp"
# 
# wind <- rast(here("Data/Environmental_predictors/Mean_Annual_Wind_Speed_ALL_YEARS_EAST_ANTARCTICA.tif"))
# names(wind) <- "wind"
# 
# # Bias covariate
# dist_station <- rast(here("Data/Environmental_predictors/distance_to_station_EAST_ANTARCTICA.tif"))
# names(dist_station) <- "dist_station"


# Load the 500m covariates ------------------------------------------------

# TWI <- rast(here("Data/Environmental_predictors/TWI_500m_IceFree_EastAnt.tif"))
# names(TWI) <- "TWI"

slope <- rast(here("Data/Environmental_predictors/slope_500m_IceFree_EastAnt.tif"))
names(slope) <- "Slope"

northness <- rast(here("Data/Environmental_predictors/northness_500m_IceFree_EastAnt.tif"))
names(northness) <- "Northness"

# dist_seasonal_water <- rast(here("Data/Environmental_predictors/distance_to_seasonal_water_ICEFREE_500m.tif"))
# names(dist_seasonal_water) <- "dist_seasonal_water"

summer_temp <- rast(here("Data/Environmental_predictors/mean_summer_temp_AntAirIce_500m.tif"))
names(summer_temp) <- "summer_temp"

wind_speed <- rast(here("Data/Environmental_predictors/AMPS_Mean_Annual_Wind_Speed_500m.tif"))
names(wind_speed) <- "wind_speed"

snow_cover <- rast(here("Data/Environmental_predictors/SummerSnowCover_500m.tif"))
names(snow_cover) <- "snow_cover"

dist_coast <- rast(here("Data/Environmental_predictors/dist_to_coast_seamask_v7_10_500m.tif"))
names(dist_coast) <- "dist_to_coast"


# Apply some transformations
sqrt_slope <- sqrt(slope)
names(sqrt_slope) <- "sqrt_slope"

# log_dist_seasonal_water <- log(dist_seasonal_water+1)
# names(log_dist_seasonal_water) <- "log_dist_seasonal_water"

log_dist_coast <- log(dist_coast+1)
names(log_dist_coast) <- "log_dist_coast"


# Stack covariates & save version w/o bias cov
covs <- c(sqrt_slope, northness, summer_temp, wind_speed, snow_cover, log_dist_coast)

# Make sure that if any predictors are NA, all become NA

# Here we're just exploiting the fact that sum will by default return NA when any layer has an NA
covs <- terra::mask(covs, sum(covs))

#************
# Now trim ice_free mask to match covs!
ice_free.EastAnt <- terra::mask(ice_free.EastAnt, sum(covs))

# Extract enviro. covs for training ---------------------------------------

train_PB_covs <- terra::extract(covs, PA[,c("x", "y")], xy = T)
train_PB_covs <- cbind(train_PB_covs, PA["Presence"])

# Remove rows where there's values missing from at least one covariate

print(paste0("RECORDS FROM ", nrow(train_PB_covs) - sum(complete.cases(train_PB_covs)), " ROWS IN TRAINING DATA REMOVED DUE TO MISSING COVARIATE VALUES"))

train_PB_covs <- train_PB_covs[complete.cases(train_PB_covs), ] 
# Reset the row IDs to adjust for removed rows
rownames(train_PB_covs) <- NULL
train_PB_covs <- dplyr::select(train_PB_covs, -ID)


# Extract enviro. covs for prediction across East Antarctica --------------

pred_cur_covs <- as.data.frame(covs, xy = T)

print(paste0("RECORDS FROM ", nrow(pred_cur_covs) - sum(complete.cases(pred_cur_covs)), " ROWS IN PREDICTION DATA REMOVED DUE TO MISSING COVARIATE VALUES"))

# Remove rows with NA in any covariates
pred_cur_covs <- pred_cur_covs[complete.cases(pred_cur_covs), ]
# Reset the row IDs to adjust for removed rows
rownames(pred_cur_covs) <- NULL


# Plot environmental conditions  ----------------------------------------------

cov_names <- names(covs)

# 1. Background samples vs. presences

for(i in seq_along(cov_names)) {
  
  plot <- ggplot() +
    geom_density(data = train_PB_covs, 
                 aes(x = .data[[names(train_PB_covs)[i]]], fill = as.factor(Presence)), 
                 alpha = 0.5) +
    theme_bw() +
    labs(title = names(train_PB_covs)[i])
  
  ggsave(file.path(outpath, paste0("Covariate_Density_Plot_", names(train_PB_covs)[i], "_Scenario_", scenario, ".png")),
         plot = plot,
         width = 13, height = 10, dpi = 300)
  
}

############################################
# FITTING MODELS TO PA DATASET -----------------------------------------------
############################################


############################################
# Normalise covariates for some models  ---------------------------------
############################################

# Models that don't require it: 
# Lasso GLM
# MaxEnt

# Models that do require it:
# BRT
# RF - downsampled
# GAM

train_PB_covs_norm <- train_PB_covs
pred_cur_covs_norm <- pred_cur_covs
#pred_fut_covs_norm <- pred_fut_covs

# Normalise all training, test, current, and future covariates by the mean and sd of the training data 

for (v in cov_names) {
  
  meanv <- mean(train_PB_covs[, v])
  sdv <- sd(train_PB_covs[, v])
  train_PB_covs_norm[, v] <- (train_PB_covs[, v] - meanv) / sdv
  pred_cur_covs_norm[, v] <- (pred_cur_covs[, v] - meanv) / sdv
  #pred_fut_covs_norm[, v] <- (pred_fut_covs[, v] - meanv) / sdv
}



############################################
# ALL - Calculating the case weights (down-weighting)  --------------------------
############################################

# Presences and background will have equal weight in some models

prNum <- as.numeric(table(train_PB_covs$Presence)["1"]) # number of presences
bgNum <- as.numeric(table(train_PB_covs$Presence)["0"]) # number of backgrounds
# wt <- ifelse(train_PB_covs$Presence == 1, 1, prNum / bgNum) # down-weighting


############################################
# ALL - LASSO regression --------------------------------------------------------
############################################

# Set type of model
mod.type = "LASSO"


# Make folder:

dir.create(file.path(outpath, "LASSO_outputs"), showWarnings = F)

# Convert input data to a matrix
# Make quadratic terms for glmnet package

# To make the orthogonal quadratic features for glmnet package (see more detail in the supplementary material
# of Guillera-Arroita et al. 2014), the make_quadratic function in myspatial package is used. The object
# generated by this function can be used in the generic predict() function to apply the transformation on the
# training and testing datasets and later used in predicting to rasters. The package myspatial is archived in
# GitHub and can be installed using the following code.

# installing the package from github
# remotes::install_github("rvalavi/myspatial")

quad_obj <- make_quadratic(train_PB_covs, cols = cov_names)

# now we can predict this quadratic object on the training and testing data
# this make two columns for each covariates used in the transformation
train_quad <- predict(quad_obj, newdata = train_PB_covs)
pred_cur_quad <- predict(quad_obj, newdata = pred_cur_covs)
# pred_fut_quad <- predict(quad_obj, newdata = pred_fut_covs)

# convert the data.frames to sparse matrices
# select all quadratic (and non-quadratic) columns, except the y (occ)

new_vars <- names(train_quad)[!names(train_quad) %in% c("Presence", "x", "y")]
train_sparse <- sparse.model.matrix(~. -1, train_quad[, new_vars])
pred_cur_sparse <- sparse.model.matrix(~. -1, pred_cur_quad[, new_vars])
# pred_fut_sparse <- sparse.model.matrix(~. -1, pred_fut_quad[, new_vars])

# Fitting the lasso GLM

lasso <- glmnet(x = train_sparse,
                y = train_quad$Presence,
                family = "binomial",
                standardize = T, # DEFAULT
                alpha = 1)


png(paste0(outpath,  "/LASSO_outputs/lasso_lambda_plot.png"), width = 800, height = 600)
plot(lasso, xvar = "lambda", label = TRUE)
dev.off()

lasso_cv <- cv.glmnet(x = train_sparse,
                      y = train_quad$Presence,
                      family = "binomial",
                      alpha = 1, # here 1 means fitting lasso
                      standardize = T, # DEFAULT
                      nfolds = 5) # number of folds for cross-validation

png(paste0(outpath,  "/LASSO_outputs/lasso_CV_plot.png"), width = 800, height = 600)
plot(lasso_cv)
dev.off()


# Choose to use the lambda (regularisation parameter which adds a penalty to large coefficients) that is 1SD from the minimum deviance (to avoid over-fitting if use the minimum deviance lambda)


# Take the coefficients for the chosen regularisation parameter value
coef_df <- round(coef(lasso_cv, s = "lambda.1se"), 4) %>% 
  as.matrix(.) %>% 
  as.data.frame()

coef_df$Variable <- rownames(coef_df)
colnames(coef_df)[1] <- "Coefficient"

coef_df <- coef_df %>%
  filter(Coefficient != 0) %>%
  filter(Variable != "(Intercept)") %>%
  arrange(desc(Coefficient))


png(paste0(outpath,  "/LASSO_outputs/lasso_deviance_plot.png"), width = 800, height = 600)
# How much deviance is explained by predictors?
plot(lasso, xvar = "dev", label = T)
dev.off()


# Partial dependence plots (all other covs at their mean)
# 
# png(paste0(outpath,  "/LASSO_outputs/lasso_pdps_plot.png"), width = 800, height = 400)
# pdps_for_lasso(cov_names = cov_names, 
#                quad_obj = quad_obj,
#                lasso_cv = lasso_cv,
#                training_data = train_PB_covs) # set to all data or a fold
# dev.off()


# Current prediction
pred_cur.lasso <- predict(lasso_cv, pred_cur_sparse, s = "lambda.1se", type = "response")
pred_cur.lasso <- cbind(pred_cur_covs, pred_cur.lasso)
colnames(pred_cur.lasso)[grepl("lambda.1se", colnames(pred_cur.lasso))] <- "pred"
pred_cur.lasso.rast <- rast(pred_cur.lasso[, c("x", "y", "pred")], type = "xyz", crs = "EPSG:3031")

writeRaster(pred_cur.lasso.rast, here(outpath, "LASSO_outputs", paste0("Prediction_East_Antarctica.tif")), overwrite = T)


############################################
# ALL - GAM ---------------------------------------------------------------------
############################################

# Set type of model
mod.type = "GAM"


# Make folder:

dir.create(file.path(outpath, "GAM_outputs"), showWarnings = F)


# Building GAM formula (adjust k for smooth complexity per covariate)
myform <- paste(
  "Presence ~",
  paste(paste0("s(", cov_names, ", k = 10)"), collapse = " + ")
)

gam <- mgcv::gam(formula = as.formula(myform), 
                 data = train_PB_covs_norm,
                 family = binomial(link = "logit"),
                 method = "REML")

# Save some model check outputs

gam_kcheck <- mgcv::k.check(gam)

write.csv(gam_kcheck, file = file.path(outpath, "GAM_outputs", paste0("GAM_kcheck.csv")))

png(paste0(outpath,  "/GAM_outputs/GAM.check_plot.png"), width = 800, height = 600)
par(mfrow = c(2, 2))  
mgcv::gam.check(gam, warning = F)
dev.off()

# Plot response with the residuals

png(paste0(outpath, "/GAM_outputs/GAM_response_residual_plot.png"), width = 800, height = 600)
plot(gam, shade=T, pages = 1, scale = 0, residuals = TRUE)
dev.off()


# Pull out model summary results
summary(gam)

mod_summary <- summary(gam)
mod_summary_df <- data.frame(mod_summary$s.table,
                             deviance = mod_summary$dev.expl,
                             r.sq = mod_summary$r.sq,
                             dispersion = mod_summary$dispersion)

write.csv(mod_summary_df, file = file.path(outpath, "GAM_outputs", paste0("GAM_summary_df.csv")))

png(paste0(outpath,  "/GAM_outputs/GAM_pdp_plots.png"), width = 800, height = 600)
print(flexsdm::p_pdp(gam, 
                     predictors = cov_names,
                     resolution = 100,
                     training_data = train_PB_covs_scv,
                     projection_data = NULL,
                     clamping = FALSE))
dev.off()


# Current prediction
pred_cur.gam <- predict(gam, pred_cur_covs_norm, type = "response")
pred_cur.gam <- cbind(pred_cur_covs_norm, pred_cur.gam)
colnames(pred_cur.gam)[grepl("pred", colnames(pred_cur.gam))] <- "pred"
pred_cur.gam.rast <- rast(pred_cur.gam[, c("x", "y", "pred")], type = "xyz", crs = "EPSG:3031")

writeRaster(pred_cur.gam.rast, here(outpath, "GAM_outputs", paste0("Prediction_East_Antarctica.tif")), overwrite = T)


############################################
# ALL - Random Forest -----------------------------------------------------------
############################################

# Set type of model
mod.type = "RF"


# Make folder:

dir.create(file.path(outpath, "RF_outputs"), showWarnings = F)

# Convert the response to factor for producing class relative likelihood
train_PB_covs_norm_rf <- train_PB_covs_norm
train_PB_covs_norm_rf$Presence <- as.factor(train_PB_covs_norm_rf$Presence)


# Using the default for mtry (sqrt(ncovs))
rf <- randomForest::randomForest(formula = Presence ~.,
                                 data = train_PB_covs_norm_rf[, !names(train_PB_covs_norm_rf) %in% c("x", "y")],
                                 ntree = 1000,
                                 replace = T,
                                 importance = T)

# Save some model check outputs

# Model summary
oob_error <- rf$err.rate[nrow(rf$err.rate), "OOB"] # OOB error rate from final point
write.csv(oob_error, file = file.path(outpath, "RF_outputs", paste0("RF_OOB_error.csv")))

confusion_matrix <- rf$confusion
write.csv(confusion_matrix, file = file.path(outpath, "RF_outputs", paste0("RF_confusion_matrix.csv")))

# This plot looks at how the error (out-of-bag prediction error) changes as the number of trees increases
# Gives some idea of how the model converges (and how many trees it really needed)
png(paste0(outpath, "/RF_outputs/RF_error_vs_trees_plot.png"), width = 800, height = 600)
plot(rf, main = "RF down-sampled")
dev.off()

# Plot variable importance
png(paste0(outpath, "/RF_outputs/RF_var_importance_Gini_plot.png"), width = 800, height = 600)
varImpPlot(rf, type = 2) # Gini impurity index
dev.off()

png(paste0(outpath, "/RF_outputs/RF_var_importance_Permutation_plot.png"), width = 800, height = 600)
varImpPlot(rf, type = 1) # Permutation
dev.off()

# Partial dependence plots
png(paste0(outpath,  "/RF_outputs/RF_pdp_plots.png"), width = 800, height = 600)
flexsdm::p_pdp(rf,
               predictors = cov_names,
               resolution = 100,
               training_data = train_PB_covs,
               projection_data = NULL
)
dev.off()


# Current prediction
pred_cur.rf <- predict(rf, pred_cur_covs_norm, type = "prob")[, "1"]
pred_cur.rf <- cbind(pred_cur_covs_norm, pred_cur.rf)
colnames(pred_cur.rf)[grepl("pred", colnames(pred_cur.rf))] <- "pred"
pred_cur.rf.rast <- rast(pred_cur.rf[, c("x", "y", "pred")], type = "xyz", crs = "EPSG:3031")

writeRaster(pred_cur.rf.rast, here(outpath, "RF_outputs", paste0("Prediction_CURRENT.tif")), overwrite = T)


############################################
# ALL - Boosted regression tree -------------------------------------------------
############################################

# Set type of model
mod.type = "BRT"


# Make folder:

dir.create(file.path(outpath, "BRT_outputs"), showWarnings = F)

# Initialise the tracker
brt <- NULL
b <- 0 

# Attempt the model up to 30 times, with adjustments to parameterisation. 
while(is.null(brt)){
  b <- b + 1
  if(b < 11){
    ntrees <- 50 # Number of trees to add at each step
    lrate <- 0.001 # Shrinkage or weight applied to individual trees (controls how fast the learning is)
  } else if(b < 21){
    lrate <- 0.0001 
  } else if(b < 31){
    ntrees <- 25
    lrate <- 0.0001
  } else{
    break # After 30 tries, give up and move to next model
  }
  
  png(paste0(outpath,  "/BRT_outputs/BRT_holdout_deviance_cv_plot.png"), width = 800, height = 400)
  
  # k-fold cross-validation (here, n.folds = 5) is used to estimate the optimal number of trees while keeping the other parameters constant
  brt <- dismo::gbm.step(data = train_PB_covs_norm,
                         gbm.x = which(colnames(train_PB_covs_norm) %in% cov_names), # Column indices for covs
                         gbm.y = which(colnames(train_PB_covs_norm) == "Presence"), # Column index for Presence
                         family = "bernoulli",
                         tree.complexity = ifelse(prNum < 50, 1, 5), # Depends on record number
                         learning.rate = lrate,
                         bag.fraction = 0.75, # Fraction witheld for CV
                         max.trees = 10000,
                         n.trees = ntrees,
                         n.folds = 5, # 5-fold cross-validation
                         silent = FALSE) # Avoid printing cv results
  dev.off()
  
  
}


if(is.null(brt)) {
  
  print(paste0("BRT model failed to converge"))
  
  
} else { # If model converged:
  
  # Model output checks
  
  model_performance <- data.frame(lrate = lrate,
                                  ntrees = ntrees,
                                  best.trees = brt$gbm.call$best.trees,
                                  cv.deviance = brt$cv.statistics$deviance.mean,
                                  null.deviance = brt$self.statistics$mean.null,
                                  cv.discrimination = brt$cv.statistics$discrimination.mean,
                                  train.discrimination = brt$self.statistics$discrimination)
  
  write.csv(model_performance, file = file.path(outpath, "BRT_outputs", paste0("BRT_model_performance.csv")))
  
  # Variable importance
  png(paste0(outpath,  "/BRT_outputs/BRT_variable_importance_plot.png"), width = 800, height = 800)
  
  summary(brt)
  
  dev.off()
  
  var.imp <- summary(brt)
  
  write.csv(var.imp, file = file.path(outpath, "BRT_outputs", paste0("BRT_variable_importance.csv")))
  
  # Partial dependence plots & fitted vs. covariate plots
  
  png(paste0(outpath,  "/BRT_outputs/BRT_partial_effects_plot.png"), width = 800, height = 800)
  
  p <- ceiling(length(cov_names)/2)
  gbm.plot(brt, plot.layout = c(p,p))
  
  dev.off()
  
  
  png(paste0(outpath,  "/BRT_outputs/BRT_fitted_vs_covariate_plot.png"), width = 800, height = 400)
  
  gbm.plot.fits(brt)
  
  dev.off()
  
  # Interactions
  
  find.int <- gbm.interactions(brt)
  interactions <- find.int$interactions
  
  write.csv(interactions, file = file.path(outpath, "BRT_outputs", paste0("BRT_interactions.csv")))
  
  
  # Current prediction
  pred_cur.brt <- dismo::predict(brt, pred_cur_covs_norm, n.trees = brt$gbm.call$best.trees, type = "response")
  pred_cur.brt <- cbind(pred_cur_covs_norm, pred_cur.brt)
  colnames(pred_cur.brt)[grepl("pred", colnames(pred_cur.brt))] <- "pred"
  pred_cur.brt.rast <- rast(pred_cur.brt[, c("x", "y", "pred")], type = "xyz", crs = "EPSG:3031")
  
  writeRaster(pred_cur.brt.rast, here(outpath, "BRT_outputs", paste0("Prediction_East_Antarctica.tif")), overwrite = T)
  
}

############################################
# ALL - Ensemble of all model predictions ---------------------------------------
############################################

pred_cur_ensemble <- data.frame(lasso = scales::rescale(pred_cur.lasso[["pred"]], to = c(0,1)), 
                                gam = scales::rescale(pred_cur.gam[["pred"]], to = c(0,1)),
                                brt = scales::rescale(pred_cur.brt[["pred"]], to = c(0,1)),
                                rf = scales::rescale(pred_cur.rf[["pred"]], to = c(0,1))) %>% 
  rowMeans()


# Make into a raster
pred_cur_ensemble <- cbind(pred_cur_covs, pred_cur_ensemble)
pred_cur_ensemble.rast <- rast(pred_cur_ensemble[, c("x", "y", "pred_cur_ensemble")], type = "xyz", crs = "EPSG:3031")

write.csv(pred_cur_ensemble, file = here(outpath, paste0("Prediction_ensemble_East_Antarctica.csv")))
#write.csv(pred_fut_ensemble, file = here(outpath, paste0("Prediction_ensemble_FUTURE_fold_", f, ".csv")))
writeRaster(pred_cur_ensemble.rast, here(outpath, paste0("Prediction_ensemble_East_Antarctica.tif")), overwrite = T)


############################################
# Evaluate the ensemble fit on PA data used for fitting ------------------
############################################

source(here("Scripts/Helper_functions_ISDM.R"))

colnames(pred_cur_ensemble)[grepl("pred", colnames(pred_cur_ensemble))] <- "pred"

# Extract locations of PO data to match the prediction raster grid cells
PA_covs <- terra::extract(covs, PA[, c("x", "y")], xy = T)
PA_covs <- cbind(PA_covs, PA["Presence"])

# Match predictions to PO training data locations
pred_with_fit_PA <- dplyr::left_join(PA_covs, pred_cur_ensemble[, c("x", "y", "pred")], by = c("x", "y"))

# Remove a couple of NAs
pred_with_fit_PA <- pred_with_fit_PA %>% 
  filter(!is.na(pred))

# Evaluate prediction on test set
fit <- evaluate_fit_PA_ensemble(x = pred_with_fit_PA,
                                outpath = outpath)

eval_df.ens <- data.frame(model = "Ensemble",
                          validation_dataset = "Training data",
                          fit[["fit_df"]])



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


PA_bunger23_covs <- terra::extract(covs, PA_bunger23[, c("x", "y")], xy = T)
PA_bunger23_covs <- cbind(PA_bunger23_covs, PA_bunger23["presence"])
PA_bunger23_covs <- PA_bunger23_covs %>% 
  rename(Presence = presence)

# Match predictions to PA locations
pred_with_PA <- dplyr::left_join(PA_bunger23_covs, pred_cur_ensemble[, c("x", "y", "pred")], by = c("x", "y"))

# Remove a couple of NAs
pred_with_PA <- pred_with_PA %>% 
  filter(!is.na(pred))

# Evaluate prediction on test set
eval <- evaluate_prediction_ensemble(pred_with_PA)

eval_df.ens <- eval_df.ens %>% 
  add_row(model = "Ensemble",
          validation_dataset = "Bunger23",
          eval[["eval_df"]])


write.csv(eval_df.ens, file = here(outpath, "Ensemble_eval_df.csv"))


# Plotting - Vestfold ------------------------------------------------------

vestfold_boundary <- vect(here("Data/Environmental_predictors/vestfold_boundary.shp"))
bunger_boundary <- vect(here("Data/Environmental_predictors/bunger_boundary.shp"))

p1 <- pred_cur_ensemble.rast %>% 
  crop(ext(vestfold_boundary)) %>% 
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 1, barheight = 6),
                     name = "Probability") +
  coord_fixed() +
  labs(title = paste0(group, " - PO Ensemble")) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) 

ggsave(paste0(outpath, "/VESTFOLD_Probability_prediction_plot.png"), p1,
       width = 10, height = 6, dpi = 300)

p2 <- pred_cur_ensemble.rast %>% 
  crop(ext(bunger_boundary)) %>% 
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 1, barheight = 6),
                     name = "Probability") +
  coord_fixed() +
  labs(title = paste0(group, " - PO Ensemble")) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

ggsave(paste0(outpath, "/BUNGER_Probability_prediction_plot.png"), p2,
       width = 10, height = 6, dpi = 300)


# ############################################
# # Evaluate the ensemble predictions on Plantarctica PA dataset ----------------
# ############################################
# 
# if(group == "Moss") {
#   
#   veg_map <- st_read(here("Data/Biological_records/PlantarcticaVegetationMap.shp")) %>% 
#     filter(Vegetation == "Vegetation") %>% 
#     vect()
#   
# }
# 
# if(group == "Lichen") {
#   
#   veg_map <- st_read(here("Data/Biological_records/PlantarcticaVegetationMap.shp")) %>% 
#     filter(Vegetation == "Lichen") %>% 
#     vect()
#   
# }
# 
# grid <- rast(ext = ext(ice_free.EastAnt),
#              resolution = 1000, 
#              vals = 1) 
# 
# 
# crs(grid) <- "epsg:3031"
# 
# gridUNIQUE <- rast(ext = ext(ice_free.EastAnt), 
#                    resolution = 1000, 
#                    vals = 1:ncell(grid)) 
# 
# crs(gridUNIQUE) <- "epsg:3031"
# 
# # Convert Plantarctica to presence-absence grid
# # Find the ice-free grid cell ID each vegetation record lies in with a unique value
# # ID is the ID of the veg record, lyr.1 is the unique ID of the cell
# extractUNIQUE <- terra::extract(gridUNIQUE, veg_map)
# 
# # Count the number of veg points per unique grid cell
# count <- count(extractUNIQUE, lyr.1)
# 
# # Make gridUNIQUE raster a data frame
# gridUNIQUEdf <- as.data.frame(gridUNIQUE, xy = TRUE)
# 
# # Full join retains all values, join the grid dataframe and  the count values by the cell unique ID
# update_rast <- full_join(gridUNIQUEdf, count, by = "lyr.1") %>% 
#   .[, -3] %>% 
#   drop_na(x) %>% # Remove if there's a row with no xy (where the veg. data didn't overlap the coastline)
#   rast(.) 
# 
# update_rast <- ifel(is.na(update_rast), 0, 1)
# 
# crs(update_rast) <- "epsg:3031"
# 
# names(update_rast) <- "Presence"
# 
# 
# ## Add in PA vegetation dataset
# 
# vegetation_PA <- mask(update_rast, ice_free.EastAnt, maskvalue = NA) %>% 
#   as.data.frame(xy = F)
# 
# vegetation_PA.rast <- mask(update_rast, ice_free.EastAnt, maskvalue = NA)
# 
# #writeRaster(vegetation_PA.rast, here("Data/Biological_records", "Plantarctica_lichen_PA.tif"))
# 
# # Add presence-absence validation data to prediction dataframe
# pred_with_PA <- cbind(pred_cur_ensemble, vegetation_PA)   
# 
# # Evaluate prediction on test set
# eval <- evaluate_prediction(pred_with_PA)
# 
# eval_df.ens <- eval_df.ens %>% 
#   add_row(model = "Ensemble",
#           validation_dataset = "Plantarctica",
#           eval[["eval_df"]])


