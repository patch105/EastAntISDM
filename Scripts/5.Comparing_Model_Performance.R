
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


# Read in Vestfold & Bunger boundaries ------------------------------------

vestfold_boundary <- vect(here("Data/Environmental_predictors/vestfold_boundary.shp"))
bunger_boundary <- vect(here("Data/Environmental_predictors/bunger_boundary.shp"))


# Set the outpath ---------------------------------------------------------

outpath <- here("Outputs/Results")


##########################################
# OVERALL SCENARIO NAME
scenario_all <- "Jul_23_linear"
##########################################


####################################################################
# 1. SINGLE DATASET APPROACHES - LICHEN -----------------------------------
###################################################################


# Ensemble PO 

scenario = "PO_Ensemble_Jul_22"

inpath <- here("Outputs/Ensemble/Lichen")

PO_ensemble_lichen_eval_df <- read.csv(file = here(inpath, scenario, "Ensemble_eval_df.csv"))

PO_ensemble_lichen_pred <- rast(here(inpath, scenario, "Prediction_ensemble_East_Antarctica.tif"))


# Ensemble Plantarctica

scenario = "Plantarctica_Ensemble_Jul_17"

inpath <- here("Outputs/Ensemble_Plantarctica/Lichen")

Plantarctica_ensemble_lichen_eval_df <- read.csv(file = here(inpath, scenario, "Ensemble_eval_df.csv"))

Plantarctica_ensemble_lichen_pred <- rast(here(inpath, scenario, "Prediction_ensemble_East_Antarctica.tif"))


# Ensemble PA (VESTFOLD DATA)

scenario = "PA_Ensemble_19_DATASET"

inpath <- here("Outputs/Ensemble/Lichen")

PA_ensemble_lichen_eval_df <- read.csv(file = here(inpath, scenario, "Ensemble_eval_df.csv")) %>% 
  add_row(X = 3,
          model = "Ensemble",
          validation_dataset = "Vestfold",
          ROC = NA,
          PRG = NA,
          boyce = NA,
          partialROC = NA,
          brier = NA)

PA_ensemble_lichen_pred <- rast(here(inpath, scenario, "Prediction_ensemble_East_Antarctica.tif"))


# Ensemble PA (BUNGER DATA)

scenario = "PA_Ensemble_BUNGER"

inpath <- here("Outputs/Ensemble/Lichen")

PA_ensemble_BUNGER_lichen_eval_df <- read.csv(file = here(inpath, scenario, "Ensemble_eval_df.csv")) %>% 
  add_row(X = 3,
          model = "Ensemble",
          validation_dataset = "Bunger",
          ROC = NA,
          PRG = NA,
          boyce = NA,
          partialROC = NA,
          brier = NA)

PA_ensemble_BUNGER_lichen_pred <- rast(here(inpath, scenario, "Prediction_ensemble_East_Antarctica.tif"))



# Poisson point process PO 

scenario = "500m_ALL_DATASETS_SEASON19"

PO_PPP_lichen_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_df.csv")) %>% 
  filter(model == "m.PO.bias") %>% 
  mutate(validation_dataset = "Bunger23") %>% 
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_PO_fit_df.csv")) [2, ]

pred_vestfold <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_PO_models_Vestfold_eval_df.csv")) [2, ]

PO_PPP_lichen_eval_df <- PO_PPP_lichen_eval_df %>% 
  add_row(X = 3,
          model = "m.PO.bias",
          validation_dataset = "Training data",
          ROC = NA,
          PRG = NA,
          boyce = fit$boyce,
          partialROC = NA,
          brier = NA) 

PO_PPP_lichen_eval_df <- PO_PPP_lichen_eval_df %>% 
  add_row(X = 3,
          model = "m.PO.bias",
          validation_dataset = "Vestfold",
          ROC = pred_vestfold$ROC,
          PRG = pred_vestfold$PRG,
          boyce = pred_vestfold$boyce,
          partialROC = pred_vestfold$partialROC,
          brier = pred_vestfold$brier)

PO_PPP_lichen_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/Probability_prediction_m.PO.bias_median.tif"))


# Poisson point process Plantarctica

scenario = "500m_ALL_DATASETS_SEASON19"

Plantarctica_PPP_lichen_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_df.csv"))%>% 
  filter(model == "m.PO.Plantarctica") %>% 
  mutate(validation_dataset = "Bunger23") %>% 
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_Plantarctica_fit_df.csv")) 

pred_vestfold <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_PO_models_Vestfold_eval_df.csv")) [3, ]

Plantarctica_PPP_lichen_eval_df <- Plantarctica_PPP_lichen_eval_df %>% 
  add_row(X = 3,
          model = "m.PO.Plantarctica",
          validation_dataset = "Training data",
          ROC = NA,
          PRG = NA,
          boyce = fit$boyce,
          partialROC = NA,
          brier = NA)

Plantarctica_PPP_lichen_eval_df <- Plantarctica_PPP_lichen_eval_df %>% 
  add_row(X = 3,
          model = "m.PO.Plantarctica",
          validation_dataset = "Vestfold",
          ROC = pred_vestfold$ROC,
          PRG = pred_vestfold$PRG,
          boyce = pred_vestfold$boyce,
          partialROC = pred_vestfold$partialROC,
          brier = pred_vestfold$brier)

Plantarctica_PPP_lichen_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/Probability_prediction_m.PO.Plantarctica_median.tif"))


# Poisson point process PA (VESTFOLD)

scenario = "500m_ALL_DATASETS_SEASON19"

PA_PPP_lichen_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_df.csv")) %>% 
  filter(model == "m.PA") %>% 
  mutate(validation_dataset = "Bunger23") %>% 
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

PA_PPP_lichen_eval_df <- PA_PPP_lichen_eval_df %>% 
  add_row(X = 3,
          model = "m.PA",
          validation_dataset = "Training data",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

PA_PPP_lichen_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/BUNGER_Probability_prediction_m.PA_median.tif"))


# Poisson point process PA (BUNGER)

scenario = "500m_ALL_DATASETS_BUNGER_linear"

PA_PPP_BUNGER_lichen_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_df.csv")) %>% 
  filter(model == "m.PA") %>% 
  mutate(validation_dataset = "Vestfold") %>% 
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

PA_PPP_BUNGER_lichen_eval_df <- PA_PPP_BUNGER_lichen_eval_df %>% 
  add_row(X = 3,
          model = "m.PA",
          validation_dataset = "Training data",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

PA_PPP_BUNGER_lichen_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/VESTFOLD_Probability_prediction_m.PA_median.tif"))


# Table x. Single dataset approaches - LICHEN---------------------------------

#ordered by validation dataset
single_dataset_lichen_eval_df <- bind_rows(
  PO_ensemble_lichen_eval_df %>% mutate(model = "PO Ensemble"),
  Plantarctica_ensemble_lichen_eval_df %>% mutate(model = "Plantarctica Ensemble"),
  PA_ensemble_lichen_eval_df %>% mutate(model = "PA Ensemble"),
  PA_ensemble_BUNGER_lichen_eval_df %>% mutate(model = "PA Ensemble"),
  PO_PPP_lichen_eval_df %>% mutate(model = "PO PPP"),
  Plantarctica_PPP_lichen_eval_df %>% mutate(model = "Plantarctica PPP"),
  PA_PPP_lichen_eval_df %>% mutate(model = "PA PPP"),
  PA_PPP_BUNGER_lichen_eval_df %>% mutate(model = "PA PPP") 
) %>% 
  mutate(validation_dataset = factor(validation_dataset, levels = c("Training data","Vestfold", "Bunger23"))) %>% 
  arrange(validation_dataset) 

write.csv(single_dataset_lichen_eval_df,
          file = here(outpath, paste0("Single_dataset_lichen_eval_df_", scenario_all, ".csv")),
          row.names = FALSE)


# Figure x. Single dataset approaches - LICHEN ----------------------------


PA_Vestfold_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_vestfold_19.shp"))
#PA_Vestfold_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_vestfold.shp"))

PA_Vestfold_Veg_df <- PA_Vestfold_Veg_sf %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  bind_cols(st_drop_geometry(PA_Vestfold_Veg_sf)) %>% 
  rename(x = X, y = Y)

PA_Vestfold <- PA_Vestfold_Veg_df %>% 
  dplyr::select(x, y, srfc_lc) %>% 
  rename(Presence = srfc_lc) %>% 
  mutate(Presence = ifelse(Presence == 1, "Presence", "Absence"))
 
PA_Bunger23_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_bunger23.shp"))

PA_Bunger23_Veg_df <- PA_Bunger23_Veg_sf %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(st_drop_geometry(PA_Bunger23_Veg_sf)) %>%
  rename(x = X, y = Y)

PA_bunger23 <- PA_Bunger23_Veg_df %>%
  dplyr::select(x, y, srfc_lc) %>%
  rename(Presence = srfc_lc) %>% 
  mutate(Presence = ifelse(Presence == 1, "Presence", "Absence"))
 
  

# 
# 
# # Vestfold plot (w/out data)
# 
# p1 <- PO_ensemble_lichen_pred %>%
#   crop(ext(vestfold_boundary)) %>%
#   as.data.frame(xy = TRUE) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Lichen - PO Ensemble") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# 
# 
# p2 <- Plantarctica_ensemble_lichen_pred %>%
#   crop(ext(vestfold_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Lichen - Plantarctica") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# p3 <- PA_ensemble_BUNGER_lichen_pred %>%
#   crop(ext(vestfold_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Lichen - PA Ensemble") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# p4 <- PO_PPP_lichen_pred %>%
#   crop(ext(vestfold_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = Median)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Lichen - PO PPP") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# p5 <- Plantarctica_PPP_lichen_pred %>%
#   crop(ext(vestfold_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = Median)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Lichen - Plantarctica PPP") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# p6 <- PA_PPP_lichen_pred %>%
#   crop(ext(vestfold_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = Median)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Lichen - PA PPP") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# 
# Vestfold_plot <- ggarrange(p1, p2, p3, p4, p5, p6,
#                            ncol = 3, nrow = 2,
#                            common.legend = TRUE, legend = "bottom")
# 
# # Save
# ggsave(paste0(outpath, "/Single_dataset_prediction_plot_lichen_VESTFOLD_", scenario_all, ".png"), Vestfold_plot,
#        width = 20, height = 13, , unit = "cm", dpi = 400)

#####################################
# Vestfold plot (WITH DATA)

p1 <- PO_ensemble_lichen_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_Vestfold, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PO Ensemble") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines



p2 <- Plantarctica_ensemble_lichen_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_Vestfold, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "Satellite Ensemble") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())

p3 <- PA_ensemble_BUNGER_lichen_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_Vestfold, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA Ensemble") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())

p4 <- PO_PPP_lichen_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_Vestfold, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PO PPP") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())

p5 <- Plantarctica_PPP_lichen_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_Vestfold, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "Satellite PPP") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())

p6 <- PA_PPP_BUNGER_lichen_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_Vestfold, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA PPP") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())


Vestfold_plot <- ggarrange(p1, p2, p3, p4, p5, p6,
                           ncol = 3, nrow = 2,
                           common.legend = TRUE, legend = "bottom")

# Save
ggsave(paste0(outpath, "/Single_dataset_prediction_plot_lichen_VESTFOLD_W_DATA_", scenario_all, ".png"), Vestfold_plot,
       width = 20, height = 13, , unit = "cm", dpi = 400)


# 
# ######
# ####### Bunger plot w/out data
# ######
# 
# p1 <- PO_ensemble_lichen_pred %>%
#   crop(ext(bunger_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Lichen - PO Ensemble") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# p2 <- Plantarctica_ensemble_lichen_pred %>%
#   crop(ext(bunger_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Lichen - Plantarctica") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# p3 <- PA_ensemble_lichen_pred %>% 
#   crop(ext(bunger_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Lichen - PA Ensemble") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# 
# p4 <- PO_PPP_lichen_pred %>%
#   crop(ext(bunger_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = Median)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Lichen - PO PPP") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# p5 <- Plantarctica_PPP_lichen_pred %>%
#   crop(ext(bunger_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = Median)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Lichen - Plantarctica PPP") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# p6 <- PA_PPP_lichen_pred %>%
#   crop(ext(bunger_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = Median)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Lichen - PA PPP") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# 
# Bunger_plot <- ggarrange(p1, p2, p3, p4, p5, p6,
#                          ncol = 3, nrow = 2,
#                          common.legend = TRUE, legend = "bottom")
# 
# # Save
# ggsave(paste0(outpath, "/Single_dataset_prediction_plot_lichen_BUNGER_", scenario_all, ".png"), Bunger_plot,
#        width = 20, height = 13, , unit = "cm", dpi = 400)


#####################################
# BUNGER plot (WITH DATA)

p1b <- PO_ensemble_lichen_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_bunger23, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PO Ensemble") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines



p2b <- Plantarctica_ensemble_lichen_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_bunger23, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "Satellite Ensemble") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines


p3b <- PA_ensemble_lichen_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_bunger23, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA Ensemble") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines


p4b <- PO_PPP_lichen_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_bunger23, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PO PPP") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines

p5b <- Plantarctica_PPP_lichen_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_bunger23, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "Satellite PPP") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines

p6b <- PA_PPP_lichen_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_bunger23, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA PPP") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines


Bunger_plot <- ggarrange(p1b, p2b, p3b, p4b, p5b, p6b,
                           ncol = 3, nrow = 2,
                           common.legend = TRUE, legend = "bottom")

# Save
ggsave(paste0(outpath, "/Single_dataset_prediction_plot_lichen_BUNGER_W_DATA_", scenario_all, ".png"), Bunger_plot,
       width = 20, height = 13, , unit = "cm", dpi = 400)


###########################
### FIGURE 4
###########################

Figure_4 <- ggarrange(p1b, p2b, p3b, p4b, p5b, p6b,
                      p1, p2, p3, p4, p5, p6,
                      ncol = 3,
                      nrow = 4,
                      common.legend = T,
                      legend.position = "bottom")

ggsave(filename = paste0(outpath, "/FIGURE_4_", scenario_all, ".png"), Figure_4[[1]],
       width = 20, height = 30, unit = "cm", dpi = 400)


####################################################################
# 1. SINGLE DATASET APPROACHES - MOSS -------------------------------------
###################################################################


# Ensemble PO 

scenario = "PO_Ensemble_Jul_22"

inpath <- here("Outputs/Ensemble/Moss")

PO_ensemble_moss_eval_df <- read.csv(file = here(inpath, scenario, "Ensemble_eval_df.csv"))

PO_ensemble_moss_pred <- rast(here(inpath, scenario, "Prediction_ensemble_East_Antarctica.tif"))


# Ensemble Plantarctica

scenario = "Plantarctica_Ensemble_Jul_17"

inpath <- here("Outputs/Ensemble_Plantarctica/Moss")

Plantarctica_ensemble_moss_eval_df <- read.csv(file = here(inpath, scenario, "Ensemble_eval_df.csv"))

Plantarctica_ensemble_moss_pred <- rast(here(inpath, scenario, "Prediction_ensemble_East_Antarctica.tif"))


# PA ensemble (VESTFOLD)

scenario = "PA_Ensemble_19_DATASET"

inpath <- here("Outputs/Ensemble/Moss")

PA_ensemble_moss_eval_df <- read.csv(file = here(inpath, scenario, "Ensemble_eval_df.csv")) %>% 
  add_row(X = 3,
          model = "Ensemble",
          validation_dataset = "Vestfold",
          ROC = NA,
          PRG = NA,
          boyce = NA,
          partialROC = NA,
          brier = NA)

PA_ensemble_moss_pred <- rast(here(inpath, scenario, "Prediction_ensemble_East_Antarctica.tif"))


# PA ensemble (BUNGER)

scenario = "PA_Ensemble_BUNGER"

inpath <- here("Outputs/Ensemble/Moss")

PA_ensemble_BUNGER_moss_eval_df <- read.csv(file = here(inpath, scenario, "Ensemble_eval_df.csv")) %>% 
  add_row(X = 3,
          model = "Ensemble",
          validation_dataset = "Bunger",
          ROC = NA,
          PRG = NA,
          boyce = NA,
          partialROC = NA,
          brier = NA)

PA_ensemble_BUNGER_moss_pred <- rast(here(inpath, scenario, "Prediction_ensemble_East_Antarctica.tif"))


# Poisson point process PO 

scenario = "500m_ALL_DATASETS_SEASON19"

PO_PPP_moss_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_df.csv")) %>% 
  filter(model == "m.PO") %>% 
  mutate(validation_dataset = "Bunger23") %>% 
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_PO_fit_df.csv")) [2, ]

pred_vestfold <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_PO_models_Vestfold_eval_df.csv")) [2, ]

PO_PPP_moss_eval_df <- PO_PPP_moss_eval_df %>% 
  add_row(X = 3,
          model = "m.PO.bias",
          validation_dataset = "Training data",
          ROC = NA,
          PRG = NA,
          boyce = fit$boyce,
          partialROC = NA,
          brier = NA)

PO_PPP_moss_eval_df <- PO_PPP_moss_eval_df %>% 
  add_row(X = 3,
          model = "m.PO.bias",
          validation_dataset = "Vestfold",
          ROC = pred_vestfold$ROC,
          PRG = pred_vestfold$PRG,
          boyce = pred_vestfold$boyce,
          partialROC = pred_vestfold$partialROC,
          brier = pred_vestfold$brier)

PO_PPP_moss_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/Probability_prediction_m.PO_median.tif"))


# Poisson point process Plantarctica

scenario = "500m_ALL_DATASETS_SEASON19"

Plantarctica_PPP_moss_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_df.csv"))%>% 
  filter(model == "m.PO.Plantarctica") %>% 
  mutate(validation_dataset = "Bunger23") %>% 
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_Plantarctica_fit_df.csv")) 

pred_vestfold <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_PO_models_Vestfold_eval_df.csv")) [3, ]

Plantarctica_PPP_moss_eval_df <- Plantarctica_PPP_moss_eval_df %>% 
  add_row(X = 3,
          model = "m.PO.Plantarctica",
          validation_dataset = "Training data",
          ROC = NA,
          PRG = NA,
          boyce = fit$boyce,
          partialROC = NA,
          brier = NA)

Plantarctica_PPP_moss_eval_df <- Plantarctica_PPP_moss_eval_df %>% 
  add_row(X = 3,
          model = "m.PO.Plantarctica",
          validation_dataset = "Vestfold",
          ROC = pred_vestfold$ROC,
          PRG = pred_vestfold$PRG,
          boyce = pred_vestfold$boyce,
          partialROC = pred_vestfold$partialROC,
          brier = pred_vestfold$brier)

Plantarctica_PPP_moss_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/Probability_prediction_m.PO.Plantarctica_median.tif"))


# Poisson point process PA (VESTFOLD)

scenario = "500m_ALL_DATASETS_SEASON19"

PA_PPP_moss_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_df.csv")) %>% 
  filter(model == "m.PA") %>% 
  mutate(validation_dataset = "Bunger23") %>% 
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

PA_PPP_moss_eval_df <- PA_PPP_moss_eval_df %>% 
  add_row(X = 3,
          model = "m.PA",
          validation_dataset = "Training data",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

PA_PPP_moss_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/BUNGER_Probability_prediction_m.PA_median.tif"))


# Poisson point process PA (BUNGER)

scenario = "500m_ALL_DATASETS_BUNGER_linear"

PA_PPP_BUNGER_moss_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_df.csv")) %>% 
  filter(model == "m.PA") %>% 
  mutate(validation_dataset = "Vestfold") %>% 
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

PA_PPP_BUNGER_moss_eval_df <- PA_PPP_BUNGER_moss_eval_df %>% 
  add_row(X = 3,
          model = "m.PA",
          validation_dataset = "Training data",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

PA_PPP_BUNGER_moss_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/VESTFOLD_Probability_prediction_m.PA_median.tif"))


# Table x. Single dataset approaches - MOSS---------------------------------

#ordered by validation dataset
single_dataset_moss_eval_df <- bind_rows(
  PO_ensemble_moss_eval_df %>% mutate(model = "PO Ensemble"),
  Plantarctica_ensemble_moss_eval_df %>% mutate(model = "Plantarctica Ensemble"),
  PA_ensemble_moss_eval_df %>% mutate(model = "PA Ensemble"),
  PA_ensemble_BUNGER_moss_eval_df %>% mutate(model = "PA Ensemble"),
  PO_PPP_moss_eval_df %>% mutate(model = "PO PPP"),
  Plantarctica_PPP_moss_eval_df %>% mutate(model = "Plantarctica PPP"),
  PA_PPP_moss_eval_df %>% mutate(model = "PA PPP"),
  PA_PPP_BUNGER_moss_eval_df %>% mutate(model = "PA PPP")
) %>% 
  mutate(validation_dataset = factor(validation_dataset, levels = c("Training data","Vestfold", "Bunger23"))) %>% 
  arrange(validation_dataset)  

write.csv(single_dataset_moss_eval_df,
          file = here(outpath, paste0("Single_dataset_moss_eval_df_", scenario_all, ".csv")),
          row.names = FALSE)

# Figure x. Single dataset approaches - moss ----------------------------


PA_Vestfold_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_vestfold_19.shp"))
#PA_Vestfold_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_vestfold.shp"))

PA_Vestfold_Veg_df <- PA_Vestfold_Veg_sf %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  bind_cols(st_drop_geometry(PA_Vestfold_Veg_sf)) %>% 
  rename(x = X, y = Y)

PA_Vestfold <- PA_Vestfold_Veg_df %>% 
  dplyr::select(x, y, srfc_ms) %>% 
  rename(Presence = srfc_ms) %>% 
  mutate(Presence = ifelse(Presence == 1, "Presence", "Absence"))

PA_Bunger23_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_bunger23.shp"))

PA_Bunger23_Veg_df <- PA_Bunger23_Veg_sf %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(st_drop_geometry(PA_Bunger23_Veg_sf)) %>%
  rename(x = X, y = Y)

PA_bunger23 <- PA_Bunger23_Veg_df %>%
  dplyr::select(x, y, srfc_ms) %>%
  rename(Presence = srfc_ms) %>% 
  mutate(Presence = ifelse(Presence == 1, "Presence", "Absence"))

# 
# ######################
# # Vestfold plot (NO DATA)
# 
# p1 <- PO_ensemble_moss_pred %>%
#   crop(ext(vestfold_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Moss - PO Ensemble") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# p2 <- Plantarctica_ensemble_moss_pred %>%
#   crop(ext(vestfold_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Moss - Plantarctica ensemble") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# p3 <- PA_ensemble_moss_pred %>%
#   crop(ext(vestfold_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Moss - PA Ensemble") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# p4 <- PO_PPP_moss_pred %>%
#   crop(ext(vestfold_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = Median)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Moss - PO PPP") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# p5 <- Plantarctica_PPP_moss_pred %>%
#   crop(ext(vestfold_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = Median)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Moss - Plantarctica PPP") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# p6 <- PA_PPP_moss_pred %>%
#   crop(ext(vestfold_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = Median)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Moss - PA PPP") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# 
# Vestfold_plot <- ggarrange(p1, p2, p3, p4, p5, p6,
#                            ncol = 3, nrow = 2,
#                            common.legend = TRUE, legend = "bottom")
# 
# # Save
# ggsave(paste0(outpath, "/Single_dataset_prediction_plot_moss_VESTFOLD_", scenario_all, ".png"), Vestfold_plot,
#        width = 20, height = 13, , unit = "cm", dpi = 400)

#####################################
# Vestfold plot (WITH DATA)

p1 <- PO_ensemble_moss_pred  %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_Vestfold, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PO Ensemble") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines



p2 <- Plantarctica_ensemble_moss_pred  %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_Vestfold, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "Satellite Ensemble") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())

p3 <- PA_ensemble_BUNGER_moss_pred  %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_Vestfold, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA Ensemble") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())

p4 <- PO_PPP_moss_pred  %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_Vestfold, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PO PPP") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())

p5 <- Plantarctica_PPP_moss_pred  %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_Vestfold, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "Satellite PPP") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())

p6 <- PA_PPP_BUNGER_moss_pred  %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_Vestfold, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA PPP") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())


Vestfold_plot <- ggarrange(p1, p2, p3, p4, p5, p6,
                           ncol = 3, nrow = 2,
                           common.legend = TRUE, legend = "bottom")

# Save
ggsave(paste0(outpath, "/Single_dataset_prediction_plot_moss_VESTFOLD_W_DATA_", scenario_all, ".png"), Vestfold_plot,
       width = 20, height = 13, , unit = "cm", dpi = 400)

# 
# ######
# ####### Bunger plot (w/o data)
# ######
# 
# p1 <- PO_ensemble_moss_pred %>%
#   crop(ext(bunger_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Moss - PO Ensemble") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# p2 <- Plantarctica_ensemble_moss_pred %>%
#   crop(ext(bunger_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Moss - Plantarctica ensemble") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# p3 <- PA_ensemble_moss_pred %>% 
#   crop(ext(bunger_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Moss - PA Ensemble") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# p4 <- PO_PPP_moss_pred %>%
#   crop(ext(bunger_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = Median)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Moss - PO PPP") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# p5 <- Plantarctica_PPP_moss_pred %>%
#   crop(ext(bunger_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = Median)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Moss - Plantarctica PPP") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# p6 <- PA_PPP_moss_pred %>%
#   crop(ext(bunger_boundary)) %>%
#   as.data.frame(xy = T) %>%
#   ggplot() +
#   geom_tile(aes(x = x, y = y, fill = Median)) +
#   scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
#                      name = "Probability") +
#   coord_fixed() +
#   labs(title = "Moss - PA PPP") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         legend.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# 
# 
# Bunger_plot <- ggarrange(p1, p2, p3, p4, p5, p6,
#                          ncol = 3, nrow = 2,
#                          common.legend = TRUE, legend = "bottom")
# 
# # Save
# ggsave(paste0(outpath, "/Single_dataset_prediction_plot_moss_BUNGER_", scenario_all, ".png"), Bunger_plot,
#        width = 20, height = 13, , unit = "cm", dpi = 400)

#####################################
# Bunger plot (WITH DATA)

p1b <- PO_ensemble_moss_pred  %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_bunger23, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PO Ensemble") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines



p2b <- Plantarctica_ensemble_moss_pred  %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_bunger23, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "Satellite Ensemble") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines

p3b <- PA_ensemble_moss_pred  %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_bunger23, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA Ensemble") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines

p4b <- PO_PPP_moss_pred  %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_bunger23, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PO PPP") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines

p5b <- Plantarctica_PPP_moss_pred  %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_bunger23, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "Satellite PPP") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines

p6b <- PA_PPP_moss_pred  %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_bunger23, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA PPP") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines


Bunger_plot <- ggarrange(p1b, p2b, p3b, p4b, p5b, p6b,
                         ncol = 3, nrow = 2,
                         common.legend = TRUE, legend = "bottom")

# Save
ggsave(paste0(outpath, "/Single_dataset_prediction_plot_moss_BUNGER_W_DATA_", scenario_all, ".png"), Bunger_plot,
       width = 20, height = 13, , unit = "cm", dpi = 400)


###########################
### FIGURE 5
###########################

Figure_5 <- ggarrange(p1b, p2b, p3b, p4b, p5b, p6b,
                      p1, p2, p3, p4, p5, p6,
                      ncol = 3,
                      nrow = 4,
                      common.legend = T,
                      legend.position = "bottom")

ggsave(filename = paste0(outpath, "/FIGURE_5_", scenario_all, ".png"), Figure_5[[1]],
       width = 20, height = 30, unit = "cm", dpi = 400)


####################################################################
# 2. INTEGRATED APPROACHES - LICHEN -----------------------------------
###################################################################


# PO + PA (VESTFOLD)

scenario = "500m_ALL_DATASETS_SEASON19"
mod_name = "m.int.occ.VH"

PO_PA_lichen_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_df.csv")) %>%
  filter(model == mod_name) %>%
  mutate(validation_dataset = "Bunger23") %>%
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

PO_PA_lichen_eval_df <- PO_PA_lichen_eval_df %>% 
  add_row(X = 3,
          model = mod_name,
          validation_dataset = "Training data (VH)",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

PO_PA_lichen_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/BUNGER_Probability_prediction_", mod_name,"_median.tif")) %>% 
  setNames("pred")


# PO + PA (BUNGER)

scenario = "500m_ALL_DATASETS_BUNGER_linear"
mod_name = "m.int.occ.VH"

PO_PA_BUNGER_lichen_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_df.csv")) %>%
  filter(model == mod_name) %>%
  mutate(validation_dataset = "Vestfold") %>%
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

PO_PA_BUNGER_lichen_eval_df <- PO_PA_BUNGER_lichen_eval_df %>% 
  add_row(X = 3,
          model = mod_name,
          validation_dataset = "Training data (BH)",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

PO_PA_BUNGER_lichen_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/VESTFOLD_Probability_prediction_", mod_name,"_median.tif")) %>% 
  setNames("pred")


# PO bias + PA (VESTFOLD)

scenario = "500m_ALL_DATASETS_SEASON19"
mod_name = "m.int.occ.VH.bias"


PO_PA_bias_lichen_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_df.csv")) %>%
  filter(model == mod_name) %>%
  mutate(validation_dataset = "Bunger23") %>%
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

PO_PA_bias_lichen_eval_df <- PO_PA_bias_lichen_eval_df %>% 
  add_row(X = 3,
          model = mod_name,
          validation_dataset = "Training data (VH)",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

PO_PA_bias_lichen_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/BUNGER_Probability_prediction_", mod_name,"_median.tif")) %>% 
  setNames("pred")


# PO bias + PA (BUNGER)

scenario = "500m_ALL_DATASETS_BUNGER_linear"
mod_name = "m.int.occ.VH.bias"


PO_PA_bias_BUNGER_lichen_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_df.csv")) %>%
  filter(model == mod_name) %>%
  mutate(validation_dataset = "Vestfold") %>%
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

PO_PA_bias_BUNGER_lichen_eval_df <- PO_PA_bias_BUNGER_lichen_eval_df %>% 
  add_row(X = 3,
          model = mod_name,
          validation_dataset = "Training data (BH)",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

PO_PA_bias_BUNGER_lichen_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/VESTFOLD_Probability_prediction_", mod_name,"_median.tif")) %>% 
  setNames("pred")


# Plantarctica + PA (VESTFOLD)

scenario = "500m_ALL_DATASETS_SEASON19"
mod_name = "m.int.Plantarctica.VH"

Plantarctica_PA_lichen_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_df.csv")) %>%
  filter(model == mod_name) %>%
  mutate(validation_dataset = "Bunger23") %>%
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

Plantarctica_PA_lichen_eval_df <- Plantarctica_PA_lichen_eval_df %>% 
  add_row(X = 3,
          model = mod_name,
          validation_dataset = "Training data (VH)",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

Plantarctica_PA_lichen_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/BUNGER_Probability_prediction_", mod_name,"_median.tif")) %>% 
  setNames("pred")


# Plantarctica + PA (BUNGER)

scenario = "500m_ALL_DATASETS_BUNGER_linear"
mod_name = "m.int.Plantarctica.VH"

Plantarctica_PA_BUNGER_lichen_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_df.csv")) %>%
  filter(model == mod_name) %>%
  mutate(validation_dataset = "Vestfold") %>%
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

Plantarctica_PA_BUNGER_lichen_eval_df <- Plantarctica_PA_BUNGER_lichen_eval_df %>% 
  add_row(X = 3,
          model = mod_name,
          validation_dataset = "Training data (BH)",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

Plantarctica_PA_BUNGER_lichen_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/VESTFOLD_Probability_prediction_", mod_name,"_median.tif")) %>% 
  setNames("pred")



# PO + Plantarctica + PA (VESTFOLD)

scenario = "500m_ALL_DATASETS_SEASON19"
mod_name = "m.int.occ.Plantarctica.VH"

PO_Plantarctica_PA_lichen_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_df.csv")) %>%
  filter(model == mod_name) %>%
  mutate(validation_dataset = "Bunger23") %>%
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

PO_Plantarctica_PA_lichen_eval_df <- PO_Plantarctica_PA_lichen_eval_df %>% 
  add_row(X = 3,
          model = mod_name,
          validation_dataset = "Training data (VH)",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

PO_Plantarctica_PA_lichen_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/BUNGER_Probability_prediction_", mod_name,"_median.tif")) %>% 
  setNames("pred")


# PO + Plantarctica + PA (BUNGER)

scenario = "500m_ALL_DATASETS_BUNGER_linear"
mod_name = "m.int.occ.Plantarctica.VH"

PO_Plantarctica_PA_BUNGER_lichen_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_df.csv")) %>%
  filter(model == mod_name) %>%
  mutate(validation_dataset = "Vestfold") %>%
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

PO_Plantarctica_PA_BUNGER_lichen_eval_df <- PO_Plantarctica_PA_BUNGER_lichen_eval_df %>% 
  add_row(X = 3,
          model = mod_name,
          validation_dataset = "Training data (BH)",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

PO_Plantarctica_PA_BUNGER_lichen_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/VESTFOLD_Probability_prediction_", mod_name,"_median.tif")) %>% 
  setNames("pred")



# Table x. Integrated approaches - LICHEN---------------------------------

#ordered by validation dataset
integrated_lichen_eval_df <- bind_rows(
  PO_PA_lichen_eval_df %>% mutate(model = "PO + PA"),
  PO_PA_bias_lichen_eval_df %>% mutate(model = "PO bias + PA"),
  Plantarctica_PA_lichen_eval_df %>% mutate(model = "Satellite + PA"),
  PO_Plantarctica_PA_lichen_eval_df %>% mutate(model = "PO + Satellite + PA"),
  PO_PA_BUNGER_lichen_eval_df %>% mutate(model = "PO + PA"),
  PO_PA_bias_BUNGER_lichen_eval_df %>% mutate(model = "PO bias + PA"),
  Plantarctica_PA_BUNGER_lichen_eval_df %>% mutate(model = "Satellite + PA"),
  PO_Plantarctica_PA_BUNGER_lichen_eval_df %>% mutate(model = "PO + Satellite + PA")
  ) 

# %>% 
#   mutate(validation_dataset = factor(validation_dataset, levels = c("Training data","Vestfold", "Bunger23"))) %>% 
#   arrange(validation_dataset) 


write.csv(integrated_lichen_eval_df,
          file = here(outpath, paste0("Integrated_lichen_eval_df_", scenario_all, ".csv")),
          row.names = FALSE)

# Figure x. Integrated approaches - LICHEN ----------------------------

PA_Vestfold_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_vestfold_19.shp"))
#PA_Vestfold_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_vestfold.shp"))

PA_Vestfold_Veg_df <- PA_Vestfold_Veg_sf %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  bind_cols(st_drop_geometry(PA_Vestfold_Veg_sf)) %>% 
  rename(x = X, y = Y)

PA_Vestfold <- PA_Vestfold_Veg_df %>% 
  dplyr::select(x, y, srfc_lc) %>% 
  rename(Presence = srfc_lc) %>% 
  mutate(Presence = ifelse(Presence == 1, "Presence", "Absence"))

PA_Bunger23_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_bunger23.shp"))

PA_Bunger23_Veg_df <- PA_Bunger23_Veg_sf %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(st_drop_geometry(PA_Bunger23_Veg_sf)) %>%
  rename(x = X, y = Y)

PA_bunger23 <- PA_Bunger23_Veg_df %>%
  dplyr::select(x, y, srfc_lc) %>%
  rename(Presence = srfc_lc) %>% 
  mutate(Presence = ifelse(Presence == 1, "Presence", "Absence"))


#####################################
# Vestfold plot (WITH DATA)

p1 <- PO_PA_bias_BUNGER_lichen_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_Vestfold, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA + PO") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines
  

p2 <- Plantarctica_PA_BUNGER_lichen_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_Vestfold, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA + Satellite") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines

p3 <- PO_Plantarctica_PA_BUNGER_lichen_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_Vestfold, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA + Satellite + PO") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines


Vestfold_plot <- ggarrange(p1, p2, p3,
                           ncol = 3, nrow = 1,
                           common.legend = TRUE, legend = "bottom")

# Save
ggsave(paste0(outpath, "/Integrated_prediction_plot_lichen_VESTFOLD.png"), Vestfold_plot,
       width = 20, height = 13, , unit = "cm", dpi = 400)


#####################################
# Bunger plot (WITH DATA)

p1b <- PO_PA_bias_lichen_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_bunger23, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA + PO") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines


p2b <- Plantarctica_PA_lichen_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_bunger23, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA + Satellite") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines

p3b <- PO_Plantarctica_PA_lichen_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_bunger23, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA + Satellite + PO") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines


Bunger_plot <- ggarrange(p1b, p2b, p3b,
                           ncol = 3, nrow = 1,
                           common.legend = TRUE, legend = "bottom")

# Save
ggsave(paste0(outpath, "/Integrated_prediction_plot_lichen_BUNGER.png"), Bunger_plot,
       width = 20, height = 13, , unit = "cm", dpi = 400)




####################################################################
# 2. INTEGRATED APPROACHES - MOSS -----------------------------------
###################################################################


# PO + PA (VESTFOLD)

scenario = "500m_ALL_DATASETS_SEASON19"
mod_name = "m.int.occ.VH"

PO_PA_moss_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_df.csv")) %>%
  filter(model == mod_name) %>%
  mutate(validation_dataset = "Bunger23") %>%
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

PO_PA_moss_eval_df <- PO_PA_moss_eval_df %>% 
  add_row(X = 3,
          model = mod_name,
          validation_dataset = "Training data (VH)",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

PO_PA_moss_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/BUNGER_Probability_prediction_", mod_name,"_median.tif")) %>% 
  setNames("pred")


# PO + PA (BUNGER)

scenario = "500m_ALL_DATASETS_BUNGER_linear"
mod_name = "m.int.occ.VH"

PO_PA_BUNGER_moss_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_df.csv")) %>%
  filter(model == mod_name) %>%
  mutate(validation_dataset = "Vestfold") %>%
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

PO_PA_BUNGER_moss_eval_df <- PO_PA_BUNGER_moss_eval_df %>% 
  add_row(X = 3,
          model = mod_name,
          validation_dataset = "Training data (BH)",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

PO_PA_BUNGER_moss_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/VESTFOLD_Probability_prediction_", mod_name,"_median.tif")) %>% 
  setNames("pred")


# PO bias + PA (VESTFOLD)

scenario = "500m_ALL_DATASETS_SEASON19"
mod_name = "m.int.occ.VH.bias"


PO_PA_bias_moss_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_df.csv")) %>%
  filter(model == mod_name) %>%
  mutate(validation_dataset = "Bunger23") %>%
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

PO_PA_bias_moss_eval_df <- PO_PA_bias_moss_eval_df %>% 
  add_row(X = 3,
          model = mod_name,
          validation_dataset = "Training data (VH)",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

PO_PA_bias_moss_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/BUNGER_Probability_prediction_", mod_name,"_median.tif")) %>% 
  setNames("pred")


# PO bias + PA (BUNGER)

scenario = "500m_ALL_DATASETS_BUNGER_linear"
mod_name = "m.int.occ.VH.bias"


PO_PA_bias_BUNGER_moss_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_df.csv")) %>%
  filter(model == mod_name) %>%
  mutate(validation_dataset = "Vestfold") %>%
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

PO_PA_bias_BUNGER_moss_eval_df <- PO_PA_bias_BUNGER_moss_eval_df %>% 
  add_row(X = 3,
          model = mod_name,
          validation_dataset = "Training data (BH)",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

PO_PA_bias_BUNGER_moss_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/VESTFOLD_Probability_prediction_", mod_name,"_median.tif")) %>% 
  setNames("pred")


# Plantarctica + PA (VESTFOLD)

scenario = "500m_ALL_DATASETS_SEASON19"
mod_name = "m.int.Plantarctica.VH"

Plantarctica_PA_moss_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_df.csv")) %>%
  filter(model == mod_name) %>%
  mutate(validation_dataset = "Bunger23") %>%
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

Plantarctica_PA_moss_eval_df <- Plantarctica_PA_moss_eval_df %>% 
  add_row(X = 3,
          model = mod_name,
          validation_dataset = "Training data (VH)",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

Plantarctica_PA_moss_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/BUNGER_Probability_prediction_", mod_name,"_median.tif")) %>% 
  setNames("pred")


# Plantarctica + PA (BUNGER)

scenario = "500m_ALL_DATASETS_BUNGER_linear"
mod_name = "m.int.Plantarctica.VH"

Plantarctica_PA_BUNGER_moss_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_df.csv")) %>%
  filter(model == mod_name) %>%
  mutate(validation_dataset = "Vestfold") %>%
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

Plantarctica_PA_BUNGER_moss_eval_df <- Plantarctica_PA_BUNGER_moss_eval_df %>% 
  add_row(X = 3,
          model = mod_name,
          validation_dataset = "Training data (BH)",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

Plantarctica_PA_BUNGER_moss_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/VESTFOLD_Probability_prediction_", mod_name,"_median.tif")) %>% 
  setNames("pred")



# PO + Plantarctica + PA (VESTFOLD)

scenario = "500m_ALL_DATASETS_SEASON19"
mod_name = "m.int.occ.Plantarctica.VH"

PO_Plantarctica_PA_moss_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_df.csv")) %>%
  filter(model == mod_name) %>%
  mutate(validation_dataset = "Bunger23") %>%
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

PO_Plantarctica_PA_moss_eval_df <- PO_Plantarctica_PA_moss_eval_df %>% 
  add_row(X = 3,
          model = mod_name,
          validation_dataset = "Training data (VH)",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

PO_Plantarctica_PA_moss_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/BUNGER_Probability_prediction_", mod_name,"_median.tif")) %>% 
  setNames("pred")


# PO + Plantarctica + PA (BUNGER)

scenario = "500m_ALL_DATASETS_BUNGER_linear"
mod_name = "m.int.occ.Plantarctica.VH"

PO_Plantarctica_PA_BUNGER_moss_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_df.csv")) %>%
  filter(model == mod_name) %>%
  mutate(validation_dataset = "Vestfold") %>%
  relocate(validation_dataset, .after = model)

fit <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_PA_fit_df.csv")) [1,]

PO_Plantarctica_PA_BUNGER_moss_eval_df <- PO_Plantarctica_PA_BUNGER_moss_eval_df %>% 
  add_row(X = 3,
          model = mod_name,
          validation_dataset = "Training data (BH)",
          ROC = fit$ROC,
          PRG = fit$PRG,
          boyce = fit$boyce,
          partialROC = fit$partialROC,
          brier = fit$brier)

PO_Plantarctica_PA_BUNGER_moss_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/VESTFOLD_Probability_prediction_", mod_name,"_median.tif")) %>% 
  setNames("pred")



# Table x. Integrated approaches - MOSS ---------------------------------

#ordered by validation dataset
integrated_moss_eval_df <- bind_rows(
  PO_PA_moss_eval_df %>% mutate(model = "PO + PA"),
  PO_PA_bias_moss_eval_df %>% mutate(model = "PO bias + PA"),
  Plantarctica_PA_moss_eval_df %>% mutate(model = "Satellite + PA"),
  PO_Plantarctica_PA_moss_eval_df %>% mutate(model = "PO + Satellite + PA"),
  PO_PA_BUNGER_moss_eval_df %>% mutate(model = "PO + PA"),
  PO_PA_bias_BUNGER_moss_eval_df %>% mutate(model = "PO bias + PA"),
  Plantarctica_PA_BUNGER_moss_eval_df %>% mutate(model = "Satellite + PA"),
  PO_Plantarctica_PA_BUNGER_moss_eval_df %>% mutate(model = "PO + Satellite + PA")
) 

# %>% 
#   mutate(validation_dataset = factor(validation_dataset, levels = c("Training data","Vestfold", "Bunger23"))) %>% 
#   arrange(validation_dataset) 


write.csv(integrated_moss_eval_df,
          file = here(outpath, paste0("Integrated_moss_eval_df_", scenario_all, ".csv")),
          row.names = FALSE)


# Figure x. Integrated approaches - MOSS ----------------------------

PA_Vestfold_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_vestfold_19.shp"))
#PA_Vestfold_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_vestfold.shp"))

PA_Vestfold_Veg_df <- PA_Vestfold_Veg_sf %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  bind_cols(st_drop_geometry(PA_Vestfold_Veg_sf)) %>% 
  rename(x = X, y = Y)

PA_Vestfold <- PA_Vestfold_Veg_df %>% 
  dplyr::select(x, y, srfc_ms) %>% 
  rename(Presence = srfc_ms) %>% 
  mutate(Presence = ifelse(Presence == 1, "Presence", "Absence"))

PA_Bunger23_Veg_sf <- st_read(here("Data/Biological_records", "PA_Veg_bunger23.shp"))

PA_Bunger23_Veg_df <- PA_Bunger23_Veg_sf %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(st_drop_geometry(PA_Bunger23_Veg_sf)) %>%
  rename(x = X, y = Y)

PA_bunger23 <- PA_Bunger23_Veg_df %>%
  dplyr::select(x, y, srfc_ms) %>%
  rename(Presence = srfc_ms) %>% 
  mutate(Presence = ifelse(Presence == 1, "Presence", "Absence"))


#####################################
# Vestfold plot (WITH DATA)

m1 <- PO_PA_bias_BUNGER_moss_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_Vestfold, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA + PO") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines


m2 <- Plantarctica_PA_BUNGER_moss_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_Vestfold, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA + Satellite") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines

m3 <- PO_Plantarctica_PA_BUNGER_moss_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_Vestfold, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA + Satellite + PO") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines


Vestfold_plot <- ggarrange(m1, m2, m3,
                           ncol = 3, nrow = 1,
                           common.legend = TRUE, legend = "bottom")

# Save
ggsave(paste0(outpath, "/Integrated_prediction_plot_moss_VESTFOLD.png"), Vestfold_plot,
       width = 20, height = 13, , unit = "cm", dpi = 400)


#####################################
# Bunger plot (WITH DATA)

m1b <- PO_PA_bias_moss_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_bunger23, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA + PO") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines


m2b <- Plantarctica_PA_moss_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_bunger23, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA + Satellite") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines

m3b <- PO_Plantarctica_PA_moss_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred), alpha = 0.75) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  geom_point(data = PA_bunger23, 
             aes(x = x, y = y, color = Presence),
             shape = 1, size = 0.7, stroke = 0.4) +  # shape = 1 is open circle
  scale_color_manual(values = c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1]),
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 2))) +
  coord_fixed() +
  labs(title = "PA + Satellite + PO") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        legend.text = element_text(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) # remove grid lines


Bunger_plot <- ggarrange(m1b, m2b, m3b,
                         ncol = 3, nrow = 1,
                         common.legend = TRUE, legend = "bottom")

# Save
ggsave(paste0(outpath, "/Integrated_prediction_plot_moss_BUNGER.png"), Bunger_plot,
       width = 20, height = 13, , unit = "cm", dpi = 400)



###########################
### FIGURE 6 (Integrated outputs, lichens and moss)
###########################

Figure_6 <- ggarrange(p1, p2, p3, p1b, p2b, p3b,
                      m1, m2, m3, m1b, m2b, m3b,
                      ncol = 3,
                      nrow = 4,
                      common.legend = T,
                      legend.position = "bottom")

ggsave(filename = paste0(outpath, "/FIGURE_6_", scenario_all, ".png"), Figure_6[[1]],
       width = 20, height = 30, unit = "cm", dpi = 600, bg = "white")



#########################
#### FIGURE PLOTTING ALL RESULTS (single-dataset and integrated)
#########################

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

##########################################
# OVERALL SCENARIO NAME
scenario_all <- "Jul_23_linear"

outpath <- here("Outputs/Results")
##########################################

# Thresholds for plotting:
# Define moderate and high performance thresholds
moderate_thresholds <- list(
  ROC = 0.7,      # AUC > 0.7 is often considered acceptable/moderate
  boyce = 0.5,    # Boyce index > 0.5 indicates moderate performance
  brier = 0.25    # Lower is better; 0.25 is a common threshold
)

high_thresholds <- list(
  ROC = 0.9,      # AUC > 0.9 is excellent
  boyce = 0.75,   # Boyce index > 0.75 indicates high performance
  brier = 0.15    # Lower is better; < 0.15 is high performance
)


single_moss_df <- read.csv(file = here(outpath, paste0("Single_dataset_moss_eval_df_", scenario_all, ".csv"))) %>% 
  mutate(group = "moss")
single_lichen_df <- read.csv(file = here(outpath, paste0("Single_dataset_lichen_eval_df_", scenario_all, ".csv"))) %>% 
  mutate(group = "lichen")

integrated_moss_df <- read.csv(file = here(outpath, paste0("Integrated_moss_eval_df_", scenario_all, ".csv"))) %>% 
  mutate(group = "moss")
integrated_lichen_df <- read.csv(file = here(outpath, paste0("Integrated_lichen_eval_df_", scenario_all, ".csv"))) %>% 
  mutate(group = "lichen")

evaluation_df_all <- rbind(single_moss_df, single_lichen_df, integrated_moss_df, integrated_lichen_df) %>% 
  filter(!is.na(validation_dataset))


evaluation_df_single <- rbind(single_moss_df, single_lichen_df) %>% 
  filter(!is.na(validation_dataset))

evaluation_df_single <- evaluation_df_single %>%
  mutate(
    # split into dataset and variant
    dataset = str_extract(model, "PA|PO|Plantarctica"),
    variant = case_when(
      str_detect(model, "PPP") ~ "ppm",
      str_detect(model, "Ensemble") ~ "Ensemble",
      TRUE ~ "Other"
    )
  ) 



# Prepare data with renamed factors
evaluation_df_single <- evaluation_df_single %>%
  mutate(
    validation_dataset = factor(validation_dataset,
                                levels = c("Training data", "Vestfold", "Bunger23"),
                                labels = c("Training data", "Prediction - Vestfold Hills", "Prediction - Bunger Hills")),
    dataset = factor(dataset,
                     levels = c("PA", "PO", "Plantarctica"),
                     labels = c("Structured surveys", "Occurrence database", "Satellite-derived detections"))
  )


evaluation_df_single <- evaluation_df_single %>% 
  mutate(
    boyce = ifelse(dataset == "Structured surveys" & validation_dataset == "Training data", NA, boyce), 
    brier = ifelse(dataset == "Structured surveys" & validation_dataset == "Training data", NA, brier)
  )

## LICHEN




base_plot <- function(metric_name, metric_label) {
  evaluation_df_single %>% 
    filter(group == "lichen") %>% 
    ggplot(aes(x = dataset, y = .data[[metric_name]], colour = dataset, shape = variant)) +
    geom_hline(yintercept = moderate_thresholds[[metric_name]], 
               linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_hline(yintercept = high_thresholds[[metric_name]], 
               linetype = "dotted", color = "gray30", linewidth = 0.5) +
    geom_point(position = position_dodge(width = 0.6), size = 3) +
    facet_wrap(~ validation_dataset, ncol = 3) +
    scale_color_manual(values = c("Structured surveys" = "black", 
                                  "Occurrence database" = "orange", 
                                  "Satellite-derived detections" = "purple")) +
    scale_shape_manual(values = c("Ensemble" = 16, "ppm" = 17)) +
    theme_bw(base_size = 13) +
    labs(x = NULL, y = metric_label, colour = "Data type", shape = "Model variant") +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.box = "vertical",
      strip.text.x = element_text(size = 13.5),
      strip.background = element_rect(fill = ifelse(levels(evaluation_df_single$validation_dataset) == "Training data", 
                                                    "gray90", "white"))
      
    ) +
    guides(colour = guide_legend(order = 1),
           shape = guide_legend(order = 2))
}

# Create individual plots
p1 <- base_plot("ROC", "AUC-ROC")
p2 <- base_plot("boyce", "Boyce index")
p3 <- base_plot("brier", "Brier score") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Combine plots with shared legend and x-axis
p <- ggarrange(p1, p2, p3,
               ncol = 1,
               nrow = 3,
               common.legend = TRUE,
               legend = "bottom",
               align = "v",
               heights = c(1, 1, 1))

ggsave(filename = paste0(outpath, "/FIGURE_X_lichen_", scenario_all, ".png"), p,
       width = 20, height = 20, unit = "cm", dpi = 600, bg = "white")



## MOSS


base_plot <- function(metric_name, metric_label) {
  evaluation_df_single %>% 
    filter(group == "moss") %>% 
    ggplot(aes(x = dataset, y = .data[[metric_name]], colour = dataset, shape = variant)) +
    geom_hline(yintercept = moderate_thresholds[[metric_name]], 
               linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_hline(yintercept = high_thresholds[[metric_name]], 
               linetype = "dotted", color = "gray30", linewidth = 0.5) +
    geom_point(position = position_dodge(width = 0.6), size = 3) +
    facet_wrap(~ validation_dataset, ncol = 3) +
    scale_color_manual(values = c("Structured surveys" = "black", 
                                  "Occurrence database" = "orange", 
                                  "Satellite-derived detections" = "purple")) +
    scale_shape_manual(values = c("Ensemble" = 16, "ppm" = 17)) +
    theme_bw(base_size = 13) +
    labs(x = NULL, y = metric_label, colour = "Data type", shape = "Model variant") +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.box = "vertical",
      strip.text.x = element_text(size = 13.5),
      strip.background = element_rect(fill = ifelse(levels(evaluation_df_single$validation_dataset) == "Training data", 
                                                    "gray90", "white"))
      
    ) +
    guides(colour = guide_legend(order = 1),
           shape = guide_legend(order = 2))
}

# Create individual plots
p1 <- base_plot("ROC", "AUC-ROC")
p2 <- base_plot("boyce", "Boyce index")
p3 <- base_plot("brier", "Brier score") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Combine plots with shared legend and x-axis
p <- ggarrange(p1, p2, p3,
               ncol = 1,
               nrow = 3,
               common.legend = TRUE,
               legend = "bottom",
               align = "v",
               heights = c(1, 1, 1))

ggsave(filename = paste0(outpath, "/FIGURE_X_moss_", scenario_all, ".png"), p,
       width = 20, height = 20, unit = "cm", dpi = 600, bg = "white")



# Now integrated: ---------------------------------------------------------

# Remove training from evaluation 
evaluation_df_single <- evaluation_df_single %>% 
  filter(validation_dataset != "Training data") %>%
  mutate(dataset = "Single-dataset")

integrated_moss_df <- read.csv(file = here(outpath, paste0("Integrated_moss_eval_df_", scenario_all, ".csv"))) %>% 
  mutate(group = "moss")
integrated_lichen_df <- read.csv(file = here(outpath, paste0("Integrated_lichen_eval_df_", scenario_all, ".csv"))) %>% 
  mutate(group = "lichen")

evaluation_df_integrated <- rbind(integrated_moss_df, integrated_lichen_df) %>% 
  filter(!is.na(validation_dataset)) %>% 
  filter(model != "PO bias + PA") %>% 
  mutate(model = ifelse(model == "PO bias + PA", "PO + PA", model))

# Remove training from evaluation 
evaluation_df_integrated <- evaluation_df_integrated %>% 
  filter(validation_dataset != "Training data (BH)") %>% 
  filter(validation_dataset != "Training data (VH)")

# Prepare data with renamed factors
evaluation_df_integrated <- evaluation_df_integrated %>%
  mutate(
    validation_dataset = factor(validation_dataset,
                                levels = c("Vestfold", "Bunger23"),
                                labels = c("Vestfold Hills", "Bunger Hills")),
    dataset = factor(model,
                     levels = c("PO + PA", "Satellite + PA", "PO + Satellite + PA"),
                     labels = c("PA + PO", "PA + Satellite", "PA + PO + Satellite"))
  )


# Update validation_dataset for single to match
evaluation_df_single <- evaluation_df_single %>%
  mutate(
    validation_dataset = factor(validation_dataset,
                                levels = c("Prediction - Vestfold Hills", "Prediction - Bunger Hills"),
                                labels = c("Vestfold Hills", "Bunger Hills"))
  )

# Combine datasets
evaluation_df_combined <- bind_rows(
  evaluation_df_single %>% select(validation_dataset, ROC, boyce, brier, group, dataset),
  evaluation_df_integrated %>% select(validation_dataset, ROC, boyce, brier, group, dataset)
) %>%
  mutate(
    dataset = factor(dataset, 
                     levels = c("Single-dataset", "PA + PO", "PA + Satellite", "PA + PO + Satellite"))
  )




## LICHEN
base_plot_lichen <- function(metric_name, metric_label) {
  evaluation_df_combined %>% 
    filter(group == "lichen") %>% 
    ggplot(aes(x = dataset, y = .data[[metric_name]], 
               colour = dataset)) +
    geom_hline(yintercept = moderate_thresholds[[metric_name]], 
               linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_hline(yintercept = high_thresholds[[metric_name]], 
               linetype = "dotted", color = "gray30", linewidth = 0.5) +
    geom_point(position = position_dodge(width = 0.6), size = 3) +
    facet_wrap(~ validation_dataset, ncol = 2) +
    scale_color_manual(
      values = c(
        "Single-dataset" = "grey60",
        "PA + PO" = "darkorange3",
        "PA + Satellite" = "purple4",
        "PA + PO + Satellite" = "darkred"
      )
    ) +
    theme_bw(base_size = 13) +
    labs(x = NULL, y = metric_label, colour = "Model type") +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.box = "vertical",
      strip.text.x = element_text(size = 13.5)
    ) +
    guides(colour = guide_legend(order = 1))
}


## MOSS
base_plot_moss <- function(metric_name, metric_label) {
  evaluation_df_combined %>% 
    filter(group == "moss") %>% 
    ggplot(aes(x = dataset, y = .data[[metric_name]], 
               colour = dataset)) +
    geom_hline(yintercept = moderate_thresholds[[metric_name]], 
               linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_hline(yintercept = high_thresholds[[metric_name]], 
               linetype = "dotted", color = "gray30", linewidth = 0.5) +
    geom_point(position = position_dodge(width = 0.6), size = 3) +
    facet_wrap(~ validation_dataset, ncol = 2) +
    scale_color_manual(
      values = c(
        "Single-dataset" = "grey60",
        "PA + PO" = "darkorange3",
        "PA + Satellite" = "purple4",
        "PA + PO + Satellite" = "darkred"
      )
    ) +
    theme_bw(base_size = 13) +
    labs(x = NULL, y = metric_label, colour = "Model type") +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.box = "vertical",
      strip.text.x = element_text(size = 13.5)
    ) +
    guides(colour = guide_legend(order = 1))
}

library(gridExtra)

# All plots without legends
p1 <- base_plot_lichen("ROC", "AUC-ROC") + theme(legend.position = "none")
p2 <- base_plot_lichen("boyce", "Boyce index") + theme(legend.position = "none")
p3 <- base_plot_lichen("brier", "Brier score") + theme(legend.position = "none")
p1b <- base_plot_moss("ROC", "AUC-ROC") + rremove("ylab") + theme(legend.position = "none")
p2b <- base_plot_moss("boyce", "Boyce index") + rremove("ylab") + theme(legend.position = "none")
p3b <- base_plot_moss("brier", "Brier score") + rremove("ylab") + theme(legend.position = "none")

lichen <- ggarrange(p1, p2, p3, ncol = 1, nrow = 3, align = "v")
moss <- ggarrange(p1b, p2b, p3b, ncol = 1, nrow = 3, align = "v")

# Create a dummy plot just to extract the legend
dummy <- base_plot_lichen("ROC", "AUC-ROC") + 
  theme(legend.position = "bottom", 
        legend.direction = "horizontal")
legend <- as_ggplot(get_legend(dummy))

# Combine with legend
combined_plot <- ggarrange(
  ggarrange(lichen, moss, ncol = 2, labels = c("(a)", "(b)"), hjust = -0.5, vjust = 1.5),
  legend,
  ncol = 1, nrow = 2,
  heights = c(1, 0.1)
)

ggsave(filename = paste0(outpath, "/FIGURE_X_combined", scenario_all, ".png"), 
       combined_plot,
       width = 20, height = 21.5, unit = "cm", dpi = 600, bg = "white")






# ARCHIVE COLOUR FOR PRESENCE - ABSENCE VIRIDIS ---------------------------

# c("Presence" = viridis(2)[2], "Absence" = viridis(2)[1])
