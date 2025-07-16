
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


####################################################################
# 1. SINGLE DATASET APPROACHES - LICHEN -----------------------------------
###################################################################


# Ensemble PO 

scenario = "PO_Ensemble_Jul_16"

inpath <- here("Outputs/Ensemble/Lichen")

PO_ensemble_lichen_eval_df <- read.csv(file = here(inpath, scenario, "Ensemble_eval_df.csv"))

PO_ensemble_lichen_pred <- rast(here(inpath, scenario, "Prediction_ensemble_East_Antarctica.tif"))


# Ensemble Plantarctica

scenario = "Plantarctica_Ensemble_Jul_16"

inpath <- here("Outputs/Ensemble_Plantarctica/Lichen")

Plantarctica_ensemble_lichen_eval_df <- read.csv(file = here(inpath, scenario, "Ensemble_eval_df.csv"))

Plantarctica_ensemble_lichen_pred <- rast(here(inpath, scenario, "Prediction_ensemble_East_Antarctica.tif"))


# Ensemble PA 

scenario = "PA_Ensemble_Jul_16"

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


# Table x. Single dataset approaches - LICHEN---------------------------------

#ordered by validation dataset
single_dataset_lichen_eval_df <- bind_rows(
  PO_ensemble_lichen_eval_df %>% mutate(model = "PO Ensemble"),
  Plantarctica_ensemble_lichen_eval_df %>% mutate(model = "Plantarctica Ensemble"),
  PA_ensemble_lichen_eval_df %>% mutate(model = "PA Ensemble")
) %>% 
  mutate(validation_dataset = factor(validation_dataset, levels = c("Training data","Vestfold", "Bunger23"))) %>% 
  arrange(validation_dataset) 

write.csv(single_dataset_lichen_eval_df,
          file = here(outpath, "Single_dataset_lichen_eval_df.csv"),
          row.names = FALSE)

# Figure x. Single dataset approaches - LICHEN ----------------------------

# Vestfold plot

p1 <- PO_ensemble_lichen_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Lichen - PO Ensemble") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p2 <- Plantarctica_ensemble_lichen_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Lichen - Plantarctica") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p3 <- PA_ensemble_lichen_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Lichen - PA Ensemble") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

Vestfold_plot <- ggarrange(p1, p2, p3,
                            ncol = 3, nrow = 1,
                            common.legend = TRUE, legend = "bottom")

# Save
ggsave(paste0(outpath, "/Single_dataset_prediction_plot_lichen_VESTFOLD.png"), Vestfold_plot,
       width = 10, height = 4, dpi = 300)

######
####### Bunger plot
######

p1 <- PO_ensemble_lichen_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Lichen - PO Ensemble") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p2 <- Plantarctica_ensemble_lichen_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Lichen - Plantarctica") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p3 <- PA_ensemble_lichen_pred %>% 
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Lichen - PA Ensemble") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

Bunger_plot <- ggarrange(p1, p2, p3,
                           ncol = 3, nrow = 1,
                           common.legend = TRUE, legend = "bottom")

# Save
ggsave(paste0(outpath, "/Single_dataset_prediction_plot_lichen_BUNGER.png"), Bunger_plot,
       width = 10, height = 4, dpi = 300)

####################################################################
# 1. SINGLE DATASET APPROACHES - MOSS -------------------------------------
###################################################################


# Ensemble PO 

scenario = "PO_Ensemble_Jul_16"

inpath <- here("Outputs/Ensemble/Moss")

PO_ensemble_moss_eval_df <- read.csv(file = here(inpath, scenario, "Ensemble_eval_df.csv"))

PO_ensemble_moss_pred <- rast(here(inpath, scenario, "Prediction_ensemble_East_Antarctica.tif"))


# Ensemble Plantarctica

scenario = "Plantarctica_Ensemble_Jul_16"

inpath <- here("Outputs/Ensemble_Plantarctica/Moss")

Plantarctica_ensemble_moss_eval_df <- read.csv(file = here(inpath, scenario, "Ensemble_eval_df.csv"))

Plantarctica_ensemble_moss_pred <- rast(here(inpath, scenario, "Prediction_ensemble_East_Antarctica.tif"))


# PA 

scenario = "PA_Ensemble_Jul_16"

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


# Table x. Single dataset approaches - MOSS---------------------------------

#ordered by validation dataset
single_dataset_moss_eval_df <- bind_rows(
  PO_ensemble_moss_eval_df %>% mutate(model = "PO Ensemble"),
  Plantarctica_ensemble_moss_eval_df %>% mutate(model = "Plantarctica Ensemble"),
  PA_ensemble_moss_eval_df %>% mutate(model = "PA Ensemble")
) %>% 
  mutate(validation_dataset = factor(validation_dataset, levels = c("Training data","Vestfold", "Bunger23"))) %>% 
  arrange(validation_dataset)  

write.csv(single_dataset_moss_eval_df,
          file = here(outpath, "Single_dataset_moss_eval_df.csv"),
          row.names = FALSE)

# Figure x. Single dataset approaches - moss ----------------------------

# Vestfold plot

p1 <- PO_ensemble_moss_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "moss - PO Ensemble") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p2 <- Plantarctica_ensemble_moss_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Moss - Plantarctica") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p3 <- PA_ensemble_moss_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Lichen - PA Ensemble") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

Vestfold_plot <- ggarrange(p1, p2, p3,
                           ncol = 3, nrow = 1,
                           common.legend = TRUE, legend = "bottom")

# Save
ggsave(paste0(outpath, "/Single_dataset_prediction_plot_moss_VESTFOLD.png"), Vestfold_plot,
       width = 10, height = 4, dpi = 300)

######
####### Bunger plot
######

p1 <- PO_ensemble_moss_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Moss - PO Ensemble") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p2 <- Plantarctica_ensemble_moss_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Moss - Plantarctica") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p3 <- PA_ensemble_moss_pred %>% 
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Lichen - PA Ensemble") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

Bunger_plot <- ggarrange(p1, p2, p3,
                         ncol = 3, nrow = 1,
                         common.legend = TRUE, legend = "bottom")

# Save
ggsave(paste0(outpath, "/Single_dataset_prediction_plot_moss_BUNGER.png"), Bunger_plot,
       width = 10, height = 4, dpi = 300)


