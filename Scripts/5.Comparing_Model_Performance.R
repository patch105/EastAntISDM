
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


# Poisson point process PO 

scenario = "500m_ALL_DATASETS"

PO_PPP_lichen_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_df.csv")) %>% 
  filter(model == "m.PO") %>% 
  mutate(validation_dataset = "Bunger23") %>% 
  relocate(validation_dataset, .after = model)

PO_PPP_lichen_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/Probability_prediction_m.PO_median.tif"))


# Poisson point process Plantarctica

scenario = "500m_ALL_DATASETS"

Plantarctica_PPP_lichen_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_df.csv"))%>% 
  filter(model == "m.PO.Plantarctica") %>% 
  mutate(validation_dataset = "Bunger23") %>% 
  relocate(validation_dataset, .after = model)

Plantarctica_PPP_lichen_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/Probability_prediction_m.PO.Plantarctica_median.tif"))


# Poisson point process PA 

scenario = "500m_ALL_DATASETS"

PA_PPP_lichen_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/RISDM_eval_df.csv")) %>% 
  filter(model == "m.PA") %>% 
  mutate(validation_dataset = "Bunger23") %>% 
  relocate(validation_dataset, .after = model)

PA_PPP_lichen_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Lichen/", scenario, "/BUNGER_Probability_prediction_m.PA_median.tif"))



# Table x. Single dataset approaches - LICHEN---------------------------------

#ordered by validation dataset
single_dataset_lichen_eval_df <- bind_rows(
  PO_ensemble_lichen_eval_df %>% mutate(model = "PO Ensemble"),
  Plantarctica_ensemble_lichen_eval_df %>% mutate(model = "Plantarctica Ensemble"),
  PA_ensemble_lichen_eval_df %>% mutate(model = "PA Ensemble"),
  PO_PPP_lichen_eval_df %>% mutate(model = "PO PPP"),
  Plantarctica_PPP_lichen_eval_df %>% mutate(model = "Plantarctica PPP"),
  PA_PPP_lichen_eval_df %>% mutate(model = "PA PPP")
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

p4 <- PO_PPP_lichen_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Lichen - PO PPP") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p5 <- Plantarctica_PPP_lichen_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Lichen - Plantarctica PPP") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p6 <- PA_PPP_lichen_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Lichen - PA PPP") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())


Vestfold_plot <- ggarrange(p1, p2, p3, p4, p5, p6,
                            ncol = 3, nrow = 2,
                            common.legend = TRUE, legend = "bottom")

# Save
ggsave(paste0(outpath, "/Single_dataset_prediction_plot_lichen_VESTFOLD.png"), Vestfold_plot,
       width = 20, height = 13, , unit = "cm", dpi = 400)



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


p4 <- PO_PPP_lichen_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Lichen - PO PPP") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p5 <- Plantarctica_PPP_lichen_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Lichen - Plantarctica PPP") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p6 <- PA_PPP_lichen_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Lichen - PA PPP") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())


Bunger_plot <- ggarrange(p1, p2, p3, p4, p5, p6,
                           ncol = 3, nrow = 2,
                           common.legend = TRUE, legend = "bottom")

# Save
ggsave(paste0(outpath, "/Single_dataset_prediction_plot_lichen_BUNGER.png"), Bunger_plot,
       width = 20, height = 13, , unit = "cm", dpi = 400)



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


# Poisson point process PO 

scenario = "500m_ALL_DATASETS"

PO_PPP_moss_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_df.csv")) %>% 
  filter(model == "m.PO") %>% 
  mutate(validation_dataset = "Bunger23") %>% 
  relocate(validation_dataset, .after = model)

PO_PPP_moss_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/Probability_prediction_m.PO_median.tif"))


# Poisson point process Plantarctica

scenario = "500m_ALL_DATASETS"

Plantarctica_PPP_moss_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_df.csv"))%>% 
  filter(model == "m.PO.Plantarctica") %>% 
  mutate(validation_dataset = "Bunger23") %>% 
  relocate(validation_dataset, .after = model)

Plantarctica_PPP_moss_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/Probability_prediction_m.PO.Plantarctica_median.tif"))


# Poisson point process PA 

scenario = "500m_ALL_DATASETS"

PA_PPP_moss_eval_df <- read.csv(file = paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/RISDM_eval_df.csv")) %>% 
  filter(model == "m.PA") %>% 
  mutate(validation_dataset = "Bunger23") %>% 
  relocate(validation_dataset, .after = model)

PA_PPP_moss_pred <- rast(paste0("Z:/ISDM/EastAntISDM/Outputs/Integrated_ALL_DATA/Moss/", scenario, "/BUNGER_Probability_prediction_m.PA_median.tif"))




# Table x. Single dataset approaches - MOSS---------------------------------

#ordered by validation dataset
single_dataset_moss_eval_df <- bind_rows(
  PO_ensemble_moss_eval_df %>% mutate(model = "PO Ensemble"),
  Plantarctica_ensemble_moss_eval_df %>% mutate(model = "Plantarctica Ensemble"),
  PA_ensemble_moss_eval_df %>% mutate(model = "PA Ensemble"),
  PO_PPP_moss_eval_df %>% mutate(model = "PO PPP"),
  Plantarctica_PPP_moss_eval_df %>% mutate(model = "Plantarctica PPP"),
  PA_PPP_moss_eval_df %>% mutate(model = "PA PPP")
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
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = pred_cur_ensemble)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Moss - Plantarctica ensemble") +
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
  labs(title = "Moss - PA Ensemble") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p4 <- PO_PPP_moss_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Moss - PO PPP") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p5 <- Plantarctica_PPP_moss_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Moss - Plantarctica PPP") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p6 <- PA_PPP_moss_pred %>%
  crop(ext(vestfold_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Moss - PA PPP") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())


Vestfold_plot <- ggarrange(p1, p2, p3, p4, p5, p6,
                           ncol = 3, nrow = 2,
                           common.legend = TRUE, legend = "bottom")

# Save
ggsave(paste0(outpath, "/Single_dataset_prediction_plot_moss_VESTFOLD.png"), Vestfold_plot,
       width = 20, height = 13, , unit = "cm", dpi = 400)

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
  labs(title = "Moss - Plantarctica ensemble") +
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
  labs(title = "Moss - PA Ensemble") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p4 <- PO_PPP_moss_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Moss - PO PPP") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p5 <- Plantarctica_PPP_moss_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Moss - Plantarctica PPP") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p6 <- PA_PPP_moss_pred %>%
  crop(ext(bunger_boundary)) %>%
  as.data.frame(xy = T) %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = Median)) +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 7, barheight = 1),
                     name = "Probability") +
  coord_fixed() +
  labs(title = "Moss - PA PPP") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())


Bunger_plot <- ggarrange(p1, p2, p3, p4, p5, p6,
                         ncol = 3, nrow = 2,
                         common.legend = TRUE, legend = "bottom")

# Save
ggsave(paste0(outpath, "/Single_dataset_prediction_plot_moss_BUNGER.png"), Bunger_plot,
       width = 20, height = 13, , unit = "cm", dpi = 400)



