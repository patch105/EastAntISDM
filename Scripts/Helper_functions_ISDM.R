

# Partial dependence plot -------------------------------------------------

# 1) create a raster layer with a constant value for the habitatArea offset, 
# 2) predict specifying which terms should be included (holds other to their mean)
# 3) plot


partial_dependence_func <- function(mod.list,
                                    covs_no_bias,
                                    outpath,
                                    ice_free.EastAnt) {
  
  # 1. First, set up the function per covariate
  partial_dependence_PER_COV_func <- function(covs_no_bias, 
                                              var_name, 
                                              ice_free.EastAnt,
                                              model) {
    
    # Adding a temporary cell area layer
    tmp_layer <- rast(rast(covs_no_bias, nlyrs = 1))
    names(tmp_layer) <- "tmp.habiArea"
    covs_no_bias <- c( covs_no_bias, tmp_layer)
    
    # Area is now constant with log(1)=0 contribution
    values( covs_no_bias$tmp.habiArea) <- 1
    
    # covs_no_bias <- covs_no_bias[[c(var_name, "tmp.habiArea")]]
    
    covs_no_bias$tmp.habiArea <- mask(covs_no_bias$tmp.habiArea, ice_free.EastAnt)
    
    # Remove NAs
    covs_no_bias <- mask(covs_no_bias, sum(covs_no_bias)) 
    
    interpPreds <- predict( model, 
                            covars=covs_no_bias,
                            habitatArea="tmp.habiArea", 
                            S=500,
                            includeFixed=var_name, 
                            includeRandom=FALSE, 
                            type="link")
    
    #compile covariate and prediction
    pred.df <- as.data.frame( cbind( slope=values( covs_no_bias[[var_name]]),
                                     values( interpPreds$field[[c("Median","Lower","Upper")]])))
    #plot
    pred.df <- pred.df[!is.na(pred.df[, var_name]),]
    pred.df <- pred.df[order( pred.df[, var_name]),]
    matplot( pred.df[,1], pred.df[,2:4], pch="", xlab=var_name, ylab="Effect",
             main=paste0("Effect plot for ", var_name))
    polygon( x=c( pred.df[, var_name], rev( pred.df[, var_name])),
             c(pred.df$Upper, rev(pred.df$Lower)),
             col=grey(0.95), bor=NA)
    lines( pred.df[,c(var_name,"Median")], type='l', lwd=2)
    
    # Capture and return the plot
    recordPlot()
  }
  
  
  
  # 2. NOW, for every model run the PDP function per covariate 
  mod_names <- names(mod.list)
  
  cov_names <- names(covs_no_bias)
  
  # For each model, run the PDP function
  
  for(i in seq_along(mod.list)) {
    
    Model <- mod.list[[i]]
    
    name <- mod_names[i] 
    
    PDP_list <- map(cov_names, ~ partial_dependence_PER_COV_func(
      covs_no_bias = covs_no_bias, 
      var_name = .x, 
      ice_free.EastAnt = ice_free.EastAnt, 
      model = Model
    ))
    
    
    # Stack and save the PDPs
    dir.create(here(outpath, paste0(name, "_PDPs")), showWarnings = FALSE)
    
    
    # Loop through PDP_list and save each plot
    walk2(PDP_list, cov_names, ~ {
      png(filename = here(outpath, paste0(name, "_PDPs"), paste0("PDP_", name, "_", .y, ".png")),
          width = 1000, height = 800, res = 150)
      
      replayPlot(.x)
      
      dev.off()
    })
    
    # pdf(here(outpath, paste0("Partial_Dependence_Plots_", name,".pdf")), width = 10, height = 10)
    # par(mfrow = c(ceiling(length(PDP_list)/2), 2))  # adjust columns as needed
    # 
    # walk(PDP_list, replayPlot)
    # 
    # dev.off()
    
    
    }
  
  
}




# EXTRACT MODEL RESULTS ---------------------------------------------------

extract_model_results_func <- function(mod.list) {
  
  # Extract and save summary results ----------------------------------------
  
  # Create an empty list to store results
  results_list <- list()
  
  mod_names <- names(mod.list)
  
  for(i in seq_along(mod.list)) {
    
    Model <- mod.list[[i]]
    
    name <- mod_names[i]
    
    mod.summary <- summary(Model)
    
    results_list[[length(results_list) + 1]] <- data.frame(
      Model = name,
      PO_intercept = NA,
      PO_intercept_25 = NA,
      PO_intercept_975 = NA,
      PO_intercept.sd = NA,
      PA_intercept = NA,
      PA_intercept_25 = NA,
      PA_intercept_975 = NA,
      PA_intercept.sd = NA,
      marg_lik = mod.summary$marg.lik,
      GRF.range.mean = NA,
      GRF.sd.mean = NA,
      GRF.range_25 = NA,
      GRF.range_975 = NA,
      GRF.sd_25 = NA,
      GRF.sd_975 = NA,
      GRF.range.sd = NA,
      GRF.sd.sd = NA,
      bias.coef.mean = NA,
      bias.coef_25 = NA,
      bias.coef_975 = NA,
      bias.coef.sd = NA
    )
    
    # If the model name contains PO or Integrated, save the PO intercept
    if(grepl("PO", name, fixed = T) | grepl("int", name, fixed = T)) {
      
      results_list[[length(results_list)]]$PO_intercept <- mod.summary$PO_BIAS["PO_Intercept", "mean"]
      results_list[[length(results_list)]]$PO_intercept_25 <- mod.summary$PO_BIAS["PO_Intercept", "0.025quant"]
      results_list[[length(results_list)]]$PO_intercept_975 <- mod.summary$PO_BIAS["PO_Intercept", "0.975quant"]
      results_list[[length(results_list)]]$PO_intercept.sd <- mod.summary$PO_BIAS["PO_Intercept", "sd"]
      
    }
    
    # If the model name contains PA or Integrated, save the PA intercept
    if(grepl("PA", name, fixed = T) | grepl("int", name, fixed = T)) {
      
      results_list[[length(results_list)]]$PA_intercept <- mod.summary$PA_ARTEFACT["PA_Intercept", "mean"]
      results_list[[length(results_list)]]$PA_intercept_25 <- mod.summary$PA_ARTEFACT["PA_Intercept", "0.025quant"]
      results_list[[length(results_list)]]$PA_intercept_975 <- mod.summary$PA_ARTEFACT["PA_Intercept", "0.975quant"]
      results_list[[length(results_list)]]$PA_intercept.sd <- mod.summary$PA_ARTEFACT["PA_Intercept", "sd"]
      
    }
    
    # If the model is spatial, save the spatial parameters
    if(grepl("GRF", name, fixed = T)) {
      
      results_list[[length(results_list)]]$GRF.range.mean <- mod.summary$SPATIAL$mean[1]
      results_list[[length(results_list)]]$GRF.sd.mean <- mod.summary$SPATIAL$mean[2]
      results_list[[length(results_list)]]$GRF.range_25 <- mod.summary$SPATIAL[[3]][1]
      results_list[[length(results_list)]]$GRF.range_975 <- mod.summary$SPATIAL[[5]][1]
      results_list[[length(results_list)]]$GRF.sd_25 <- mod.summary$SPATIAL[[3]][2]
      results_list[[length(results_list)]]$GRF.sd_975 <- mod.summary$SPATIAL[[5]][2]
      results_list[[length(results_list)]]$GRF.range.sd <- mod.summary$SPATIAL[[2]][1]
      results_list[[length(results_list)]]$GRF.sd.sd <- mod.summary$SPATIAL[[2]][2]
      
    }
    
    # If the model has a bias covariate, save the bias coefficient
    if(grepl("bias", name, fixed = T)) {
      
      results_list[[length(results_list)]]$bias.coef.mean <- mod.summary$PO_BIAS["PO_log_dist_station", "mean"]
      results_list[[length(results_list)]]$bias.coef_25 <- mod.summary$PO_BIAS["PO_log_dist_station", "0.025quant"]
      results_list[[length(results_list)]]$bias.coef_975 <- mod.summary$PO_BIAS["PO_log_dist_station", "0.975quant"]
      results_list[[length(results_list)]]$bias.coef.sd <- mod.summary$PO_BIAS["PO_log_dist_station", "sd"]
      
    }
    
    
  }
  
  # Combine all the results into a single dataframe
  extrap.scenario.df <- do.call(rbind, results_list)
  
  return(extrap.scenario.df)
  
}


# PREDICT FROM FITTED ---------------------------------------------------

predict_from_fitted_func <- function(mod.list,
                                     posterior_nsamps,
                                     covs,
                                     pred.GRF = T) {
  
  # Create an empty list to store results
  results_list <- list()
  
  mod_names <- names(mod.list)
  
  for(i in seq_along(mod.list)) {
    
    Model <- mod.list[[i]]
    
    name <- mod_names[i] 
    
    ######################
    ### NON-SPATIAL MODEL
    ######################
    
    if(!grepl("GRF", name, fixed = T)) {
      
      if(grepl("PO", name, fixed = T)) { # If models are PO, use PO intercept
        
        Model$preds.link <- predict(Model,
                                    covars = covs,
                                    S = posterior_nsamps, 
                                    intercept.terms = "PO_Intercept",
                                    type = "link",
                                    includeRandom = F) # No GRF
        
        Model$preds.intensity <- predict(Model,
                                         covars = covs,
                                         S = posterior_nsamps, 
                                         intercept.terms = "PO_Intercept",
                                         type = "intensity",
                                         includeRandom = F) # No GRF
        
        Model$preds.prob <- predict(Model,
                                    covars = covs,
                                    S = posterior_nsamps, 
                                    intercept.terms = "PO_Intercept",
                                    type = "probability",
                                    includeRandom = F) # No GRF
        
        # Save updated model
        mod.list[[i]] <- Model
        
      } else { # If models are PA or Integrated, use PA intercept
        
        Model$preds.link <- predict(Model,
                                    covars = covs,
                                    S = posterior_nsamps, 
                                    intercept.terms = "PA_Intercept",
                                    type = "link",
                                    includeRandom = F) # No GRF
        
        Model$preds.intensity <- predict(Model,
                                         covars = covs,
                                         S = posterior_nsamps, 
                                         intercept.terms = "PA_Intercept",
                                         type = "intensity",
                                         includeRandom = F) # No GRF
        
        Model$preds.prob <- predict(Model,
                                    covars = covs,
                                    S = posterior_nsamps, 
                                    intercept.terms = "PA_Intercept",
                                    type = "probability",
                                    includeRandom = F) # No GRF
        
        # Save updated model
        mod.list[[i]] <- Model
        
      }
      
    }
    
    ######################
    ### SPATIAL MODEL
    ######################
    
    if(grepl("GRF", name, fixed = T)) {
      
      if(grepl("PO", name, fixed = T)) { # If models are PO, use PO intercept
        
        Model$preds.link <- predict(Model,
                                    covars = covs,
                                    S = posterior_nsamps, 
                                    intercept.terms = "PO_Intercept",
                                    type = "link",
                                    includeRandom = T) # Add GRF
        
        Model$preds.intensity <- predict(Model,
                                         covars = covs,
                                         S = posterior_nsamps, 
                                         intercept.terms = "PO_Intercept",
                                         type = "intensity",
                                         includeRandom = T) # Add GRF
        
        Model$preds.prob <- predict(Model,
                                    covars = covs,
                                    S = posterior_nsamps, 
                                    intercept.terms = "PO_Intercept",
                                    type = "probability",
                                    includeRandom = T) # Add GRF
        
        # Save updated model
        mod.list[[i]] <- Model
        
      } else{ # If models are PA or Integrated, use PA intercept
        
        Model$preds.link <- predict(Model,
                                    covars = covs,
                                    S = posterior_nsamps, 
                                    intercept.terms = "PA_Intercept",
                                    type = "link",
                                    includeRandom = T) # Add GRF
        
        Model$preds.intensity <- predict(Model,
                                         covars = covs,
                                         S = posterior_nsamps, 
                                         intercept.terms = "PA_Intercept",
                                         type = "intensity",
                                         includeRandom = T) # Add GRF
        
        Model$preds.prob <- predict(Model,
                                    covars = covs,
                                    S = posterior_nsamps, 
                                    intercept.terms = "PA_Intercept",
                                    type = "probability",
                                    includeRandom = T) # Add GRF
        
        # Save updated model
        mod.list[[i]] <- Model
        
      }
      
      ###########
      ### If also plotting random effect
      ###########
      
      if(pred.GRF == TRUE & grepl("GRF", name, fixed = T)) {
        
        # Prediction doesn't include an intercept
        Model$preds.GRF <- predict(Model,
                                   covars = covs,
                                   S = posterior_nsamps, 
                                   intercept.terms = NULL,
                                   type = "intensity",
                                   includeRandom = T,
                                   includeFixed = F) # No fixed effect
        
        # Save updated model
        mod.list[[i]] <- Model
        
      }
      
    
    }
    
  }
  
  return(mod.list)
  
}




# PLOT PREDICTIONS --------------------------------------------------------

plot_predictions_func <- function(mod.list,
                                  outpath,
                                  vestfold_boundary,
                                  bunger_boundary) {
  
  mod_names <- names(mod.list)
  
  for(i in seq_along(mod.list)) {
    
    Model <- mod.list[[i]]
    
    name <- mod_names[i] 
    
    # If the model name contains PA or Integrated, pull out Bunger & Vestfold predictions
    if(grepl("PA", name, fixed = T) | grepl("int", name, fixed = T)) {
    
    # 1. Pull out the median intensity prediction for each cell (VESTFOLD)
    
    median <- Model$preds.INT.Vestfold$field$Median %>%
      crop(ext(vestfold_boundary)) %>% 
      as.data.frame(xy = T) %>%
      ggplot() +
      geom_tile(aes(x = x, y = y, fill = Median)) +
      scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                         name = "Intensity") +
      coord_fixed() +
      labs(title = "Median") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank())
    
    lower <- Model$preds.INT.Vestfold$field$Lower %>% 
      crop(ext(vestfold_boundary)) %>% 
      as.data.frame(xy = T) %>%
      ggplot() +
      geom_tile(aes(x = x, y = y, fill = Lower)) +
      scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                         name = "Intensity") +
      coord_fixed() +
      labs(title = "Lower") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank())
    
    upper <- Model$preds.INT.Vestfold$field$Upper %>% 
      crop(ext(vestfold_boundary)) %>% 
      as.data.frame(xy = T) %>%
      ggplot() +
      geom_tile(aes(x = x, y = y, fill = Upper)) +
      scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                         name = "Intensity") +
      coord_fixed() +
      labs(title = "Upper") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank())
    
    plot <- ggarrange(median, NULL, lower, upper, 
                      ncol = 2, nrow = 2,
                      common.legend = TRUE, legend = "bottom",
                      align = "hv")
    
    ggsave(paste0(outpath, "/VESTFOLD_Intensity_prediction_plot_", name,".png"), plot,
           width = 10, height = 6, dpi = 300)
    
    
    # 2. Pull out the median probability prediction for each cell (VESTFOLD)
    
    median <- Model$preds.prob.Vestfold$field$Median %>%
      crop(ext(vestfold_boundary)) %>% 
      as.data.frame(xy = T) %>%
      ggplot() +
      geom_tile(aes(x = x, y = y, fill = Median)) +
      scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                         name = "Probability") +
      coord_fixed() +
      labs(title = "Median") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank())
    
    lower <- Model$preds.prob.Vestfold$field$Lower %>% 
      crop(ext(vestfold_boundary)) %>% 
      as.data.frame(xy = T) %>%
      ggplot() +
      geom_tile(aes(x = x, y = y, fill = Lower)) +
      scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                         name = "Probability") +
      coord_fixed() +
      labs(title = "Lower") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank())
    
    upper <- Model$preds.prob.Vestfold$field$Upper %>% 
      crop(ext(vestfold_boundary)) %>% 
      as.data.frame(xy = T) %>%
      ggplot() +
      geom_tile(aes(x = x, y = y, fill = Upper)) +
      scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                         name = "Probability") +
      coord_fixed() +
      labs(title = "Upper") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank())
    
    plot <- ggarrange(median, NULL, lower, upper, 
                      ncol = 2, nrow = 2,
                      common.legend = TRUE, legend = "bottom",
                      align = "hv")
    
    ggsave(paste0(outpath, "/VESTFOLD_Probability_prediction_plot_", name,".png"), plot,
           width = 10, height = 6, dpi = 300)
    
    
    # 3. Pull out the median intensity prediction for each cell (BUNGER)
    
    median <- Model$preds.INT.Bunger$field$Median %>%
      crop(ext(bunger_boundary)) %>% 
      as.data.frame(xy = T) %>%
      ggplot() +
      geom_tile(aes(x = x, y = y, fill = Median)) +
      scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                         name = "Intensity") +
      coord_fixed() +
      labs(title = "Median") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank())
    
    lower <- Model$preds.INT.Bunger$field$Lower %>% 
      crop(ext(bunger_boundary)) %>% 
      as.data.frame(xy = T) %>%
      ggplot() +
      geom_tile(aes(x = x, y = y, fill = Lower)) +
      scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                         name = "Intensity") +
      coord_fixed() +
      labs(title = "Lower") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank())
    
    upper <- Model$preds.INT.Bunger$field$Upper %>% 
      crop(ext(bunger_boundary)) %>% 
      as.data.frame(xy = T) %>%
      ggplot() +
      geom_tile(aes(x = x, y = y, fill = Upper)) +
      scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                         name = "Intensity") +
      coord_fixed() +
      labs(title = "Upper") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank())
    
    plot <- ggarrange(median, NULL, lower, upper, 
                      ncol = 2, nrow = 2,
                      common.legend = TRUE, legend = "bottom",
                      align = "hv")
    
    ggsave(paste0(outpath, "/BUNGER_Intensity_prediction_plot_", name,".png"), plot,
           width = 10, height = 6, dpi = 300)
    
    
    # 4. Pull out the median probability prediction for each cell (BUNGER)
    
    median <- Model$preds.prob.Bunger$field$Median %>%
      crop(ext(bunger_boundary)) %>% 
      as.data.frame(xy = T) %>%
      ggplot() +
      geom_tile(aes(x = x, y = y, fill = Median)) +
      scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                         name = "Probability") +
      coord_fixed() +
      labs(title = "Median") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank())
    
    lower <- Model$preds.prob.Bunger$field$Lower %>% 
      crop(ext(bunger_boundary)) %>% 
      as.data.frame(xy = T) %>%
      ggplot() +
      geom_tile(aes(x = x, y = y, fill = Lower)) +
      scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                         name = "Probability") +
      coord_fixed() +
      labs(title = "Lower") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank())
    
    upper <- Model$preds.prob.Bunger$field$Upper %>% 
      crop(ext(bunger_boundary)) %>% 
      as.data.frame(xy = T) %>%
      ggplot() +
      geom_tile(aes(x = x, y = y, fill = Upper)) +
      scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                         name = "Probability") +
      coord_fixed() +
      labs(title = "Upper") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank())
    
    plot <- ggarrange(median, NULL, lower, upper, 
                      ncol = 2, nrow = 2,
                      common.legend = TRUE, legend = "bottom",
                      align = "hv")
    
    ggsave(paste0(outpath, "/BUNGER_Probability_prediction_plot_", name,".png"), plot,
           width = 10, height = 6, dpi = 300)
    
    
    } 
    
    
    # If the model name contains PO, there's just one prediction (but two plots)
    if(grepl("PO", name, fixed = T)) {
      
      # 2. Pull out the median intensity prediction for each cell (VESTFOLD) ---

      median <- Model$preds.INT$field$Median %>%
        crop(ext(vestfold_boundary)) %>% 
        as.data.frame(xy = T) %>%
        ggplot() +
        geom_tile(aes(x = x, y = y, fill = Median)) +
        scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                           name = "Intensity") +
        coord_fixed() +
        labs(title = "Median") +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank())
      
      lower <- Model$preds.INT$field$Lower %>% 
        crop(ext(vestfold_boundary)) %>% 
        as.data.frame(xy = T) %>%
        ggplot() +
        geom_tile(aes(x = x, y = y, fill = Lower)) +
        scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                           name = "Intensity") +
        coord_fixed() +
        labs(title = "Lower") +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank())
      
      upper <- Model$preds.INT$field$Upper %>% 
        crop(ext(vestfold_boundary)) %>% 
        as.data.frame(xy = T) %>%
        ggplot() +
        geom_tile(aes(x = x, y = y, fill = Upper)) +
        scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                           name = "Intensity") +
        coord_fixed() +
        labs(title = "Upper") +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank())
      
      plot <- ggarrange(median, NULL, lower, upper, 
                        ncol = 2, nrow = 2,
                        common.legend = TRUE, legend = "bottom",
                        align = "hv")
      
      ggsave(paste0(outpath, "/VESTFOLD_Intensity_prediction_plot_", name,".png"), plot,
             width = 10, height = 6, dpi = 300)      
      

# Pull out the median intensity prediction for each cell (BUNGER) ---------

      median <- Model$preds.INT$field$Median %>%
        crop(ext(bunger_boundary)) %>% 
        as.data.frame(xy = T) %>%
        ggplot() +
        geom_tile(aes(x = x, y = y, fill = Median)) +
        scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                           name = "Intensity") +
        coord_fixed() +
        labs(title = "Median") +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank())
      
      lower <- Model$preds.INT$field$Lower %>% 
        crop(ext(bunger_boundary)) %>% 
        as.data.frame(xy = T) %>%
        ggplot() +
        geom_tile(aes(x = x, y = y, fill = Lower)) +
        scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                           name = "Intensity") +
        coord_fixed() +
        labs(title = "Lower") +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank())
      
      upper <- Model$preds.INT$field$Upper %>% 
        crop(ext(bunger_boundary)) %>% 
        as.data.frame(xy = T) %>%
        ggplot() +
        geom_tile(aes(x = x, y = y, fill = Upper)) +
        scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                           name = "Intensity") +
        coord_fixed() +
        labs(title = "Upper") +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank())
      
      plot <- ggarrange(median, NULL, lower, upper, 
                        ncol = 2, nrow = 2,
                        common.legend = TRUE, legend = "bottom",
                        align = "hv")
      
      ggsave(paste0(outpath, "/BUNGER_Intensity_prediction_plot_", name,".png"), plot,
             width = 10, height = 6, dpi = 300)      
      
      # 2. Pull out the median probability prediction for each cell (VESTFOLD)
      
      median <- Model$preds.prob$field$Median %>%
        crop(ext(vestfold_boundary)) %>%
        as.data.frame(xy = T) %>%
        ggplot() +
        geom_tile(aes(x = x, y = y, fill = Median)) +
        scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                           name = "Probability") +
        coord_fixed() +
        labs(title = "Median") +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank())
      
      lower <- Model$preds.prob$field$Lower %>% 
        crop(ext(vestfold_boundary)) %>%
        as.data.frame(xy = T) %>%
        ggplot() +
        geom_tile(aes(x = x, y = y, fill = Lower)) +
        scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                           name = "Probability") +
        coord_fixed() +
        labs(title = "Lower") +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank())
      
      upper <- Model$preds.prob$field$Upper %>% 
        crop(ext(vestfold_boundary)) %>%
        as.data.frame(xy = T) %>%
        ggplot() +
        geom_tile(aes(x = x, y = y, fill = Upper)) +
        scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                           name = "Probability") +
        coord_fixed() +
        labs(title = "Upper") +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank())
      
      plot <- ggarrange(median, NULL, lower, upper, 
                        ncol = 2, nrow = 2,
                        common.legend = TRUE, legend = "bottom",
                        align = "hv")
      
      ggsave(paste0(outpath, "/VESTFOLD_Probability_prediction_plot_", name,".png"), plot,
             width = 10, height = 6, dpi = 300)
      
      # 2. Pull out the median probability prediction for each cell (BUNGER)
      
      median <- Model$preds.prob$field$Median %>%
        crop(ext(bunger_boundary)) %>%
        as.data.frame(xy = T) %>%
        ggplot() +
        geom_tile(aes(x = x, y = y, fill = Median)) +
        scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                           name = "Probability") +
        coord_fixed() +
        labs(title = "Median") +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank())
      
      lower <- Model$preds.prob$field$Lower %>% 
        crop(ext(bunger_boundary)) %>%
        as.data.frame(xy = T) %>%
        ggplot() +
        geom_tile(aes(x = x, y = y, fill = Lower)) +
        scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                           name = "Probability") +
        coord_fixed() +
        labs(title = "Lower") +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank())
      
      upper <- Model$preds.prob$field$Upper %>% 
        crop(ext(bunger_boundary)) %>%
        as.data.frame(xy = T) %>%
        ggplot() +
        geom_tile(aes(x = x, y = y, fill = Upper)) +
        scale_fill_viridis(guide = guide_colorbar(barwidth = 10, barheight = 0.5),
                           name = "Probability") +
        coord_fixed() +
        labs(title = "Upper") +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank())
      
      plot <- ggarrange(median, NULL, lower, upper, 
                        ncol = 2, nrow = 2,
                        common.legend = TRUE, legend = "bottom",
                        align = "hv")
      
      ggsave(paste0(outpath, "/BUNGER_Probability_prediction_plot_", name,".png"), plot,
             width = 10, height = 6, dpi = 300)
      
      
    }
    
    
  
  }
}


# SAVE OUTPUT RASTERS --------------------------------------------------------

save_output_rasters_func <- function(mod.list,
                                     outpath) {
  
  mod_names <- names(mod.list)
  
  for(i in seq_along(mod.list)) {
    
    Model <- mod.list[[i]]
    
    name <- mod_names[i] 
    
    # If the model name contains PA or Integrated, pull out Bunger & Vestfold predictions
    if(grepl("PA", name, fixed = T) | grepl("int", name, fixed = T)) {
      
      # 1. Pull out the median intensity prediction for each cell (VESTFOLD)
      
      Model$preds.INT.Vestfold$field$Median %>% 
        writeRaster(paste0(outpath, "/VESTFOLD_Intensity_prediction_", name, "_median.tif"),
                    overwrite = T)
      
      Model$preds.INT.Vestfold$field$Lower %>% 
        writeRaster(paste0(outpath, "/VESTFOLD_Intensity_prediction_", name, "_lower.tif"),
                    overwrite = T)
      
      Model$preds.INT.Vestfold$field$Upper %>% 
        writeRaster(paste0(outpath, "/VESTFOLD_Intensity_prediction_", name, "_upper.tif"),
                    overwrite = T)
      
      
      # 2. Pull out the median probability prediction for each cell (VESTFOLD)
      
      Model$preds.prob.Vestfold$field$Median %>%
        writeRaster(paste0(outpath, "/VESTFOLD_Probability_prediction_", name, "_median.tif"),
                    overwrite = T)
      
      Model$preds.prob.Vestfold$field$Lower %>% 
        writeRaster(paste0(outpath, "/VESTFOLD_Probability_prediction_", name, "_lower.tif"),
                    overwrite = T)
      
      Model$preds.prob.Vestfold$field$Upper %>% 
        writeRaster(paste0(outpath, "/VESTFOLD_Probability_prediction_", name, "_upper.tif"),
                    overwrite = T)
      
      # 3. Pull out the median intensity prediction for each cell (BUNGER)
      
      Model$preds.INT.Bunger$field$Median %>%
        writeRaster(paste0(outpath, "/BUNGER_Intensity_prediction_", name, "_median.tif"),
                    overwrite = T)
        
      
      Model$preds.INT.Bunger$field$Lower %>% 
        writeRaster(paste0(outpath, "/BUNGER_Intensity_prediction_", name, "_lower.tif"),
                    overwrite = T)
        
      
      Model$preds.INT.Bunger$field$Upper %>% 
        writeRaster(paste0(outpath, "/BUNGER_Intensity_prediction_", name, "_upper.tif"),
                    overwrite = T)
        
      
      # 4. Pull out the median probability prediction for each cell (BUNGER)
      
      Model$preds.prob.Bunger$field$Median %>%
        writeRaster(paste0(outpath, "/BUNGER_Probability_prediction_", name, "_median.tif"),
                    overwrite = T)
        
      
      Model$preds.prob.Bunger$field$Lower %>%
        writeRaster(paste0(outpath, "/BUNGER_Probability_prediction_", name, "_lower.tif"),
                    overwrite = T)
        
      
      Model$preds.prob.Bunger$field$Upper %>%
        writeRaster(paste0(outpath, "/BUNGER_Probability_prediction_", name, "_upper.tif"),
                    overwrite = T)
      
      
    } 
    
    
    # If the model name contains PO, there's just one prediction
    if(grepl("PO", name, fixed = T)) {
      
      # 2. Pull out the median intensity prediction for each cell --------
      
      Model$preds.INT$field$Median %>%
        writeRaster(paste0(outpath, "/Intensity_prediction_", name, "_median.tif"),
                    overwrite = T)
        
      
      Model$preds.INT$field$Lower %>% 
        writeRaster(paste0(outpath, "/Intensity_prediction_", name, "_lower.tif"),
                    overwrite = T)
        
      
     Model$preds.INT$field$Upper %>% 
        writeRaster(paste0(outpath, "/Intensity_prediction_", name, "_upper.tif"),
                    overwrite = T)
        
      
      # 2. Pull out the median probability prediction for each cell (BUNGER)
      
      Model$preds.prob$field$Median %>%
        writeRaster(paste0(outpath, "/Probability_prediction_", name, "_median.tif"),
                    overwrite = T)
        
      
      Model$preds.prob$field$Lower %>% 
        writeRaster(paste0(outpath, "/Probability_prediction_", name, "_lower.tif"),
                    overwrite = T)
        
      
      Model$preds.prob$field$Upper %>% 
        writeRaster(paste0(outpath, "/Probability_prediction_", name, "_upper.tif"),
                    overwrite = T)
        
      
    }
    
    
  }
}

