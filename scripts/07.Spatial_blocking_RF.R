############################################
# Spatial autocorrelation estimation
# + Random Forest spatial block validation
# + Partial Dependence Plots by fold
############################################

# --- Load libraries ---
library(randomForest)
library(dplyr)
library(pROC)
library(tictoc)
library(doParallel)
library(foreach)
library(openxlsx)
library(ggplot2)
library(sf)
library(blockCV)
library(pdp)
library(ecospat)

set.seed(723)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
base_path <- "E:/TFM_gangas/GPS/ExtractedV.4"
model_dir <- file.path(base_path, "CV_models")
dir.create(model_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# Species and pseudoabsence methods
# ------------------------------------------------------------
species_list <- c("PTS", "BBS")

methods <- data.frame(
  method = c("Random", "P95", "MCP40"),
  file   = c(
    "pseudoabsences_Random_env.csv",
    "pseudoabsences_P95_decay_env.csv",
    "pseudoabsences_MCP40_decay_env.csv"
  ),
  stringsAsFactors = FALSE
)

############################################
# PART 1 — SPATIAL AUTOCORRELATION RANGE
############################################

cat("\n====================================\n")
cat("Estimating spatial autocorrelation range\n")
cat("====================================\n")

auto_data <- read.csv(
  file.path(base_path, paste0("BBS_", methods$file[1]))
)

auto_data <- auto_data %>% na.omit()

set.seed(111)

auto_sample <- auto_data %>%
  slice_sample(n = min(5000, nrow(auto_data)))

auto_sf <- st_as_sf(
  auto_sample,
  coords = c("X_25830", "Y_25830"),
  crs = 25830
)

env_vars <- auto_sample %>%
  dplyr::select(
    -birdID,
    -date,
    -species,
    -presence,
    -X_25830,
    -Y_25830,
    -Tmin,
    -Tmax,
    -TminSD100,
    -TmaxSD100,
    -AltRange,
    -Aspect,
    -LC_RiceFields,
    -LC_Greenhouses
  )

auto_range <- cv_spatial_autocor(
  x = auto_sf,
  column = names(env_vars),
  num_sample = 500
)

print(auto_range$range)

block_size <- round(max(auto_range$range, na.rm = TRUE))

cat("\nEstimated block size:", block_size, "meters\n")

############################################
# PARALLEL SETUP
############################################

n_cores <- 3
cl <- makeCluster(n_cores)
registerDoParallel(cl)

############################################
# STORAGE OBJECTS
############################################

all_results <- data.frame()
all_pdp <- data.frame()
all_importance <- data.frame()
all_calibration <- data.frame()

############################################
# MAIN LOOP
############################################

for (sp in species_list) {
  for (m in 1:nrow(methods)) {
    
    cat("\n====================================\n")
    cat("Species:", sp, "\n")
    cat("Pseudoabsence method:", methods$method[m], "\n")
    cat("====================================\n")
    
    ############################################
    # LOAD DATA
    ############################################
    
    data_pts <- read.csv(
      file.path(base_path, paste0(sp, "_", methods$file[m]))
    )
    
    data_model <- data_pts %>%
      mutate(presence = factor(presence, levels = c(0,1))) %>%
      dplyr::select(
        -birdID,
        -date,
        -species,
        -Tmin,
        -Tmax,
        -TminSD100,
        -TmaxSD100,
        -AltRange,
        -Aspect,
        -LC_RiceFields,
        -LC_Greenhouses
      ) %>%
      na.omit()
    
    ############################################
    # SPATIAL BLOCK
    ############################################
    
    set.seed(723)
    
    data_sf <- st_as_sf(
      data_model,
      coords = c("X_25830","Y_25830"),
      crs = 25830
    )
    
    sb <- spatialBlock(
      speciesData = data_sf,
      species = "presence",
      k = 5,
      theRange = 42000,
      selection = "random",
      iteration = 100
    )
    
    data_model$fold <- sb$foldID
    
    data_model <- data_model %>%
      dplyr::select(-X_25830,-Y_25830)
    
    ############################################
    # GLOBAL PDP GRID
    ############################################
    
    predictors <- setdiff(names(data_model), c("presence","fold"))
    
    grid_list <- lapply(predictors, function(var){
      rng <- range(data_model[[var]], na.rm = TRUE)
      seq(rng[1], rng[2], length.out = 50)
    })
    
    names(grid_list) <- predictors
    
    ############################################
    # MODEL
    ############################################
    
    tic(paste("RF spatial CV —", sp, methods$method[m]))
    
    results <- foreach(
      k = 1:5,
      .packages = c("randomForest","pROC","dplyr","pdp","ecospat")
    ) %dopar% {
      
      set.seed(1000 + k)
      
      train <- data_model[data_model$fold != k, ]
      test  <- data_model[data_model$fold == k, ]
      
      train <- train %>% select(-fold)
      test  <- test  %>% select(-fold)
      
      n_pres <- sum(train$presence==1)
      sampsize <- c("0"=n_pres,"1"=n_pres)
      
      predictors <- setdiff(names(train), "presence")
      n_vars <- length(predictors)
      
      rf_model <- randomForest(
        presence ~ .,
        data=train,
        ntree=500,
        mtry = floor(n_vars / 2),
        nodesize = 1,
        sampsize=sampsize,
        importance=TRUE
      )
      
      ##########################################
      # SAVE FOLDS
      ##########################################
      
      saveRDS(
        rf_model,
        file = file.path(
          model_dir,
          paste0("RF_", sp, "_", methods$method[m], "_fold", k, ".rds")
        )
      )
      
      ##########################################
      # METRICS
      ##########################################
      
      preds_prob <- predict(rf_model,test,type="prob")[,"1"]
      
      # Save calibration predictions
      calibration_df <- data.frame(
        observed = as.numeric(as.character(test$presence)),
        predicted = preds_prob,
        fold = k,
        species = sp,
        method = methods$method[m]
      )
      
      auc_val <- as.numeric(auc(test$presence,preds_prob))
      
      # ROC threshold
      roc_curve <- roc(test$presence, preds_prob)
      thr <- coords(roc_curve, "best")$threshold
      
      # binarize
      preds_bin <- ifelse(preds_prob >= thr, 1, 0)
      
      # confusion
      TP <- sum(preds_bin==1 & test$presence==1)
      FN <- sum(preds_bin==0 & test$presence==1)
      TN <- sum(preds_bin==0 & test$presence==0)
      FP <- sum(preds_bin==1 & test$presence==0)
      
      sens <- ifelse((TP+FN)==0, NA, TP/(TP+FN))
      spec <- ifelse((TN+FP)==0, NA, TN/(TN+FP))
      
      tss <- sens + spec - 1
      
      # Boyce
      boyce <- tryCatch({
        res <- ecospat.boyce(
          fit = preds_prob,
          obs = preds_prob[test$presence == 1],
          PEplot = FALSE
        )$cor
        
        if(length(res) == 0 || is.nan(res)) NA else res
        
      }, error = function(e) NA)
      
      ##########################################
      # METRICS TABLE
      ##########################################
      
      metrics <- data.frame(
        fold=k,
        AUC=auc_val,
        Sensitivity=sens,
        Specificity=spec,
        TSS=tss,
        Boyce=boyce,
        Threshold=thr,
        species=sp,
        method=methods$method[m]
      )
      
      ##########################################
      # IMPORTANCE
      ##########################################
      
      imp <- importance(rf_model)
      
      imp_df <- data.frame(
        variable=rownames(imp),
        MeanDecreaseAccuracy = imp[,1],
        fold=k,
        species=sp,
        method=methods$method[m]
      )
      
      ##########################################
      # PDP
      ##########################################
      
      train_sample <- train %>% slice_sample(n=min(5000,nrow(train)))
      
      pdp_list <- lapply(predictors,function(var){
        
        grid_df <- data.frame(value = grid_list[[var]])
        colnames(grid_df) <- var
        
        pd <- partial(
          rf_model,
          pred.var = var,
          pred.grid = grid_df,
          train = train_sample,
          prob = TRUE,
          which.class = '1'
        )
        
        colnames(pd)[1] <- "value"
        
        pd$variable <- var
        pd$fold <- k
        pd$species <- sp
        pd$method <- methods$method[m]
        
        pd
      })
      
      pdp_df <- bind_rows(pdp_list)
      
      list(
        metrics = metrics,
        pdp = pdp_df,
        importance = imp_df,
        calibration = calibration_df
      )
    }
    
    toc()
    
    ############################################
    # COMBINE
    ############################################
    
    all_results <- rbind(all_results,
                         bind_rows(lapply(results, `[[`, "metrics")))
    
    all_pdp <- rbind(all_pdp,
                     bind_rows(lapply(results, `[[`, "pdp")))
    
    all_importance <- rbind(all_importance,
                            bind_rows(lapply(results, `[[`, "importance")))
   
     all_calibration <- rbind(
      all_calibration,
      bind_rows(lapply(results, `[[`, "calibration"))
    )
    
  }
}

stopCluster(cl)
registerDoSEQ()

############################################
# SAVE RESULTS TO EXCEL
############################################

wb <- createWorkbook()

############################################
# SUMMARY
############################################

summary_table <- all_results %>%
  group_by(species, method) %>%
  summarise(
    mean_AUC = mean(AUC),
    sd_AUC   = sd(AUC),
    mean_Sens = mean(Sensitivity),
    sd_Sens   = sd(Sensitivity),
    mean_Specificity = mean(Specificity, na.rm=TRUE),
    sd_Specificity   = sd(Specificity, na.rm=TRUE),
    mean_TSS = mean(TSS, na.rm=TRUE),
    sd_TSS   = sd(TSS, na.rm=TRUE),
    mean_Threshold = mean(Threshold),
    sd_Threshold   = sd(Threshold),
    mean_Boyce = mean(Boyce, na.rm=TRUE),
    sd_Boyce   = sd(Boyce, na.rm=TRUE),
    n_folds_Boyce = sum(!is.na(Boyce)),
    .groups = "drop"
  )

addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_table)

############################################
# METRICS PER MODEL
############################################

for (sp in unique(all_results$species)) {
  for (met in unique(all_results$method)) {
    
    df <- all_results %>%
      filter(species == sp, method == met)
    
    sheet_name <- paste(sp, met, sep = "_")
    
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, df)
  }
}

############################################
# IMPORTANCE
############################################

addWorksheet(wb, "Importance_all")
writeData(wb, "Importance_all", all_importance)

importance_summary <- all_importance %>%
  group_by(species, method, variable) %>%
  summarise(
    mean_MDA = mean(MeanDecreaseAccuracy),
    sd_MDA   = sd(MeanDecreaseAccuracy),
    .groups = "drop"
  ) %>%
  arrange(species, method, desc(mean_MDA))

addWorksheet(wb, "Importance_summary")
writeData(wb, "Importance_summary", importance_summary)

############################################
# SAVE FILE
############################################

saveWorkbook(
  wb,
  file.path(base_path, "FINAL_RF_RESULTS.xlsx"),
  overwrite = TRUE
)

write.csv(
  all_calibration,
  file.path(base_path, "Calibration_data.csv"),
  row.names = FALSE
)

cat("\nSAVED EXCELL\n")

############################################
# PDP MEAN + SD
############################################

pdp_summary <- all_pdp %>%
  group_by(species, method, variable, value) %>%
  summarise(
    mean_yhat = mean(yhat),
    sd_yhat   = sd(yhat),
    .groups = "drop"
  )
############################################
# PDP PLOTS (MEAN + SD)
############################################

plot_dir <- file.path(base_path,"PDP_plots_sd")
dir.create(plot_dir,showWarnings=FALSE)

for (sp in unique(all_pdp$species)) {
  for (met in unique(all_pdp$method)) {
    
    pdp_subset <- pdp_summary %>%
      filter(species == sp, method == met)
    
    p <- ggplot(pdp_subset, aes(x = value)) +
      
      # SD
      geom_ribbon(
        aes(
          ymin = mean_yhat - sd_yhat,
          ymax = mean_yhat + sd_yhat,
          fill = "Variability (± SD)"
        ),
        alpha = 0.4
      ) +
      
      # Mean line
      geom_line(
        aes(
          y = mean_yhat,
          color = "Mean response"
        ),
        linewidth = 1
      ) +
      
      facet_wrap(~variable, scales = "free_x") +
      
      scale_color_manual(
        name = "",
        values = c("Mean response" = "#2C2C2C")
      ) +
      
      scale_fill_manual(
        name = "",
        values = c("Variability (± SD)" = "#8FB9A8")
      ) +
      
      theme_classic(base_size = 14) +
      
      theme(
        legend.position = "top",
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold")
      ) +
      
      labs(
        title = paste("Partial dependence plots -", sp, "-", met),
        x = "Environmental gradient",
        y = "Predicted probability"
      ) +
      
      coord_cartesian(ylim = c(0, 0.35))
    
    ggsave(
      filename = paste0("PDP_SD_", sp, "_", met, ".png"),
      plot = p,
      path = plot_dir,
      width = 14,
      height = 10,
      dpi = 300
    )
  }
}

############################################
# IMPORTANCE FINAL FIGURE
############################################

plot_dir_imp <- file.path(base_path, "Importance_plots")
dir.create(plot_dir_imp, showWarnings = FALSE)

p_imp <- ggplot(
  importance_summary,
  aes(
    x = reorder(variable, mean_MDA),
    y = mean_MDA
  )
) +
  
  geom_col(fill = "#5A7D9A") +
  
  geom_errorbar(
    aes(
      ymin = mean_MDA - sd_MDA,
      ymax = mean_MDA + sd_MDA
    ),
    width = 0.2
  ) +
  
  coord_flip() +
  
  facet_grid(
    species ~ method,
    scales = "free_y"
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  ) +
  
  labs(
    title = "Variable importance (Random Forest models)",
    x = "Variable",
    y = "Mean Decrease Accuracy"
  )

ggsave(
  filename = "Importance_ALL.png",
  plot = p_imp,
  path = plot_dir_imp,
  width = 11,
  height = 7,
  dpi = 300
)



############################################
# FINAL FIGURES — RANDOM RF MODELS
# IMPORTANCE + PDPs
############################################

library(randomForest)
library(dplyr)
library(ggplot2)
library(pdp)
library(openxlsx)
library(patchwork)

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

base_path <- "E:/TFM_gangas/GPS/ExtractedV.4"

model_dir <- file.path(base_path,"CV_models")

plot_dir_imp <- file.path(base_path,"Importance_plots")

plot_dir_pdp <- file.path(base_path,"PDP_plots_sd")

# ------------------------------------------------------------
# LOAD IMPORTANCE
# ------------------------------------------------------------

importance_all <- read.xlsx(
  file.path(base_path,"FINAL_RF_RESULTS.xlsx"),
  sheet = "Importance_all"
) %>%
  
  filter(method == "Random")

importance_all$MDA <- (
  importance_all$MeanDecreaseAccuracy
)

# ------------------------------------------------------------
# CATEGORIES
# ------------------------------------------------------------

importance_all <- importance_all %>%
  
  mutate(
    
    category = case_when(
      
      variable %in% c(
        "Prcp","Tmean","TmeanSD100"
      ) ~ "Climate",
      
      variable == "NDVI" ~ "Productivity",
      
      variable %in% c(
        "Altitude","Slope","Heterogeneity"
      ) ~ "Topography",
      
      variable %in% c(
        "Population","HFP","DistRoad"
      ) ~ "Human pressure",
      
      TRUE ~ "Land cover"
    )
  )

# ------------------------------------------------------------
# LABELS
# ------------------------------------------------------------

var_labels <- c(
  
  Altitude = "Altitude",
  Slope = "Slope",
  Heterogeneity = "Habitat heterogeneity",
  NDVI = "NDVI",
  Population = "Population density",
  HFP = "Human Footprint",
  DistRoad = "Distance to roads",
  
  LC_Forest = "Forest",
  LC_Vineyards = "Vineyards",
  LC_TreeCrops = "Tree crops",
  LC_AnnualCrops = "Annual crops",
  LC_TreePasture = "Tree pasture",
  LC_ShrubPasture = "Shrub pasture",
  LC_HerbPasture = "Herbaceous pasture",
  LC_WaterBodies = "Water bodies",
  LC_Marshes = "Marshes",
  LC_Artificial = "Artificial areas",
  LC_OtherLand = "Other land",
  LC_AgriMosaic = "Agricultural mosaic",
  
  Prcp = "Precipitation",
  Tmean = "Mean temperature",
  TmeanSD100 = "Temperature seasonality"
)

importance_all$label <- var_labels[
  importance_all$variable
]

# ------------------------------------------------------------
# COLORS
# ------------------------------------------------------------

cols <- c(
  "Climate" = "#95b8f6",
  "Productivity" = "#7FBF7B",
  "Topography" = "#f9d99a",
  "Human pressure" = "#dcd9f8",
  "Land cover" = "#fa5f49"
)

# ------------------------------------------------------------
# IMPORTANCE FUNCTION
# ------------------------------------------------------------

make_imp_plot <- function(sp_name, latin){
  
  df <- importance_all %>%
    
    filter(species == sp_name)
  
  order_df <- df %>%
    
    group_by(label) %>%
    
    summarise(mean_imp = mean(MDA), .groups="drop") %>%
    
    arrange(mean_imp)
  
  df$label <- factor(
    df$label,
    levels = order_df$label
  )
  
  ggplot(
    df,
    aes(x = label, y = MDA, fill = category)
  ) +
    
    geom_boxplot(
      alpha = 0.85,
      outlier.size = 1.5
    ) +
    
    coord_flip() +
    
    scale_fill_manual(values = cols) +
    
    theme_bw(base_size = 14) +
    
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      
      axis.title.y = element_blank(),
      
      axis.text.y = element_text(size = 11),
      axis.text.x = element_text(size = 11),
      
      plot.title = element_text(
        face = "italic",
        hjust = 0.5,
        size = 16
      )
    ) +
    
    labs(
      title = latin,
      y = "Mean Decrease Accuracy"
    )
}

# ------------------------------------------------------------
# IMPORTANCE PLOTS
# ------------------------------------------------------------

p_pts_imp <- make_imp_plot(
  "PTS",
  "Pterocles alchata"
)

p_bbs_imp <- make_imp_plot(
  "BBS",
  "Pterocles orientalis"
)

p_imp_final <- p_pts_imp / p_bbs_imp

ggsave(
  file.path(plot_dir_imp,"Importance_FINAL.png"),
  p_imp_final,
  width = 10,
  height = 14,
  dpi = 600,
  bg = "white"
)

cat("\nIMPORTANCE FIGURE SAVED\n")

############################################
# PDPs
############################################

species_list <- c("PTS","BBS")

methods <- data.frame(
  method = "Random",
  file = "pseudoabsences_Random_env.csv"
)

# ------------------------------------------------------------
# TOP VARIABLES
# ------------------------------------------------------------

top_vars <- importance_all %>%
  
  group_by(species, variable) %>%
  
  summarise(
    mean_imp = mean(MDA),
    .groups = "drop"
  ) %>%
  
  filter(mean_imp > 0) %>%
  
  group_by(species) %>%
  
  arrange(desc(mean_imp)) %>%
  
  slice_head(n = 8)

# ------------------------------------------------------------
# PDP STORAGE
# ------------------------------------------------------------

all_pdp <- data.frame()

# ------------------------------------------------------------
# LOOP
# ------------------------------------------------------------

for(sp in species_list){
  
  cat("\n====================================\n")
  cat("Species:", sp, "\n")
  cat("====================================\n")
  
  # ------------------------------------------------------------
  # LOAD DATA
  # ------------------------------------------------------------
  
  data_pts <- read.csv(
    file.path(
      base_path,
      paste0(sp,"_",methods$file)
    )
  )
  
  data_model <- data_pts %>%
    
    mutate(
      presence = factor(
        presence,
        levels = c(0,1)
      )
    ) %>%
    
    dplyr::select(
      -birdID,
      -date,
      -species,
      -Tmin,
      -Tmax,
      -TminSD100,
      -TmaxSD100,
      -AltRange,
      -Aspect,
      -LC_RiceFields,
      -LC_Greenhouses
    ) %>%
    
    na.omit()
  
  vars_sp <- top_vars %>%
    
    filter(species == sp) %>%
    
    pull(variable)
  
  # ------------------------------------------------------------
  # GRID
  # ------------------------------------------------------------
  
  grid_list <- lapply(
    vars_sp,
    
    function(var){
      
      rng <- range(
        data_model[[var]],
        na.rm = TRUE
      )
      
      seq(
        rng[1],
        rng[2],
        length.out = 50
      )
    }
  )
  
  names(grid_list) <- vars_sp
  
  # ------------------------------------------------------------
  # LOAD MODELS
  # ------------------------------------------------------------
  
  fold_models <- lapply(
    1:5,
    
    function(k){
      
      readRDS(
        file.path(
          model_dir,
          
          paste0(
            "RF_",
            sp,
            "_Random_fold",
            k,
            ".rds"
          )
        )
      )
    }
  )
  
  # ------------------------------------------------------------
  # PDPs
  # ------------------------------------------------------------
  
  pdp_list <- list()
  
  for(k in 1:5){
    
    cat("Fold:", k, "\n")
    
    rf_model <- fold_models[[k]]
    
    set.seed(1000 + k)
    
    train_sample <- data_model %>%
      
      slice_sample(
        n = min(5000,nrow(data_model))
      )
    
    pd_fold <- lapply(
      vars_sp,
      
      function(var){
        
        grid_df <- data.frame(
          value = grid_list[[var]]
        )
        
        colnames(grid_df) <- var
        
        pd <- pdp::partial(
          object = rf_model,
          pred.var = var,
          pred.grid = grid_df,
          train = train_sample,
          prob = TRUE,
          which.class = "1"
        )
        
        colnames(pd)[1] <- "value"
        
        pd$variable <- var
        pd$fold <- k
        pd$species <- sp
        
        pd
      }
    )
    
    pdp_list[[k]] <- bind_rows(pd_fold)
  }
  
  pdp_df <- bind_rows(pdp_list)
  
  all_pdp <- rbind(all_pdp,pdp_df)
}

# ------------------------------------------------------------
# PDP SUMMARY
# ------------------------------------------------------------

pdp_summary <- all_pdp %>%
  
  group_by(species, variable, value) %>%
  
  summarise(
    mean_yhat = mean(yhat),
    sd_yhat = sd(yhat),
    .groups = "drop"
  )

pdp_summary$label <- var_labels[
  pdp_summary$variable
]

# ------------------------------------------------------------
# PDP FUNCTION
# ------------------------------------------------------------

make_pdp_plot <- function(
    df,
    latin,
    line_col,
    fill_col
){
  
  ggplot(df, aes(x = value)) +
    
    geom_ribbon(
      aes(
        ymin = mean_yhat - sd_yhat,
        ymax = mean_yhat + sd_yhat
      ),
      fill = fill_col,
      alpha = 0.3
    ) +
    
    geom_line(
      aes(y = mean_yhat),
      color = line_col,
      linewidth = 1
    ) +
    
    facet_wrap(
      ~label,
      scales = "free_x",
      ncol = 4
    ) +
    
    theme_bw(base_size = 14) +
    
    theme(
      strip.text = element_text(
        face = "bold",
        size = 11
      ),
      
      axis.text = element_text(
        color = "black"
      ),
      
      plot.title = element_text(
        face = "italic",
        hjust = 0.5,
        size = 20
      )
    ) +
    
    labs(
      title = latin,
      x = "Environmental gradient",
      y = "Predicted suitability"
    )
}

# ------------------------------------------------------------
# PTS
# ------------------------------------------------------------

pts_df <- pdp_summary %>%
  filter(species == "PTS")

pts_order <- top_vars %>%
  filter(species == "PTS") %>%
  arrange(desc(mean_imp)) %>%
  pull(variable)

pts_df$label <- factor(
  pts_df$label,
  levels = var_labels[pts_order]
)

p_pts <- make_pdp_plot(
  
  pts_df,
  
  expression(
    "Partial dependence plots — " *
      italic("Pterocles alchata")
  ),
  
  "#3B6FB6",
  
  "#95b8f6"
)

ggsave(
  file.path(plot_dir_pdp,"PDP_FINAL_PTS.png"),
  p_pts,
  width = 15,
  height = 7,
  dpi = 600,
  bg = "white"
)

# ------------------------------------------------------------
# BBS
# ------------------------------------------------------------

bbs_df <- pdp_summary %>%
  filter(species == "BBS")

bbs_order <- top_vars %>%
  filter(species == "BBS") %>%
  arrange(desc(mean_imp)) %>%
  pull(variable)

bbs_df$label <- factor(
  bbs_df$label,
  levels = var_labels[bbs_order]
)

p_bbs <- make_pdp_plot(
  
  bbs_df,
  
  expression(
    "Partial dependence plots — " *
      italic("Pterocles orientalis")
  ),
  
  "#D95F02",
  
  "#FDB863"
)
ggsave(
  file.path(plot_dir_pdp,"PDP_FINAL_BBS.png"),
  p_bbs,
  width = 15,
  height = 7,
  dpi = 600,
  bg = "white"
)

cat("\nFINAL PDP FIGURES SAVED\n")
