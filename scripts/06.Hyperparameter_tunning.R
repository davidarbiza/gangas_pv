############################################
# Random Forest ntree tuning
# Species: PTS
# Pseudoabsences: Random / MCP40 / P95
# Repetitions: 5
############################################

# --- Load libraries ---
library(randomForest)
library(dplyr)
library(tictoc)
library(doParallel)
library(foreach)
library(ggplot2)

set.seed(453)

# ------------------------------------------------------------
# Methods
# ------------------------------------------------------------
methods <- c("Random", "MCP40", "P95")

# ------------------------------------------------------------
# File names
# ------------------------------------------------------------
file_names <- c(
  Random = "BBS_pseudoabsences_Random_env.csv",
  MCP40  = "BBS_pseudoabsences_MCP40_decay_env.csv",
  P95    = "BBS_pseudoabsences_P95_decay_env.csv"
)
# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
base_path <- "E:/TFM_gangas/GPS/ExtractedV.4/"
out_dir   <- paste0(base_path, "Hyperparameter")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# Parallel setup
# ------------------------------------------------------------
n_cores <- 2
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------
data_list <- list()

for (method in methods) {
  
  cat("Loading:", method, "\n")
  
  file <- paste0(base_path, file_names[method])
  
  if (!file.exists(file)) {
    stop(paste("File not found:", file))
  }
  
  data_pts <- read.csv(file)
  
  data_list[[method]] <- data_pts %>%
    mutate(presence = factor(presence, levels = c(0, 1))) %>%
    dplyr::select(
      -birdID, -date, -species,
      -X_25830, -Y_25830,
      -LC_RiceFields, -LC_Greenhouses,
      -AltRange, -Aspect,
      -Tmin, -Tmax,
      -TminSD100, -TmaxSD100
    ) %>%
    na.omit()
}

gc()

# ------------------------------------------------------------
# ntree values to test
# ------------------------------------------------------------
ntree_vals <- c(50, 100, 200, 300, 500, 700)

# ------------------------------------------------------------
# Storage
# ------------------------------------------------------------
results <- data.frame(
  method = character(),
  ntree = integer(),
  repetition = integer(),
  OOB_error = numeric()
)

# ------------------------------------------------------------
# Run tuning
# ------------------------------------------------------------
tic("RF ntree tuning (OOB error)")

results <- foreach(
  method = methods,
  .combine = rbind,
  .packages = c("randomForest", "dplyr")
) %:%
  foreach(
    i = 1:5,
    .combine = rbind
  ) %dopar% {
    
    data_model <- data_list[[method]]
    
    set.seed(100 + i)
    
    n_pres <- sum(data_model$presence == 1)
    sampsize <- c("0" = n_pres, "1" = n_pres)
    
    out_rep <- data.frame()
    
    for (nt in ntree_vals) {
      
      rf_model <- randomForest(
        presence ~ .,
        data = data_model,
        ntree = nt,
        sampsize = sampsize,
        importance = FALSE
      )
      
      oob_err <- rf_model$err.rate[nt, "OOB"]
      
      out_rep <- rbind(
        out_rep,
        data.frame(
          method = method,
          ntree = nt,
          repetition = i,
          OOB_error = oob_err
        )
      )
      
      rm(rf_model)
    }
    
    gc()
    
    out_rep
  }

toc()

# ------------------------------------------------------------
# Stop parallel
# ------------------------------------------------------------
stopCluster(cl)
registerDoSEQ()

# ------------------------------------------------------------
# Save raw results
# ------------------------------------------------------------
write.csv(
  results,
  file.path(out_dir, "RF_ntree_tuning_BBSS_ALL_raw.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# Summary statistics
# ------------------------------------------------------------
summary_ntree <- results %>%
  group_by(method, ntree) %>%
  summarise(
    mean_OOB = mean(OOB_error),
    sd_OOB   = sd(OOB_error),
    .groups = "drop"
  )

write.csv(
  summary_ntree,
  file.path(out_dir, "RF_ntree_tuning_BBS_ALL_summary.csv"),
  row.names = FALSE
)

print(summary_ntree)

# ------------------------------------------------------------
# Plot OOB error vs ntree
# ------------------------------------------------------------
ggplot(summary_ntree, aes(x = ntree, y = mean_OOB)) +
  
  geom_ribbon(aes(ymin = mean_OOB - sd_OOB,
                  ymax = mean_OOB + sd_OOB),
              fill = "#3B5B92", alpha = 0.15) +
  
  geom_line(color = "#1B3A6F", linewidth = 1.2) +
  
  geom_point(size = 3, color = "#1B3A6F") +
  
  facet_wrap(~method, scales = "free_y") +
  
  scale_x_continuous(breaks = ntree_vals) +
  
  labs(
    x = "Number of trees",
    y = "Out-of-bag error",
    title = bquote("Stabilization of OOB error with increasing number of trees for" ~ italic("P. orientalis"))
  ) +
  
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )



############################################
# Random Forest hyperparameter tuning
# (mtry + nodesize) using OOB error
# Species: PTS
# Pseudoabsences: Random / MCP40 / P95
############################################

# --- Load libraries ---
library(randomForest)
library(dplyr)
library(tictoc)
library(doParallel)
library(foreach)
library(openxlsx)
library(ggplot2)

set.seed(453)

# ------------------------------------------------------------
# Methods
# ------------------------------------------------------------
methods <- c("Random", "MCP40", "P95")

# ------------------------------------------------------------
# File names
# ------------------------------------------------------------
file_names <- c(
  Random = "BBS_pseudoabsences_Random_env.csv",
  MCP40  = "BBS_pseudoabsences_MCP40_decay_env.csv",
  P95    = "BBS_pseudoabsences_P95_decay_env.csv"
)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
base_path <- "E:/TFM_gangas/GPS/ExtractedV.4/"
out_dir   <- paste0(base_path, "Hyperparameter")

# ------------------------------------------------------------
# Parallel setup
# ------------------------------------------------------------
n_cores <- 2
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------
data_list <- list()

for (method in methods) {
  
  file <- paste0(base_path, file_names[method])
  
  if (!file.exists(file)) {
    stop(paste("File not found:", file))
  }
  
  data_pts <- read.csv(file)
  
  data_list[[method]] <- data_pts %>%
    mutate(presence = factor(presence, levels = c(0, 1))) %>%
    dplyr::select(
      -birdID, -date, -species,
      -X_25830, -Y_25830,
      -LC_RiceFields, -LC_Greenhouses,
      -AltRange, -Aspect,
      -Tmin, -Tmax,
      -TminSD100, -TmaxSD100
    ) %>%
    na.omit()
}

gc()

# ------------------------------------------------------------
# Number of predictors
# ------------------------------------------------------------
predictor_names <- setdiff(names(data_list[[1]]), "presence")
n_vars <- length(predictor_names)

# ------------------------------------------------------------
# Hyperparameter grid
# ------------------------------------------------------------
mtry_vals <- unique(round(c(sqrt(n_vars), n_vars/3, n_vars/2)))
nodesize_vals <- c(1, 5, 10)

ntree_val <- 500

# ------------------------------------------------------------
# Storage
# ------------------------------------------------------------
results <- data.frame(
  method = character(),
  mtry = integer(),
  nodesize = integer(),
  repetition = integer(),
  OOB_error = numeric()
)

# ------------------------------------------------------------
# Run tuning
# ------------------------------------------------------------
tic("RF hyperparameter tuning")

results <- foreach(
  method = methods,
  .combine = rbind,
  .packages = c("randomForest", "dplyr")
) %:%
  foreach(
    i = 1:5,
    .combine = rbind
  ) %dopar% {
    
    data_model <- data_list[[method]]
    
    set.seed(100 + i)
    
    n_pres <- sum(data_model$presence == 1)
    sampsize <- c("0" = n_pres, "1" = n_pres)
    
    out_rep <- data.frame()
    
    for (mtry_val in mtry_vals) {
      for (node_val in nodesize_vals) {
        
        rf_model <- randomForest(
          presence ~ .,
          data = data_model,
          ntree = ntree_val,
          mtry = mtry_val,
          nodesize = node_val,
          sampsize = sampsize,
          importance = FALSE
        )
        
        oob_err <- rf_model$err.rate[ntree_val, "OOB"]
        
        out_rep <- rbind(
          out_rep,
          data.frame(
            method = method,
            mtry = mtry_val,
            nodesize = node_val,
            repetition = i,
            OOB_error = oob_err
          )
        )
        
        rm(rf_model)
      }
    }
    
    gc()
    
    out_rep
  }

toc()

# ------------------------------------------------------------
# Stop parallel
# ------------------------------------------------------------
stopCluster(cl)
registerDoSEQ()

# ------------------------------------------------------------
# Summary
# ------------------------------------------------------------
summary_results <- results %>%
  group_by(method, mtry, nodesize) %>%
  summarise(
    mean_OOB = mean(OOB_error),
    sd_OOB   = sd(OOB_error),
    .groups = "drop"
  ) %>%
  arrange(method, mean_OOB)

# ------------------------------------------------------------
# Save Excel
# ------------------------------------------------------------
wb <- createWorkbook()

addWorksheet(wb, "Raw")
writeData(wb, "Raw", results)

addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_results)

saveWorkbook(
  wb,
  file.path(out_dir, "RF_hyperparameter_tuning_BBS.xlsx"),
  overwrite = TRUE
)

print(summary_results)

cat("\nHyperparameter tuning finished\n")

# ------------------------------------------------------------
# Heatmap visualization (MULTIPANEL)
# ------------------------------------------------------------
plot_data <- summary_results %>%
  group_by(method) %>%
  mutate(
    mean_OOB_scaled = (mean_OOB - min(mean_OOB)) / (max(mean_OOB) - min(mean_OOB)),
    mtry = factor(mtry, levels = sort(unique(mtry))),
    nodesize = factor(nodesize, levels = sort(unique(nodesize)))
  ) %>%
  ungroup()

heatmap_plot <- ggplot(plot_data,
                       aes(x = nodesize,
                           y = mtry,
                           fill = mean_OOB_scaled)) +
  
  geom_tile(color = "grey90", linewidth = 0.4) +
  
  geom_text(aes(label = sprintf("%.4f", mean_OOB)),
            size = 3.5,
            fontface = "bold",
            color = "black") +
  
  facet_wrap(~method) +
  
  scale_fill_gradientn(
    colours = c("#2166AC", "#67A9CF", "#F7F7F7", "#EF8A62", "#B2182B"),
    name = "OOB error"
  ) +
  
  labs(
    x = "Node size",
    y = "mtry",
    title = bquote("Hyperparameter optimization of Random Forest model for" ~ italic("P. orientalis"))
  ) +
  
  coord_fixed() +
  
  theme_classic(base_size = 14) +
  
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )

print(heatmap_plot)

# ------------------------------------------------------------
# Save heatmap
# ------------------------------------------------------------
ggsave(
  filename = file.path(out_dir, "RF_hyperparameter_heatmap_BBS.png"),
  plot = heatmap_plot,
  width = 9,
  height = 6,
  dpi = 300
)

cat("\nHeatmap saved\n")