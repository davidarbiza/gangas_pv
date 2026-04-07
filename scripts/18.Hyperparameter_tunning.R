############################################
# Random Forest ntree tuning
# Species: BBS
# Pseudoabsences: Random
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
# Paths
# ------------------------------------------------------------

data_file <- "E:/TFM_gangas/GPS/ExtractedV.2/BBS_pseudoabsences_Random_env.csv"
out_dir   <- "E:/TFM_gangas/GPS/ExtractedV.2/Hyperparameter"
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
data_pts <- read.csv(data_file)

data_model <- data_pts %>%
  mutate(presence = factor(presence, levels = c(0, 1))) %>%
  dplyr::select(
    -birdID,
    -date,
    -species,
    -X_25830,
    -Y_25830,
    -LC_RiceFields,
    -LC_Greenhouses,
    -AltRange,
    -Aspect,
    -Tmin,
    -Tmax,
    -TminSD100,
    -TmaxSD100
  ) %>%
  na.omit()

rm(data_pts)
gc()

# ------------------------------------------------------------
# ntree values to test
# ------------------------------------------------------------
ntree_vals <- c(25, 50, 100, 200, 300, 400, 500, 600, 750)

# ------------------------------------------------------------
# Storage
# ------------------------------------------------------------
results <- data.frame(
  ntree = integer(),
  repetition = integer(),
  OOB_error = numeric()
)

# ------------------------------------------------------------
# Run tuning
# ------------------------------------------------------------
tic("RF ntree tuning (OOB error)")

results <- foreach(
  i = 1:5,
  .combine = rbind,
  .packages = c("randomForest", "dplyr")
) %dopar% {
  
  set.seed(100 + i)
  
  # --- Balance classes ---
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
        ntree = nt,
        repetition = i,
        OOB_error = oob_err
      )
    )
  }
  
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
  file.path(out_dir, "RF_ntree_tuning_BBS_Random_raw.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# Summary statistics
# ------------------------------------------------------------
summary_ntree <- results %>%
  group_by(ntree) %>%
  summarise(
    mean_OOB = mean(OOB_error),
    sd_OOB   = sd(OOB_error),
    .groups = "drop"
  )

write.csv(
  summary_ntree,
  file.path(out_dir, "RF_ntree_tuning_BBS_Random_summary.csv"),
  row.names = FALSE
)

print(summary_ntree)

# ------------------------------------------------------------
# Plot OOB error vs ntree
# ------------------------------------------------------------
library(ggplot2)

summary_ntree <- read.csv(
  "E:/TFM_gangas/GPS/ExtractedV.2/Hyperparameter/RF_ntree_tuning_BBS_Random_summary.csv"
)

ggplot(summary_ntree, aes(x = ntree, y = mean_OOB)) +
  
  geom_ribbon(aes(ymin = mean_OOB - sd_OOB,
                  ymax = mean_OOB + sd_OOB),
              fill = "#3B5B92", alpha = 0.15) +
  
  geom_line(color = "#1B3A6F", linewidth = 1.2) +
  
  geom_point(size = 3, color = "#1B3A6F") +
  
  geom_vline(xintercept = 500,
             linetype = "dashed",
             color = "grey40",
             linewidth = 0.8) +
  
  scale_x_continuous(breaks = summary_ntree$ntree) +
  
  labs(
    x = "Number of trees",
    y = "Out-of-bag error",
    title = "Stabilization of OOB error with increasing number of trees"
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
# Species: BBS
# Pseudoabsences: Random
############################################

# --- Load libraries ---
library(randomForest)
library(dplyr)
library(tictoc)
library(doParallel)
library(foreach)
library(openxlsx)

set.seed(453)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
data_file <- "E:/TFM_gangas/GPS/ExtractedV.2/BBS_pseudoabsences_Random_env.csv"
out_dir   <- "E:/TFM_gangas/GPS/ExtractedV.2/Hyperparameter"
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
data_pts <- read.csv(data_file)

data_model <- data_pts %>%
  mutate(presence = factor(presence, levels = c(0, 1))) %>%
  dplyr::select(
    -birdID,
    -date,
    -species,
    -X_25830,
    -Y_25830,
    -LC_RiceFields,
    -LC_Greenhouses,
    -AltRange,
    -Aspect,
    -Tmin,
    -Tmax,
    -TminSD100,
    -TmaxSD100
  ) %>%
  na.omit()

rm(data_pts)
gc()

# ------------------------------------------------------------
# Number of predictors
# ------------------------------------------------------------
predictor_names <- setdiff(names(data_model), "presence")
n_vars <- length(predictor_names)

# ------------------------------------------------------------
# Hyperparameter grid
# ------------------------------------------------------------
mtry_vals <- unique(round(c(sqrt(n_vars), n_vars/3, n_vars/2)))
nodesize_vals <- c(1, 5, 10)

# ntree fixed
ntree_val <- 500

grid <- expand.grid(
  mtry = mtry_vals,
  nodesize = nodesize_vals
)

# ------------------------------------------------------------
# Storage
# ------------------------------------------------------------
results <- data.frame(
  mtry = integer(),
  nodesize = integer(),
  repetition = integer(),
  OOB_error = numeric()
)

# ------------------------------------------------------------
# Run tuning
# ------------------------------------------------------------

results <- foreach(
  i = 1:5,
  .combine = rbind,
  .packages = c("randomForest", "dplyr")
) %dopar% {
  
  set.seed(100 + i)
  
  # --- Balance classes ---
  n_pres <- sum(data_model$presence == 1)
  sampsize <- c("0" = n_pres, "1" = n_pres)
  
  out_rep <- data.frame()
  
  for (j in 1:nrow(grid)) {
    
    rf_model <- randomForest(
      presence ~ .,
      data = data_model,
      ntree = ntree_val,
      mtry = grid$mtry[j],
      nodesize = grid$nodesize[j],
      sampsize = sampsize,
      importance = FALSE
    )
    
    
    oob_err <- rf_model$err.rate[ntree_val, "OOB"]
    
    out_rep <- rbind(
      out_rep,
      data.frame(
        mtry = grid$mtry[j],
        nodesize = grid$nodesize[j],
        repetition = i,
        OOB_error = oob_err
      )
    )
  }
  
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
  group_by(mtry, nodesize) %>%
  summarise(
    mean_OOB = mean(OOB_error),
    sd_OOB   = sd(OOB_error),
    .groups = "drop"
  ) %>%
  arrange(mean_OOB)

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
  file.path(out_dir, "RF_hyperparameter_tuning.xlsx"),
  overwrite = TRUE
)

print(summary_results)

cat("\n✅ Hyperparameter tuning finished\n")

# ------------------------------------------------------------
# Heatmap visualization
# ------------------------------------------------------------
library(ggplot2)

plot_data <- summary_results %>%
  mutate(
    mtry = factor(mtry, levels = sort(unique(mtry))),
    nodesize = factor(nodesize, levels = sort(unique(nodesize)))
  )

heatmap_plot <- ggplot(plot_data, aes(x = nodesize, y = mtry, fill = mean_OOB)) +
  
  geom_tile(color = "grey90", linewidth = 0.4) +
  
  geom_text(aes(label = sprintf("%.4f", mean_OOB)),
            size = 4,
            fontface = "bold",
            color = "black") +
  
  scale_fill_gradientn(
    colours = c("#2166AC", "#67A9CF", "#F7F7F7", "#EF8A62", "#B2182B"),
    name = "OOB error"
  ) +
  
  labs(
    x = "Node size",
    y = "mtry",
    title = "Hyperparameter optimization of Random Forest model"
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
  filename = file.path(out_dir, "RF_hyperparameter_heatmap.png"),
  plot = heatmap_plot,
  width = 7,
  height = 5,
  dpi = 300
)

cat("\n📊 Heatmap (publication quality) guardado\n")