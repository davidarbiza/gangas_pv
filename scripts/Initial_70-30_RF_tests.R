############################################
# RF validation (70/30)
# 2 species × 3 pseudoabsence methods
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

set.seed(723)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
base_path <- "E:/TFM_gangas/GPS/Extracted"

# ------------------------------------------------------------
# Species and methods
# ------------------------------------------------------------
species_list <- c("PTS", "BBS")

methods <- data.frame(
  method = c("Random", "P95", "MCP40km"),
  file   = c(
    "pseudoabsences_Random_env.csv",
    "pseudoabsences_P95_env.csv",
    "pseudoabsences_MCP40km_env.csv"
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Parallel setup
# ------------------------------------------------------------
n_cores <- 3
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# ------------------------------------------------------------
# Excel workbook
# ------------------------------------------------------------
wb <- createWorkbook()

# ------------------------------------------------------------
# Accumulator for all results
# ------------------------------------------------------------
all_results <- data.frame()

############################################
# MAIN LOOP
############################################

for (sp in species_list) {
  for (m in 1:nrow(methods)) {
    
    cat("\n====================================\n")
    cat("Species:", sp, "\n")
    cat("Pseudoabsence method:", methods$method[m], "\n")
    cat("====================================\n")
    
    # --- Load data ---
    data_pts <- read.csv(
      file.path(base_path, paste0(sp, "_", methods$file[m]))
    )
    
    # --- Prepare variables ---
    data_model <- data_pts %>%
      mutate(
        presence = factor(presence, levels = c(0, 1))
      ) %>%
      select(
        -birdID,
        -date,
        -species,
        -X_25830,
        -Y_25830,
        -LC_RiceFields,
        -LC_Greenhouses,
        -Slope,
        -Aspect,
        -Tmin,
        -Tmax,
        -TminSD100,
        -TmaxSD100
      ) %>%
      na.omit()
    
    rm(data_pts)
    gc()
    
    ############################################
    # RF VALIDATION
    ############################################
    
    tic(paste("RF 70/30 —", sp, methods$method[m]))
    
    results <- foreach(
      i = 1:5,
      .combine = rbind,
      .packages = c("randomForest", "pROC", "dplyr")
    ) %dopar% {
      
      set.seed(100 + i)
      
      pres_idx <- which(data_model$presence == 1)
      abs_idx  <- which(data_model$presence == 0)
      
      train_pres <- sample(pres_idx, size = floor(0.7 * length(pres_idx)))
      train_abs  <- sample(abs_idx,  size = floor(0.7 * length(abs_idx)))
      
      train_idx <- c(train_pres, train_abs)
      test_idx  <- setdiff(seq_len(nrow(data_model)), train_idx)
      
      train <- data_model[train_idx, ]
      test  <- data_model[test_idx, ]
      
      n_pres <- sum(train$presence == 1)
      sampsize <- c("0" = n_pres, "1" = n_pres)
      
      rf_model <- randomForest(
        presence ~ .,
        data = train,
        ntree = 500,
        sampsize = sampsize
      )
      
      preds_prob <- predict(rf_model, newdata = test, type = "prob")[, "1"]
      
      auc_val <- as.numeric(auc(test$presence, preds_prob))
      
      roc_curve <- roc(test$presence, preds_prob)
      thr <- coords(roc_curve, "best", best.method = "youden")$threshold
      
      preds_bin <- ifelse(preds_prob >= thr, 1, 0)
      
      TP <- sum(preds_bin == 1 & test$presence == 1)
      TN <- sum(preds_bin == 0 & test$presence == 0)
      FP <- sum(preds_bin == 1 & test$presence == 0)
      FN <- sum(preds_bin == 0 & test$presence == 1)
      
      sens <- TP / (TP + FN)
      spec <- TN / (TN + FP)
      tss  <- sens + spec - 1
      
      data.frame(
        repetition = i,
        AUC = auc_val,
        Sensitivity = sens,
        Specificity = spec,
        TSS = tss
      )
    }
    
    toc()
    
    # --- Add identifiers ---
    results$Species <- sp
    results$Method  <- methods$method[m]
    
    # --- Accumulate ---
    all_results <- rbind(all_results, results)
    
    # --- Save sheet ---
    sheet_name <- paste(sp, methods$method[m], sep = "_")
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, results)
    
    rm(data_model, results)
    gc()
  }
}

# ------------------------------------------------------------
# Stop parallel
# ------------------------------------------------------------
stopCluster(cl)
registerDoSEQ()

############################################
# SUMMARY TABLE
############################################

summary_table <- all_results %>%
  group_by(Species, Method) %>%
  summarise(
    mean_AUC = mean(AUC),
    sd_AUC   = sd(AUC),
    mean_TSS = mean(TSS),
    sd_TSS   = sd(TSS),
    mean_Sens = mean(Sensitivity),
    sd_Sens   = sd(Sensitivity),
    mean_Spec = mean(Specificity),
    sd_Spec   = sd(Specificity),
    .groups = "drop"
  )

addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_table)

# ------------------------------------------------------------
# Save Excel 
# ------------------------------------------------------------
saveWorkbook(
  wb,
  file.path(base_path, "RF_comparison_pseudoabsences.xlsx"),
  overwrite = TRUE
)

cat("\nFINISHED — Excel + Summary\n")

