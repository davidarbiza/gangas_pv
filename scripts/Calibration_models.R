############################################
# CALIBRATION ANALYSIS
############################################

# ------------------------------------------------------------
# Libraries
# ------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(mgcv)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

base_path <- "E:/TFM_gangas/GPS/ExtractedV.4"

out_dir <- file.path(base_path, "Calibration_analysis")

dir.create(out_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# Load calibration data
# ------------------------------------------------------------

calib_data <- read.csv(
  file.path(base_path, "Calibration_data.csv")
)

# ------------------------------------------------------------
# Storage objects
# ------------------------------------------------------------

all_stats <- data.frame()

############################################
# Main loop
############################################

for(sp in unique(calib_data$species)){
  
  for(met in unique(calib_data$method)){
    
    cat("\n============================\n")
    cat("Species:", sp, "\n")
    cat("Method:", met, "\n")
    cat("============================\n")
    
    # --------------------------------------------------------
    # Subset data
    # --------------------------------------------------------
    
    df <- calib_data %>%
      filter(
        species == sp,
        method == met
      )
    
    # --------------------------------------------------------
    # Link-scale transformation
    # --------------------------------------------------------
    
    eps <- 1e-6
    
    df$pred_clamped <- pmin(
      pmax(df$predicted, eps),
      1 - eps
    )
    
    df$logit_pred <- qlogis(df$pred_clamped)
    
    # --------------------------------------------------------
    # Calibration GLM
    # --------------------------------------------------------
    
    calib_glm <- glm(
      observed ~ logit_pred,
      data = df,
      family = binomial
    )
    
    intercept <- coef(calib_glm)[1]
    
    slope <- coef(calib_glm)[2]
    
    # --------------------------------------------------------
    # Flexible calibration GAM
    # --------------------------------------------------------
    
    calib_gam <- gam(
      observed ~ s(predicted, k = 5),
      data = df,
      family = binomial
    )
    
    # --------------------------------------------------------
    # Save calibration statistics
    # --------------------------------------------------------
    
    stats_df <- data.frame(
      species = sp,
      method = met,
      intercept = intercept,
      slope = slope
    )
    
    all_stats <- rbind(all_stats, stats_df)
    
    # --------------------------------------------------------
    # Calibration plot
    # --------------------------------------------------------
    
    p <- ggplot(df, aes(x = predicted, y = observed)) +
      
      geom_jitter(
        width = 0,
        height = 0.05,
        alpha = 0.15,
        size = 1
      ) +
      
      geom_abline(
        slope = 1,
        intercept = 0,
        linetype = "dashed",
        linewidth = 1
      ) +
      
      stat_smooth(
        method = "gam",
        formula = y ~ s(x, k = 5),
        method.args = list(family = "binomial"),
        color = "#D95F02",
        linewidth = 1.2,
        se = TRUE
      ) +
      
      coord_cartesian(
        xlim = c(0,1),
        ylim = c(0,1)
      ) +
      
      theme_classic(base_size = 14) +
      
      labs(
        title = paste(
          "Calibration plot -",
          sp,
          "-",
          met
        ),
        subtitle = paste(
          "Intercept =",
          round(intercept, 3),
          "| Slope =",
          round(slope, 3)
        ),
        x = "Predicted probability",
        y = "Observed occurrence"
      )
    
    # --------------------------------------------------------
    # Save figure
    # --------------------------------------------------------
    
    ggsave(
      filename = paste0(
        "Calibration_",
        sp,
        "_",
        met,
        ".png"
      ),
      plot = p,
      path = out_dir,
      width = 7,
      height = 6,
      dpi = 300
    )
  }
}

############################################
# Save calibration statistics
############################################

write.csv(
  all_stats,
  file.path(out_dir, "Calibration_statistics.csv"),
  row.names = FALSE
)

cat("CALIBRATION ANALYSIS FINISHED\n")



############################################
# RF CALIBRATION — BUILD GAM CALIBRATORS
############################################

library(dplyr)
library(mgcv)

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

base_path <- "E:/TFM_gangas/GPS/ExtractedV.4"

calibration_dir <- file.path(base_path, "Calibration_models")
dir.create(calibration_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# LOAD CALIBRATION DATA
# ------------------------------------------------------------

cal_data <- read.csv(
  file.path(base_path, "Calibration_data.csv")
)

# ------------------------------------------------------------
# STORAGE
# ------------------------------------------------------------

calibration_summary <- data.frame()

# ------------------------------------------------------------
# LOOP
# ------------------------------------------------------------

species_list <- unique(cal_data$species)
methods_list <- unique(cal_data$method)

for(sp in species_list){

  for(met in methods_list){

    cat("\n============================\n")
    cat("Species:", sp, "\n")
    cat("Method:", met, "\n")
    cat("============================\n")

    # --------------------------------------------------------
    # SUBSET
    # --------------------------------------------------------

    df <- cal_data %>%
      filter(
        species == sp,
        method == met
      )

    # --------------------------------------------------------
    # FIT GAM CALIBRATOR
    # --------------------------------------------------------

    gam_cal <- gam(
      observed ~ s(predicted),
      data = df,
      family = binomial(link = "logit"),
      method = "REML"
    )

    # --------------------------------------------------------
    # SAVE MODEL
    # --------------------------------------------------------

    saveRDS(
      gam_cal,
      file.path(
        calibration_dir,
        paste0("GAM_", sp, "_", met, ".rds")
      )
    )

    # --------------------------------------------------------
    # SUMMARY
    # --------------------------------------------------------

    sm <- summary(gam_cal)

    calibration_summary <- rbind(
      calibration_summary,
      data.frame(
        species = sp,
        method = met,
        edf = sm$s.table[1, "edf"],
        deviance_explained = sm$dev.expl
      )
    )

  }
}

# ------------------------------------------------------------
# SAVE SUMMARY
# ------------------------------------------------------------

write.csv(
  calibration_summary,
  file.path(base_path, "GAM_calibration_summary.csv"),
  row.names = FALSE
)

cat("\nGAM CALIBRATORS READY\n")




############################################
# CALIBRATED PROJECTIONS
############################################

library(terra)
library(mgcv)
library(stringr)

rm(list = ls())
gc()

# ------------------------------------------------------------
# TEMP DIRECTORY
# ------------------------------------------------------------

dir.create("E:/temp_terra", showWarnings = FALSE)

terraOptions(
  memfrac = 0.6,
  tempdir = "E:/temp_terra",
  progress = 1
)

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

base_path <- "E:/TFM_gangas/GPS/ExtractedV.4"

projection_path <- "E:/TFM_gangas/GPS/ExtractedV.4/CV_models/PROJECTIONS_ENSEMBLE_10DAYS"

gam_path <- file.path(
  base_path,
  "Calibration_models"
)

out_dir <- file.path(
  base_path,
  "CALIBRATED_PROJECTIONS"
)

dir.create(out_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# SPECIES AND METHODS
# ------------------------------------------------------------

species_list <- c("PTS", "BBS")

methods <- c("Random")

# ------------------------------------------------------------
# MAIN LOOP
# ------------------------------------------------------------

for(sp in species_list){
  
  for(met in methods){
    
    cat("\n====================================\n")
    cat("Species:", sp, "\n")
    cat("Method:", met, "\n")
    cat("====================================\n")
    
    # --------------------------------------------------------
    # LOAD GAM CALIBRATION MODEL
    # --------------------------------------------------------
    
    gam_model <- readRDS(
      file.path(
        gam_path,
        paste0("GAM_", sp, "_", met, ".rds")
      )
    )
    
    # --------------------------------------------------------
    # FIND RF ENSEMBLE MAPS
    # --------------------------------------------------------
    
    ensemble_files <- list.files(
      projection_path,
      pattern = paste0(
        "^",
        sp,
        "_",
        met,
        "_.*ENSEMBLE\\.tif$"
      ),
      full.names = TRUE
    )
    
    # --------------------------------------------------------
    # LOOP THROUGH MAPS
    # --------------------------------------------------------
    
    for(f in ensemble_files){
      
      cat("\nProcessing:\n")
      cat(basename(f), "\n")
      
      # ------------------------------------------------------
      # OUTPUT NAME
      # ------------------------------------------------------
      
      out_name <- str_replace(
        basename(f),
        "_ENSEMBLE.tif",
        "_CALIBRATED.tif"
      )
      
      out_file <- file.path(
        out_dir,
        out_name
      )
      
      # ------------------------------------------------------
      # SKIP IF EXISTS
      # ------------------------------------------------------
      
      if(file.exists(out_file)){
        
        cat("Skipping existing\n")
        
        next
      }
      
      # ------------------------------------------------------
      # LOAD RF MAP
      # ------------------------------------------------------
      
      r <- rast(f)
      
      names(r) <- "predicted"
      
      # ------------------------------------------------------
      # APPLY GAM CALIBRATION
      # ------------------------------------------------------
      
      calibrated_r <- terra::predict(
        r,
        gam_model,
        type = "response",
        
        filename = file.path(
          "E:/temp_terra",
          paste0(
            tools::file_path_sans_ext(out_name),
            "_tmp.tif"
          )
        ),
        
        overwrite = TRUE
      )
      
      # ------------------------------------------------------
      # SAVE FINAL MAP
      # ------------------------------------------------------
      
      writeRaster(
        calibrated_r,
        out_file,
        overwrite = TRUE
      )
      
      # ------------------------------------------------------
      # CLEAN TEMP FILES
      # ------------------------------------------------------
      
      file.remove(
        file.path(
          "E:/temp_terra",
          paste0(
            tools::file_path_sans_ext(out_name),
            "_tmp.tif"
          )
        )
      )
      
      rm(r, calibrated_r)
      
      gc()
    }
  }
}

cat("\nCALIBRATED PROJECTIONS FINISHED\n")




############################################
# COMPARE RF VS CALIBRATED SUITABILITY
# Distribution summary statistics
############################################

library(terra)
library(dplyr)

rm(list = ls())
gc()

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

# Original RF ensemble maps
rf_path <- "E:/TFM_gangas/GPS/ExtractedV.4/CV_models/PROJECTIONS_ENSEMBLE_10DAYS"

# Calibrated maps
cal_path <- "E:/TFM_gangas/GPS/ExtractedV.4/CALIBRATED_PROJECTIONS"

# Output
out_dir <- "E:/TFM_gangas/GPS/ExtractedV.4"

# ------------------------------------------------------------
# SPECIES AND METHODS
# ------------------------------------------------------------

species_list <- c("PTS", "BBS")
methods <- c("Random")

# ------------------------------------------------------------
# STORAGE
# ------------------------------------------------------------

all_results <- data.frame()

# ------------------------------------------------------------
# MAIN LOOP
# ------------------------------------------------------------

for(sp in species_list){
  
  for(met in methods){
    
    cat("\n====================================\n")
    cat("Species:", sp, "\n")
    cat("Method:", met, "\n")
    cat("====================================\n")
    
    # --------------------------------------------------------
    # LOAD ORIGINAL RF MAP
    # --------------------------------------------------------
    
    rf_file <- list.files(
      rf_path,
      pattern = paste0(
        "^",
        sp,
        "_",
        met,
        "_.*ENSEMBLE\\.tif$"
      ),
      full.names = TRUE
    )
    
    rf_rast <- rast(rf_file)
    
    
    # --------------------------------------------------------
    # LOAD CALIBRATED MAP
    # --------------------------------------------------------
    
    cal_file <- list.files(
      cal_path,
      pattern = paste0(
        "^",
        sp,
        "_",
        met,
        "_.*CALIBRATED\\.tif$"
      ),
      full.names = TRUE
    )
    
    cal_rast <- rast(cal_file)
    
    # --------------------------------------------------------
    # SUMMARY TABLE
    # --------------------------------------------------------
    
    rf_summary <- data.frame(
      species = sp,
      method = met,
      version = "Original_RF",
      
      mean = global(rf_rast, mean, na.rm=TRUE)[1,1],
      sd   = global(rf_rast, sd, na.rm=TRUE)[1,1],
      
      q50 = global(rf_rast, \(x) quantile(x, 0.50, na.rm=TRUE))[1,1],
      q75 = global(rf_rast, \(x) quantile(x, 0.75, na.rm=TRUE))[1,1],
      q90 = global(rf_rast, \(x) quantile(x, 0.90, na.rm=TRUE))[1,1],
      q95 = global(rf_rast, \(x) quantile(x, 0.95, na.rm=TRUE))[1,1],
      
      max = global(rf_rast, max, na.rm=TRUE)[1,1]
    )
    
    cal_summary <- data.frame(
      species = sp,
      method = met,
      version = "Calibrated_GAM",
      
      mean = global(cal_rast, mean, na.rm=TRUE)[1,1],
      sd   = global(cal_rast, sd, na.rm=TRUE)[1,1],
      
      q50 = global(cal_rast, \(x) quantile(x, 0.50, na.rm=TRUE))[1,1],
      q75 = global(cal_rast, \(x) quantile(x, 0.75, na.rm=TRUE))[1,1],
      q90 = global(cal_rast, \(x) quantile(x, 0.90, na.rm=TRUE))[1,1],
      q95 = global(cal_rast, \(x) quantile(x, 0.95, na.rm=TRUE))[1,1],
      
      max = global(cal_rast, max, na.rm=TRUE)[1,1]
    )
    
    all_results <- rbind(
      all_results,
      rf_summary,
      cal_summary
    )
    
    # --------------------------------------------------------
    # PRINT RESULTS
    # --------------------------------------------------------
    
    cat("\n--- ORIGINAL RF ---\n")
    print(rf_summary)
    
    cat("\n--- CALIBRATED GAM ---\n")
    print(cal_summary)
    
    rm(rf_rast, cal_rast)
    gc()
  }
}

# ------------------------------------------------------------
# SAVE CSV
# ------------------------------------------------------------

write.csv(
  all_results,
  file.path(out_dir, "RF_vs_Calibrated_summary.csv"),
  row.names = FALSE
)

cat("\nSUMMARY SAVED\n")




############################################
# SDM FINAL RESULTS — CALIBRATED PROJECTIONS
############################################

library(terra)
library(ggplot2)
library(tidyterra)
library(dplyr)
library(sf)
library(mapSpain)
library(rnaturalearth)
library(patchwork)
library(magick)

dir.create("E:/terra_temp", showWarnings = FALSE)

terraOptions(
  memfrac = 0.6,
  tempdir = "E:/terra_temp",
  progress = 1
)

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

base_path <- "E:/TFM_gangas/GPS/ExtractedV.4/CALIBRATED_PROJECTIONS"

sd_path <- "E:/TFM_gangas/GPS/ExtractedV.4/CV_models/PROJECTIONS_ENSEMBLE_10DAYS"

out_dir <- file.path(base_path, "FINAL_RESULTS")
dir.create(out_dir, showWarnings = FALSE)

gif_dir <- file.path(out_dir, "GIF_frames")
dir.create(gif_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# DEKAD LABELS
# ------------------------------------------------------------

dekad_labels <- c(
  "Early Jan","Mid Jan","Late Jan",
  "Early Feb","Mid Feb","Late Feb",
  "Early Mar","Mid Mar","Late Mar",
  "Early Apr","Mid Apr","Late Apr",
  "Early May","Mid May","Late May",
  "Early Jun","Mid Jun","Late Jun",
  "Early Jul","Mid Jul","Late Jul",
  "Early Aug","Mid Aug","Late Aug",
  "Early Sep","Mid Sep","Late Sep",
  "Early Oct","Mid Oct","Late Oct",
  "Early Nov","Mid Nov","Late Nov",
  "Early Dec","Mid Dec","Late Dec"
)

# ------------------------------------------------------------
# FILES
# ------------------------------------------------------------

methods <- c("Random")

files <- list()

for(met in methods){
  
  files[[met]] <- list(
    
    PTS = sort(list.files(base_path,
                          pattern = paste0("PTS_",met,"_.*CALIBRATED\\.tif$"),
                          full.names = TRUE)),
    
    BBS = sort(list.files(base_path,
                          pattern = paste0("BBS_",met,"_.*CALIBRATED\\.tif$"),
                          full.names = TRUE)),
    
    PTS_sd = sort(list.files(sd_path,
                             pattern = paste0("PTS_",met,"_.*SD\\.tif$"),
                             full.names = TRUE)),
    
    BBS_sd = sort(list.files(sd_path,
                             pattern = paste0("BBS_",met,"_.*SD\\.tif$"),
                             full.names = TRUE))
  )
}

# ------------------------------------------------------------
# MASK
# ------------------------------------------------------------

provinces <- esp_get_prov()

provinces <- provinces[
  !provinces$iso2.prov.name.es %in%
    c("Baleares","Las Palmas","Santa Cruz de Tenerife","Ceuta","Melilla"), ]

provinces <- st_transform(provinces, 25830)

mask_spain <- st_union(provinces)

por <- ne_countries(
  country = "Portugal",
  scale = "medium",
  returnclass = "sf"
)

por <- st_transform(por, 25830)
por <- st_cast(por, "POLYGON")

por$area <- st_area(por)
por <- por[which.max(por$area), ]

iberia_mask <- st_union(mask_spain, por)
mask_vect <- vect(iberia_mask)

# ============================================================
# LOOP
# ============================================================

for(met in methods){
  
  cat("\n====================================\n")
  cat("METHOD:", met, "\n")
  cat("====================================\n")
  
  # ------------------------------------------------------------
  # STACKS
  # ------------------------------------------------------------
  
  stack_pts <- mask(
    crop(rast(files[[met]]$PTS), mask_vect),
    mask_vect
  )
  
  stack_bbs <- mask(
    crop(rast(files[[met]]$BBS), mask_vect),
    mask_vect
  )
  
  stack_pts_sd <- mask(
    crop(rast(files[[met]]$PTS_sd), mask_vect),
    mask_vect
  )
  
  stack_bbs_sd <- mask(
    crop(rast(files[[met]]$BBS_sd), mask_vect),
    mask_vect
  )
  
  # ------------------------------------------------------------
  # GLOBAL SCALE
  # ------------------------------------------------------------
  
  global_min <- min(
    minmax(stack_pts)[1],
    minmax(stack_bbs)[1]
  )
  
  global_max <- max(
    minmax(stack_pts)[2],
    minmax(stack_bbs)[2]
  )
  
  # ------------------------------------------------------------
  # MAP FUNCTION
  # ------------------------------------------------------------
  
  plot_map <- function(r, title){
    
    r <- focal(r, w = 3, fun = mean, na.rm = TRUE)
    
    ggplot() +
      geom_spatraster(data = r) +
      geom_sf(data = iberia_mask,
              fill = NA,
              color = "black",
              linewidth = 0.2) +
      
      scale_fill_viridis_c(
        option = "inferno",
        limits = c(global_min, global_max),
        name = "Suitability"
      ) +
      
      theme_void(base_size = 14) +
      
      theme(
        plot.background = element_rect(fill="white", color="white"),
        panel.background = element_rect(fill="white", color="white"),
        legend.background = element_rect(fill="white", color="white"),
        plot.title = element_text(face="bold", hjust=0.5),
        legend.position = "right"
      ) +
      
      labs(title = title)
  }
  
  # ============================================================
  # FIGURE 1 — TEMPORAL CURVE
  # ============================================================
  
  get_curve <- function(stack, stack_sd, label){
    
    data.frame(
      time = 1:nlyr(stack),
      mean = global(stack, mean, na.rm=TRUE)[,1],
      sd = global(stack_sd, mean, na.rm=TRUE)[,1],
      species = label
    )
  }
  
  df_mean <- rbind(
    get_curve(stack_pts, stack_pts_sd, "P. alchata"),
    get_curve(stack_bbs, stack_bbs_sd, "P. orientalis")
  )
  
  p_mean <- ggplot(df_mean,
                   aes(time, mean, color=species, fill=species)) +
    
    geom_ribbon(
      aes(ymin = mean-sd, ymax = mean+sd),
      alpha = 0.25,
      color = NA
    ) +
    
    geom_line(linewidth = 1.2) +
    geom_point(size = 1.6) +
    
    scale_x_continuous(
      breaks = seq(2,36,3),
      labels = month.abb
    ) +
    
    theme_classic(base_size = 14) +
    
    theme(
      legend.title = element_blank(),
      plot.title = element_text(face="bold", hjust=0.5)
    ) +
    
    labs(
      title = "Seasonal suitability dynamics",
      x = "Month",
      y = "Mean suitability"
    )
  
  ggsave(
    file.path(out_dir,
              paste0("Fig1_mean_suitability_",met,".png")),
    p_mean,
    width = 8,
    height = 5,
    dpi = 300,
    bg = "white"
  )
  
  # ============================================================
  # FIGURE 2 — MAX / MIN MAPS
  # ============================================================
  
  mean_pts <- global(stack_pts, mean, na.rm=TRUE)[,1]
  mean_bbs <- global(stack_bbs, mean, na.rm=TRUE)[,1]
  
  pts_max <- which.max(mean_pts)
  pts_min <- which.min(mean_pts)
  
  bbs_max <- which.max(mean_bbs)
  bbs_min <- which.min(mean_bbs)
  
  p_pts_max <- plot_map(
    stack_pts[[pts_max]],
    paste0(
      "P. alchata — Max suitability (",
      dekad_labels[pts_max],
      ")"
    )
  )
  
  p_pts_min <- plot_map(
    stack_pts[[pts_min]],
    paste0(
      "P. alchata — Min suitability (",
      dekad_labels[pts_min],
      ")"
    )
  )
  
  p_bbs_max <- plot_map(
    stack_bbs[[bbs_max]],
    paste0(
      "P. orientalis — Max suitability (",
      dekad_labels[bbs_max],
      ")"
    )
  )
  
  p_bbs_min <- plot_map(
    stack_bbs[[bbs_min]],
    paste0(
      "P. orientalis — Min suitability (",
      dekad_labels[bbs_min],
      ")"
    )
  )
  
  p_all <- (p_pts_max + p_pts_min) /
    (p_bbs_max + p_bbs_min)
  
  ggsave(
    file.path(out_dir,
              paste0("Fig2_all_maps_",met,".png")),
    p_all,
    width = 12,
    height = 10,
    dpi = 300,
    bg = "white"
  )
  
  p_pts_fix <- (p_pts_max + p_pts_min) &
    
    theme(
      plot.title = element_text(
        size = 13,
        face = "bold",
        hjust = 0.5
      )
    )
  
  ggsave(
    file.path(out_dir,
              paste0("Fig2_PTS_maps_",met,".png")),
    p_pts_fix,
    width = 10,
    height = 5,
    dpi = 300,
    bg = "white"
  )
  
  p_bbs_fix <- (p_bbs_max + p_bbs_min) &
    
    theme(
      plot.title = element_text(
        size = 13,
        face = "bold",
        hjust = 0.5
      )
    )
  
  ggsave(
    file.path(out_dir,
              paste0("Fig2_BBS_maps_",met,".png")),
    p_bbs_fix,
    width = 10,
    height = 5,
    dpi = 300,
    bg = "white"
  )
  
  # ============================================================
  # FIGURE 3 — GIFS
  # ============================================================
  
  make_gif <- function(stack, species_name){
    
    frame_paths <- c()
    
    latin_name <- ifelse(
      species_name == "PTS",
      "P. alchata",
      "P. orientalis"
    )
    
    for(i in 1:nlyr(stack)){
      
      cat(species_name, "- frame:", i, "\n")
      
      p <- plot_map(
        stack[[i]],
        
        paste0(
          latin_name,
          " — Mean suitability (",
          dekad_labels[i],
          ")"
        )
      )
      
      frame_file <- file.path(
        gif_dir,
        paste0(
          species_name,
          "_frame_",
          sprintf("%02d",i),
          ".png"
        )
      )
      
      ggsave(
        frame_file,
        p,
        width = 7,
        height = 5,
        dpi = 200,
        bg = "white"
      )
      
      frame_paths <- c(frame_paths, frame_file)
      
      rm(p)
      gc()
    }
    
    imgs <- image_read(frame_paths)
    
    gif <- image_animate(
      imgs,
      delay = 20
    )
    
    image_write(
      gif,
      path = file.path(
        out_dir,
        paste0(
          species_name,
          "_",
          met,
          ".gif"
        )
      )
    )
    
    rm(imgs, gif)
    gc()
  }
  
  make_gif(stack_pts, "PTS")
  make_gif(stack_bbs, "BBS")
  
  # ============================================================
  # FIGURE 4 — OVERLAP
  # ============================================================
  
  mean_pts_r <- app(stack_pts, mean, na.rm=TRUE)
  mean_bbs_r <- app(stack_bbs, mean, na.rm=TRUE)
  
  thr_pts <- quantile(
    values(mean_pts_r),
    0.9,
    na.rm=TRUE
  )
  
  thr_bbs <- quantile(
    values(mean_bbs_r),
    0.9,
    na.rm=TRUE
  )
  
  hot_pts <- mean_pts_r > thr_pts
  hot_bbs <- mean_bbs_r > thr_bbs
  
  overlap <- hot_pts*1 + hot_bbs*2
  
  overlap[overlap == 0] <- NA
  
  overlap <- as.factor(overlap)
  
  levels(overlap) <- data.frame(
    ID = c(1,2,3),
    class = c(
      "P. alchata",
      "P. orientalis",
      "Overlap"
    )
  )
  
  p_overlap <- ggplot() +
    
    geom_spatraster(data = overlap) +
    
    scale_fill_manual(
      values = c(
        "P. alchata" = "#E64B35",
        "P. orientalis" = "#4DBBD5",
        "Overlap" = "#7E6148"
      ),
      name = "Species",
      na.value = NA,
      na.translate = FALSE
    ) +
    
    geom_sf(
      data = iberia_mask,
      fill = NA,
      color = "black",
      linewidth = 0.17
    ) +
    
    theme_void(base_size = 14) +
    
    theme(
      plot.background = element_rect(fill="white", color="white"),
      panel.background = element_rect(fill="white", color="white"),
      
      plot.title = element_text(
        face = "bold",
        hjust = 0.56,
        size = 20,
        margin = margin(t = 14, b = 8)
      ),
      
      legend.position = "right",
      
      plot.margin = margin(
        t = 10,
        r = 35,
        b = 10,
        l = 10
      )
    ) +
    
    labs(
      title = "Overlap of core suitable areas"
    )
  
  ggsave(
    file.path(out_dir,
              paste0("Fig4_overlap_",met,".png")),
    p_overlap,
    width = 7.4,
    height = 5,
    dpi = 300,
    bg = "white"
  )
  
  terra::tmpFiles(remove = TRUE)
  
  gc()
}

cat("\nFINAL RESULTS READY\n")