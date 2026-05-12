############################################
# SDM PROJECTION — CV ENSEMBLE (10 DAYS)
############################################

library(terra)
library(sf)
library(stringr)
library(rnaturalearth)
library(randomForest)

rm(list=ls())
gc()

terraOptions(
  memfrac = 0.6,
  tempdir = "C:/temp_terra",
  progress = 1
)

dir.create("C:/temp_terra", showWarnings = FALSE)

base_path  <- "E:/TFM_gangas"
model_path <- "E:/TFM_gangas/GPS/ExtractedV.3/CV_models"

out_dir <- file.path(model_path, "PROJECTIONS_ENSEMBLE_10DAYS_V.2")
dir.create(out_dir, showWarnings = FALSE)

# ---------------- MODELS ----------------
methods <- c("Random","P95","MCP40")

load_models <- function(species, method){
  files <- list.files(
    model_path,
    pattern = paste0("RF_", species, "_", method, "_fold[1-5]\\.rds"),
    full.names = TRUE
  )
  lapply(files, readRDS)
}

methods <- c("Random","P95","MCP40")

models <- list()

for(sp in c("PTS","BBS")){
  for(met in methods){
    models[[paste(sp, met, sep="_")]] <- load_models(sp, met)
  }
}

# ---------------- VARIABLES ----------------
dem  <- rast(file.path(base_path,"Topograficas/300m/Spain_DEM_reproject_300m.tif"))
slope <- rast(file.path(base_path,"Topograficas/300m/Slope_map_Spain_300m.tif"))
hetero <- rast(file.path(base_path,"Heterogeneidad/300m/shannon_01_05_1km_Spain_25830_300m.tif"))
roads <- rast(file.path(base_path,"DistanciaCarreteras/300m/Distroads_spain_merged_300m.tif"))

names(dem)    <- "Altitude"
names(slope)  <- "Slope"
names(hetero) <- "Heterogeneity"
names(roads)  <- "DistRoad"

pop <- rast(file.path(base_path,"DensidadPoblacion/300m/GHS_POP_2020_25830_300m.tif"))
hfp <- rast(file.path(base_path,"HumanFootprint/300m/hfp_2020_100m_25830_300m.tif"))

names(pop) <- "Population"
names(hfp) <- "HFP"

# ---------------- LAND COVER ----------------
lulucf <- rast(file.path(base_path,"UsosSuelo/LULUCF/300m/LULUCF_LC_2021_300m.tif"))
cos    <- rast(file.path(base_path,"UsosSuelo/COS2023/300m/COS2023_LC_300m.tif"))

lc_names <- c(
  "LC_Forest","LC_Vineyards","LC_TreeCrops","LC_RiceFields",
  "LC_Greenhouses","LC_AnnualCrops","LC_TreePasture","LC_ShrubPasture",
  "LC_HerbPasture","LC_WaterBodies","LC_Marshes","LC_Artificial",
  "LC_OtherLand","LC_AgriMosaic"
)

names(lulucf) <- lc_names
names(cos)    <- lc_names

# PORTUGAL
por <- ne_countries(country = "Portugal", scale = "medium", returnclass = "sf")
por <- st_transform(por, 25830)
por <- st_cast(por, "POLYGON")
por$area <- st_area(por)
por <- por[which.max(por$area), ]
por_vect <- vect(por)

lulucf_list <- vector("list", nlyr(lulucf))

for(i in 1:nlyr(lulucf)){
  r_port  <- mask(cos[[i]], por_vect)
  r_final <- cover(r_port, lulucf[[i]])
  lulucf_list[[i]] <- r_final
}

lulucf_final <- rast(lulucf_list)
names(lulucf_final) <- lc_names

# ---------------- CLIMATE ----------------
ndvi_path <- file.path(base_path,"NDVI/SpainReprojected/300m")
ndvi_files <- sort(list.files(ndvi_path, full.names = TRUE))
dates <- as.Date(str_extract(ndvi_files, "\\d{8}"), "%Y%m%d")

tmean_stack   <- rast(list.files(file.path(base_path,"Climaticas/10_days/Tmean/300m"),
                                 pattern="mean_.*\\.tif$", full.names=TRUE))
tmeansd_stack <- rast(list.files(file.path(base_path,"Climaticas/10_days/Tmean/300m"),
                                 pattern="sd_.*\\.tif$", full.names=TRUE))
prcp_stack    <- rast(list.files(file.path(base_path,"Climaticas/10_days/Prcp/300m"),
                                 pattern="\\.tif$", full.names=TRUE))

get_band_indices_all_years <- function(band_i, total_layers){
  n_years <- total_layers / 36
  seq(from = band_i, by = 36, length.out = n_years)
}

# ---------------- LOOP ----------------
for(band_i in c(27)){
  
  cat("\nDEKAD:", band_i, "\n")
  
  ndvi_idx <- which(
    ((as.numeric(format(dates, "%m")) - 1) * 3 +
       ceiling(as.numeric(format(dates, "%d")) / 10)) == band_i
  )
  
  ndvi_m <- mean(rast(ndvi_files[ndvi_idx]), na.rm=TRUE)
  names(ndvi_m) <- "NDVI"
  
  idx <- get_band_indices_all_years(band_i, nlyr(tmean_stack))
  
  tmean_m   <- mean(tmean_stack[[idx]], na.rm=TRUE)
  tmeansd_m <- mean(tmeansd_stack[[idx]], na.rm=TRUE)
  prcp_m    <- mean(prcp_stack[[idx]], na.rm=TRUE)
  
  names(tmean_m)   <- "Tmean"
  names(tmeansd_m) <- "TmeanSD100"
  names(prcp_m)    <- "Prcp"
  
  env_stack <- c(
    dem,
    slope,
    hetero,
    ndvi_m,
    pop,
    hfp,
    roads,
    lulucf_final[[c(
      "LC_Forest",
      "LC_Vineyards",
      "LC_TreeCrops",
      "LC_AnnualCrops",
      "LC_TreePasture",
      "LC_ShrubPasture",
      "LC_HerbPasture",
      "LC_WaterBodies",
      "LC_Marshes",
      "LC_Artificial",
      "LC_OtherLand",
      "LC_AgriMosaic"
    )]],
    prcp_m,
    tmean_m,
    tmeansd_m
  )
  
  for(model_name in names(models)){
    
    parts <- strsplit(model_name, "_")[[1]]
    sp  <- parts[1]
    met <- parts[2]
    
    mean_r <- NULL
    sq_r   <- NULL
    n <- 0
    
    for(i in 1:5){
      
      cat("   ", sp, "- fold", i, "\n")
      
      r <- terra::predict(
        env_stack,
        models[[model_name]][[i]],
        fun = function(model, data, ...) {
          predict(model, newdata = data, type = "prob")[,2]
        },
        filename = tempfile(fileext = ".tif"),
        overwrite = TRUE
      )
      
      if(is.null(mean_r)){
        mean_r <- r
        sq_r   <- r * r
      } else {
        mean_r <- mean_r + r
        sq_r   <- sq_r + (r * r)
      }
      
      n <- n + 1
      
      rm(r)
      gc()
    }
    
    pred_mean <- mean_r / n
    pred_sd   <- sqrt((sq_r / n) - (pred_mean^2))
    
    writeRaster(pred_mean,
                file.path(out_dir, paste0(sp, "_", met, "_dekad_", sprintf("%02d", band_i), "_ENSEMBLE.tif")),
                overwrite=TRUE)
    
    writeRaster(pred_sd,
                file.path(out_dir, paste0(sp, "_", met, "_dekad_", sprintf("%02d", band_i), "_SD.tif")),
                overwrite=TRUE)
  }
  
  gc()
}

cat("\nENSEMBLE PROJECTIONS DONE\n")



############################################
# CALIBRATION ANALYSIS
# Following paper workflow
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

base_path <- "E:/TFM_gangas/GPS/ExtractedV.3/CalibrationVersion"

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

base_path <- "E:/TFM_gangas/GPS/ExtractedV.3/CalibrationVersion"

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

terraOptions(
  memfrac = 0.6,
  tempdir = "C:/temp_terra",
  progress = 1
)

dir.create("C:/temp_terra", showWarnings = FALSE)

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

base_path <- "E:/TFM_gangas/GPS/ExtractedV.3/CalibrationVersion"

projection_path <- "E:/TFM_gangas/GPS/ExtractedV.3/CV_models/PROJECTIONS_ENSEMBLE_10DAYS_V.2"

gam_path <- file.path(base_path, "Calibration_models")

out_dir <- file.path(base_path, "CALIBRATED_PROJECTIONS")
dir.create(out_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# SPECIES AND METHODS
# ------------------------------------------------------------

species_list <- c("PTS", "BBS")
methods <- c("Random", "P95", "MCP40")

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
    # LOAD GAM
    # --------------------------------------------------------
    
    gam_model <- readRDS(
      file.path(
        gam_path,
        paste0("GAM_", sp, "_", met, ".rds")
      )
    )
    
    # --------------------------------------------------------
    # FIND RF MAPS
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
      # LOAD RF MAP
      # ------------------------------------------------------
      
      r <- rast(f)
      
      # IMPORTANT:
      # GAM expects variable name "predicted"
      
      names(r) <- "predicted"
      
      # ------------------------------------------------------
      # APPLY CALIBRATION
      # ------------------------------------------------------
      
      calibrated_r <- terra::predict(
        r,
        gam_model,
        type = "response"
      )
      
      # ------------------------------------------------------
      # OUTPUT NAME
      # ------------------------------------------------------
      
      out_name <- str_replace(
        basename(f),
        "_ENSEMBLE.tif",
        "_CALIBRATED.tif"
      )
      
      # ------------------------------------------------------
      # SAVE
      # ------------------------------------------------------
      
      writeRaster(
        calibrated_r,
        file.path(out_dir, out_name),
        overwrite = TRUE
      )
      
      rm(r, calibrated_r)
      gc()
    }
  }
}

cat("CALIBRATED PROJECTIONS FINISHED\n")



###################################################################
# SDM FINAL RESULTS — ENSEMBLE + SD (10 DAYS) - CALIBRATED
###################################################################

library(terra)
library(ggplot2)
library(tidyterra)
library(dplyr)
library(sf)
library(mapSpain)
library(rnaturalearth)
library(patchwork)

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------
base_path <- "E:/TFM_gangas/GPS/ExtractedV.3/CalibrationVersion/CALIBRATED_PROJECTIONS"
out_dir <- file.path(base_path, "FINAL_RESULTS")
dir.create(out_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# FILES
# ------------------------------------------------------------
methods <- c("Random","P95","MCP40")

files <- list()

for(met in methods){
  
  files[[met]] <- list(
    PTS = sort(list.files(base_path,
                          pattern = paste0("PTS_", met, "_.*CALIBRATED\\.tif$"),
                          full.names = TRUE)),
    
    BBS = sort(list.files(base_path,
                          pattern = paste0("BBS_", met, "_.*CALIBRATED\\.tif$"),
                          full.names = TRUE)),
    
    PTS_sd = sort(list.files("E:/TFM_gangas/GPS/ExtractedV.3/CV_models/PROJECTIONS_ENSEMBLE_10DAYS_V.2",
                             pattern = paste0("PTS_", met, "_.*SD\\.tif$"),
                             full.names = TRUE)),
    
    BBS_sd = sort(list.files("E:/TFM_gangas/GPS/ExtractedV.3/CV_models/PROJECTIONS_ENSEMBLE_10DAYS_V.2",
                             pattern = paste0("BBS_", met, "_.*SD\\.tif$"),
                             full.names = TRUE))
  )
}

# ------------------------------------------------------------
# MASK
# ------------------------------------------------------------
provinces <- esp_get_prov()
provinces <- provinces[!provinces$iso2.prov.name.es %in% 
                         c("Baleares","Las Palmas","Santa Cruz de Tenerife","Ceuta","Melilla"), ]
provinces <- st_transform(provinces, 25830)

mask_spain <- st_union(provinces)

por <- ne_countries(country = "Portugal", scale = "medium", returnclass = "sf")
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
  
  cat("\nProcessing method:", met, "\n")
  
  # ------------------------------------------------------------
  # LOAD STACKS
  # ------------------------------------------------------------
  stack_pts <- mask(crop(rast(files[[met]]$PTS), mask_vect), mask_vect)
  stack_bbs <- mask(crop(rast(files[[met]]$BBS), mask_vect), mask_vect)
  
  stack_pts_sd <- mask(crop(rast(files[[met]]$PTS_sd), mask_vect), mask_vect)
  stack_bbs_sd <- mask(crop(rast(files[[met]]$BBS_sd), mask_vect), mask_vect)
  
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
      geom_sf(data = iberia_mask, fill = NA, color = "black", linewidth = 0.2) +
      scale_fill_viridis_c(
        option = "inferno",
        limits = c(global_min, global_max),
        name = "Suitability"
      ) +
      theme_void() +
      labs(title = title)
  }
  
  # ------------------------------------------------------------
  # 1. TEMPORAL CURVE
  # ------------------------------------------------------------
  get_curve <- function(stack, stack_sd, label){
    data.frame(
      time = 1:nlyr(stack),
      mean = global(stack, mean, na.rm=TRUE)[,1],
      sd   = global(stack_sd, mean, na.rm=TRUE)[,1],
      species = label
    )
  }
  
  df_mean <- rbind(
    get_curve(stack_pts, stack_pts_sd,"P. alchata"),
    get_curve(stack_bbs, stack_bbs_sd,"P. orientalis")
  )
  
  p_mean <- ggplot(df_mean, aes(time, mean, color = species, fill = species)) +
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.25) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 1.5) +
    theme_classic(base_size = 14)
  
  ggsave(file.path(out_dir, paste0("Fig1_mean_suitability_", met, ".png")),
         p_mean, width = 8, height = 5, dpi = 300)
  
  # ------------------------------------------------------------
  # 2. SUITABLE AREA
  # ------------------------------------------------------------
  get_area <- function(stack, stack_sd, label){
    
    area_vec <- c()
    sd_vec   <- c()
    
    for(i in 1:nlyr(stack)){
      
      r <- stack[[i]]
      
      thr <- quantile(values(r), 0.9, na.rm=TRUE)
      
      bin <- r > thr
      
      area_vec[i] <- global(bin, mean, na.rm=TRUE)[,1]
      sd_vec[i]   <- global(stack_sd[[i]], mean, na.rm=TRUE)[,1]
    }
    
    data.frame(
      time = 1:nlyr(stack),
      area = area_vec,
      sd   = sd_vec,
      species = label
    )
  }
  
  df_area <- rbind(
    get_area(stack_pts, stack_pts_sd,"P. alchata"),
    get_area(stack_bbs, stack_bbs_sd,"P. orientalis")
  )
  
  p_area <- ggplot(df_area, aes(time, area, color = species, fill = species)) +
    geom_ribbon(aes(ymin = area - sd, ymax = area + sd), alpha = 0.25) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 1.5) +
    theme_classic(base_size = 14)
  
  ggsave(file.path(out_dir, paste0("Fig2_suitable_area_", met, ".png")),
         p_area, width = 8, height = 5, dpi = 300)
  
  # ------------------------------------------------------------
  # 3. MAP
  # ------------------------------------------------------------
  sep <- 1
  
  p_maps <- (
    plot_map(stack_pts[[sep]], paste("P. alchata — Sep —", met)) +
      plot_map(stack_bbs[[sep]], paste("P. orientalis — Sep —", met))
  )
  
  ggsave(file.path(out_dir, paste0("Fig3_maps_", met, ".png")),
         p_maps, width = 10, height = 5, dpi = 300)
  
  # ------------------------------------------------------------
  # 4. OVERLAP
  # ------------------------------------------------------------
  mean_pts_r <- app(stack_pts, mean, na.rm=TRUE)
  mean_bbs_r <- app(stack_bbs, mean, na.rm=TRUE)
  
  thr_pts <- quantile(values(mean_pts_r), 0.9, na.rm=TRUE)
  thr_bbs <- quantile(values(mean_bbs_r), 0.9, na.rm=TRUE)
  
  hot_pts <- mean_pts_r > thr_pts
  hot_bbs <- mean_bbs_r > thr_bbs
  
  overlap <- hot_pts*1 + hot_bbs*2
  
  overlap[overlap == 0] <- NA
  
  overlap <- as.factor(overlap)
  
  levels(overlap) <- data.frame(
    ID = c(1,2,3),
    class = c("P. alchata","P. orientalis","Overlap")
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
    geom_sf(data = iberia_mask, fill = NA, color = "black", linewidth = 0.17) +
    theme_void() +
    labs(title = paste("Core habitat overlap —", met))
  
  ggsave(file.path(out_dir, paste0("Fig4_overlap_", met, ".png")),
         p_overlap,
         width = 6, height = 5, dpi = 300, bg = "white")
  
}

cat("\nFINAL RESULTS READY\n")



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
rf_path <- "E:/TFM_gangas/GPS/ExtractedV.3/CV_models/PROJECTIONS_ENSEMBLE_10DAYS_V.2"

# Calibrated maps
cal_path <- "E:/TFM_gangas/GPS/ExtractedV.3/CalibrationVersion/CALIBRATED_PROJECTIONS"

# Output
out_dir <- "E:/TFM_gangas/GPS/ExtractedV.3/CalibrationVersion"

# ------------------------------------------------------------
# SPECIES AND METHODS
# ------------------------------------------------------------

species_list <- c("PTS", "BBS")
methods <- c("Random", "P95", "MCP40")

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
    
    rf_vals <- values(rf_rast)
    rf_vals <- rf_vals[!is.na(rf_vals)]
    
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
    
    cal_vals <- values(cal_rast)
    cal_vals <- cal_vals[!is.na(cal_vals)]
    
    # --------------------------------------------------------
    # SUMMARY TABLE
    # --------------------------------------------------------
    
    rf_summary <- data.frame(
      species = sp,
      method = met,
      version = "Original_RF",
      
      mean = mean(rf_vals),
      sd = sd(rf_vals),
      
      q50 = quantile(rf_vals, 0.50),
      q75 = quantile(rf_vals, 0.75),
      q90 = quantile(rf_vals, 0.90),
      q95 = quantile(rf_vals, 0.95),
      
      max = max(rf_vals)
    )
    
    cal_summary <- data.frame(
      species = sp,
      method = met,
      version = "Calibrated_GAM",
      
      mean = mean(cal_vals),
      sd = sd(cal_vals),
      
      q50 = quantile(cal_vals, 0.50),
      q75 = quantile(cal_vals, 0.75),
      q90 = quantile(cal_vals, 0.90),
      q95 = quantile(cal_vals, 0.95),
      
      max = max(cal_vals)
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