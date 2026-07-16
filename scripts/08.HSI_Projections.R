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

dir.create("E:/temp_terra", showWarnings = FALSE)

terraOptions(
  memfrac = 0.4,
  tempdir = "E:/temp_terra",
  progress = 1
)


base_path  <- "E:/TFM_gangas"
model_path <- "E:/TFM_gangas/GPS/ExtractedV.4/CV_models"

out_dir <- file.path(model_path, "PROJECTIONS_ENSEMBLE_10DAYS")
dir.create(out_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# MODELS
# ------------------------------------------------------------

methods <- c("Random")

load_models <- function(species, method){
  
  files <- sort(list.files(
    model_path,
    pattern = paste0("RF_", species, "_", method, "_fold[1-5]\\.rds"),
    full.names = TRUE
    )
  )
  
  lapply(files, readRDS)
}

models <- list()

for(sp in c("PTS","BBS")){
  for(met in methods){
    models[[paste(sp, met, sep="_")]] <- load_models(sp, met)
  }
}

# ------------------------------------------------------------
# STATIC VARIABLES
# ------------------------------------------------------------

dem <- rast(file.path(base_path,"Topograficas/300m/Spain_DEM_reproject_300m.tif"))
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

# ------------------------------------------------------------
# LAND COVER
# ------------------------------------------------------------

lulucf <- rast(file.path(base_path,"UsosSuelo/LULUCF/300m/LULUCF_LC_2021_300m.tif"))
cos <- rast(file.path(base_path,"UsosSuelo/COS2023/300m/COS2023_LC_300m.tif"))

lc_names <- c(
  "LC_Forest","LC_Vineyards","LC_TreeCrops","LC_RiceFields",
  "LC_Greenhouses","LC_AnnualCrops","LC_TreePasture","LC_ShrubPasture",
  "LC_HerbPasture","LC_WaterBodies","LC_Marshes","LC_Artificial",
  "LC_OtherLand","LC_AgriMosaic"
)

names(lulucf) <- lc_names
names(cos)    <- lc_names

# ------------------------------------------------------------
# PORTUGAL
# ------------------------------------------------------------

por <- ne_countries(country = "Portugal", scale = "medium", returnclass = "sf")
por <- st_transform(por, 25830)
por <- st_cast(por, "POLYGON")
por$area <- st_area(por)
por <- por[which.max(por$area), ]

por_vect <- vect(por)

lulucf_files <- c()

for(i in 1:nlyr(lulucf)){
  
  cat("Processing LULUCF layer:", i, "\n")
  
  r_port <- mask(cos[[i]], por_vect)
  
  r_final <- cover(r_port, lulucf[[i]])
  
  out_file <- file.path(
    "E:/temp_terra",
    paste0("lulucf_", i, ".tif")
  )
  
  writeRaster(
    r_final,
    out_file,
    overwrite = TRUE
  )
  
  lulucf_files[i] <- out_file
  
  rm(r_port, r_final)
  
  terra::tmpFiles(remove=TRUE)
  
  gc()
}

lulucf_final <- rast(lulucf_files)

names(lulucf_final) <- lc_names

# ------------------------------------------------------------
# CLIMATE
# ------------------------------------------------------------

ndvi_path <- file.path(base_path,"NDVI/SpainReprojected/300m")

ndvi_files <- sort(list.files(ndvi_path, full.names = TRUE))

dates <- as.Date(
  str_extract(ndvi_files, "\\d{8}"),
  "%Y%m%d"
)

tmean_stack <- rast(
  list.files(
    file.path(base_path,"Climaticas/10_days/Tmean/300m"),
    pattern="Tmean_mean_.*_300m\\.tif$",
    full.names=TRUE
  )
)

tmeansd_stack <- rast(
  list.files(
    file.path(base_path,"Climaticas/10_days/Tmean/300m"),
    pattern="Tmean_sd_.*_300m\\.tif$",
    full.names=TRUE
  )
)

prcp_stack <- rast(
  list.files(
    file.path(base_path,"Climaticas/10_days/Prcp/300m"),
    pattern="^Prcp_sum_.*_300m\\.tif$",
    full.names=TRUE
  )
)

get_band_indices_all_years <- function(band_i, total_layers){
  
  n_years <- total_layers / 36
  
  seq(from = band_i, by = 36, length.out = n_years)
}

# ============================================================
# MAIN LOOP
# ============================================================

for(band_i in 1:36){
  
  cat("\n====================================\n")
  cat("DEKAD:", band_i, "\n")
  cat("====================================\n")
  
  # ------------------------------------------------------------
  # SKIP COMPLETE DEKADS
  # ------------------------------------------------------------
  
  all_done <- TRUE
  
  for(model_name in names(models)){
    
    parts <- strsplit(model_name, "_")[[1]]
    
    sp  <- parts[1]
    met <- parts[2]
    
    out_mean <- file.path(
      out_dir,
      paste0(
        sp, "_", met,
        "_dekad_",
        sprintf("%02d", band_i),
        "_ENSEMBLE.tif"
      )
    )
    
    out_sd <- file.path(
      out_dir,
      paste0(
        sp, "_", met,
        "_dekad_",
        sprintf("%02d", band_i),
        "_SD.tif"
      )
    )
    
    if(!(file.exists(out_mean) & file.exists(out_sd))){
      all_done <- FALSE
    }
  }
  
  if(all_done){
    
    cat("Skipping dekad", band_i, "- already completed\n")
    
    next
  }
  
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
    
    out_mean <- file.path(
      out_dir,
      paste0(sp, "_", met, "_dekad_", sprintf("%02d", band_i), "_ENSEMBLE.tif")
    )
    
    out_sd <- file.path(
      out_dir,
      paste0(sp, "_", met, "_dekad_", sprintf("%02d", band_i), "_SD.tif")
    )
    
    t0 <- Sys.time()
    
    cat("\nPROJECTING:", sp, "|", met, "| dekad", band_i, "\n")
    
    fold_files <- c()
    
    for(i in 1:5){
      
      cat("   Fold", i, "\n")
      
      out_fold <- file.path(
        "E:/temp_terra",
        paste0(
          sp, "_",
          met, "_dekad_",
          sprintf("%02d", band_i),
          "_fold_", i, ".tif"
        )
      )
      
      terra::predict(
        
        env_stack,
        
        models[[model_name]][[i]],
        
        fun = function(model, data, ...) {
          predict(model, newdata = data, type = "prob")[,2]
        },
        
        filename = out_fold,
        overwrite = TRUE
      )
      
      fold_files[i] <- out_fold
      
      gc()
    }
    
    # ------------------------------------------------------------
    # LOAD STACK FROM DISK
    # ------------------------------------------------------------
    
    pred_stack <- rast(fold_files)
    
    # ------------------------------------------------------------
    # ENSEMBLE MEAN
    # ------------------------------------------------------------
    
    pred_mean <- app(
      pred_stack,
      mean,
      filename = file.path(
        "E:/temp_terra",
        paste0(sp, "_", met, "_mean_tmp.tif")
      ),
      overwrite = TRUE
    )
    
    # ------------------------------------------------------------
    # ENSEMBLE SD
    # ------------------------------------------------------------
    
    pred_sd <- app(
      pred_stack,
      sd,
      filename = file.path(
        "E:/temp_terra",
        paste0(sp, "_", met, "_sd_tmp.tif")
      ),
      overwrite = TRUE
    )
    
    # ------------------------------------------------------------
    # SAVE FINAL OUTPUTS
    # ------------------------------------------------------------
    
    writeRaster(
      pred_mean,
      out_mean,
      overwrite = TRUE
    )
    
    writeRaster(
      pred_sd,
      out_sd,
      overwrite = TRUE
    )
    
    # ------------------------------------------------------------
    # CLEAN TEMP FILES
    # ------------------------------------------------------------
    
    file.remove(fold_files)
    
    file.remove(
      file.path(
        "E:/temp_terra",
        paste0(sp, "_", met, "_mean_tmp.tif")
      )
    )
    
    file.remove(
      file.path(
        "E:/temp_terra",
        paste0(sp, "_", met, "_sd_tmp.tif")
      )
    )
    
    elapsed <- round(
      as.numeric(difftime(Sys.time(), t0, units = "secs")),
      1
    )
    
    cat(
      "Finished:",
      sp,
      met,
      "dekad",
      band_i,
      "|",
      elapsed,
      "sec\n"
    )
    
    rm(pred_stack, pred_mean, pred_sd)
    gc()
  }
  
  rm(ndvi_m, tmean_m, tmeansd_m, prcp_m, env_stack)
  gc()
}

cat("\nENSEMBLE PROJECTIONS FINISHED\n")




############################################
# SDM FINAL RESULTS — ENSEMBLE + SD (10 DAYS)
############################################

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
base_path <- "E:/TFM_gangas/GPS/ExtractedV.4/CV_models/PROJECTIONS_ENSEMBLE_10DAYS"
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
                          pattern = paste0("PTS_", met, "_.*ENSEMBLE\\.tif$"),
                          full.names = TRUE)),
    
    BBS = sort(list.files(base_path,
                          pattern = paste0("BBS_", met, "_.*ENSEMBLE\\.tif$"),
                          full.names = TRUE)),
    
    PTS_sd = sort(list.files(base_path,
                             pattern = paste0("PTS_", met, "_.*SD\\.tif$"),
                             full.names = TRUE)),
    
    BBS_sd = sort(list.files(base_path,
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
  # 2. MAP
  # ------------------------------------------------------------
  sep <- 1
  
  p_maps <- (
    plot_map(stack_pts[[sep]], paste("P. alchata — Sep —", met)) +
      plot_map(stack_bbs[[sep]], paste("P. orientalis — Sep —", met))
  )
  
  ggsave(file.path(out_dir, paste0("Fig2_maps_", met, ".png")),
         p_maps, width = 10, height = 5, dpi = 300)
  
  # ------------------------------------------------------------
  # 3. OVERLAP
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
    
    ggsave(file.path(out_dir, paste0("Fig3_overlap_", met, ".png")),
           p_overlap,
           width = 6, height = 5, dpi = 300, bg = "white")
  
}

cat("\nFINAL RESULTS READY\n")
