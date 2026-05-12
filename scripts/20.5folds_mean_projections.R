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
base_path <- "E:/TFM_gangas/GPS/ExtractedV.3/CV_models/PROJECTIONS_ENSEMBLE_10DAYS_V.2"
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
# COMPARE SUITABILITY — ALL METHODS (PTS + BBS)
############################################

library(terra)
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------
# PATH
# ------------------------------------------------------------
base_path <- "E:/TFM_gangas/GPS/ExtractedV.3/CV_models/PROJECTIONS_ENSEMBLE_10DAYS_V.2"

methods <- c("Random","P95","MCP40")
species_list <- c("PTS","BBS")

# ------------------------------------------------------------
# FUNCTION
# ------------------------------------------------------------
get_values <- function(files){
  
  r <- rast(files)
  
  vals <- values(r)
  vals <- vals[!is.na(vals)]
  
  return(vals)
}

############################################
# MAIN LOOP
############################################

for(sp in species_list){
  
  cat("\n====================================\n")
  cat("Species:", sp, "\n")
  cat("====================================\n")
  
  vals_list <- list()
  
  # ------------------------------------------------------------
  # LOAD VALUES
  # ------------------------------------------------------------
  for(met in methods){
    
    cat("Loading:", met, "\n")
    
    files <- list.files(
      base_path,
      pattern = paste0(sp, "_", met, "_.*ENSEMBLE\\.tif$"),
      full.names = TRUE
    )
    
    vals_list[[met]] <- get_values(files)
  }
  
  # ------------------------------------------------------------
  # DATAFRAME
  # ------------------------------------------------------------
  df <- bind_rows(
    lapply(names(vals_list), function(met){
      data.frame(
        value = vals_list[[met]],
        method = met
      )
    })
  )
  
  # ------------------------------------------------------------
  # RANGE
  # ------------------------------------------------------------
  range_table <- df %>%
    group_by(method) %>%
    summarise(
      min = min(value),
      max = max(value),
      mean = mean(value)
    )
  
  cat("\n--- RANGE ---\n")
  print(range_table)
  
  # ------------------------------------------------------------
  # QUANTILES
  # ------------------------------------------------------------
  quant_table <- df %>%
    group_by(method) %>%
    summarise(
      q50 = quantile(value, 0.5),
      q75 = quantile(value, 0.75),
      q90 = quantile(value, 0.9),
      q95 = quantile(value, 0.95),
      q99 = quantile(value, 0.99)
    )
  
  cat("\n--- QUANTILES ---\n")
  print(quant_table)
  
  # ------------------------------------------------------------
  # DENSITY PLOT
  # ------------------------------------------------------------
  p1 <- ggplot(df, aes(x = value, fill = method)) +
    
    geom_density(alpha = 0.4) +
    
    theme_classic(base_size = 14) +
    
    labs(
      title = paste("Suitability distribution —", sp),
      x = "Suitability",
      y = "Density"
    )
  
  print(p1)
  
  # ------------------------------------------------------------
  # HISTOGRAM
  # ------------------------------------------------------------
  p2 <- ggplot(df, aes(x = value, fill = method)) +
    
    geom_histogram(alpha = 0.5, bins = 50, position = "identity") +
    
    theme_classic(base_size = 14) +
    
    labs(
      title = paste("Suitability histogram —", sp),
      x = "Suitability",
      y = "Count"
    )
  
  print(p2)
}

cat("\nDONE\n")






library(sf)
library(ggplot2)
library(dplyr)

# ============================================================
# FILES
# ============================================================

files <- list(
  
  PTS_P95_V2 = "E:/TFM_gangas/GPS/ExtractedV.2/PTS_pseudoabsences_P95_env.csv",
  PTS_P95_V3 = "E:/TFM_gangas/GPS/ExtractedV.3/PTS_pseudoabsences_P95_decay_env.csv",
  
  BBS_P95_V2 = "E:/TFM_gangas/GPS/ExtractedV.2/BBS_pseudoabsences_P95_env.csv",
  BBS_P95_V3 = "E:/TFM_gangas/GPS/ExtractedV.3/BBS_pseudoabsences_P95_decay_env.csv",
  
  PTS_MCP_V2 = "E:/TFM_gangas/GPS/ExtractedV.2/PTS_pseudoabsences_MCP40km_env.csv",
  PTS_MCP_V3 = "E:/TFM_gangas/GPS/ExtractedV.3/PTS_pseudoabsences_MCP40_decay_env.csv",
  
  BBS_MCP_V2 = "E:/TFM_gangas/GPS/ExtractedV.2/BBS_pseudoabsences_MCP40km_env.csv",
  BBS_MCP_V3 = "E:/TFM_gangas/GPS/ExtractedV.3/BBS_pseudoabsences_MCP40_decay_env.csv"
)

# ============================================================
# LOAD
# ============================================================

load_pa <- function(path, label){
  
  df <- read.csv(path)
  
  df <- df %>%
    filter(presence == 0)
  
  sf_obj <- st_as_sf(
    df,
    coords = c("X_25830","Y_25830"),
    crs = 25830
  )
  
  sf_obj$dataset <- label
  
  return(sf_obj)
}

# ============================================================
# PLOT FUNCTION
# ============================================================

plot_compare <- function(v2_name, v3_name, title){
  
  pa_v2 <- load_pa(files[[v2_name]], "V2")
  pa_v3 <- load_pa(files[[v3_name]], "V3")
  
  all_pa <- rbind(pa_v2, pa_v3)
  
  p <- ggplot(all_pa) +
    
    geom_sf(aes(color = dataset),
            alpha = 0.15,
            size = 0.1) +
    
    facet_wrap(~dataset) +
    
    scale_color_manual(values = c(
      "V2" = "#E64B35",
      "V3" = "#4DBBD5"
    )) +
    
    theme_void() +
    
    ggtitle(title)
  
  print(p)
}

# ============================================================
# RUN
# ============================================================

x11()
plot_compare(
  "PTS_P95_V2",
  "PTS_P95_V3",
  "PTS — P95"
)

x11()
plot_compare(
  "BBS_P95_V2",
  "BBS_P95_V3",
  "BBS — P95"
)

x11()
plot_compare(
  "PTS_MCP_V2",
  "PTS_MCP_V3",
  "PTS — MCP40"
)

x11()
plot_compare(
  "BBS_MCP_V2",
  "BBS_MCP_V3",
  "BBS — MCP40"
)



############################################
# CHECK RF INTERNAL PROBABILITIES
# ALL METHODS
############################################

library(randomForest)
library(dplyr)
library(ggplot2)
library(sf)
library(blockCV)

set.seed(723)

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------
base_path <- "E:/TFM_gangas/GPS/ExtractedV.3"

out_dir <- file.path(
  base_path,
  "CV_models",
  "PRUEBAS"
)

dir.create(out_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# METHODS
# ------------------------------------------------------------
methods <- data.frame(
  method = c("Random", "P95", "MCP40"),
  file   = c(
    "PTS_pseudoabsences_Random_env.csv",
    "PTS_pseudoabsences_P95_decay_env.csv",
    "PTS_pseudoabsences_MCP40_decay_env.csv"
  )
)

# ============================================================
# LOOP
# ============================================================

for(m in 1:nrow(methods)){
  
  cat("\n====================================\n")
  cat("METHOD:", methods$method[m], "\n")
  cat("====================================\n")
  
  # ----------------------------------------------------------
  # LOAD
  # ----------------------------------------------------------
  data_pts <- read.csv(
    file.path(base_path, methods$file[m])
  )
  
  # ----------------------------------------------------------
  # PREPARE
  # ----------------------------------------------------------
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
  
  # ----------------------------------------------------------
  # SPATIAL BLOCK
  # ----------------------------------------------------------
  data_sf <- st_as_sf(
    data_model,
    coords = c("X_25830","Y_25830"),
    crs = 25830
  )
  
  sb <- spatialBlock(
    speciesData = data_sf,
    species = "presence",
    k = 5,
    theRange = 30000,
    selection = "random",
    iteration = 100
  )
  
  data_model$fold <- sb$foldID
  
  data_model <- data_model %>%
    dplyr::select(-X_25830,-Y_25830)
  
  # ----------------------------------------------------------
  # FOLD 1
  # ----------------------------------------------------------
  train <- data_model[data_model$fold != 1, ]
  test  <- data_model[data_model$fold == 1, ]
  
  train <- dplyr::select(train, -fold)
  test  <- dplyr::select(test, -fold)
  
  # ----------------------------------------------------------
  # BALANCE
  # ----------------------------------------------------------
  n_pres <- sum(train$presence == 1)
  
  sampsize <- c(
    "0" = n_pres,
    "1" = n_pres
  )
  
  # ----------------------------------------------------------
  # RF
  # ----------------------------------------------------------
  predictors <- setdiff(names(train), "presence")
  
  rf_model <- randomForest(
    presence ~ .,
    data = train,
    ntree = 500,
    mtry = floor(length(predictors)/2),
    nodesize = 1,
    sampsize = sampsize,
    importance = TRUE
  )
  
  # ----------------------------------------------------------
  # PREDICTIONS
  # ----------------------------------------------------------
  pred_train <- predict(
    rf_model,
    train,
    type = "prob"
  )[,2]
  
  pred_test <- predict(
    rf_model,
    test,
    type = "prob"
  )[,2]
  
  # ----------------------------------------------------------
  # SAVE SUMMARY
  # ----------------------------------------------------------
  cat("\nTRAIN SUMMARY\n")
  print(summary(pred_train))
  
  cat("\nTEST SUMMARY\n")
  print(summary(pred_test))
  
  # ----------------------------------------------------------
  # HISTOGRAM TRAIN
  # ----------------------------------------------------------
  png(
    file.path(
      out_dir,
      paste0("TRAIN_hist_", methods$method[m], ".png")
    ),
    width = 1200,
    height = 800,
    res = 150
  )
  
  hist(
    pred_train,
    breaks = 50,
    main = paste("TRAIN probabilities -", methods$method[m]),
    xlab = "Probability",
    col = "grey70"
  )
  
  dev.off()
  
  # ----------------------------------------------------------
  # HISTOGRAM TEST
  # ----------------------------------------------------------
  png(
    file.path(
      out_dir,
      paste0("TEST_hist_", methods$method[m], ".png")
    ),
    width = 1200,
    height = 800,
    res = 150
  )
  
  hist(
    pred_test,
    breaks = 50,
    main = paste("TEST probabilities -", methods$method[m]),
    xlab = "Probability",
    col = "grey70"
  )
  
  dev.off()
  
  # ----------------------------------------------------------
  # SAVE MODEL
  # ----------------------------------------------------------
  saveRDS(
    rf_model,
    file.path(
      out_dir,
      paste0("RF_", methods$method[m], "_fold1_ONLY.rds")
    )
  )
}

cat("\nDONE\n")



############################################
# PROJECT ONLY FOLD 1
# ALL METHODS
############################################

library(terra)
library(randomForest)

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------
base_path  <- "E:/TFM_gangas"

model_dir <- file.path(
  base_path,
  "GPS",
  "ExtractedV.3",
  "CV_models",
  "PRUEBAS"
)

out_dir <- file.path(
  model_dir,
  "PROJ_FOLD1"
)

dir.create(out_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# METHODS
# ------------------------------------------------------------
methods <- c("Random","P95","MCP40")

# ------------------------------------------------------------
# STATIC VARIABLES
# ------------------------------------------------------------
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

# ------------------------------------------------------------
# NDVI
# ------------------------------------------------------------
ndvi <- rast(
  file.path(
    base_path,
    "NDVI/SpainReprojected/300m/c_gls_NDVI300_202009210000_GLOBE_OLCI_V2.0.1_25830_300m.tif"
  )
)

names(ndvi) <- "NDVI"

# ------------------------------------------------------------
# CLIMATE
# ------------------------------------------------------------
tmean <- rast(
  file.path(
    base_path,
    "Climaticas/10_days/Tmean/300m/Tmean_mean_2020_10d_300m.tif"
  )
)[[27]]

tmeansd <- rast(
  file.path(
    base_path,
    "Climaticas/10_days/Tmean/300m/Tmean_sd_2020_10d_300m.tif"
  )
)[[27]]

prcp <- rast(
  file.path(
    base_path,
    "Climaticas/10_days/Prcp/300m/Prcp_sum_2020_10d_300m.tif"
  )
)[[27]]

names(tmean)   <- "Tmean"
names(tmeansd) <- "TmeanSD100"
names(prcp)    <- "Prcp"

# ------------------------------------------------------------
# LAND COVER
# ------------------------------------------------------------
lulucf <- rast(
  file.path(
    base_path,
    "UsosSuelo/LULUCF/300m/LULUCF_LC_2021_300m.tif"
  )
)

names(lulucf) <- c(
  "LC_Forest","LC_Vineyards","LC_TreeCrops","LC_RiceFields",
  "LC_Greenhouses","LC_AnnualCrops","LC_TreePasture",
  "LC_ShrubPasture","LC_HerbPasture","LC_WaterBodies",
  "LC_Marshes","LC_Artificial","LC_OtherLand","LC_AgriMosaic"
)

# ------------------------------------------------------------
# STACK
# ------------------------------------------------------------
env_stack <- c(
  dem,
  slope,
  hetero,
  ndvi,
  pop,
  hfp,
  roads,
  lulucf[[c(
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
  prcp,
  tmean,
  tmeansd
)

# ============================================================
# LOOP
# ============================================================

for(met in methods){
  
  cat("\nPROJECTING:", met, "\n")
  
  model <- readRDS(
    file.path(
      model_dir,
      paste0("RF_", met, "_fold1_ONLY.rds")
    )
  )
  
  pred <- terra::predict(
    env_stack,
    model,
    fun = function(model, data, ...) {
      predict(model, newdata = data, type = "prob")[,2]
    }
  )
  
  writeRaster(
    pred,
    file.path(
      out_dir,
      paste0("PTS_", met, "_FOLD1.tif")
    ),
    overwrite = TRUE
  )
  
  png(
    file.path(
      out_dir,
      paste0("PTS_", met, "_FOLD1.png")
    ),
    width = 1600,
    height = 1200,
    res = 200
  )
  
  plot(
    pred,
    main = paste("PTS -", met, "- ONLY FOLD 1")
  )
  
  dev.off()
}

cat("\nDONE\n")