
# ======= VECTORIZED SCRIPT =======

library(terra)
library(sf)
library(dplyr)
library(lubridate)

# ---- Paths ----
base_dir <- "E:/TFM_gangas"
gps_dir  <- file.path(base_dir, "GPS")
processed_dir <- file.path(gps_dir, "Processed")
dir.create(processed_dir, showWarnings = FALSE, recursive = TRUE)

# ---- List all csvs ----
csv_files <- list.files(gps_dir, pattern="^filtered_prep.*\\.csv$", full.names = TRUE)

# ---- Helper: raster safe loader ----
raster_safe_crop <- function(path, ref_ext = NULL) {
  if(!file.exists(path)) return(NULL)
  r <- rast(path)
  if(is.na(crs(r))) crs(r) <- "EPSG:25830"
  if(!is.null(ref_ext)) {
    r <- tryCatch(crop(r, ref_ext), error = function(e) r)
  }
  return(r)
}

# ---- Loop over files ----
for(pts_file in csv_files){
  original_basename <- tools::file_path_sans_ext(basename(pts_file))
  out_file <- file.path(processed_dir, paste0(original_basename,"_processed.csv"))
  if(file.exists(out_file)){
    cat("Skipping already processed file:", pts_file, "\n")
    next
  }
  
  cat("Processing file:", pts_file, "\n")
  
  # ---- Read GPS points ----
  selected_cols <- c(
    "device_id","birdID","species","date","time_gmt0",
    "X_25830","Y_25830","outliers","hdop",
    "tipo","sex","age","region","logger_type"
  )
  gps_raw <- read.csv(pts_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  cols_exist <- intersect(selected_cols, colnames(gps_raw))
  gps <- gps_raw[, cols_exist, drop = FALSE]
  if(!all(c("X_25830","Y_25830") %in% colnames(gps))) stop("X_25830 / Y_25830 not found in CSV")
  
  gps_sf <- st_as_sf(gps, coords = c("X_25830","Y_25830"), crs = 25830, remove = FALSE)
  gps_vect <- vect(gps_sf)
  gps_dates <- as.Date(gps$date)
  gps_years <- year(gps_dates)
  gps_ext <- st_bbox(gps_sf)
  gps_ext_terra <- ext(c(gps_ext$xmin-5000, gps_ext$xmax+5000, gps_ext$ymin-5000, gps_ext$ymax+5000))
  
  # ---- Topography ----
  topo_files <- list(
    DEM      = file.path(base_dir,"Topograficas","Spain_DEM_reproject.tif"),
    Slope    = file.path(base_dir,"Topograficas","Slope_map_Spain.tif"),
    Aspect   = file.path(base_dir,"Topograficas","Orientation_map_Spain.tif"),
    AltRange = file.path(base_dir,"Topograficas","altitudinal_range_Spain.tif")
  )
  topo_list <- lapply(topo_files, raster_safe_crop, ref_ext=gps_ext_terra)
  gps$Altitude <- terra::extract(topo_list$DEM, gps_vect)[,2]
  gps$Slope    <- terra::extract(topo_list$Slope, gps_vect)[,2]
  gps$Aspect   <- terra::extract(topo_list$Aspect, gps_vect)[,2]
  gps$AltRange <- terra::extract(topo_list$AltRange, gps_vect)[,2]
  rm(topo_list); gc()
  
  # ---- LULUCF ----
  lulucf_files <- list(
    "2015" = file.path(base_dir,"LULUCF","LULUCF2015_PB.tif"),
    "2018" = file.path(base_dir,"LULUCF","LULUCF2018_PB.tif"),
    "2021" = file.path(base_dir,"LULUCF","LULUCF2021_PB.tif")
  )
  lulucf_rasters <- lapply(lulucf_files, raster_safe_crop, ref_ext=gps_ext_terra)
  years_available <- as.numeric(names(lulucf_rasters))
  get_lulucf_value <- function(pt_year, pt_vect) {
    nearest_year <- years_available[which.min(abs(years_available - pt_year))]
    r <- lulucf_rasters[[as.character(nearest_year)]]
    terra::extract(r, pt_vect)[,2]
  }
  gps$LULUCF <- mapply(get_lulucf_value, gps_years, split(gps_vect, 1:nrow(gps)))
  
  # ---- Heterogeneity ----
  het_file <- file.path(base_dir,"Heterogeneidad","shannon_01_05_1km_Spain_25830.tif")
  het_r <- raster_safe_crop(het_file, gps_ext_terra)
  gps$Heterogeneity <- if(!is.null(het_r)) terra::extract(het_r, gps_vect)[,2]/10000 else NA
  rm(het_r); gc()
  
  # ---- NDVI ----
  ndvi_dir <- file.path(base_dir,"NDVI","SpainReprojected")
  ndvi_files <- list.files(ndvi_dir, pattern="\\.tif$", full.names = TRUE)
  extract_ndvi_date <- function(fn) as.Date(regmatches(fn, regexpr("[0-9]{8}", fn)), "%Y%m%d")
  ndvi_dates <- sapply(ndvi_files, extract_ndvi_date)
  ndvi_rasters <- lapply(ndvi_files, raster_safe_crop, ref_ext=gps_ext_terra)
  
  gps_ndvi <- numeric(nrow(gps))
  for(i in seq_len(nrow(gps))){
    d <- gps_dates[i]
    before_idx <- max(which(ndvi_dates <= d))
    after_idx  <- min(which(ndvi_dates >= d))
    if(before_idx == after_idx){
      gps_ndvi[i] <- terra::extract(ndvi_rasters[[before_idx]], gps_vect[i,])[,2]
    } else {
      v_before <- terra::extract(ndvi_rasters[[before_idx]], gps_vect[i,])[,2]
      v_after  <- terra::extract(ndvi_rasters[[after_idx]], gps_vect[i,])[,2]
      w <- as.numeric(d - ndvi_dates[before_idx]) / as.numeric(ndvi_dates[after_idx] - ndvi_dates[before_idx])
      gps_ndvi[i] <- v_before + (v_after - v_before) * w
    }
  }
  gps$NDVI <- gps_ndvi
  rm(ndvi_rasters); gc()
  
  # ---- Population ----
  pop_files <- list(
    "2015" = file.path(base_dir,"DensidadPoblacion","GHS_POP_2015_25830.tif"),
    "2020" = file.path(base_dir,"DensidadPoblacion","GHS_POP_2020_25830.tif"),
    "2025" = file.path(base_dir,"DensidadPoblacion","GHS_POP_2025_25830.tif")
  )
  pop_rasters <- lapply(pop_files, raster_safe_crop, ref_ext=gps_ext_terra)
  pop_years_available <- as.numeric(names(pop_rasters))
  pop_mat <- sapply(pop_rasters, function(r) terra::extract(r, gps_vect)[,2])
  colnames(pop_mat) <- names(pop_rasters)
  gps_pop <- numeric(nrow(gps))
  for(i in seq_len(nrow(gps))){
    yr <- gps_years[i]
    yr_low <- max(pop_years_available[pop_years_available <= yr])
    yr_high <- min(pop_years_available[pop_years_available >= yr])
    val_low <- pop_mat[i, as.character(yr_low)]
    if(yr_low == yr_high) gps_pop[i] <- val_low else {
      val_high <- pop_mat[i, as.character(yr_high)]
      w <- (yr - yr_low)/(yr_high - yr_low)
      gps_pop[i] <- val_low + (val_high - val_low)*w
    }
  }
  gps$Population <- gps_pop
  rm(pop_rasters, pop_mat); gc()
  
  # ---- Human Footprint ----
  hfp_files <- list(
    "2016" = file.path(base_dir,"HumanFootprint","hfp_2016_100m_25830.tif"),
    "2017" = file.path(base_dir,"HumanFootprint","hfp_2017_100m_25830.tif"),
    "2018" = file.path(base_dir,"HumanFootprint","hfp_2018_100m_25830.tif"),
    "2019" = file.path(base_dir,"HumanFootprint","hfp_2019_100m_25830.tif"),
    "2020" = file.path(base_dir,"HumanFootprint","hfp_2020_100m_25830.tif")
  )
  hfp_rasters <- lapply(hfp_files, raster_safe_crop, ref_ext=gps_ext_terra)
  hfp_years <- as.numeric(names(hfp_rasters))
  gps_hfp <- numeric(nrow(gps))
  for(i in seq_len(nrow(gps))){
    yr <- gps_years[i]
    yr_use <- ifelse(yr > max(hfp_years), max(hfp_years), yr)
    val <- terra::extract(hfp_rasters[[as.character(yr_use)]], gps_vect[i,])[,2]
    gps_hfp[i] <- val/1000
  }
  gps$HFP <- gps_hfp
  rm(hfp_rasters); gc()
  
  # ---- Distance to Roads ----
  dist_file <- file.path(base_dir,"DistanciaCarreteras","Distroads_spain_merged.tif")
  dist_rast <- raster_safe_crop(dist_file, gps_ext_terra)
  gps$DistRoad <- terra::extract(dist_rast, gps_vect)[,2]
  
  # ---- Save output CSV ----
  write.csv(gps, out_file, row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)
  cat("DONE. Output saved to:", out_file, "\n")
}

