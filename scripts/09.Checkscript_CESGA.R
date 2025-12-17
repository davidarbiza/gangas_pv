# ============ TEST SCRIPT FOR JOINING LAYERS TO GPS DATA 1 BY 1 =====================

# === Libraries ======================

library(terra)
library(sf)
library(dplyr)
library(lubridate)

# === Paths =====================

base_dir <- "E:/TFM_gangas"
gps_dir  <- file.path(base_dir, "GPS")
pts_file <- file.path(gps_dir, "filtered_prep_BBS_SAND07.csv")

#Time
start_time <- Sys.time()


# === Read GPS Points ==================

selected_cols <- c(
  "device_id","birdID","species","date","time_gmt0",
  "X_25830","Y_25830","outliers","hdop",
  "tipo","sex","age","region","logger_type"
)

cat("📌 Reading GPS points...\n")
existing_cols <- selected_cols[selected_cols %in% colnames(read.csv(pts_file))]
gps <- (read.csv(pts_file) %>% select(all_of(existing_cols)))[1:100, ]

gps_sf   <- st_as_sf(gps, coords = c("X_25830","Y_25830"), crs = 25830)
gps_vect <- vect(gps_sf)

gps_ext       <- st_bbox(gps_sf)
gps_ext_terra <- terra::ext(c(
  gps_ext$xmin-5000, gps_ext$xmax+5000,
  gps_ext$ymin-5000, gps_ext$ymax+5000
))

gps_dates <- as.Date(gps$date)
gps_years <- unique(year(gps_dates))

# === Safe Raster Loader ======================

raster_safe <- function(raster_path, ref_ext = gps_ext_terra, ref_raster = NULL){
  if(!file.exists(raster_path)) return(NULL)
  r <- rast(raster_path)
  if(is.na(crs(r))) crs(r) <- "EPSG:25830"
  r_crop <- crop(r, ref_ext)
  if(!is.null(ref_raster)) r_crop <- resample(r_crop, ref_raster)
  return(r_crop)
}

# === Topography =====================

topo_files <- list(
  "DEM"      = file.path(base_dir,"Topograficas","Spain_DEM_reproject.tif"),
  "Slope"    = file.path(base_dir,"Topograficas","Slope_map_Spain.tif"),
  "Aspect"   = file.path(base_dir,"Topograficas","Orientation_map_Spain.tif"),
  "AltRange" = file.path(base_dir,"Topograficas","altitudinal_range_Spain.tif")
)

topo_list <- lapply(topo_files, raster_safe)
topo_list <- topo_list[!sapply(topo_list,is.null)]

gps$Altitude <- extract(topo_list$DEM, gps_vect)[,2]
gps$Slope    <- extract(topo_list$Slope, gps_vect)[,2]
gps$Aspect   <- extract(topo_list$Aspect, gps_vect)[,2]
gps$AltRange <- extract(topo_list$AltRange, gps_vect)[,2]

# === Heterogeneity ===============

het_file <- file.path(base_dir,"Heterogeneidad","shannon_01_05_1km_Spain_25830.tif")
het_ras  <- raster_safe(het_file)
gps$Heterogeneity <- if(!is.null(het_ras)) extract(het_ras, gps_vect)[,2]/10000 else NA

# === NDVI — Daily Interpolation ===========

ndvi_dir   <- file.path(base_dir,"NDVI","SpainReprojected")
ndvi_files <- list.files(ndvi_dir, pattern="\\.tif$", full.names=TRUE)

extract_ndvi_date <- function(filename){
  as.Date(regmatches(filename, regexpr("[0-9]{8}", filename)), format="%Y%m%d")
}

ndvi_df <- data.frame(file = ndvi_files, date = sapply(ndvi_files, extract_ndvi_date))

get_ndvi_interpolated <- function(point_vect, gps_date){
  before_df <- ndvi_df[ndvi_df$date <= gps_date, ]
  after_df  <- ndvi_df[ndvi_df$date >= gps_date, ]
  if(nrow(before_df)==0 | nrow(after_df)==0) return(NA)
  
  f_before   <- before_df$file[which.max(before_df$date)]
  f_after    <- after_df$file[which.min(after_df$date)]
  date_before <- max(before_df$date)
  date_after  <- min(after_df$date)
  
  r_before <- crop(rast(f_before), ext(point_vect)+1000)
  r_after  <- crop(rast(f_after ), ext(point_vect)+1000)
  
  v_before <- extract(r_before, point_vect)[,2]
  v_after  <- extract(r_after , point_vect)[,2]
  
  if(date_before == gps_date) return(v_before)
  if(date_after  == gps_date) return(v_after)
  
  w <- as.numeric(gps_date - date_before) / as.numeric(date_after - date_before)
  return(v_before + (v_after - v_before) * w)
}

gps$NDVI <- sapply(1:nrow(gps), function(i){
  get_ndvi_interpolated(gps_vect[i], as.Date(gps$date[i]))
})

# === Population =================

pop_dir <- file.path(base_dir,"DensidadPoblacion")
pop_all <- list.files(pop_dir, pattern="\\.tif$", full.names=TRUE)
pop_years_available <- c(2015,2020,2025)

get_population_raster <- function(point_vect, year){
  year_files <- pop_all[grepl(year, pop_all)]
  point_ext <- ext(point_vect)+1000
  for(f in year_files){
    r <- rast(f)
    if(is.na(crs(r))) crs(r) <- "EPSG:25830"
    r_ext <- ext(r)
    if(r_ext$xmax >= point_ext$xmin && r_ext$xmin <= point_ext$xmax &&
       r_ext$ymax >= point_ext$ymin && r_ext$ymin <= point_ext$ymax) return(r)
  }
  stop("No population raster covers point")
}

extract_population <- function(point_vect, year){
  year_low  <- max(pop_years_available[pop_years_available <= year])
  year_high <- min(pop_years_available[pop_years_available >= year])
  
  r_low  <- crop(get_population_raster(point_vect, year_low), ext(point_vect)+1000)
  val_low <- extract(r_low, point_vect)[,2]
  if(year_low == year_high) return(val_low)
  
  r_high  <- crop(get_population_raster(point_vect, year_high), ext(point_vect)+1000)
  val_high <- extract(r_high, point_vect)[,2]
  
  return(val_low + (val_high - val_low)*(year - year_low)/(year_high - year_low))
}

gps$Population <- sapply(1:nrow(gps), function(i){
  extract_population(gps_vect[i], year(as.Date(gps$date[i])))
})

# === Human Footprint =================

hfp_dir <- file.path(base_dir,"HumanFootprint")
hfp_all <- list.files(hfp_dir, pattern="\\.tif$", full.names=TRUE)

get_hfp_raster <- function(point_vect, year){
  year_use  <- if(year>2020) 2020 else year
  files_y   <- hfp_all[grepl(paste0("_",year_use,"_"), hfp_all)]
  point_ext <- ext(point_vect)+1000
  for(f in files_y){
    r <- rast(f)
    if(is.na(crs(r))) crs(r) <- "EPSG:25830"
    r_ext <- ext(r)
    if(r_ext$xmax >= point_ext$xmin && r_ext$xmin <= point_ext$xmax &&
       r_ext$ymax >= point_ext$ymin && r_ext$ymin <= point_ext$ymax){
      return(crop(r, ext(point_vect)+1000))
    }
  }
  stop("No HFP raster covers point")
}

gps$HFP <- sapply(1:nrow(gps), function(i){
  extract(get_hfp_raster(gps_vect[i], year(as.Date(gps$date[i]))), gps_vect[i])[,2]/1000
})

# === Distance to Roads ===============

dist_dir <- file.path(base_dir,"DistanciaCarreteras","Final")
dist_all <- list.files(dist_dir, pattern="\\.tif$", full.names=TRUE)

get_dist_raster <- function(point_vect){
  point_ext <- ext(point_vect)+1000
  for(f in dist_all){
    r <- rast(f)
    if(is.na(crs(r))) crs(r) <- "EPSG:25830"
    r_ext <- ext(r)
    if(r_ext$xmax >= point_ext$xmin && r_ext$xmin <= point_ext$xmax &&
       r_ext$ymax >= point_ext$ymin && r_ext$ymin <= point_ext$ymax){
      return(crop(r, ext(point_vect)+1000))
    }
  }
  stop("No distance raster covers point")
}

gps$DistRoad <- sapply(1:nrow(gps), function(i){
  extract(get_dist_raster(gps_vect[i]), gps_vect[i])[,2]
})

# === LULUCF ================

lulucf_files <- list(
  "2015" = file.path(base_dir,"LULUCF","LULUCF2015_PB.tif"),
  "2018" = file.path(base_dir,"LULUCF","LULUCF2018_PB.tif"),
  "2021" = file.path(base_dir,"LULUCF","LULUCF2021_PB.tif")
)

lulucf_rasters <- lapply(lulucf_files, rast)
years_available <- as.numeric(names(lulucf_rasters))

get_lulucf_value <- function(point_vect, year){
  nearest_year <- years_available[which.min(abs(years_available - year))]
  r <- lulucf_rasters[[as.character(nearest_year)]]
  r_crop <- crop(r, ext(point_vect)+1000)
  extract(r_crop, point_vect)[,2]
}

gps$LULUCF <- sapply(1:nrow(gps), function(i){
  get_lulucf_value(gps_vect[i], year(as.Date(gps$date[i])))
})

# === Save Final CSV =================

out_file <- file.path(base_dir,"GPS","Processed","test_joined_FINAL.csv")
write.csv(gps, out_file, row.names = FALSE, fileEncoding = "UTF-8")

cat("\n\n🎉 DONE — Final file saved at:\n", out_file, "\n")

# === Print elapsed time in minutes ===
end_time <- Sys.time()
elapsed_min <- as.numeric(difftime(end_time, start_time, units = "mins"))
cat("\n⏱ Total time elapsed (minutes):", round(elapsed_min, 2), "\n")
cat("💾 Approx. memory usage (MB):", sum(gc()[,2])/1024, "\n")










# ============ TEST SCRIPT FOR JOINING LAYERS TO GPS DATA 1 BY 1 — OPTIMIZED ============

# === Libraries ======================
library(terra)
library(sf)
library(dplyr)
library(lubridate)

# === Paths ==========================
base_dir <- "E:/TFM_gangas"
gps_dir  <- file.path(base_dir, "GPS")
pts_file <- file.path(gps_dir, "filtered_prep_PTS_GUE03.csv")

#Time
start_time <- Sys.time()

# === Read GPS Points ==================
selected_cols <- c(
  "device_id","birdID","species","date","time_gmt0",
  "X_25830","Y_25830","outliers","hdop",
  "tipo","sex","age","region","logger_type"
)

cat("📌 Reading GPS points...\n")
existing_cols <- selected_cols[selected_cols %in% colnames(read.csv(pts_file))]
gps <- (read.csv(pts_file) %>% select(all_of(existing_cols)))

gps_sf   <- st_as_sf(gps, coords = c("X_25830","Y_25830"), crs = 25830)
gps_vect <- vect(gps_sf)

gps_ext       <- st_bbox(gps_sf)
gps_ext_terra <- terra::ext(c(
  gps_ext$xmin-5000, gps_ext$xmax+5000,
  gps_ext$ymin-5000, gps_ext$ymax+5000
))
gps_dates <- as.Date(gps$date)
gps_years <- unique(year(gps_dates))

# === Safe Raster Loader ======================
raster_safe <- function(raster_path, ref_ext = gps_ext_terra, ref_raster = NULL){
  if(!file.exists(raster_path)) return(NULL)
  r <- rast(raster_path)
  if(is.na(crs(r))) crs(r) <- "EPSG:25830"
  r_crop <- crop(r, ref_ext)
  if(!is.null(ref_raster)) r_crop <- resample(r_crop, ref_raster)
  return(r_crop)
}

# === Topography ======================
topo_files <- list(
  "DEM"      = file.path(base_dir,"Topograficas","Spain_DEM_reproject.tif"),
  "Slope"    = file.path(base_dir,"Topograficas","Slope_map_Spain.tif"),
  "Aspect"   = file.path(base_dir,"Topograficas","Orientation_map_Spain.tif"),
  "AltRange" = file.path(base_dir,"Topograficas","altitudinal_range_Spain.tif")
)

# Load topography once
topo_list <- lapply(topo_files, raster_safe)
topo_list <- topo_list[!sapply(topo_list,is.null)]

gps$Altitude <- extract(topo_list$DEM, gps_vect)[,2]
gps$Slope    <- extract(topo_list$Slope, gps_vect)[,2]
gps$Aspect   <- extract(topo_list$Aspect, gps_vect)[,2]
gps$AltRange <- extract(topo_list$AltRange, gps_vect)[,2]

# === Heterogeneity =================
het_file <- file.path(base_dir,"Heterogeneidad","shannon_01_05_1km_Spain_25830.tif")
het_ras  <- raster_safe(het_file)
gps$Heterogeneity <- if(!is.null(het_ras)) extract(het_ras, gps_vect)[,2]/10000 else NA

# === NDVI — Daily Interpolation ===========
ndvi_dir   <- file.path(base_dir,"NDVI","SpainReprojected")
ndvi_files <- list.files(ndvi_dir, pattern="\\.tif$", full.names=TRUE)
extract_ndvi_date <- function(filename){
  as.Date(regmatches(filename, regexpr("[0-9]{8}", filename)), format="%Y%m%d")
}
ndvi_df <- data.frame(file = ndvi_files, date = sapply(ndvi_files, extract_ndvi_date))

get_ndvi_interpolated <- function(point_vect, gps_date){
  before_df <- ndvi_df[ndvi_df$date <= gps_date, ]
  after_df  <- ndvi_df[ndvi_df$date >= gps_date, ]
  if(nrow(before_df)==0 | nrow(after_df)==0) return(NA)
  
  f_before   <- before_df$file[which.max(before_df$date)]
  f_after    <- after_df$file[which.min(after_df$date)]
  date_before <- max(before_df$date)
  date_after  <- min(after_df$date)
  
  r_before <- crop(rast(f_before), ext(point_vect)+1000)
  r_after  <- crop(rast(f_after ), ext(point_vect)+1000)
  
  v_before <- extract(r_before, point_vect)[,2]
  v_after  <- extract(r_after , point_vect)[,2]
  
  if(date_before == gps_date) return(v_before)
  if(date_after  == gps_date) return(v_after)
  
  w <- as.numeric(gps_date - date_before) / as.numeric(date_after - date_before)
  return(v_before + (v_after - v_before) * w)
}

gps$NDVI <- sapply(1:nrow(gps), function(i){
  get_ndvi_interpolated(gps_vect[i], as.Date(gps$date[i]))
})

# === Population =================
pop_dir <- file.path(base_dir, "DensidadPoblacion")
pop_all <- list.files(pop_dir, pattern="\\.tif$", full.names=TRUE)
pop_years_available <- c(2015, 2020, 2025)

pop_rasters <- lapply(pop_years_available, function(y){
  files_y <- pop_all[grepl(y, pop_all)]
  rasters_y <- lapply(files_y, function(f){
    r <- rast(f)
    if(is.na(crs(r))) crs(r) <- "EPSG:25830"
    r
  })
  return(rasters_y)
})
names(pop_rasters) <- as.character(pop_years_available)

extract_from_subrasters <- function(point_vect, rasters_list){
  for(r in rasters_list){
    r_ext <- ext(r)
    p_ext <- ext(point_vect)
    if(r_ext$xmin <= p_ext$xmin && r_ext$xmax >= p_ext$xmax &&
       r_ext$ymin <= p_ext$ymin && r_ext$ymax >= p_ext$ymax){
      return(extract(r, point_vect)[,2])
    }
  }
  return(NA)
}

extract_population <- function(point_vect, year_point){
  year_low  <- max(pop_years_available[pop_years_available <= year_point])
  year_high <- min(pop_years_available[pop_years_available >= year_point])
  
  val_low  <- extract_from_subrasters(point_vect, pop_rasters[[as.character(year_low)]])
  if(year_low == year_high) return(val_low)
  val_high <- extract_from_subrasters(point_vect, pop_rasters[[as.character(year_high)]])
  return(val_low + (val_high - val_low)*(year_point - year_low)/(year_high - year_low))
}

gps$Population <- sapply(1:nrow(gps), function(i){
  extract_population(gps_vect[i], year(as.Date(gps$date[i])))
})

# === Human Footprint =================
hfp_all <- list.files(file.path(base_dir,"HumanFootprint"), pattern="\\.tif$", full.names=TRUE)
hfp_rasters <- lapply(hfp_all, rast)

get_hfp_value <- function(point_vect, year){
  year_use  <- if(year>2020) 2020 else year
  candidates <- hfp_rasters[grepl(paste0("_",year_use,"_"), hfp_all)]
  point_ext <- ext(point_vect)+1000
  for(r in candidates){
    r_ext <- ext(r)
    if(r_ext$xmax >= point_ext$xmin && r_ext$xmin <= point_ext$xmax &&
       r_ext$ymax >= point_ext$ymin && r_ext$ymin <= point_ext$ymax){
      return(extract(crop(r, point_ext), point_vect)[,2]/1000)
    }
  }
  return(NA)
}

gps$HFP <- sapply(1:nrow(gps), function(i){
  get_hfp_value(gps_vect[i], year(as.Date(gps$date[i])))
})

# === Distance to Roads ===============
dist_all <- list.files(file.path(base_dir,"DistanciaCarreteras","Final"),
                       pattern="\\.tif$", full.names=TRUE)
dist_rasters <- lapply(dist_all, function(f) {
  r <- rast(f)
  if (is.na(crs(r))) crs(r) <- "EPSG:25830"
  return(r)
})

extract_road_dist <- function(point_vect){
  point_ext <- ext(point_vect)
  for(r in dist_rasters){
    r_ext <- ext(r)
    if(
      r_ext$xmax >= point_ext$xmin &&
      r_ext$xmin <= point_ext$xmax &&
      r_ext$ymax >= point_ext$ymin &&
      r_ext$ymin <= point_ext$ymax
    ){
      return(extract(r, point_vect)[,2])
    }
  }
  return(NA)
}

gps$DistRoad <- sapply(1:nrow(gps), function(i){
  extract_road_dist(gps_vect[i])
})

# === LULUCF =======================
lulucf_files <- list(
  "2015" = file.path(base_dir,"LULUCF","LULUCF2015_PB.tif"),
  "2018" = file.path(base_dir,"LULUCF","LULUCF2018_PB.tif"),
  "2021" = file.path(base_dir,"LULUCF","LULUCF2021_PB.tif")
)
lulucf_rasters <- lapply(lulucf_files, rast)
years_available <- as.numeric(names(lulucf_rasters))

get_lulucf_value <- function(point_vect, year){
  nearest_year <- years_available[which.min(abs(years_available - year))]
  r <- lulucf_rasters[[as.character(nearest_year)]]
  r_crop <- crop(r, ext(point_vect)+1000)
  extract(r_crop, point_vect)[,2]
}

gps$LULUCF <- sapply(1:nrow(gps), function(i){
  get_lulucf_value(gps_vect[i], year(as.Date(gps$date[i])))
})

# === Save Final CSV =================
original_filename <- tools::file_path_sans_ext(basename(pts_file))
out_file <- file.path(base_dir,"GPS","Processed", paste0(original_filename,"_processed2.csv"))
write.csv(gps, out_file, row.names = FALSE, fileEncoding = "UTF-8")
cat("\n\n🎉 DONE — Final file saved at:\n", out_file, "\n")

# === Print elapsed time in minutes ===
end_time <- Sys.time()
elapsed_min <- as.numeric(difftime(end_time, start_time, units = "mins"))
cat("\n⏱ Total time elapsed (minutes):", round(elapsed_min, 2), "\n")
cat("💾 Approx. memory usage (MB):", sum(gc()[,2])/1024, "\n")

