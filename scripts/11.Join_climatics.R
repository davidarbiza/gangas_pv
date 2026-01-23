# ======= VECTORIZED SCRIPT FOR DAILY CLIMATIC VARIABLES (EPSG:4326) =======

library(terra)
library(sf)
library(dplyr)
library(lubridate)

# ---- Paths ----
base_dir <- "E:/TFM_gangas"
gps_dir  <- file.path(base_dir, "GPS")
processed_dir <- file.path(gps_dir, "Processed")
output_dir <- file.path(gps_dir, "Processed_2")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---- List all processed CSVs ----
csv_files <- list.files(processed_dir, pattern="_processed\\.csv$", full.names = TRUE)
if(length(csv_files)==0) stop("No processed CSVs found in ", processed_dir)

# ---- Climatic rasters paths ----
clim_vars <- list(
  Tmin = file.path(base_dir, "Climaticas", "Tmin"),
  Tmax = file.path(base_dir, "Climaticas", "Tmax"),
  Prcp = file.path(base_dir, "Climaticas", "Prcp")
)

# ---- Helper: load raster ----
raster_safe_load <- function(path){
  if(!file.exists(path)) return(NULL)
  r <- rast(path)
  if(is.na(crs(r))) crs(r) <- "EPSG:4326"
  return(r)
}

# ---- Loop over CSVs ----
for(pts_file in csv_files){
  start_time <- Sys.time()
  
  original_basename <- tools::file_path_sans_ext(basename(pts_file))
  out_file <- file.path(output_dir, paste0(original_basename, "_2.csv"))
  
  if(file.exists(out_file)){
    cat("✅ Already processed (skipping):", original_basename, "\n")
    next
  }
  
  cat("🔹 Processing:", original_basename, "\n")
  
  # ---- Read GPS points ----
  gps <- read.csv(pts_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  gps_dates <- as.Date(gps$date)
  gps_years <- unique(year(gps_dates))
  
  # ---- Convert to SpatVector in EPSG:4326 ----
  gps_sf <- st_as_sf(gps, coords=c("X_25830","Y_25830"), crs=25830, remove=FALSE)
  gps_sf <- st_transform(gps_sf, 4326)
  gps_vect <- vect(gps_sf)
  
  # ---- Extract climatic variables ----
  for(var in names(clim_vars)){
    var_dir <- clim_vars[[var]]
    cat("   ⏳ Extracting", var, "...\n")
    
    # ---- List rasters by year ----
    tifs <- list.files(var_dir, pattern="\\.tif$", full.names = TRUE)
    if(length(tifs)==0) {
      warning("⚠️ No rasters found for variable ", var)
      next
    }
    
    extract_year <- function(fn) as.numeric(regmatches(fn, regexpr("[0-9]{4}", fn)))
    tif_years <- sapply(tifs, extract_year)
    tif_map <- setNames(tifs, tif_years)
    
    # Prepare output column
    gps_var <- numeric(nrow(gps))
    
    # ---- Process per year ----
    for(yr in gps_years){
      idx_year <- which(year(gps_dates) == yr)
      if(!(yr %in% names(tif_map))){
        warning("⚠️ No raster for year ", yr, " for variable ", var, "- values set to NA")
        gps_var[idx_year] <- NA
        next
      }
      
      r <- raster_safe_load(tif_map[[as.character(yr)]])
      if(is.null(r)){
        warning("⚠️ Failed to load raster for ", var, " year ", yr)
        gps_var[idx_year] <- NA
        next
      }
      
      # ---- Extract all bands at once for these points ----
      vals <- terra::extract(r, gps_vect[idx_year, ], ID=FALSE)
      
      # ---- Determine day-of-year for extraction ----
      doy <- as.numeric(format(gps_dates[idx_year], "%j"))
      # Adjust in case raster has fewer bands (leap years)
      doy[doy > ncol(vals)] <- ncol(vals)
      
      # Extract the value for the exact day
      gps_var[idx_year] <- vals[cbind(seq_along(idx_year), doy)]
    }
    
    # ---- Apply scale factor if needed ----
    if(var %in% c("Tmin","Tmax","Prcp")) gps_var <- gps_var / 100
    
    gps[[var]] <- gps_var
    cat("   ✔", var, "extraction done.\n")
    rm(vals, r); gc()
  }
  
  # ---- Save output CSV ----
  write.csv(gps, out_file, row.names = FALSE, fileEncoding = "UTF-8", quote=TRUE)
  
  end_time <- Sys.time()
  elapsed <- round(as.numeric(difftime(end_time, start_time, units="mins")), 2)
  cat("✅ Saved:", out_file, "| Time:", elapsed, "mins\n\n")
}




#### --- Check script --- ####

library(terra)

# 1️⃣ Exact point from the CSV
x <- 650750.029673804
y <- 4304365.84953068
d <- as.Date("2023-08-26")

# 2️⃣ Load the raster for the correct year
r <- rast("E:/TFM_gangas/Climaticas/Tmin/DownscaledTmin2023_cogeo.tif")

# 3️⃣ Create point in EPSG:25830
pt <- vect(matrix(c(x, y), ncol=2), crs="EPSG:25830")

# 4️⃣ Reproject the point to the raster CRS
pt4326 <- project(pt, crs(r))

# 5️⃣ Calculate day of the year
doy <- as.numeric(format(d, "%j"))

# 6️⃣ Extract pixel value
val <- terra::extract(r[[doy]], pt4326)[,2] / 100 

# 7️⃣ Print value
val




