###### Crop Climatics to Spain ######

library(terra)
library(sf)
dir.create("E:/TFM_gangas/temp", showWarnings = FALSE)
terraOptions(tempdir = "E:/TFM_gangas/temp")

# ------------------------------
# Base directory and variables
# ------------------------------
base_dir <- "E:/TFM_gangas/Climaticas"
variables <- c("Tmin", "Tmax", "Prcp")
years <- 2016:2024

# ------------------------------
# 2. Iberian Peninsula bounding box
# ------------------------------
peninsula_bbox <- st_bbox(c(xmin = -10, ymin = 35.5, xmax = 5, ymax = 44.5), crs = 4326)
peninsula_vect <- vect(st_as_sfc(peninsula_bbox))

# ------------------------------
# 3. Loop over variables and years
# ------------------------------
for (var in variables) {
  
  # Create folder for variable if it doesn't exist
  var_dir <- file.path(base_dir, var)
  dir.create(var_dir, showWarnings = FALSE)
  
  for (yr in years) {
    
    file_name <- paste0("Downscaled", var, yr, "_cogeo.tif")
    input_file <- file.path(base_dir, file_name)
    output_file <- file.path(var_dir, file_name)
    
    # Skip missing files
    if (!file.exists(input_file)) {
      message("⚠️ Missing: ", file_name)
      next
    }
    
    # Skip already processed files
    if (file.exists(output_file)) {
      message("✅ Already done: ", file_name)
      next
    }
    
    message("✂️ Cropping ", file_name)
    
    # Load raster (metadata only)
    r <- rast(input_file)
    
    # Crop to Iberian Peninsula
    r_crop <- crop(r, peninsula_vect)
    
    # Save as simple COG without compression
    writeRaster(
      r_crop,
      output_file,
      overwrite = TRUE,
      filetype = "COG",
      gdal = c("COMPRESS=NONE", "BLOCKSIZE=512")
    )
    
    # Clean memory
    rm(r, r_crop)
    gc()
    
    message("✅ Saved: ", file_name)
  }
}

message("🎉 All rasters cropped successfully!")







###### =============================
# Reproject climatics
###### =============================


library(terra)

# =============================
# 1. GLOBAL SETUP
# =============================

base_dir <- "E:/TFM_gangas/Climaticas"
variables <- c("Tmin", "Tmax", "Prcp")
years <- 2016:2024
target_crs <- "EPSG:25830"

# Optional: force terra to clean temp files more aggressively
terraOptions(progress = 1)

# =============================
# 2. LOOP OVER VARIABLES & YEARS
# =============================

for (var in variables) {
  
  cat("\n=============================\n")
  cat("Processing variable:", var, "\n")
  cat("=============================\n")
  
  var_dir <- file.path(base_dir, var)
  
  for (yr in years) {
    
    input_tif <- file.path(
      var_dir,
      paste0("Downscaled", var, yr, "_cogeo.tif")
    )
    
    output_tif <- file.path(
      var_dir,
      paste0("Downscaled", var, yr, "_25830.tif")
    )
    
    # -----------------------------
    # Skip if output already exists
    # -----------------------------
    if (file.exists(output_tif)) {
      cat("✔ Already exists, skipping:", basename(output_tif), "\n")
      next
    }
    
    # -----------------------------
    # Check input exists
    # -----------------------------
    if (!file.exists(input_tif)) {
      cat("✖ Input file not found:", basename(input_tif), "\n")
      next
    }
    
    cat("\n→ Reprojecting:", basename(input_tif), "\n")
    
    # -----------------------------
    # Load raster
    # -----------------------------
    raster <- rast(input_tif)
    cat("  Layers:", nlyr(raster), "\n")
    
    # -----------------------------
    # Reproject
    # -----------------------------
    start_time <- Sys.time()
    
    project(
      raster,
      target_crs,
      method = "bilinear",
      filename = output_tif,
      overwrite = TRUE,
      wopt = list(datatype = "FLT4S")
    )
    
    end_time <- Sys.time()
    
    cat("  Finished in",
        round(difftime(end_time, start_time, units = "mins"), 2),
        "minutes\n")
    
    # -----------------------------
    # Clean memory
    # -----------------------------
    rm(raster)
    gc()
    
  }
}

cat("\nALL DONE.\n")








###### =============================
# Check script reprojection
###### =============================

library(terra)

# =============================
# 1. LOAD RASTERS
# =============================

orig <- rast("E:/TFM_gangas/Climaticas/Tmin/DownscaledTmin2016_cogeo.tif")
proj <- rast("E:/TFM_gangas/Climaticas/Tmin/DownscaledTmin2016_25830.tif")

# =============================
# 2. BASIC STRUCTURE CHECK
# =============================

cat("Number of layers (original):", nlyr(orig), "\n")
cat("Number of layers (reprojected):", nlyr(proj), "\n")

cat("Original CRS:\n")
print(crs(orig))

cat("Reprojected CRS:\n")
print(crs(proj))

cat("Original resolution:\n")
print(res(orig))

cat("Reprojected resolution:\n")
print(res(proj))

# =============================
# 3. NA CONSISTENCY CHECK
# =============================

cat("\nComputing NA counts per layer (this may take some time)...\n")

na_orig <- global(orig, fun = function(x) sum(is.na(x)))
na_proj <- global(proj, fun = function(x) sum(is.na(x)))

na_orig <- na_orig[,1]
na_proj <- na_proj[,1]

cat("Summary of NA differences (original - reprojected):\n")
print(summary(na_orig - na_proj))

# =============================
# 4. VALUE CONSISTENCY CHECK
# =============================

layers_to_check <- c(1, 100, 200, 300)

for (i in layers_to_check) {
  cat("\nLayer", i, "\n")
  
  cat("Original stats:\n")
  print(global(orig[[i]], c("min","max","mean"), na.rm = TRUE))
  
  cat("Reprojected stats:\n")
  print(global(proj[[i]], c("min","max","mean"), na.rm = TRUE))
}

# =============================
# 5. QUICK VISUAL CHECK
# =============================

par(mfrow = c(1,2))S
plot(orig[[1]], main = "Original raster - layer 1")
plot(proj[[1]], main = "Reprojected raster - layer 1")
par(mfrow = c(1,1))








###### Aggregate 10-day & Monthly Climatic Variables ######

library(terra)
library(lubridate)

# ------------------------------
# 1. Global setup
# ------------------------------
base_dir <- "E:/TFM_gangas/Climaticas"
variables <- c("Tmin", "Tmax", "Prcp")
years <- 2016:2024

# Output folders
out_10d_dir <- file.path(base_dir, "10_days")
out_month_dir <- file.path(base_dir, "Monthly")

for (d in c(out_10d_dir, out_month_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  for (v in variables) {
    vd <- file.path(d, v)
    if (!dir.exists(vd)) dir.create(vd)
  }
}

# Temp dir for Terra
dir.create("E:/TFM_gangas/temp", showWarnings = FALSE)
terraOptions(tempdir = "E:/TFM_gangas/temp", progress = 1)

# ------------------------------
# 2. NDVI-like 10-day function
# ------------------------------
ndvi_period_id <- function(dates) {
  day <- day(dates)
  start_day <- ifelse(day <= 10, "01",
                      ifelse(day <= 20, "11", "21"))
  paste0(year(dates), sprintf("%02d", month(dates)), start_day)
}

# ------------------------------
# 3. Main loop
# ------------------------------
global_start <- Sys.time()

for (var in variables) {
  
  cat("\n========================================\n")
  cat("VARIABLE:", var, "\n")
  cat("========================================\n")
  
  var_dir <- file.path(base_dir, var)
  
  for (yr in years) {
    
    year_start <- Sys.time()
    cat("\n----------------------------------------\n")
    cat("Processing:", var, "-", yr, "\n")
    cat("----------------------------------------\n")
    
    # Use only the reprojected files (_25830.tif)
    in_file <- file.path(var_dir, paste0("Downscaled", var, yr, "_25830.tif"))
    
    if (!file.exists(in_file)) {
      cat("✖ Input not found, skipping year.\n")
      next
    }
    
    # Load raster (entire year)
    r <- rast(in_file)
    n <- nlyr(r)
    dates <- seq(as.Date(paste0(yr, "-01-01")), length.out = n, by = "day")
    
    # ---------- 10-day ----------
    cat("→ 10-day aggregation\n")
    t10_start <- Sys.time()
    groups_10d <- ndvi_period_id(dates)
    
    if (var %in% c("Tmin", "Tmax")) {
      out_mean <- file.path(out_10d_dir, var, paste0(var, "_mean_", yr, "_10d.tif"))
      out_sd   <- file.path(out_10d_dir, var, paste0(var, "_sd_", yr, "_10d.tif"))
      
      if (!file.exists(out_mean)) {
        cat("   · Calculating mean\n")
        tapp(r, groups_10d, mean,
             filename = out_mean, overwrite = TRUE,
             wopt = list(datatype="FLT4S"))
      } else cat("   · Mean already exists\n")
      
      if (!file.exists(out_sd)) {
        cat("   · Calculating SD x100\n")
        tapp(r, groups_10d, function(x) sd(x)*100,
             filename = out_sd, overwrite = TRUE,
             wopt = list(datatype="FLT4S"))
      } else cat("   · SD already exists\n")
      
    } else { # Prcp
      out_sum <- file.path(out_10d_dir, var, paste0(var, "_sum_", yr, "_10d.tif"))
      if (!file.exists(out_sum)) {
        cat("   · Calculating sum\n")
        tapp(r, groups_10d, sum,
             filename = out_sum, overwrite = TRUE,
             wopt = list(datatype="FLT4S"))
      } else cat("   · Sum already exists\n")
    }
    
    cat("✓ 10-day done in", round(difftime(Sys.time(), t10_start, units="mins"),2), "min\n")
    
    # ---------- Monthly ----------
    cat("→ Monthly aggregation\n")
    tm_start <- Sys.time()
    groups_m <- paste0(year(dates), sprintf("%02d", month(dates)))
    
    if (var %in% c("Tmin", "Tmax")) {
      out_mean <- file.path(out_month_dir, var, paste0(var, "_mean_", yr, "_month.tif"))
      out_sd   <- file.path(out_month_dir, var, paste0(var, "_sd_", yr, "_month.tif"))
      
      if (!file.exists(out_mean)) {
        cat("   · Calculating mean\n")
        tapp(r, groups_m, mean,
             filename = out_mean, overwrite = TRUE,
             wopt = list(datatype="FLT4S"))
      } else cat("   · Mean already exists\n")
      
      if (!file.exists(out_sd)) {
        cat("   · Calculating SD x100\n")
        tapp(r, groups_m, function(x) sd(x)*100,
             filename = out_sd, overwrite = TRUE,
             wopt = list(datatype="FLT4S"))
      } else cat("   · SD already exists\n")
      
    } else { # Prcp
      out_sum <- file.path(out_month_dir, var, paste0(var, "_sum_", yr, "_month.tif"))
      if (!file.exists(out_sum)) {
        cat("   · Calculating sum\n")
        tapp(r, groups_m, sum,
             filename = out_sum, overwrite = TRUE,
             wopt = list(datatype="FLT4S"))
      } else cat("   · Sum already exists\n")
    }
    
    cat("✓ Monthly done in", round(difftime(Sys.time(), tm_start, units="mins"),2), "min\n")
    
    # Clean memory
    rm(r)
    gc()
    
    cat("✓ Finished", var, yr, "in", round(difftime(Sys.time(), year_start, units="mins"),2), "min\n")
  }
}

cat("\n🎉 All done!\n")





###### Aggregate 10-day Tmean (from daily Tmin & Tmax) ######

library(terra)
library(lubridate)

# ------------------------------
# 1. Global setup
# ------------------------------
base_dir <- "E:/TFM_gangas/Climaticas"
years <- 2016:2024

# Output folder
out_10d_dir <- file.path(base_dir, "10_days")
tmean_dir <- file.path(out_10d_dir, "Tmean")

if (!dir.exists(tmean_dir)) dir.create(tmean_dir, recursive = TRUE)

# Temp dir
dir.create("E:/TFM_gangas/temp", showWarnings = FALSE)
terraOptions(tempdir = "E:/TFM_gangas/temp", progress = 1)

# ------------------------------
# 2. NDVI-like 10-day function
# ------------------------------
ndvi_period_id <- function(dates) {
  day <- day(dates)
  start_day <- ifelse(day <= 10, "01",
                      ifelse(day <= 20, "11", "21"))
  paste0(year(dates), sprintf("%02d", month(dates)), start_day)
}

# ------------------------------
# 3. Main loop (Tmean only)
# ------------------------------
global_start <- Sys.time()

cat("\n========================================\n")
cat("VARIABLE: Tmean\n")
cat("========================================\n")

for (yr in years) {
  
  year_start <- Sys.time()
  
  cat("\n----------------------------------------\n")
  cat("Processing: Tmean -", yr, "\n")
  cat("----------------------------------------\n")
  
  # Input files (daily rasters already reprojected)
  tmin_file <- file.path(base_dir, "Tmin",
                         paste0("DownscaledTmin", yr, "_25830.tif"))
  
  tmax_file <- file.path(base_dir, "Tmax",
                         paste0("DownscaledTmax", yr, "_25830.tif"))
  
  if (!file.exists(tmin_file) | !file.exists(tmax_file)) {
    cat("✖ Input not found, skipping year.\n")
    next
  }
  
  # Load daily rasters
  rmin <- rast(tmin_file)
  rmax <- rast(tmax_file)
  
  if (nlyr(rmin) != nlyr(rmax)) {
    cat("✖ Different number of layers, skipping.\n")
    next
  }
  
  n <- nlyr(rmin)
  dates <- seq(as.Date(paste0(yr, "-01-01")), length.out = n, by = "day")
  
  # Create daily Tmean
  cat("→ Calculating daily Tmean\n")
  r_tmean <- (rmin + rmax) / 2
  
  # ---------- 10-day aggregation ----------
  cat("→ 10-day aggregation\n")
  t10_start <- Sys.time()
  
  groups_10d <- ndvi_period_id(dates)
  
  out_mean <- file.path(tmean_dir,
                        paste0("Tmean_mean_", yr, "_10d.tif"))
  
  out_sd <- file.path(tmean_dir,
                      paste0("Tmean_sd_", yr, "_10d.tif"))
  
  if (!file.exists(out_mean)) {
    cat("   · Calculating mean\n")
    tapp(r_tmean, groups_10d, mean,
         filename = out_mean,
         overwrite = TRUE,
         wopt = list(datatype="FLT4S"))
  } else cat("   · Mean already exists\n")
  
  if (!file.exists(out_sd)) {
    cat("   · Calculating SD x100\n")
    tapp(r_tmean, groups_10d,
         function(x) sd(x) * 100,
         filename = out_sd,
         overwrite = TRUE,
         wopt = list(datatype="FLT4S"))
  } else cat("   · SD already exists\n")
  
  cat("✓ 10-day done in",
      round(difftime(Sys.time(), t10_start, units="mins"),2),
      "min\n")
  
  # Clean memory
  rm(rmin, rmax, r_tmean)
  gc()
  
  cat("✓ Finished Tmean", yr, "in",
      round(difftime(Sys.time(), year_start, units="mins"),2),
      "min\n")
}

cat("\n🎉 Tmean 10-day aggregation completed!\n")

