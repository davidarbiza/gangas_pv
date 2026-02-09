###### =============================
# Resample and align all rasters to 300m NDVI template
###### =============================

library(terra)
library(fs)
library(tictoc) 

# =============================
# 1. GLOBAL SETUP
# =============================
base_dir <- "E:/TFM_gangas"

# NDVI 300m as reference template
ndvi_template_file <- file.path(base_dir, "NDVI/SpainReprojected",
                                "c_gls_NDVI300_201601010000_GLOBE_PROBAV_V1.0.1_25830.tif")
ndvi_template <- rast(ndvi_template_file)

target_res <- 300  # meters
target_crs <- crs(ndvi_template)

# Terra options to manage temp files
terraOptions(progress = 1, tempdir = file.path(base_dir, "temp"))

# =============================
# 2. GENERAL RESAMPLE FUNCTION
# =============================
resample_to_300m <- function(input_file, output_dir, method = "bilinear") {
  
  if (!file_exists(input_file)) {
    cat("[SKIP] Input file not found:", input_file, "\n")
    return(NULL)
  }
  
  dir_create(output_dir)
  
  output_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(input_file)), "_300m.tif"))
  
  if (file_exists(output_file)) {
    cat("[SKIP] Already processed:", basename(output_file), "\n")
    return(NULL)
  }
  
  cat("\n[START] Processing:", basename(input_file), "\n")
  tic()
  
  # Load raster
  r <- rast(input_file)
  cat("  Bands:", nlyr(r), "\n")
  
  # Resample and align to NDVI template
  r_resampled <- resample(r, ndvi_template, method = method)
  
  # Save raster
  writeRaster(r_resampled, output_file, overwrite = TRUE, datatype = "FLT4S")
  
  toc() 
  cat("[DONE] Saved:", output_file, "\n")
  
  # Clean memory
  rm(r, r_resampled)
  gc()
}

# =============================
# 3. VARIABLE CONFIGURATION
# =============================

# 3a. Climatic variables
climatic_vars <- list(
  Tmin = c("10_days", "Monthly"),
  Tmax = c("10_days", "Monthly"),
  Prcp = c("10_days", "Monthly")
)

# 3b. Other datasets
datasets <- list(
  DensidadPoblacion = "bilinear",
  DistanciaCarreteras = "bilinear",
  HumanFootprint = "bilinear",
  NDVI = "bilinear",
  Topograficas = "bilinear",
  Heterogeneidad = "bilinear",
  UsosSuelo = "near"
)

# =============================
# 4. PROCESS CLIMATIC VARIABLES
# =============================
for (var in names(climatic_vars)) {
  for (period in climatic_vars[[var]]) {
    var_dir <- file.path(base_dir, "Climaticas", period, var)
    files <- dir_ls(var_dir, glob = "*.tif")
    output_dir <- file.path(var_dir, "300m")
    
    cat("\n=== Processing", var, "(", period, ") ===\n")
    for (f in files) {
      resample_to_300m(f, output_dir, method = "bilinear")
    }
  }
}

# =============================
# 5. PROCESS OTHER DATASETS
# =============================
for (dataset in names(datasets)) {
  data_dir <- file.path(base_dir, dataset)
  
  if (dataset == "NDVI") {
    data_dir <- file.path(data_dir, "SpainReprojected")
  }
  
  files <- dir_ls(data_dir, glob = "*.tif")
  output_dir <- file.path(data_dir, "300m")
  
  cat("\n=== Processing", dataset, "dataset ===\n")
  for (f in files) {
    resample_to_300m(f, output_dir, method = datasets[[dataset]])
  }
}

cat("\n✅ ALL DONE: All rasters resampled to 300m and aligned with NDVI template.\n")







###### =============================
# Check-script
###### =============================

library(terra)
library(fs)

# ====================
# 1. GLOBAL SETUP
# ====================
base_dir <- "E:/TFM_gangas"

all_300m_dirs <- dir_ls(base_dir, recurse = TRUE, type = "directory", regexp = "300m$")

rasters_300m <- unlist(lapply(all_300m_dirs, function(d) dir_ls(d, glob = "*.tif")))
cat("Total rasters 300m found:", length(rasters_300m), "\n\n")

# =============================
# 2. LOAD ALL RASTERS METADATA
# =============================
raster_info <- lapply(rasters_300m, function(f) {
  r <- rast(f)
  info <- list(
    file = basename(f),
    crs = crs(r),
    res_x = res(r)[1],
    res_y = res(r)[2],
    ncol = ncol(r),
    nrow = nrow(r),
    xmin = xmin(r),
    xmax = xmax(r),
    ymin = ymin(r),
    ymax = ymax(r),
    bands = nlyr(r)
  )
  rm(r); gc()
  info
})

# Convert to data.frame
raster_df <- do.call(rbind, lapply(raster_info, as.data.frame))

# =======================
# 3. CHECK CONSISTENCY
# =======================
check_cols <- c("res_x","res_y","ncol","nrow","xmin","xmax","ymin","ymax")

# Tolerance
tol <- 1e-6  # ~1 micrometer

ref <- raster_df[1, check_cols]

raster_df$aligned <- apply(raster_df[, check_cols], 1, function(x) all(abs(x - ref) < tol))

# ============
# 4. REPORT
# ============
if(all(raster_df$aligned)){
  cat("✅ ALL 300m rasters perfectly aligned with each other!\n")
} else {
  cat("⚠️ Some rasters are misaligned:\n")
  print(raster_df[!raster_df$aligned, c("file", check_cols)])
}

cat("\nSummary:\n")
cat("Total rasters checked:", nrow(raster_df), "\n")
cat("Example number of bands (first 5):", raster_df$bands[1:5], "\n")

# =============================
# 5. REPORT SIMPLIFIED
# =============================
aligned_count <- sum(raster_df$aligned)
misaligned_count <- nrow(raster_df) - aligned_count

cat("✅ Total rasters checked:", nrow(raster_df), "\n")
cat("✅ Aligned rasters:", aligned_count, "\n")
cat("⚠️ Misaligned rasters:", misaligned_count, "\n")

if(misaligned_count > 0){
  cat("List of misaligned rasters:\n")
  print(raster_df$file[!raster_df$aligned])
}
