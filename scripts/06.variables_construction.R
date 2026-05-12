# --- 1. Merge multiple SRTM tiles ---

library(terra)

# Set terra options
terraOptions(
  tempdir = "E:/TFM_gangas/DEM/temp",  
  memfrac = 0.6,                       
  progress = 1                        
)

dem_files <- list.files("E:/TFM_gangas/Topograficas/Auxiliares", pattern = "\\.tif$", full.names = TRUE)

# Load as SpatRaster
dem_list <- lapply(dem_files, rast)

# Merge all DEM tiles
output_path <- "E:/TFM_gangas/DEM/Spain_DEM_merged.tif"
mosaic_dem <- do.call(mosaic, c(dem_list, list(filename = output_path, overwrite = TRUE)))


gc()


######## Reproject Spain DEM to 25830 #########

library(terra)

# Input
dem_in <- "E:/TFM_gangas/Topograficas/Auxiliares/Spain_DEM_merged.tif"

# Output
dem_out <- "E:/TFM_gangas/Topograficas/Spain_DEM_reproject.tif"

# Load DEM
dem <- rast(dem_in)

# Reproject
dem_reproj <- project(
  dem,
  "EPSG:25830",
  method = "bilinear"
)

# Save
writeRaster(
  dem_reproj,
  dem_out,
  overwrite = TRUE
)

cat("DONE\n")








# --- 2. Generate country-specific year-month table for NDVI download ---
library(dplyr)
library(lubridate)
library(readr)
library(writexl) 

# Portugal files
pt_files <- c(
  "filtered_prep_BBS_2022_pt_1.csv",
  "filtered_prep_BBS_2022_pt_3.csv",
  "filtered_prep_BBS_2022_pt_5.csv",
  "filtered_prep_BBS_2022_pt_6.csv",
  "filtered_prep_BBS_2022_pt_8.csv",
  "filtered_prep_BBS_2021_pt_9.csv",
  "filtered_prep_BBS_2021_pt_10.csv",
  "filtered_prep_BBS_2021_pt_12.csv",
  "filtered_prep_BBS_2021_pt_13.csv",
  "filtered_prep_BBS_2021_pt_14.csv",
  "filtered_prep_BBS_2021_pt_15.csv"
)

# All files in GPS folder
all_files <- list.files("data/GPS", pattern = "^filtered_prep_.*\\.csv$", full.names = FALSE)

# Spain files = all except exclusive Portugal files
es_files <- setdiff(all_files, pt_files)
es_files <- c(es_files, "filtered_prep_BBS_2022_pt_8.csv")  

# Read dates
read_dates <- function(files) {
  lapply(files, function(f) {
    df <- read_csv(file.path("data/GPS", f), col_types = cols(.default = col_guess()))
    cols_keep <- intersect(c("date"), names(df)) # keep only the 'date' column
    df <- df %>% select(all_of(cols_keep))
    if("date" %in% names(df)) df$date <- as.Date(df$date) # ensure Date type
    df
  }) %>% bind_rows()
}

# Read Portugal and Spain data
pt_data <- read_dates(pt_files) %>% mutate(country = "Portugal")
es_data <- read_dates(es_files) %>% mutate(country = "Spain")

# Combine data and extract unique year-month combinations
months_needed <- bind_rows(pt_data, es_data) %>%
  mutate(year = year(date),
         month = month(date)) %>%
  distinct(country, year, month) %>%  
  arrange(country, year, month)

# Save to Excel file
write_xlsx(months_needed, "months_needed_NDVI_by_country.xlsx")


# --- 3. Clip Heterogeneity and NDVI Rasters to Spain  ---

library(terra)
library(sf)

# Define bounding box for Peninsula + Balearic Islands 
peninsula_bbox <- st_bbox(c(xmin = -10, ymin = 35.5, xmax = 5, ymax = 44.5), crs = 4326)
peninsula_poly <- st_as_sfc(peninsula_bbox)
peninsula_vect <- vect(peninsula_poly)

# Load Heterogeneity raster
hetero_path <- "E:/TFM_gangas/Heterogeneidad/shannon_01_05_1km_uint16.tif"
hetero_rast <- rast(hetero_path)

# Crop and mask raster to Spain
hetero_crop <- crop(hetero_rast, peninsula_vect)
hetero_mask <- mask(hetero_crop, peninsula_vect)

# Save cropped raster
output_hetero <- "E:/TFM_gangas/Heterogeneidad/shannon_01_05_1km_Spain.tif"
writeRaster(hetero_mask, output_hetero, overwrite = TRUE)

# Clean memory
rm(hetero_rast, hetero_crop, hetero_mask)
gc()

# Load and process NDVI files
ndvi_dir <- "E:/TFM_gangas/NDVI/Original"
ndvi_files <- list.files(ndvi_dir, pattern = "\\.nc$", full.names = TRUE)
output_ndvi_dir <- "E:/TFM_gangas/NDVI/Spain"

# Create output folder if it doesn't exist
if (!dir.exists(output_ndvi_dir)) dir.create(output_ndvi_dir)

# Loop over NDVI files
for (ndvi_file in ndvi_files) {
  
  out_file <- file.path(output_ndvi_dir, basename(ndvi_file))
  
  # Skip if already processed
  if (file.exists(out_file)) {
    message("⏭️ ", basename(ndvi_file), " already exists. Skipping.")
    next
  }
  
  # Load NDVI raster directly
  ndvi_rast <- rast(ndvi_file)
  
  # Crop and mask to Peninsula
  ndvi_crop <- crop(ndvi_rast, peninsula_vect)
  ndvi_mask <- mask(ndvi_crop, peninsula_vect)
  
  # Save cropped NDVI as NetCDF
  out_file <- file.path(output_ndvi_dir, basename(ndvi_file))
  writeCDF(ndvi_mask, out_file, overwrite = TRUE)
  
  # Clean memory
  rm(ndvi_rast, ndvi_crop, ndvi_mask)
  gc()
  
  # Progress message
  message("✅ ", basename(ndvi_file), " processed.")
}


# --- 4. Human Footprint Iberia Filtering Script ---

library(terra)
library(stringr)

# Base configuration
base_dir <- "E:/TFM_gangas/HumanFootprint"

# Selected tiles 
tiles_es <- c(
  "hfp_2017_100m_4728978.852256801_-836895.6961472929_cog.tif",
  "hfp_2017_100m_5548178.852256801_-836895.6961472929_cog.tif",
  "hfp_2017_100m_5548178.852256801_-17695.696147292852_cog.tif",
  "hfp_2017_100m_4728978.852256801_-17695.696147292852_cog.tif"
)

# Target years
years <- 2016:2020

# Output folder
out_dir <- file.path(base_dir, "tiles_espana_WGS84")
dir.create(out_dir, showWarnings = FALSE)

# Loop for extracting selected tifs
for (yr in years) {
  
  tgz_path <- file.path(base_dir, paste0("HFP-100m-", yr, ".tgz"))
  if (!file.exists(tgz_path)) {
    message("❌ Not found: ", tgz_path)
    next
  }
  
  message("📦 Extracting ", tgz_path)
  
  # Temporary folder for extraction
  tmp_dir <- file.path(tempdir(), paste0("hfp_", yr))
  dir.create(tmp_dir, showWarnings = FALSE)
  
  # List contents inside .tgz
  all_files <- tryCatch(utils::untar(tgz_path, list = TRUE), error = function(e) NULL)
  if (is.null(all_files)) {
    message("⚠️ Could not list contents of ", tgz_path)
    next
  }
  
  # Search for the selected tiles
  for (tile in tiles_es) {
    tile_pattern <- str_replace(tile, "2017", as.character(yr))
    match_path <- grep(tile_pattern, all_files, value = TRUE)
    
    if (length(match_path) == 0) {
      message("⚠️ Tile not found: ", tile_pattern, " in ", yr)
      next
    }
    
    # Extract specific file
    tryCatch(
      utils::untar(tgz_path, files = match_path, exdir = tmp_dir),
      error = function(e) message("❌ Error extracting ", match_path)
    )
    
    tif_path <- file.path(tmp_dir, match_path)
    if (!file.exists(tif_path)) {
      message("⚠️ File not properly extracted: ", tif_path)
      next
    }
    
    message("   ✅ Processing ", basename(tif_path))
    
    # Read and reproject to WGS84
    r <- rast(tif_path)
    r_wgs <- project(r, "EPSG:4326", method = "bilinear")
    
    # Save reprojected raster
    out_file <- file.path(
      out_dir,
      paste0("hfp_", yr, "_", basename(tile_pattern), "_WGS84.tif")
    )
    writeRaster(r_wgs, out_file, overwrite = TRUE)
    
    message("   💾 Saved: ", basename(out_file))
    
    # Memory cleanup
    rm(r, r_wgs); gc()
  }
  
  # Remove temporary folder
  unlink(tmp_dir, recursive = TRUE)
}

cat("\n✅ Process completed. Reprojected files located at:\n", out_dir, "\n")


# --- 5. Reproject variables to same CRS (EPSG:25830) ---

# 1. NDVI
library(terra)
# Paths
ndvi_files <- list.files("E:/TFM_gangas/NDVI/Spain/",
                         pattern="\\.nc$", full.names = TRUE)

out_dir <- "E:/TFM_gangas/NDVI/SpainReprojected"
dir.create(out_dir, showWarnings = FALSE)

# Detect processed TIFFs
processed_files <- list.files(out_dir, pattern="_25830\\.tif$", full.names = TRUE)
processed_basenames <- gsub("_25830$", "", tools::file_path_sans_ext(basename(processed_files)))
input_basenames <- tools::file_path_sans_ext(basename(ndvi_files))
ndvi_to_process <- ndvi_files[ !input_basenames %in% processed_basenames ]

cat("Total .nc:", length(ndvi_files), "\n")
cat("Ya reproyectados:", length(processed_basenames), "\n")
cat("Pendientes:", length(ndvi_to_process), "\n\n")

# PROCESS MISSING FILES
for(f in ndvi_to_process){
  cat("Procesando:", basename(f), "\n")
  
  r <- rast(f)
  
  # KEEP ONLY BAND 1
  r_ndvi <- r[[1]]
  
  # Reproject
  r_25830 <- project(r_ndvi, "EPSG:25830", method="bilinear")
  
  # Name
  out_name <- file.path(
    out_dir,
    paste0(tools::file_path_sans_ext(basename(f)), "_25830.tif")
  )
  
  writeRaster(r_25830, out_name, overwrite = TRUE, datatype = "FLT4S")
  
  rm(r, r_ndvi, r_25830)
  gc()
}


# 2. Population density
pop_files <- list.files("E:/TFM_gangas/DensidadPoblacion/", pattern="*.tif", full.names = TRUE)
dir.create("E:/TFM_gangas/DensidadPoblacion/Reprojected", showWarnings = FALSE)

for(f in pop_files){
  cat("Processing Population:", basename(f), "\n")
  r <- rast(f)
  r_25830 <- project(r, "EPSG:25830", method="bilinear")
  
  out_name <- paste0("E:/TFM_gangas/DensidadPoblacion/Reprojected/", tools::file_path_sans_ext(basename(f)), "_25830.tif")
  writeRaster(r_25830, out_name, overwrite=TRUE)
  
  rm(r, r_25830); gc()
}

# 3. Heterogeneity (Shannon index)
het <- rast("E:/TFM_gangas/Heterogeneidad/shannon_01_05_1km_Spain.tif")
het_25830 <- project(het, "EPSG:25830", method="bilinear")
writeRaster(het_25830, "E:/TFM_gangas/Heterogeneidad/shannon_01_05_1km_Spain_25830.tif", overwrite=TRUE)

# 4. Distance to roads
infra_files <- list.files("E:/TFM_gangas/DistanciaCarreteras/Final/", pattern="*.tif", full.names = TRUE)
dir.create("E:/TFM_gangas/DistanciaCarreteras/Reprojected", showWarnings = FALSE)

for(f in infra_files){
  cat("Processing Roads distance:", basename(f), "\n")
  r <- rast(f)
  r_25830 <- project(r, "EPSG:25830", method="bilinear")
  
  out_name <- paste0("E:/TFM_gangas/DistanciaCarreteras/Reprojected/", tools::file_path_sans_ext(basename(f)), "_25830.tif")
  writeRaster(r_25830, out_name, overwrite=TRUE)
  
  rm(r, r_25830); gc()
}
# 5. Human Footprint
hfp_files <- list.files("E:/TFM_gangas/HumanFootprint/", pattern="*.tif", full.names = TRUE)
dir.create("E:/TFM_gangas/HumanFootprint/Reprojected", showWarnings = FALSE)

for(f in hfp_files){
  cat("Processing Human Footprint:", basename(f), "\n")
  r <- rast(f)
  r_25830 <- project(r, "EPSG:25830", method="bilinear")
  
  out_name <- paste0("E:/TFM_gangas/HumanFootprint/Reprojected/", tools::file_path_sans_ext(basename(f)), "_25830.tif")
  writeRaster(r_25830, out_name, overwrite=TRUE)
  
  rm(r, r_25830); gc()
}


# Cleaning environment
rm(ndvi, pop, het, dist_infra, r, r_25830)
gc()







#### -- Check reprojection 25830 was done propertly -- ####
# Original rasters
originals <- c(
  "E:/TFM_gangas/NDVI/Spain/c_gls_NDVI300_201601010000_GLOBE_PROBAV_V1.0.1.nc",
  "E:/TFM_gangas/NDVI/Spain/c_gls_NDVI300_202202010000_GLOBE_OLCI_V2.0.1.nc",
  "E:/TFM_gangas/Heterogeneidad/Auxiliares/shannon_01_05_1km_Spain.tif",
  "E:/TFM_gangas/DistanciaCarreteras/Final/dist_quad1_sub9.tif",
  "E:/TFM_gangas/DensidadPoblacion/GHSoriginal/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R4_C18_WGS84.tif"
)

# Reprojected rasters
reprojected <- c(
  "E:/TFM_gangas/NDVI/SpainReprojected/c_gls_NDVI300_201601010000_GLOBE_PROBAV_V1.0.1_25830.tif",
  "E:/TFM_gangas/NDVI/SpainReprojected/c_gls_NDVI300_202202010000_GLOBE_OLCI_V2.0.1_25830.tif",
  "E:/TFM_gangas/Heterogeneidad/shannon_01_05_1km_Spain_25830.tif",
  "E:/TFM_gangas/DistanciaCarreteras/Reprojected/dist_quad1_sub9_25830.tif",
  "E:/TFM_gangas/DensidadPoblacion/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0_R4_C18_25830.tif"
)

# Seed for reproducibility
set.seed(123)

# Loop through each raster pair
for (i in seq_along(originals)) {
  
  cat("\n---------------------------------\n")
  cat("Checking:", basename(originals[i]), "\n")
  
  # Load rasters
  r_orig <- rast(originals[i])
  r_proj <- rast(reprojected[i])
  
  # CRS
  cat("Original CRS:", crs(r_orig), "\n")
  cat("Reprojected CRS:", crs(r_proj), "\n")
  
  # Extent
  ex_orig <- ext(r_orig)
  ex_proj <- ext(r_proj)
  cat("Original extent: xmin =", ex_orig$xmin, "xmax =", ex_orig$xmax,
      "ymin =", ex_orig$ymin, "ymax =", ex_orig$ymax, "\n")
  cat("Reprojected extent: xmin =", ex_proj$xmin, "xmax =", ex_proj$xmax,
      "ymin =", ex_proj$ymin, "ymax =", ex_proj$ymax, "\n")
  
  # Resolution
  cat("Original resolution:", res(r_orig), "\n")
  cat("Reprojected resolution:", res(r_proj), "\n")
  
  # Basic statistics
  cat("Original - min:", min(values(r_orig), na.rm=TRUE),
      "max:", max(values(r_orig), na.rm=TRUE),
      "mean:", mean(values(r_orig), na.rm=TRUE), "\n")
  cat("Reprojected - min:", min(values(r_proj), na.rm=TRUE),
      "max:", max(values(r_proj), na.rm=TRUE),
      "mean:", mean(values(r_proj), na.rm=TRUE), "\n")
  
  # Clean memory
  rm(r_orig, r_proj); gc()
}

cat("\n✅ Reprojection check completed.\n")



