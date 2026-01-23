
# --- Check metadata of environmental and anthropogenic layers ---

# Libraries
library(terra)
library(sf)
library(openxlsx)

# Base directory
base_dir <- "E:/TFM_gangas"

# Initialize list to store variable info
info_list <- list()

# Topographic rasters
topo_files <- list.files(file.path(base_dir, "Topograficas"), pattern = "\\.tif$", full.names = TRUE)
for (f in topo_files) {
  r <- rast(f)
  info_list[[basename(f)]] <- data.frame(
    category = "Topographic",
    name = basename(f),
    nrow = nrow(r),
    ncol = ncol(r),
    n_layers = NA,
    resolution = paste(res(r), collapse = " x "),
    xmin = xmin(r), xmax = xmax(r),
    ymin = ymin(r), ymax = ymax(r),
    crs = as.character(crs(r)),
    units = terra::units(r),
    min_value = global(r, "min", na.rm = TRUE)[1],
    max_value = global(r, "max", na.rm = TRUE)[1]
  )
  rm(r); gc()
}

# NDVI rasters
ndvi_files <- list.files(file.path(base_dir, "NDVI", "SpainReprojected"), pattern = "\\.tif$", full.names = TRUE)
ndvi_sample <- ndvi_files[1]
r <- rast(ndvi_sample)
info_list[["NDVI_sample"]] <- data.frame(
  category = "NDVI",
  name = basename(ndvi_sample),
  nrow = NA,
  ncol = NA,
  n_layers = nlyr(r),
  resolution = paste(res(r), collapse = " x "),
  xmin = xmin(r), xmax = xmax(r),
  ymin = ymin(r), ymax = ymax(r),
  crs = as.character(crs(r)),
  units = terra::units(r),
  min_value = NA,
  max_value = NA
)
rm(r); gc()

# Human Footprint
hfp_files <- list.files(file.path(base_dir, "HumanFootprint", "tiles_espana_WGS84"), pattern = "\\.tif$", full.names = TRUE)
for (f in hfp_files) {
  r <- rast(f)
  info_list[[basename(f)]] <- data.frame(
    category = "HumanFootprint",
    name = basename(f),
    nrow = NA,
    ncol = NA,
    n_layers = NA,
    resolution = paste(res(r), collapse = " x "),
    xmin = xmin(r), xmax = xmax(r),
    ymin = ymin(r), ymax = ymax(r),
    crs = as.character(crs(r)),
    units = terra::units(r),
    min_value = global(r, "min", na.rm = TRUE)[1],
    max_value = global(r, "max", na.rm = TRUE)[1]
  )
  rm(r); gc()
}

# Population density
pop_files <- list.files(file.path(base_dir, "DensidadPoblacion"), pattern = "\\.tif$", full.names = TRUE)
for (f in pop_files) {
  r <- rast(f)
  info_list[[basename(f)]] <- data.frame(
    category = "PopulationDensity",
    name = basename(f),
    nrow = NA,
    ncol = NA,
    n_layers = NA,
    resolution = paste(res(r), collapse = " x "),
    xmin = xmin(r), xmax = xmax(r),
    ymin = ymin(r), ymax = ymax(r),
    crs = as.character(crs(r)),
    units = terra::units(r),
    min_value = global(r, "min", na.rm = TRUE)[1],
    max_value = global(r, "max", na.rm = TRUE)[1]
  )
  rm(r); gc()
}

# LULUCF
lulucf_path <- file.path(base_dir, "LULUCF", "Serie20240920.gdb")
lulucf_layers <- st_layers(lulucf_path)$name
first_layer <- st_read(lulucf_path, layer = lulucf_layers[1], quiet = TRUE)
info_list[["LULUCF"]] <- data.frame(
  category = "LULUCF",
  name = paste(lulucf_layers, collapse = "; "),
  nrow = NA,
  ncol = NA,
  n_layers = length(lulucf_layers),
  resolution = NA,
  xmin = NA, xmax = NA,
  ymin = NA, ymax = NA,
  crs = st_crs(first_layer)$input,
  units = NA,
  min_value = NA,
  max_value = NA
)
rm(first_layer); gc()

# Heterogeneity (Shannon index)
het_file <- file.path(base_dir, "Heterogeneidad", "shannon_01_05_1km_Spain_25830.tif")
r <- rast(het_file)
info_list[["Heterogeneity"]] <- data.frame(
  category = "Heterogeneity",
  name = basename(het_file),
  nrow = NA,
  ncol = NA,
  n_layers = NA,
  resolution = paste(res(r), collapse = " x "),
  xmin = xmin(r), xmax = xmax(r),
  ymin = ymin(r), ymax = ymax(r),
  crs = as.character(crs(r)),
  units = terra::units(r),
  min_value = global(r, "min", na.rm = TRUE)[1],
  max_value = global(r, "max", na.rm = TRUE)[1]
)
rm(r); gc()

# Distance to roads
road_files <- list.files(file.path(base_dir, "DistanciaCarreteras", "Final"), pattern = "\\.tif$", full.names = TRUE)
for (f in road_files) {
  r <- rast(f)
  info_list[[basename(f)]] <- data.frame(
    category = "DistanceToRoads",
    name = basename(f),
    nrow = nrow(r),
    ncol = ncol(r),
    n_layers = NA,
    resolution = paste(res(r), collapse = " x "),
    xmin = xmin(r), xmax = xmax(r),
    ymin = ymin(r), ymax = ymax(r),
    crs = as.character(crs(r)),
    units = terra::units(r),
    min_value = global(r, "min", na.rm = TRUE)[1],
    max_value = global(r, "max", na.rm = TRUE)[1]
  )
  rm(r); gc()
}

# Standardize all data.frames columns
standardize_df <- function(df) {
  all_cols <- c("category","name","nrow","ncol","n_layers","resolution",
                "xmin","xmax","ymin","ymax","crs","units","min_value","max_value")
  for (col in all_cols) {
    if (!col %in% colnames(df)) df[[col]] <- NA
  }
  df <- df[all_cols]
  return(df)
}
info_list <- lapply(info_list, standardize_df)

# Combine all info and export to Excel
final_table <- do.call(rbind, info_list)
output_path <- file.path(base_dir, "Variable_Check.xlsx")
write.xlsx(final_table, output_path, rowNames = FALSE)
cat("✅ All variable checks completed. Excel saved at:\n", output_path, "\n")





