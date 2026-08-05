# --- Distance to Roads Subquadrants ---

# Libraries
library(terra)
library(sf)

# Paths
roads_file    <- "E:/TFM_gangas/DistanciaInfra/IberianRoads_25830.gpkg"
output_folder <- "E:/TFM_gangas/DistanciaInfra/Final"
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

# Parameters
quad_size <- 200000   # 200 km main quadrants
sub_size  <- 50000    # 50 km subquadrants 
r_res     <- 100      # 100 m raster resolution
buffer_m  <- 100000   # 100 km buffer around each subquadrant
na_fill   <- 99999    # Distance assigned to NA cells

# Load roads and create base grid
roads <- vect(roads_file)
pen_bbox <- as.polygons(ext(roads), crs = crs(roads)) |> st_as_sf()
grid_tmp <- st_make_grid(pen_bbox, cellsize = quad_size, what = "polygons")
quads <- st_sf(geometry = grid_tmp, quad_id = 1:length(grid_tmp))

# Processing function for subquadrants
process_sub <- function(sub, quad_id, r_res, sub_size, buffer_m, na_fill, output_folder, roads) {
  
  # Convert subquadrant to SpatVector
  sub_v <- vect(sub)
  
  # Add buffer
  sub_v_buff <- buffer(sub_v, width = buffer_m)
  
  # Create raster template
  r_template <- rast(ext(sub_v), res = r_res, crs = crs(roads))
  
  # Crop roads to buffered area
  roads_buff <- crop(roads, ext(sub_v_buff))
  
  # Handle empty / invalid road geometries
  if (is.null(roads_buff) || nrow(roads_buff) == 0 || all(is.na(geom(roads_buff)))) {
    cat(" No valid roads found in buffer — filling with constant distance.\n")
    
    dist_r <- rast(ext(sub_v), res = r_res, crs = crs(roads))
    values(dist_r) <- na_fill
    
  } else {
    # Compute distance normally
    dist_r <- distance(r_template, roads_buff)
    dist_r[is.na(dist_r)] <- na_fill
  }
  
  # Reproject to WGS84
  dist_r_wgs84 <- project(dist_r, "EPSG:4326", method = "near")
  
  # Save raster
  out_file <- file.path(output_folder, paste0("dist_quad", quad_id, "_sub", sub$sub_id, ".tif"))
  writeRaster(dist_r_wgs84, out_file, overwrite = TRUE)
  
  rm(sub_v, sub_v_buff, r_template, dist_r, dist_r_wgs84, roads_buff); gc()
  invisible(NULL)
}

# Resume from last processed subquadrant
existing_files <- list.files(output_folder, pattern = "\\.tif$", full.names = TRUE)

if (length(existing_files) == 0) {
  processed_pairs <- data.frame(quad_id = integer(), sub_id = integer())
} else {
  existing_ids <- gsub(".*dist_quad(\\d+)_sub(\\d+)\\.tif", "\\1_\\2", existing_files)
  processed_pairs <- do.call(rbind, strsplit(existing_ids, "_"))
  processed_pairs <- data.frame(
    quad_id = as.integer(processed_pairs[, 1]),
    sub_id  = as.integer(processed_pairs[, 2])
  )
}

# Main loop over quadrants
for (q in seq(nrow(quads))) {
  cat("\nProcessing quadrant", q, "of", nrow(quads), "...\n")
  
  sub_tmp <- st_make_grid(quads[q, ], cellsize = sub_size, what = "polygons")
  subgrid <- st_sf(geometry = sub_tmp, sub_id = 1:length(sub_tmp))
  
  # Skip already processed subquadrants
  done_subs <- processed_pairs$sub_id[processed_pairs$quad_id == q]
  if (length(done_subs) > 0) {
    subgrid <- subgrid[!subgrid$sub_id %in% done_subs, ]
  }
  
  if (nrow(subgrid) == 0) {
    cat("Quadrant", q, "already complete. Skipping...\n")
    next
  }
  
  cat("Remaining subquadrants:", nrow(subgrid), "\n")
  
  # Sequential processing for stability
  for (i in 1:nrow(subgrid)) {
    cat("Subquadrant", i, "of", nrow(subgrid), "\n")
    process_sub(subgrid[i, ], quads$quad_id[q], r_res, sub_size, buffer_m, na_fill, output_folder, roads)
  }
}

cat("\nAll quadrants processed successfully.\n")

