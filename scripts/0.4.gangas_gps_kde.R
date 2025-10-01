
# Heatmap of Iberian Bustard GPS Locations

library(sf)
library(dplyr)
library(readr)
library(mapview)

# List all csv files from data/ folder that match the pattern
files <- list.files("data/GPS", pattern = "^filtered_prep_BBS_.*\\.csv$", full.names = TRUE)

# 1. Read and combine all files into one dataframe
gps_data <- lapply(files, function(f) {
  df <- read_csv(f, guess_max = 10000, col_types = cols(.default = col_guess()))
  # Keep only relevant columns if they exist
  cols_to_keep <- intersect(c("birdID", "device_id", "X_4326", "Y_4326", "date", "time_gmt0"), names(df))
  df <- df %>% select(all_of(cols_to_keep))
  # Force column types
  if("birdID" %in% cols_to_keep) df$birdID <- as.character(df$birdID)
  if("device_id" %in% cols_to_keep) df$device_id <- as.character(df$device_id)
  if("X_4326" %in% cols_to_keep) df$X_4326 <- as.numeric(df$X_4326)
  if("Y_4326" %in% cols_to_keep) df$Y_4326 <- as.numeric(df$Y_4326)
  if("date" %in% cols_to_keep) df$date <- as.Date(df$date)
  return(df)
}) %>% bind_rows()

# Convert to sf object (assuming lon/lat columns are "location_long" and "location_lat")
gps_sf <- st_as_sf(gps_data, coords = c("X_4326", "Y_4326"), crs = 4326)



# 2. HeatMap
library(viridis)
library(ggplot2)
library(spatstat.geom)
library(spatstat)
library(raster)
library(stars)

# Transform coordinates to metric CRS (meters)
gps_sf_m <- st_transform(gps_sf, 3857)


res_m <- 5000

# 6. Compute bounding box
bbox <- st_bbox(gps_sf_m)

# 7. Create stars grid template
grid <- st_as_stars(st_bbox(gps_sf_m), dx = res_m, dy = res_m)

# 8. Rasterize points to count per cell
grid_counts <- st_rasterize(gps_sf_m, template = grid)
# 9. Make zero cells transparent
grid_counts[grid_counts == 0] <- NA

# 10. Mapview
x11()
mapview(grid_counts,
        col.regions = viridis(100),
        alpha = 0.7,
        legend = TRUE,
        na.color = "transparent")


# 3. Detect points outside Spain to identify birds giving weird locations

library(rnaturalearth)
library(rnaturalearthdata)

# Get Iberia polygon
iberia <- ne_countries(country = c("Spain", "Portugal"), scale = "medium", returnclass = "sf")
iberia <- st_transform(iberia, 4326)  # same CRS as gps_sf

# Check which points are inside Spain
inside <- st_within(gps_sf, iberia, sparse = FALSE)

# Transform to vector
inside_vec <- apply(st_within(gps_sf, iberia, sparse = FALSE), 1, any)

# Filter outside Iberia
gps_outside <- gps_sf[!inside_vec, ]

# BirdIDs with points outside Iberia
if(nrow(gps_outside) > 0){
  cat("Birds with points outside Spain and Portugal:\n")
  print(unique(gps_outside$birdID))
} else {
  cat("No points outside Spain and Portugal detected.\n")
}

# Optional: mapview to see points outside in red
x11()
mapview(gps_outside, col.region = "red", cex = 5, alpha = 0.7, legend = FALSE)
