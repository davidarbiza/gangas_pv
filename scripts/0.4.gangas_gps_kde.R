
# Kernel Density Estimation and Map of Iberian Bustard GPS Locations

library(sf)
library(dplyr)
library(readr)
library(readxl)
library(mapview)
library(viridis)
library(ggplot2)
library(spatstat.geom)
library(spatstat)
library(raster)

# List all csv files from data/ folder that match the pattern
files <- list.files("data/GPS", pattern = "^filtered_prep_BBS_.*\\.csv$", full.names = TRUE)

# Read and combine all files into one dataframe
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
gps_sf <- st_as_sf(
  gps_data,
  coords = c("X_4326", "Y_4326"),
  crs = 4326
)

# Quick mapview of points
mapview(
  gps_sf, 
  zcol = "birdID",        # color by individual bird
  col.regions = viridis(length(unique(gps_sf$birdID))),
  cex = 5,
  alpha = 0.7,
  legend = FALSE
)

# Transform coordinates to metric CRS (meters)
gps_sf_proj <- st_transform(gps_sf, 3857)

# Extract coordinates
coords <- st_coordinates(gps_sf_proj)

# Define observation window
win <- owin(xrange = range(coords[,1]), yrange = range(coords[,2]))

# Create point pattern object
ppp_obj <- ppp(x = coords[,1], y = coords[,2], window = win)

# Compute KDE (adjust sigma for smoothing)
kde <- density.ppp(ppp_obj, sigma = 5000)

# Convert KDE to raster for mapview
kde_raster <- raster::raster(kde) 
crs(kde_raster) <- st_crs(gps_sf_proj)$proj4string  # assure correct CRS 

# Mapview
x11()
mapview(kde_raster, col.regions = viridis(100), alpha = 0.7, legend = TRUE) +
  mapview(gps_sf_proj, col.region = "red", cex = 2, alpha = 0.6, legend = FALSE)
plot(kde, main = "Kernel Density of Gangas GPS locations")
