# Join pv_spain.gpkg (obteined from Global PV Excell) and allsolar_cut.shp (Polygon PV Shapefile)
library(sf)
library(dplyr)
library(janitor)

# Read PV points (Excel/Global PV data) and PV polygons (allsolar)
pv_spain_excell <- st_read("data/pv_spain.gpkg", quiet = TRUE)
allsolar <- st_read("data/allsolar_cut_diss.shp", quiet = TRUE)

# Assure same CRS
allsolar <- st_transform(allsolar, st_crs(pv_spain_excell))

# Filter only operating PV plants
pv_spain_excell <- pv_spain_excell %>% filter(Status == "operating")

# Create unique IDs
pv_spain_excell <- pv_spain_excell %>% mutate(point_id = row_number())
allsolar <- allsolar %>% mutate(poly_id = row_number())

# Fix invalid geometries
allsolar <- st_make_valid(allsolar)
pv_spain_excell <- st_make_valid(pv_spain_excell)

# Centroids of polygons
poly_centroids <- st_centroid(allsolar)

# Find nearest PV point for each polygon
nearest_idx <- st_nearest_feature(poly_centroids, pv_spain_excell)

# Create assignment table
assign_nearest <- tibble(
  poly_id = allsolar$poly_id,
  nearest_idx = nearest_idx
) %>% mutate(
  point_id = pv_spain_excell$point_id[nearest_idx],
  project_name = pv_spain_excell$Project.Name[nearest_idx],
  start_year = pv_spain_excell$`Start.year`[nearest_idx],
  capacity_mw = pv_spain_excell$`Capacity..MW.`[nearest_idx]
)

# Join assignment to polygons
allsolar_assigned_nearest <- allsolar %>%
  left_join(assign_nearest %>% select(-nearest_idx), by = "poly_id")

# Compute distance from polygon centroid to assigned PV point
allsolar_assigned_nearest <- allsolar_assigned_nearest %>%
  mutate(distance_m = as.numeric(st_distance(
    st_transform(st_centroid(geometry), 3857),
    st_transform(pv_spain_excell[match(point_id, pv_spain_excell$point_id), ], 3857),
    by_element = TRUE
  )))

# QC: summary of distances
summary(allsolar_assigned_nearest$distance_m)
sum(!is.na(allsolar_assigned_nearest$point_id))

# Outliers: polygons further than 5 km from assigned point
outliers <- allsolar_assigned_nearest %>%
  filter(distance_m > 5000)

nrow(outliers)

# Map outliers and PV points
mapview(outliers, zcol = "distance_m") + mapview(pv_spain_excell)

# Save results
st_write(allsolar_assigned_nearest, "data/allsolar_assigned_nearest.gpkg", delete_layer = TRUE)