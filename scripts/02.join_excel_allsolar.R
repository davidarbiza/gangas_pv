# Join pv_spain.gpkg (obtained from Global PV Excell) and allsolar_cut.shp (Polygon PV Shapefile)
library(sf)
library(dplyr)
library(mapview)

# Load data
pv_spain_excell <- st_read("data/pv_spain.gpkg", quiet = TRUE)   
allsolar <- st_read("data/allsolar_cut_diss.shp", quiet = TRUE)    

# Assure same CRS
allsolar <- st_transform(allsolar, 4326)
pv_spain_excell <- st_transform(pv_spain_excell, 4326)

# Filter only operating plants
pv_spain_excell <- pv_spain_excell %>% filter(Status == "operating")

# Create IDs
pv_spain_excell <- pv_spain_excell %>% mutate(point_id = row_number())
allsolar <- allsolar %>% mutate(poly_id = row_number())

# Repair invalid geometries
allsolar <- st_make_valid(allsolar)
allsolar <- st_buffer(allsolar, 0)

# Transform to metric CRS
pv_spain_excell_3857 <- st_transform(pv_spain_excell, 3857)
allsolar_3857 <- st_transform(allsolar, 3857)

# Buffer with fixed distance
buffer_radius <- 500
pv_buffers <- st_buffer(pv_spain_excell_3857, buffer_radius)

# Spatial join: assign polygons to buffer areas
allsolar_buffer_join <- st_join(
  allsolar_3857,
  pv_buffers %>% select(point_id, Project.Name, Start.year, Capacity..MW.),
  join = st_intersects,
  left = FALSE
)

# Back to EPSG:4326
allsolar_buffer_join <- st_transform(allsolar_buffer_join, 4326)

# QC: how many got assigned
nrow(allsolar_buffer_join)
length(unique(allsolar_buffer_join$point_id))

# Map visualization
mapview(allsolar_buffer_join, zcol = "Capacity..MW.") +
  mapview(pv_spain_excell, col.region = "red", cex = 3, alpha = 0.6, legend = FALSE)

# Save results
st_write(allsolar_buffer_join, "data/allsolar_assigned_buffer.gpkg", delete_layer = TRUE)
