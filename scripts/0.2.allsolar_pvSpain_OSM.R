# --- PV SPAIN + OSM SOLAR PLANTS MERGE SCRIPT ---

library(sf)
library(dplyr)
library(mapview)
library(osmdata)

# 1. Read pv_spain.csv and convert to sf
pv_spain <- read.csv("data/pv_spain.csv", stringsAsFactors = FALSE)

# Keep only operating plants
pv_spain_operating <- pv_spain %>% 
  filter(Status == "operating")

# Convert CSV points to sf object with WGS84 coordinates
pv_sf <- st_as_sf(pv_spain, coords = c("Longitude", "Latitude"), crs = 4326)

# 2. Prepare OSM solar plants layer
solar_polygons <- spain_solar$osm_polygons
solar_multipolygons <- spain_solar$osm_multipolygons

# Make geometries valid and combine
solar_plants <- bind_rows(
  st_make_valid(solar_polygons),
  st_make_valid(solar_multipolygons)
)

# Transform OSM layer to WGS84 CRS to match pv_sf
solar_plants <- st_transform(solar_plants, 4326)

# --- 3. Add missing attributes to OSM layer for compatibility ---
# Keep placeholder columns to match pv_spain attributes we care about
# e.g., Name, Capacity, Start year
solar_plants <- solar_plants %>%
  mutate(
    Project.Name = ifelse(!is.na(name), name, NA),
    Capacity.MW = NA_real_,
    Start.Year = NA_integer_
  )

# --- 4. Combine layers complementarily ---
# pv_spain points + OSM polygons
# st_union could dissolve geometries; instead use bind_rows to keep all features
combined_pv <- bind_rows(
  st_make_valid(pv_sf),
  st_make_valid(solar_plants)
)

# --- 5. Quick mapview visualization ---
x11()
mapview(combined_pv, zcol = "Project.Name", col.regions = "orange", cex = 5, alpha = 0.7, legend = TRUE)

# --- 6. Save merged layer ---
st_write(combined_pv, "data/pv_spain_osm_combined.shp", delete_layer = TRUE)
