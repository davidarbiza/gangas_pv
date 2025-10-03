# --- PV SPAIN + OSM SOLAR PLANTS MERGE SCRIPT WITH DISTANCE + INTERSECTS FILTER AND ADDITIONAL COLUMNS ---

library(sf)
library(dplyr)
library(mapview)
library(osmdata)
library(units)

# 1. Read PV Spain CSV and convert to sf
pv_spain <- read.csv("data/pv_spain.csv", stringsAsFactors = FALSE)

# Keep all columns, but rename key columns for consistency
pv_spain <- pv_spain %>% 
  rename(
    `Project name` = Project.Name,
    `Phase name` = Phase.Name,
    `Location accuracy` = Location.accuracy
  )

# Convert to sf object with WGS84 coordinates
pv_sf <- st_as_sf(pv_spain, coords = c("Longitude", "Latitude"), crs = 4326)

# 2. Prepare OSM solar plants layer
solar_polygons <- st_make_valid(spain_solar$osm_polygons)
solar_multipolygons <- st_make_valid(spain_solar$osm_multipolygons)

# Combine polygons and transform to WGS84
solar_plants <- bind_rows(solar_polygons, solar_multipolygons) %>%
  st_transform(4326) %>%
  mutate(
    Project.Name = ifelse(!is.na(name), name, NA),
    Capacity.MW = NA_real_,
    Start.Year = NA_integer_
  )

# 3. Compute nearest distances between PV Spain points and OSM polygons
nearest <- st_nearest_feature(solar_plants, pv_sf)
distances <- st_distance(solar_plants, pv_sf[nearest, ], by_element = TRUE)

# Threshold distance for considering as duplicate
threshold <- set_units(500, "m")

# 4. Intersects check (OSM polygons that contain a PV Spain point)
intersects_matrix <- st_intersects(solar_plants, pv_sf, sparse = FALSE)
inside <- rowSums(intersects_matrix) > 0

# 5. Discard OSM entries if too close or containing a PV Spain point
osm_discarded <- solar_plants[distances <= threshold | inside, ]
osm_unique <- solar_plants[!(distances <= threshold | inside), ]

# 6. Combine PV Spain points with unique OSM polygons
combined_pv <- bind_rows(pv_sf, osm_unique)

# 7. Save outputs as GeoPackage 
st_write(combined_pv, "data/pv_spain_osm_unique.gpkg", delete_layer = TRUE)

# 8. Quick visualization with popups to check names
x11()
mapview(pv_sf, col.regions = "orange", alpha = 0.4, legend = FALSE, popup = "Project name") +
  mapview(osm_discarded, col.regions = "blue", alpha = 0.6, legend = FALSE, popup = "Project.Name")
