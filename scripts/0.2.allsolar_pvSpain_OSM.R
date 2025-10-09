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

# 3. Check spatial overlap and proximity
# Transform both layers to metric CRS for distance calculations
pv_proj <- st_transform(pv_sf, 3857)          # metric projection
solar_proj <- st_transform(solar_plants, 3857)

# Define threshold in meters
threshold <- set_units(500, "m")  # 500 meters

# Compute spatial relationships
# TRUE if the OSM polygon contains a PV Spain point
intersects_matrix <- st_intersects(solar_proj, pv_proj, sparse = FALSE)
inside <- rowSums(intersects_matrix) > 0

# TRUE if the OSM polygon is within threshold distance from any PV Spain point
near_matrix <- st_is_within_distance(solar_proj, pv_proj, dist = threshold)
near <- lengths(near_matrix) > 0

# Combine logical conditions to identify duplicates
duplicated_osm <- near | inside

# Split OSM polygons into unique and discarded
osm_discarded <- solar_proj[duplicated_osm, ]
osm_unique <- solar_proj[!duplicated_osm, ]

# 6. Combine PV Spain points with unique OSM polygons
combined_pv <- bind_rows(pv_sf, st_transform(osm_unique, st_crs(pv_sf)))  # back to original CRS

# 7. Save 
combined_pv <- combined_pv %>% 
  dplyr::select(Project.Name, Capacity.MW, Start.Year, Status, 'Phase name', 'Location accuracy', geometry)
st_write(combined_pv, "data/pv_spain_osm_unique.gpkg", delete_layer = TRUE)

# 8. Quick visualization with popups to check names
mapview(pv_sf, col.regions = "orange", alpha = 0.4, legend = FALSE) +
  mapview(osm_discarded, col.regions = "blue", alpha = 0.6, legend = FALSE, popup = "Project.Name")
