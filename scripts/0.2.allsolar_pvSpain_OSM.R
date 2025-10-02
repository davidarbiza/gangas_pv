# --- PV SPAIN + OSM SOLAR PLANTS MERGE USING DISTANCE CRITERION ---

library(sf)
library(dplyr)
library(mapview)
library(osmdata)
library(units)

# 1. Read pv_spain.csv and convert to sf
pv_spain <- read.csv("data/pv_spain.csv", stringsAsFactors = FALSE)

# Convert to sf points (WGS84)
pv_sf <- st_as_sf(pv_spain, coords = c("Longitude", "Latitude"), crs = 4326)

# 2. Prepare OSM solar plants layer
solar_polygons <- st_make_valid(spain_solar$osm_polygons)
solar_multipolygons <- st_make_valid(spain_solar$osm_multipolygons)

# Combine polygons and multipolygons
solar_plants <- bind_rows(solar_polygons, solar_multipolygons) %>%
  st_transform(4326) %>%  # Match CRS of pv_sf
  mutate(
    Project.Name = ifelse(!is.na(name), name, NA),
    Capacity.MW = NA_real_,
    Start.Year = NA_integer_
  )

# 3. Compute centroids of OSM polygons 
osm_points <- st_centroid(solar_plants)

# 4. Compute distances to nearest pv_spain point
nearest <- st_nearest_feature(osm_points, pv_sf)
distances <- st_distance(osm_points, pv_sf[nearest, ], by_element = TRUE)

pv_sf <- clean_names(pv_sf)
osm_unique <- clean_names(osm_unique)
combined_pv <- clean_names(combined_pv)
osm_discarded <- clean_names(osm_discarded)


# 5. Set distance threshold for considering duplicates
threshold <- set_units(500, "m")

# 6. Identify unique OSM plants 
osm_unique <- osm_points[distances > threshold, ]

# 7. Identify discarded OSM plants 
osm_discarded <- osm_points[distances <= threshold, ]

#4. Select only relevant columns to avoid duplicate names ---
  pv_sf_sel <- pv_sf %>% select(Project.Name, Capacity..MW., Start.year, geometry)
osm_unique <- osm_unique %>% select(name)

# 8. Combine PV Spain + unique OSM plants
combined_pv <- bind_rows(pv_sf, osm_unique)

# 9. Mapview visualization: combined + discarded
x11()
mapview(combined_pv, col.regions = "orange", cex = 5, alpha = 0.5, legend = FALSE) +
  mapview(osm_discarded, col.regions = "red", cex = 5, alpha = 0.7, legend = FALSE)

# 10. Save 
st_write(combined_pv, "data/pv_spain_osm_unique.shp", delete_layer = TRUE)

