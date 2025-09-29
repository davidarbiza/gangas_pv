# Second attempt: join pv_spain.gpkg (obteined from Global PV Excell) and allsolar_cut.shp (Polygon PV Shapefile)
library(sf)
library(dplyr)
library(mapview)
library(viridis)
pv_spain_excell <- st_read("data/pv_spain.gpkg", quiet = TRUE)   
allsolar <- st_read("data/allsolar_cut_diss.shp", quiet = TRUE)    

# Assure same CRS
allsolar <- st_transform(allsolar, 4326)
pv_spain_excell <- st_transform(pv_spain_excell, 4326)

# Filter only operating plants + Exclude Canarias and Baleares
pv_spain_excell <- pv_spain_excell %>%
  filter(Status == "operating") %>%
  filter(!(
    # Baleares
    (st_coordinates(.)[,2] >= 38.5 & st_coordinates(.)[,2] <= 40.2 &
       st_coordinates(.)[,1] >= 1 & st_coordinates(.)[,1] <= 4.5) |
      # Canarias
      (st_coordinates(.)[,2] >= 27 & st_coordinates(.)[,2] <= 30 &
         st_coordinates(.)[,1] >= -19 & st_coordinates(.)[,1] <= -13)
  ))


# Create id for each obs.
pv_spain_excell <- pv_spain_excell %>% mutate(point_id = row_number())
allsolar <- allsolar %>% mutate(poly_id = row_number())

# Repair invalid geometries
allsolar <- st_make_valid(allsolar)
# Remove duplicates vertices
allsolar <- st_buffer(allsolar, 0)

# Assign nearest PV point
nearest_idx <- st_nearest_feature(st_geometry(allsolar), st_geometry(pv_spain_excell))

allsolar_assigned_nearest <- allsolar %>%
  mutate(
    point_id = pv_spain_excell$point_id[nearest_idx],
    project_name = pv_spain_excell$Project.Name[nearest_idx],
    start_year = pv_spain_excell$`Start.year`[nearest_idx],
    capacity_mw = pv_spain_excell$`Capacity..MW.`[nearest_idx],
    location_accuracy = pv_spain_excell$Location.accuracy[nearest_idx]
  )

# Transform to metric CRS for distance calculation 
allsolar_3857 <- st_transform(allsolar_assigned_nearest, 3857)
pv_points_3857 <- st_transform(pv_spain_excell, 3857)

# Match polygon IDs with point IDs
idx <- match(allsolar_assigned_nearest$point_id, pv_spain_excell$point_id)

# Calculate polygon → point distance in meters
allsolar_assigned_nearest$distance_m <- as.numeric(
  st_distance(st_geometry(allsolar_3857), st_geometry(pv_points_3857)[idx], by_element = TRUE)
)

# Estimate area-based radius for validation (MW → 2–5 ha/MW)
allsolar_assigned_nearest <- allsolar_assigned_nearest %>%
  mutate(
    max_area_ha = capacity_mw * 5,    # 5 ha/MW
    max_allow_m = sqrt((max_area_ha * 10000) / pi)*  # max. radius
      ifelse(location_accuracy == "approximate", 1.15, 1), # +15% if location is "aprproximate"
    assigned_valid = distance_m <= max_allow_m
  )

# QC
summary(allsolar_assigned_nearest$distance_m)
sum(!is.na(allsolar_assigned_nearest$point_id))
outliers <- allsolar_assigned_nearest %>% filter(!assigned_valid)
table(allsolar_assigned_nearest$assigned_valid)

# Mapview
mapview(allsolar_assigned_nearest, zcol = "distance_m") + # visualize all polygons colored by distance 
mapview(pv_spain_excell, col.region = "steelblue", cex = 3, alpha = 0.5, legend = FALSE)

# Mapview grouped by assigned_valid
poligonos_invalidos <- allsolar_assigned_nearest %>% filter(!assigned_valid)
mapview(poligonos_invalidos,zcol = "distance_m",col.regions = viridis(100), legend = TRUE) +
  mapview(pv_spain_excell, col.region = "red", cex = 5, alpha = 0.7, legend = FALSE)

mapview(allsolar_assigned_nearest,zcol = "assigned_valid") +
  mapview(pv_spain_excell, col.region = "red", cex = 5, alpha = 0.7, legend = FALSE)


# Save
st_write(allsolar_assigned_nearest, "data/allsolar_assigned_nearest.gpkg", delete_layer = TRUE)
st_write(allsolar_assigned_nearest, "data/allsolar_assigned_nearest.shp", delete_layer = TRUE)


