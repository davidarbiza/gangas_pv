# Second attempt: join pv_spain.gpkg (obteined from Global PV Excell) and allsolar_cut.shp (Polygon PV Shapefile)
library(sf)
library(dplyr)
library(mapview)

pv_spain_excell <- st_read("data/pv_spain.gpkg", quiet = TRUE)   
allsolar <- st_read("data/allsolar_cut_diss.shp", quiet = TRUE)    

# Assure same CRS
allsolar <- st_transform(allsolar, 4326)
pv_spain_excell <- st_transform(pv_spain_excell, 4326)

# Filter only operating plants
pv_spain_excell <- pv_spain_excell %>% filter(Status == "operating")

# Create id for each obs.
pv_spain_excell <- pv_spain_excell %>% mutate(point_id = row_number())
allsolar <- allsolar %>% mutate(poly_id = row_number())

# Repair invalid geometries
allsolar <- st_make_valid(allsolar)
# Remove duplicates vertices
allsolar <- st_buffer(allsolar, 0)

# Assign nearest PV point
nearest_idx <- st_nearest_feature(allsolar, pv_spain_excell)

assign_nearest <- tibble(
  poly_id = allsolar$poly_id,
  nearest_idx = nearest_idx
) %>%
  mutate(
    point_id = pv_spain_excell$point_id[nearest_idx],
    project_name = pv_spain_excell$Project.Name[nearest_idx],
    start_year = pv_spain_excell$`Start.year`[nearest_idx],
    capacity_mw = pv_spain_excell$`Capacity..MW.`[nearest_idx]
  )

# Spatial join: associate each PV polygon with the closest PV point metadata
allsolar_assigned_nearest <- allsolar %>%
  left_join(assign_nearest %>% select(poly_id, point_id, project_name, start_year, capacity_mw),
            by = "poly_id")

# Distances from each polygon to its assigned PV point
allsolar_assigned_nearest <- allsolar_assigned_nearest %>%
  mutate(distance_m = as.numeric(st_distance(
    st_transform(geometry, 3857),
    st_transform(pv_spain_excell[match(point_id, pv_spain_excell$point_id), ], 3857),
    by_element = TRUE
  )))


# Estimate area-based radius for validation (MW → 2–5 ha/MW)
allsolar_assigned_nearest <- allsolar_assigned_nearest %>%
  mutate(
    max_area_ha = capacity_mw * 5,    # 5 ha/MW
    max_allow_m = sqrt((max_area_ha * 10000) / pi),  # max. radius
    assigned_valid = distance_m <= max_allow_m
  )

# QC
summary(allsolar_assigned_nearest$distance_m)
sum(!is.na(allsolar_assigned_nearest$point_id))
outliers <- allsolar_assigned_nearest %>% filter(!assigned_valid)
table(allsolar_assigned_nearest$assigned_valid)


# Mapview
mapview(allsolar_assigned_nearest, zcol = "distance_m")  # visualize all polygons colored by distance

# Mapview grouped by assigned_valid
poligonos_invalidos <- allsolar_assigned_nearest %>% filter(!assigned_valid)
mapview(poligonos_invalidos, zcol = "distance_m", col.regions = "green", legend = TRUE) +
  mapview(pv_spain_excell, col.region = "red", cex = 5, alpha = 0.7, legend = FALSE)

# Save
st_write(allsolar_assigned_nearest, "data/allsolar_assigned_nearest.gpkg", delete_layer = TRUE)
st_write(allsolar_assigned_nearest, "data/allsolar_assigned_nearest.shp", delete_layer = TRUE)