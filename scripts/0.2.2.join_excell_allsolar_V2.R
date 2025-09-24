# Second attempt: join pv_spain.gpkg (obteined from Global PV Excell) and allsolar_cut.shp (Polygon PV Shapefile)
library(sf)
library(dplyr)
library(mapview)

pv_spain_excell <- st_read("data/pv_spain.gpkg", quiet = TRUE)   
allsolar <- st_read("data/allsolar_cut_diss.shp", quiet = TRUE)    

# Assure same CRS
allsolar <- st_transform(allsolar, 4326)
pv_spain_excell <- st_transform(pv_spain_excell, 4326)

# Create id for each obs.
pv_spain_excell <- pv_spain_excell %>% mutate(point_id = row_number())
allsolar <- allsolar %>% mutate(poly_id = row_number())

# Polygons centroids
poly_centroids <- st_centroid(allsolar)

# Nearest pv point 
nearest_idx <- st_nearest_feature(poly_centroids, pv_spain_excell)  

assign_nearest <- tibble(
  poly_id = allsolar$poly_id,
  nearest_idx = st_nearest_feature(st_centroid(allsolar), pv_spain_excell)
) %>%
  mutate(
    point_id = pv_spain_excell$point_id[nearest_idx],
    project_name = pv_spain_excell$Project.Name[nearest_idx],
    start_year = pv_spain_excell$`Start.year`[nearest_idx],
    capacity_mw = pv_spain_excell$`Capacity..MW.`[nearest_idx]
  )


# Spatial join: associate each PV polygon with the closest PV point’s metadata
allsolar_assigned_nearest <- allsolar %>%
  left_join(assign_nearest, by = "poly_id")

# Distances from each polygon to its assigned PV point (QC check)
allsolar_assigned_nearest <- allsolar_assigned_nearest %>%
  mutate(distance_m = as.numeric(st_distance(
    st_transform(st_centroid(geometry), 3857),
    st_transform(pv_spain_excell[match(point_id, pv_spain_excell$point_id), ], 3857),
    by_element = TRUE
  )))

# QC
summary(allsolar_assigned_nearest$distance_m)

sum(!is.na(allsolar_assigned_nearest$point_id))

# Outliers: distancia > 5 km
outliers <- allsolar_assigned_nearest %>%
  filter(distance_m > 5000)

nrow(outliers)

mapview(outliers, zcol = "distance_m") + mapview(pv_spain_excell)


# Guardar si te convence
st_write(allsolar_assigned_nearest, "data/allsolar_assigned_nearest.gpkg", delete_layer = TRUE)
