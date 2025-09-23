
# Join pv_spain.gpkg (obteined from Global PV Excell) and allsolar_cut.shp (Polygon PV Shapefile) 

library(sf)
library(dplyr)
library(janitor)

pv_spain_excell <- st_read("data/pv_spain.gpkg", quiet = TRUE)
allsolar <- st_read("data/allsolar_cut.shp", quiet = TRUE)

# Assure same CRS
allsolar <- st_transform(allsolar, st_crs(pv_spain_excell))

#Create id por each obs.
pv_spain_excell <- pv_spain_excell %>%
  mutate(point_id = row_number())

#Buffer
pv_spain_excell <- pv_spain_excell %>%
  mutate(buffer_radius = sqrt(`Capacity..MW.`) * 50) %>%
  st_buffer(dist = .$buffer_radius)

# Spacial Join
joined <- st_join(allsolar, pv_spain_excell, join = st_intersects)

# Clean columns names
joined_clean <- joined %>%
  clean_names()

# Select relevant columns
cols_to_keep <- c("point_id", "project_name", "start_year", "capacity_mw")
joined_final <- joined_clean %>%
  select(all_of(cols_to_keep), geometry)


# Save results
st_write(joined_final, "data/allsolar_Spain_joined.gpkg", delete_layer = TRUE)
st_write(joined_final, "data/allsolar_Spain_joined.shp", delete_layer = TRUE)

#Check if the layer works
joined <- st_read("data/allsolar_Spain_joined.gpkg", quiet = TRUE)
st_geometry_type(joined) #Geometry
st_crs(joined) #CRS
sum(!is.na(joined_final$project_name)) #Polygons with data

# Check overlaps
poligonos_solapados <- joined_final %>%
  group_by(geometry) %>% 
  summarise(n_points = n_distinct(point_id)) %>%
  filter(n_points > 1)
nrow(poligonos_solapados)

