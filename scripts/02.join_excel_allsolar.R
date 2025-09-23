
# Join pv_spain.gpkg (obteined from Global PV Excell) and allsolar_cut.shp (Polygon PV Shapefile) 

library(sf)
library(dplyr)
library(janitor)

pv_spain_excell <- st_read("data/pv_spain.gpkg", quiet = TRUE)
allsolar <- st_read("data/allsolar_cut.shp", quiet = TRUE)

# Assure same CRS
allsolar <- st_transform(allsolar, st_crs(pv_spain_excell))

# Spacial Join
joined <- st_join(allsolar, pv_spain_excell, join = st_intersects)

# Clean columns names
joined_clean <- joined %>%
  clean_names()

# Select relevant columns
cols_to_keep <- c("project_name", "start_year", "capacity_mw")
joined_final <- joined_clean %>%
  select(all_of(cols_to_keep), geometry)

# Verify geometry
st_geometry(joined_final)

# Save results
st_write(joined_final, "data/allsolar_Spain_joined.gpkg", delete_layer = TRUE)
st_write(joined_final, "data/allsolar_Spain_joined.shp", delete_layer = TRUE)
