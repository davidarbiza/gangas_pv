# 1. Install and load required packages

install.packages("osmdata")
library(osmdata)
library(sf)
library(dplyr)
library(mapview)

# 2. Select Spain as study area

spain <- getbb("Spain") 


# 3. Get PV data

q <- opq(bbox = spain) %>%
  add_osm_feature(key = "power", value = "plant") %>%
  add_osm_feature(key = "plant:source", value = "solar")


spain_solar <- osmdata_sf(q)

# 4. Explore data 

PV_panels <- spain_solar$osm_points # solar PV panels
mapview(PV_panels) # not much info

# Select polygons only
solar_polygons <- spain_solar$osm_polygons
solar_multipolygons <- spain_solar$osm_multipolygons

solar_plants <- bind_rows(
  st_make_valid(solar_polygons),
  st_make_valid(solar_multipolygons)
)

mapview(solar_plants)

# 5. Compare with our PV layer

PV <- read_sf("data/allsolar_cut_diss.shp")
solar_plants_laea <- st_transform(solar_plants, crs = st_crs(PV))

solar_plants_laea$area <-


mapview(solar_plants, col.regions = "orange") + mapview(PV)
