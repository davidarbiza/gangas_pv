
# Filter global solar PV dataset to keep only installations in Spain and transform data to spatial data

library(readxl)
library(dplyr)
library(sf)
library(mapview)

pv_data <- "data/Global-Solar-Power-Tracker-February-2025.xlsx"

#Read both sheets
pv_up20MW   <- read_excel(pv_data, sheet = "20 MW+")
pv_down20MW <- read_excel(pv_data, sheet = "1-20 MW")

# Filter only Spain
pv_up20MW <- pv_up20MW %>%
  filter(`Country/Area` == "Spain")

pv_down20MW <- pv_down20MW %>%
  filter(`Country/Area` == "Spain")

# Combine into one dataset
pv_spain <- bind_rows(pv_up20MW, pv_down20MW)

# Check result
cat("Total solar PV plants in Spain:", nrow(pv_spain), "\n")

# Save filtered dataset to CSV
write.csv(pv_spain, "data/pv_spain.csv", row.names = FALSE)

# convert to spatial using sf
pv_spain_sf <- st_as_sf(
  pv_spain,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

mapview(pv_spain_sf)

#Save spatial as shapefile
st_write(pv_spain_sf, "data/pv_spain.shp") 
#Save spatial as geopckage
st_write(pv_spain_sf, "data/pv_spain.gpkg")


