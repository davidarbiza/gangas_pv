# ========================================
# PSEUDO-ABSENCES GENERATION SCRIPTS
# ========================================

# -----------------------
# RANDOM POINTS METHOD
# -----------------------
# 1. Load libraries
library(sf)
library(sp)
library(raster)
library(dismo)
library(tidyverse)
library(mapSpain)
library(rnaturalearth)

# 2. Clean environment
rm(list = ls())
gc()

set.seed(12345)

# 3. Paths
bbs_file <- "E:/TFM_gangas/GPS/Merged/BBS_filtered_NoPseudoreplication.csv"
pts_file <- "E:/TFM_gangas/GPS/Merged/PTS_filtered_NoPseudoreplication.csv"

# 4. Load data
bbs_data <- read_csv(bbs_file)
pts_data <- read_csv(pts_file)

# 5. Create Iberian Peninsula mask

# --- Insular Spain  ---
provinces <- esp_get_prov()
provinces <- provinces[!provinces$iso2.prov.name.es %in% 
                         c('Las Palmas', 'Santa Cruz de Tenerife', 'Baleares', 'Ceuta', 'Melilla'), ]
mask_spain <- st_union(provinces)

# --- Insular Portugal ---
por <- ne_countries(country = "Portugal", scale = "medium", returnclass = "sf")
por_polys <- st_cast(por, "POLYGON")
por_polys$area <- st_area(por_polys)
por_cont <- por_polys %>% slice_max(area, n = 1)
por_cont <- st_transform(por_cont, st_crs(mask_spain))

# --- Join Iberian Peninsula ---
iberia_mask <- st_union(mask_spain, por_cont)

# Transform to UTM 30N
iberia_mask <- st_transform(iberia_mask, 25830)

# Convert to SpatialPolygons for rasterization
iberia_sp <- as(iberia_mask, "Spatial")

# 6. Convert mask to raster (300m resolution)
iberia_raster <- raster(extent(iberia_sp))
res(iberia_raster) <- 300
iberia_raster[] <- 1
iberia_raster <- mask(iberia_raster, iberia_sp)

# 7. Pseudo-absence generator
generate_pseudoabsences <- function(presences, mask_raster, n_per_presence = 5) {
  
  cat("Generating pseudo-absences...\n")
  
  total_n <- nrow(presences) * n_per_presence
  
  pts <- randomPoints(mask = mask_raster,
                      n = total_n,
                      extf = 1.0,
                      warn = 0)
  
  pseudo_data <- tibble(
    birdID   = NA,
    species  = rep(presences$species, each = n_per_presence),
    date     = rep(presences$date, each = n_per_presence),
    X_25830  = pts[,1],
    Y_25830  = pts[,2],
    presence = 0
  )
  
  return(pseudo_data)
}

# 8. Generate pseudo-absences
bbs_pseudo <- generate_pseudoabsences(bbs_data, iberia_raster, 5)
pts_pseudo <- generate_pseudoabsences(pts_data, iberia_raster, 5)

# 9. Add presence flag
bbs_data$presence <- 1
pts_data$presence <- 1

# 10. Combine presences + pseudo-absences
bbs_final <- bind_rows(bbs_data, bbs_pseudo)
pts_final <- bind_rows(pts_data, pts_pseudo)

# 11. Quick plot check (pseudo-absences only)
x11()
plot(iberia_mask, col = "lightgrey", main = "BBS pseudo-absences")
points(bbs_pseudo$X_25830,
       bbs_pseudo$Y_25830,
       col = "red", pch = 20, cex = 0.1)
x11()
plot(iberia_mask, col = "lightgrey", main = "PTS pseudo-absences")
points(pts_pseudo$X_25830,
       pts_pseudo$Y_25830,
       col = "red", pch = 20, cex = 0.1)

# 12. Save results
write_csv(bbs_final, "E:/TFM_gangas/GPS/Merged/BBS_pseudoabsences_Random.csv")
write_csv(pts_final, "E:/TFM_gangas/GPS/Merged/PTS_pseudoabsences_Random.csv")

cat("Done! Pseudo-absences generated and saved.\n")






# --------------------------------
# PSEUDO-ABSENCES – P95 METHOD
# --------------------------------

library(sf)
library(sp)
library(raster)
library(dismo)
library(tidyverse)
library(mapSpain)
library(rnaturalearth)

rm(list = ls())
gc

set.seed(12345)

# Paths
bbs_file <- "E:/TFM_gangas/GPS/Merged/BBS_filtered_NoPseudoreplication.csv"
pts_file <- "E:/TFM_gangas/GPS/Merged/PTS_filtered_NoPseudoreplication.csv"

# Load data
bbs_data <- read_csv(bbs_file)
pts_data <- read_csv(pts_file)

bbs_sf <- st_as_sf(bbs_data, coords = c("X_25830", "Y_25830"), crs = 25830)
pts_sf <- st_as_sf(pts_data, coords = c("X_25830", "Y_25830"), crs = 25830)

# Iberian Peninsula mask
provinces <- esp_get_prov()
provinces <- provinces[!provinces$iso2.prov.name.es %in%
                         c("Las Palmas","Santa Cruz de Tenerife",
                           "Baleares","Ceuta","Melilla"), ]
mask_spain <- st_union(provinces)

por <- ne_countries(country = "Portugal", scale = "medium", returnclass = "sf")
por_polys <- st_cast(por, "POLYGON")
por_polys$area <- st_area(por_polys)
por_cont <- por_polys |> slice_max(area, n = 1)
por_cont <- st_transform(por_cont, st_crs(mask_spain))

iberia_mask <- st_union(mask_spain, por_cont)
iberia_mask <- st_transform(iberia_mask, 25830)
iberia_sp <- as(iberia_mask, "Spatial")

# Base raster (300 m)
base_raster <- raster(extent(iberia_sp), res = 300)
base_raster[] <- 1
base_raster <- mask(base_raster, iberia_sp)

# P95 values
d95_values <- tibble(
  species = c("BBS","PTS"),
  d95 = c(7320, 5317)
)

# -----------------------------------------
# Function to build accessible area (P95)
# -----------------------------------------
build_p95_area <- function(sf_data, d95){
  
  ids <- unique(sf_data$birdID)
  
  buffers <- lapply(ids, function(id){
    ind_pts <- sf_data[sf_data$birdID == id, ]
    st_union(st_buffer(ind_pts, dist = d95))
  })
  
  area <- do.call(c, buffers)
  area <- st_intersection(area, iberia_mask)
  
  # minimum distance exclusion (300 m)
  min_buf <- st_union(st_buffer(sf_data, 300))
  area <- st_difference(area, min_buf)
  
  return(area)
}

# Build areas
bbs_area <- build_p95_area(bbs_sf, d95_values$d95[d95_values$species=="BBS"])
pts_area <- build_p95_area(pts_sf, d95_values$d95[d95_values$species=="PTS"])

# Rasterize areas
bbs_raster <- rasterize(as(bbs_area, "Spatial"), base_raster, field = 1, background = NA)
pts_raster <- rasterize(as(pts_area, "Spatial"), base_raster, field = 1, background = NA)

# -------------------------
# Generate pseudo-absences
# -------------------------
generate_pseudoabsences <- function(presences, area_raster, n_per_presence = 4){
  
  total_n <- nrow(presences) * n_per_presence
  
  pts <- randomPoints(area_raster, n = total_n, warn = 0)
  
  tibble(
    birdID   = NA,
    species  = rep(presences$species, each = n_per_presence),
    date     = rep(presences$date, each = n_per_presence),
    X_25830  = pts[,1],
    Y_25830  = pts[,2],
    presence = 0
  )
}

bbs_pseudo <- generate_pseudoabsences(bbs_data, bbs_raster, 4)
pts_pseudo <- generate_pseudoabsences(pts_data, pts_raster, 4)

# Combine
bbs_data$presence <- 1
pts_data$presence <- 1

bbs_final <- bind_rows(bbs_data, bbs_pseudo)
pts_final <- bind_rows(pts_data, pts_pseudo)

# -------------------------
# Quick plot check
# -------------------------
x11()
plot(iberia_mask, col = "lightgrey", main = "BBS pseudo-absences (P95)")
points(bbs_pseudo$X_25830,
       bbs_pseudo$Y_25830,
       col = "red", pch = 20, cex = 0.1)

x11()
plot(iberia_mask, col = "lightgrey", main = "PTS pseudo-absences (P95)")
points(pts_pseudo$X_25830,
       pts_pseudo$Y_25830,
       col = "red", pch = 20, cex = 0.1)

# Save
write_csv(bbs_final, "E:/TFM_gangas/GPS/Merged/BBS_pseudoabsences_P95.csv")
write_csv(pts_final, "E:/TFM_gangas/GPS/Merged/PTS_pseudoabsences_P95.csv")

cat("Done! P95 pseudo-absences generated.\n")







# ---------------------------------------------
# PSEUDOABSENCES – MCP 40 km METHOD
# ---------------------------------------------

library(sf)
library(sp)
library(raster)
library(dismo)
library(tidyverse)
library(mapSpain)
library(rnaturalearth)

rm(list = ls())
gc()

set.seed(12345)

# Paths
bbs_file <- "E:/TFM_gangas/GPS/Merged/BBS_filtered_NoPseudoreplication.csv"
pts_file <- "E:/TFM_gangas/GPS/Merged/PTS_filtered_NoPseudoreplication.csv"

# Load data
bbs_data <- read_csv(bbs_file)
pts_data <- read_csv(pts_file)

bbs_sf <- st_as_sf(bbs_data, coords = c("X_25830","Y_25830"), crs = 25830)
pts_sf <- st_as_sf(pts_data, coords = c("X_25830","Y_25830"), crs = 25830)

# Iberian Peninsula mask
provinces <- esp_get_prov()
provinces <- provinces[!provinces$iso2.prov.name.es %in%
                         c("Las Palmas","Santa Cruz de Tenerife",
                           "Baleares","Ceuta","Melilla"), ]
mask_spain <- st_union(provinces)

por <- ne_countries(country = "Portugal", scale = "medium", returnclass = "sf")
por_polys <- st_cast(por, "POLYGON")
por_polys$area <- st_area(por_polys)
por_cont <- por_polys |> slice_max(area, n = 1)
por_cont <- st_transform(por_cont, st_crs(mask_spain))

iberia_mask <- st_union(mask_spain, por_cont)
iberia_mask <- st_transform(iberia_mask, 25830)
iberia_sp <- as(iberia_mask, "Spatial")

# Base raster (300 m)
base_raster <- raster(extent(iberia_sp), res = 300)
base_raster[] <- 1
base_raster <- mask(base_raster, iberia_sp)

# Build accessible area: MCP per individual + 40 km
build_mcp40_area <- function(sf_data, max_dist = 40000, min_dist = 600){
  
  ids <- unique(sf_data$birdID)
  
  patches <- lapply(ids, function(id){
    
    ind_pts <- sf_data[sf_data$birdID == id, ]
    if(nrow(ind_pts) < 3) return(NULL)
    
    mcp <- st_convex_hull(st_union(ind_pts))
    area <- st_buffer(mcp, dist = max_dist)
    
    # minimum distance exclusion
    min_buf <- st_union(st_buffer(ind_pts, min_dist))
    area <- st_difference(area, min_buf)
    
    area
  })
  
  # Join MCPs + buffer 40km
  area <- do.call(c, patches)
  
  # Mask Iberia
  area <- st_intersection(area, iberia_mask)
  
  return(area)
}

bbs_area <- build_mcp40_area(bbs_sf, 40000)
pts_area <- build_mcp40_area(pts_sf, 40000)

# Rasterize areas
bbs_raster <- rasterize(as(bbs_area,"Spatial"), base_raster, field = 1, background = NA)
pts_raster <- rasterize(as(pts_area,"Spatial"), base_raster, field = 1, background = NA)

# Generate pseudo-absences
generate_pseudoabsences <- function(presences, area_raster, n_per_presence = 5){
  
  total_n <- nrow(presences) * n_per_presence
  
  pts <- randomPoints(area_raster, n = total_n, warn = 0)
  
  n_gen <- nrow(pts)
  
  tibble(
    birdID   = NA,
    species  = rep(presences$species, length.out = n_gen),
    date     = rep(presences$date, length.out = n_gen),
    X_25830  = pts[,1],
    Y_25830  = pts[,2],
    presence = 0
  )
}

bbs_pseudo <- generate_pseudoabsences(bbs_data, bbs_raster, 5)
pts_pseudo <- generate_pseudoabsences(pts_data, pts_raster, 5)

# Combine
bbs_data$presence <- 1
pts_data$presence <- 1

bbs_final <- bind_rows(bbs_data, bbs_pseudo)
pts_final <- bind_rows(pts_data, pts_pseudo)

# -------------------------------------------------
# Quick visual check
# -------------------------------------------------
x11()
plot(iberia_mask, col = "lightgrey", main = "BBS pseudo-absences (MCP + 40 km)")
points(bbs_pseudo$X_25830,
       bbs_pseudo$Y_25830,
       col = "red", pch = 20, cex = 0.1)

x11()
plot(iberia_mask, col = "lightgrey", main = "PTS pseudo-absences (MCP + 40 km)")
points(pts_pseudo$X_25830,
       pts_pseudo$Y_25830,
       col = "red", pch = 20, cex = 0.1)

# Save
write_csv(bbs_final, "E:/TFM_gangas/GPS/Merged/BBS_pseudoabsences_MCP40km.csv")
write_csv(pts_final, "E:/TFM_gangas/GPS/Merged/PTS_pseudoabsences_MCP40km.csv")

cat("Done! MCP + 40 km pseudo-absences generated.\n")
