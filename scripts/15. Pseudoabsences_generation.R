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

rm(list = ls())
gc()

set.seed(12345)

# 3. Paths
bbs_file <- "E:/TFM_gangas/GPS/MergedV.2/BBS_filtered_NoPseudoreplication.csv"
pts_file <- "E:/TFM_gangas/GPS/MergedV.2/PTS_filtered_NoPseudoreplication.csv"

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
  
  # -----------------------------------
  # EXCLUDE PRESENCE CELLS (≈300 m)
  # -----------------------------------
  presence_sp <- SpatialPoints(
    presences[, c("X_25830","Y_25830")],
    proj4string = CRS(projection(mask_raster))
  )
  
  presence_raster <- rasterize(presence_sp, mask_raster, field = 1)
  
  mask_clean <- mask_raster
  mask_clean[!is.na(presence_raster)] <- NA
  
  # -----------------------------------
  # GENERATE POINTS
  # -----------------------------------
  pts <- randomPoints(mask = mask_clean,
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
plot(iberia_mask, col = "lightgrey", main = " BBS pseudo-absences")
points(bbs_pseudo$X_25830,
       bbs_pseudo$Y_25830,
       col = "red", pch = 20, cex = 0.1)
x11()
plot(iberia_mask, col = "lightgrey", main = "PTS pseudo-absences")
points(pts_pseudo$X_25830,
       pts_pseudo$Y_25830,
       col = "red", pch = 20, cex = 0.1)

# 12. Save results
write_csv(bbs_final, "E:/TFM_gangas/GPS/MergedV.2/BBS_pseudoabsences_Random.csv")
write_csv(pts_final, "E:/TFM_gangas/GPS/MergedV.2/PTS_pseudoabsences_Random.csv")

cat("Done! Pseudo-absences generated and saved.\n")






# --------------------------------
# PSEUDO-ABSENCES – P95 METHOD (DECAY)
# --------------------------------

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
bbs_file <- "E:/TFM_gangas/GPS/MergedV.2/BBS_filtered_NoPseudoreplication.csv"
pts_file <- "E:/TFM_gangas/GPS/MergedV.2/PTS_filtered_NoPseudoreplication.csv"

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

# Base raster
base_raster <- raster(extent(iberia_sp), res = 300)
base_raster[] <- 1
base_raster <- mask(base_raster, iberia_sp)

# Build accessible area
build_p95_area <- function(sf_data, d95){
  ids <- unique(sf_data$birdID)
  buffers <- lapply(ids, function(id){
    ind_pts <- sf_data[sf_data$birdID == id, ]
    st_union(st_buffer(ind_pts, dist = d95))
  })
  area <- do.call(c, buffers)
  area <- st_intersection(area, iberia_mask)
  min_buf <- st_union(st_buffer(sf_data, 300))
  st_difference(area, min_buf)
}

bbs_area <- build_p95_area(bbs_sf, 7320)
pts_area <- build_p95_area(pts_sf, 5317)

# --------------------------------
# CREATE DECAY RASTER
# --------------------------------
create_decay_raster <- function(area_sf, lambda){
  
  area_raster <- rasterize(as(area_sf, "Spatial"),
                           base_raster,
                           field = 1,
                           background = NA)
  
  dist_raster <- distance(area_raster)
  
  decay_raster <- exp(-dist_raster / lambda)
  
  decay_raster <- mask(decay_raster, base_raster)
  
  return(decay_raster)
}

bbs_decay <- create_decay_raster(bbs_area, 7320)
pts_decay <- create_decay_raster(pts_area, 5317)

# Generate pseudoabsences
generate_pseudoabsences <- function(presences, prob_raster, n_per_presence = 5){
  
  total_n <- nrow(presences) * n_per_presence
  
  pts <- randomPoints(prob_raster,
                      n = total_n,
                      prob = TRUE,
                      warn = 0)
  
  tibble(
    birdID   = NA,
    species  = rep(presences$species, each = n_per_presence),
    date     = rep(presences$date, each = n_per_presence),
    X_25830  = pts[,1],
    Y_25830  = pts[,2],
    presence = 0
  )
}

bbs_pseudo <- generate_pseudoabsences(bbs_data, bbs_decay, 5)
pts_pseudo <- generate_pseudoabsences(pts_data, pts_decay, 5)

# Combine
bbs_data$presence <- 1
pts_data$presence <- 1

bbs_final <- bind_rows(bbs_data, bbs_pseudo)
pts_final <- bind_rows(pts_data, pts_pseudo)

# Plot
x11()
plot(iberia_mask, col = "lightgrey")
points(bbs_pseudo$X_25830, bbs_pseudo$Y_25830, col = "red", pch = 20, cex = 0.1)

x11()
plot(iberia_mask, col = "lightgrey")
points(pts_pseudo$X_25830, pts_pseudo$Y_25830, col = "red", pch = 20, cex = 0.1)

# Save
write_csv(bbs_final, "E:/TFM_gangas/GPS/MergedV.2/BBS_pseudoabsences_P95_decay.csv")
write_csv(pts_final, "E:/TFM_gangas/GPS/MergedV.2/PTS_pseudoabsences_P95_decay.csv")

cat("Done! P95 pseudo-absences generated.\n")





# ---------------------------------------------
# PSEUDOABSENCES – MCP 40 km METHOD (DECAY)
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
bbs_file <- "E:/TFM_gangas/GPS/MergedV.2/BBS_filtered_NoPseudoreplication.csv"
pts_file <- "E:/TFM_gangas/GPS/MergedV.2/PTS_filtered_NoPseudoreplication.csv"

# Load data
bbs_data <- read_csv(bbs_file)
pts_data <- read_csv(pts_file)

bbs_sf <- st_as_sf(bbs_data, coords = c("X_25830","Y_25830"), crs = 25830)
pts_sf <- st_as_sf(pts_data, coords = c("X_25830","Y_25830"), crs = 25830)

# Iberia mask
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

# Base raster
base_raster <- raster(extent(iberia_sp), res = 300)
base_raster[] <- 1
base_raster <- mask(base_raster, iberia_sp)

# MCP area
build_mcp40_area <- function(sf_data, max_dist = 40000, min_dist = 600){
  
  ids <- unique(sf_data$birdID)
  
  patches <- lapply(ids, function(id){
    ind_pts <- sf_data[sf_data$birdID == id, ]
    if(nrow(ind_pts) < 3) return(NULL)
    mcp <- st_convex_hull(st_union(ind_pts))
    area <- st_buffer(mcp, dist = max_dist)
    min_buf <- st_union(st_buffer(ind_pts, min_dist))
    st_difference(area, min_buf)
  })
  
  area <- do.call(c, patches)
  st_intersection(area, iberia_mask)
}

bbs_area <- build_mcp40_area(bbs_sf, 40000)
pts_area <- build_mcp40_area(pts_sf, 40000)

# --------------------------------
# CREATE DECAY RASTER
# --------------------------------
create_decay_raster <- function(area_sf, lambda){
  
  area_raster <- rasterize(as(area_sf, "Spatial"),
                           base_raster,
                           field = 1,
                           background = NA)
  
  dist_raster <- distance(area_raster)
  
  decay_raster <- exp(-dist_raster / lambda)
  decay_raster <- mask(decay_raster, base_raster)
  
  return(decay_raster)
}

bbs_decay <- create_decay_raster(bbs_area, 40000)
pts_decay <- create_decay_raster(pts_area, 40000)

# Generate pseudoabsences
generate_pseudoabsences <- function(presences, prob_raster, n_per_presence = 5){
  
  total_n <- nrow(presences) * n_per_presence
  
  pts <- randomPoints(prob_raster,
                      n = total_n,
                      prob = TRUE,
                      warn = 0)
  
  tibble(
    birdID   = NA,
    species  = rep(presences$species, each = n_per_presence),
    date     = rep(presences$date, each = n_per_presence),
    X_25830  = pts[,1],
    Y_25830  = pts[,2],
    presence = 0
  )
}

bbs_pseudo <- generate_pseudoabsences(bbs_data, bbs_decay, 5)
pts_pseudo <- generate_pseudoabsences(pts_data, pts_decay, 5)

# Combine
bbs_data$presence <- 1
pts_data$presence <- 1

bbs_final <- bind_rows(bbs_data, bbs_pseudo)
pts_final <- bind_rows(pts_data, pts_pseudo)

# Plot
x11()
plot(iberia_mask, col = "lightgrey")
points(bbs_pseudo$X_25830, bbs_pseudo$Y_25830, col = "red", pch = 20, cex = 0.1)

x11()
plot(iberia_mask, col = "lightgrey")
points(pts_pseudo$X_25830, pts_pseudo$Y_25830, col = "red", pch = 20, cex = 0.1)

# Save
write_csv(bbs_final, "E:/TFM_gangas/GPS/MergedV.2/BBS_pseudoabsences_MCP40_decay.csv")
write_csv(pts_final, "E:/TFM_gangas/GPS/MergedV.2/PTS_pseudoabsences_MCP40_decay.csv")

cat("Done! MCP + 40 km pseudo-absences generated.\n")





# ------------------------------------------------------------
# COMPARE FIRST PSEUDOAUBSENCES METHODS VS DECAY METHODS
# ------------------------------------------------------------
library(sf)
library(ggplot2)
library(dplyr)

# ============================================================
# FILES
# ============================================================

files <- list(
  
  PTS_P95_V2 = "E:/TFM_gangas/GPS/ExtractedV.2/PTS_pseudoabsences_P95_env.csv",
  PTS_P95_V3 = "E:/TFM_gangas/GPS/ExtractedV.3/PTS_pseudoabsences_P95_decay_env.csv",
  
  BBS_P95_V2 = "E:/TFM_gangas/GPS/ExtractedV.2/BBS_pseudoabsences_P95_env.csv",
  BBS_P95_V3 = "E:/TFM_gangas/GPS/ExtractedV.3/BBS_pseudoabsences_P95_decay_env.csv",
  
  PTS_MCP_V2 = "E:/TFM_gangas/GPS/ExtractedV.2/PTS_pseudoabsences_MCP40km_env.csv",
  PTS_MCP_V3 = "E:/TFM_gangas/GPS/ExtractedV.3/PTS_pseudoabsences_MCP40_decay_env.csv",
  
  BBS_MCP_V2 = "E:/TFM_gangas/GPS/ExtractedV.2/BBS_pseudoabsences_MCP40km_env.csv",
  BBS_MCP_V3 = "E:/TFM_gangas/GPS/ExtractedV.3/BBS_pseudoabsences_MCP40_decay_env.csv"
)

# ============================================================
# LOAD
# ============================================================

load_pa <- function(path, label){
  
  df <- read.csv(path)
  
  df <- df %>%
    filter(presence == 0)
  
  sf_obj <- st_as_sf(
    df,
    coords = c("X_25830","Y_25830"),
    crs = 25830
  )
  
  sf_obj$dataset <- label
  
  return(sf_obj)
}

# ============================================================
# PLOT FUNCTION
# ============================================================

plot_compare <- function(v2_name, v3_name, title){
  
  pa_v2 <- load_pa(files[[v2_name]], "V2")
  pa_v3 <- load_pa(files[[v3_name]], "V3")
  
  all_pa <- rbind(pa_v2, pa_v3)
  
  p <- ggplot(all_pa) +
    
    geom_sf(aes(color = dataset),
            alpha = 0.15,
            size = 0.1) +
    
    facet_wrap(~dataset) +
    
    scale_color_manual(values = c(
      "V2" = "#E64B35",
      "V3" = "#4DBBD5"
    )) +
    
    theme_void() +
    
    ggtitle(title)
  
  print(p)
}

# ============================================================
# RUN
# ============================================================

x11()
plot_compare(
  "PTS_P95_V2",
  "PTS_P95_V3",
  "PTS — P95"
)

x11()
plot_compare(
  "BBS_P95_V2",
  "BBS_P95_V3",
  "BBS — P95"
)

x11()
plot_compare(
  "PTS_MCP_V2",
  "PTS_MCP_V3",
  "PTS — MCP40"
)

x11()
plot_compare(
  "BBS_MCP_V2",
  "BBS_MCP_V3",
  "BBS — MCP40"
)


