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

# --------------------------------------------------
# PATHS
# --------------------------------------------------

bbs_file <- "E:/TFM_gangas/GPS/MergedV.2/BBS_filtered_merged.csv"
pts_file <- "E:/TFM_gangas/GPS/MergedV.2/PTS_filtered_merged.csv"

# --------------------------------------------------
# LOAD DATA
# --------------------------------------------------

bbs_data <- read_csv(bbs_file)
pts_data <- read_csv(pts_file)

# --------------------------------------------------
# CALCULATE P95 MOVEMENT DISTANCES
# --------------------------------------------------

calculate_p95 <- function(df){
  
  df$date <- as.POSIXct(df$date)
  
  sf_data <- st_as_sf(
    df,
    coords = c("X_25830","Y_25830"),
    crs = 25830
  )
  
  dist_data <- sf_data %>%
    arrange(birdID, date) %>%
    group_by(birdID) %>%
    mutate(
      lag_geom = lag(geometry)
    ) %>%
    filter(!is.na(lag_geom)) %>%
    mutate(
      dist_m = as.numeric(
        st_distance(
          geometry,
          lag_geom,
          by_element = TRUE
        )
      )
    ) %>%
    ungroup()
  
  quantile(
    dist_data$dist_m,
    probs = 0.95,
    na.rm = TRUE
  )
}

bbs_p95 <- as.numeric(calculate_p95(bbs_data))
pts_p95 <- as.numeric(calculate_p95(pts_data))

cat("\n")
cat("=====================================\n")
cat("P95 MOVEMENT DISTANCES\n")
cat("=====================================\n")
cat("BBS =", round(bbs_p95,1), "m\n")
cat("PTS =", round(pts_p95,1), "m\n")
cat("=====================================\n\n")

bbs_p95 <- 3998
pts_p95 <- 3821

# --------------------------------------------------
# LOAD FINAL PRESENCE DATASET
# --------------------------------------------------

bbs_data <- read_csv(
  "E:/TFM_gangas/GPS/MergedV.2/BBS_filtered_NoPseudoreplication.csv"
)

pts_data <- read_csv(
  "E:/TFM_gangas/GPS/MergedV.2/PTS_filtered_NoPseudoreplication.csv"
)

# --------------------------------------------------
# SF OBJECTS
# --------------------------------------------------

bbs_sf <- st_as_sf(
  bbs_data,
  coords = c("X_25830", "Y_25830"),
  crs = 25830
)

pts_sf <- st_as_sf(
  pts_data,
  coords = c("X_25830", "Y_25830"),
  crs = 25830
)

# --------------------------------------------------
# IBERIAN PENINSULA MASK
# --------------------------------------------------

provinces <- esp_get_prov()

provinces <- provinces[
  !provinces$iso2.prov.name.es %in%
    c(
      "Las Palmas",
      "Santa Cruz de Tenerife",
      "Baleares",
      "Ceuta",
      "Melilla"
    ),
]

mask_spain <- st_union(provinces)

por <- ne_countries(
  country = "Portugal",
  scale = "medium",
  returnclass = "sf"
)

por_polys <- st_cast(por, "POLYGON")

por_polys$area <- st_area(por_polys)

por_cont <- por_polys |>
  slice_max(area, n = 1)

por_cont <- st_transform(
  por_cont,
  st_crs(mask_spain)
)

iberia_mask <- st_union(
  mask_spain,
  por_cont
)

iberia_mask <- st_transform(
  iberia_mask,
  25830
)

iberia_sp <- as(
  iberia_mask,
  "Spatial"
)

# --------------------------------------------------
# BASE RASTER
# --------------------------------------------------

base_raster <- raster(
  extent(iberia_sp),
  res = 300
)

base_raster[] <- 1

base_raster <- mask(
  base_raster,
  iberia_sp
)

# --------------------------------------------------
# BUILD ACCESSIBLE AREA
# --------------------------------------------------

build_p95_area <- function(sf_data, d95){
  
  ids <- unique(sf_data$birdID)
  
  buffers <- lapply(ids, function(id){
    
    ind_pts <- sf_data[
      sf_data$birdID == id,
    ]
    
    st_union(
      st_buffer(
        ind_pts,
        dist = d95
      )
    )
  })
  
  area <- do.call(c, buffers)
  
  area <- st_intersection(
    area,
    iberia_mask
  )
  
  min_buf <- st_union(
    st_buffer(
      sf_data,
      300
    )
  )
  
  st_difference(
    area,
    min_buf
  )
}

bbs_area <- build_p95_area(
  bbs_sf,
  bbs_p95
)

pts_area <- build_p95_area(
  pts_sf,
  pts_p95
)

# --------------------------------------------------
# CREATE DECAY RASTER
# --------------------------------------------------

create_decay_raster <- function(area_sf, lambda){
  
  area_raster <- rasterize(
    as(area_sf, "Spatial"),
    base_raster,
    field = 1,
    background = NA
  )
  
  dist_raster <- distance(
    area_raster
  )
  
  decay_raster <- exp(
    -dist_raster / lambda
  )
  
  decay_raster <- mask(
    decay_raster,
    base_raster
  )
  
  return(decay_raster)
}

bbs_decay <- create_decay_raster(
  bbs_area,
  bbs_p95
)

pts_decay <- create_decay_raster(
  pts_area,
  pts_p95
)

# --------------------------------------------------
# GENERATE PSEUDOABSENCES
# --------------------------------------------------

generate_pseudoabsences <- function(
    presences,
    prob_raster,
    n_per_presence = 5){
  
  total_n <- nrow(presences) *
    n_per_presence
  
  pts <- randomPoints(
    prob_raster,
    n = total_n,
    prob = TRUE,
    warn = 0
  )
  
  tibble(
    birdID   = NA,
    species  = rep(
      presences$species,
      each = n_per_presence
    ),
    date     = rep(
      presences$date,
      each = n_per_presence
    ),
    X_25830  = pts[,1],
    Y_25830  = pts[,2],
    presence = 0
  )
}

bbs_pseudo <- generate_pseudoabsences(
  bbs_data,
  bbs_decay,
  5
)

pts_pseudo <- generate_pseudoabsences(
  pts_data,
  pts_decay,
  5
)

# --------------------------------------------------
# COMBINE
# --------------------------------------------------

bbs_data$presence <- 1
pts_data$presence <- 1

bbs_final <- bind_rows(
  bbs_data,
  bbs_pseudo
)

pts_final <- bind_rows(
  pts_data,
  pts_pseudo
)

# --------------------------------------------------
# PLOTS
# --------------------------------------------------

x11()

plot(
  iberia_mask,
  col = "lightgrey"
)

points(
  bbs_pseudo$X_25830,
  bbs_pseudo$Y_25830,
  col = "red",
  pch = 20,
  cex = 0.1
)

x11()

plot(
  iberia_mask,
  col = "lightgrey"
)

points(
  pts_pseudo$X_25830,
  pts_pseudo$Y_25830,
  col = "red",
  pch = 20,
  cex = 0.1
)

# --------------------------------------------------
# SAVE
# --------------------------------------------------

write_csv(
  bbs_final,
  "E:/TFM_gangas/GPS/MergedV.2/BBS_pseudoabsences_P95_decay.csv"
)

write_csv(
  pts_final,
  "E:/TFM_gangas/GPS/MergedV.2/PTS_pseudoabsences_P95_decay.csv"
)

cat(
  "\nDone! P95 pseudo-absences generated.\n"
)


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





#=========================================
# FIGURE - PSEUDOABSENCES
#=========================================

library(sf)
library(tidyverse)
library(mapSpain)
library(rnaturalearth)
library(ggplot2)

rm(list = ls())
gc()

#-----------------------------------------
# IBERIAN PENINSULA MASK
#-----------------------------------------

provinces <- esp_get_prov()

provinces <- provinces[
  !provinces$iso2.prov.name.es %in%
    c("Las Palmas",
      "Santa Cruz de Tenerife",
      "Balearic Islands",
      "Ceuta",
      "Melilla"),
]

mask_spain <- st_union(provinces)

por <- ne_countries(
  country = "Portugal",
  scale = "medium",
  returnclass = "sf"
)

por_polys <- st_cast(por, "POLYGON")
por_polys$area <- st_area(por_polys)

por_cont <- por_polys |>
  slice_max(area, n = 1)

por_cont <- st_transform(
  por_cont,
  st_crs(mask_spain)
)

iberia_mask <- st_union(
  mask_spain,
  por_cont
)

iberia_mask <- st_transform(
  iberia_mask,
  25830
)

#-----------------------------------------
# READ DATA
#-----------------------------------------

data_path <- "E:/TFM_gangas/GPS/MergedV.2/"

read_data <- function(file, species, method){
  
  read_csv(file, show_col_types = FALSE) |>
    filter(presence == 0) |>
    mutate(
      Species = species,
      Method = method
    )
  
}

data <- bind_rows(
  
  read_data(
    paste0(data_path,"BBS_pseudoabsences_Random.csv"),
    "Pterocles orientalis",
    "Random"
  ),
  
  read_data(
    paste0(data_path,"PTS_pseudoabsences_Random.csv"),
    "Pterocles alchata",
    "Random"
  ),
  
  read_data(
    paste0(data_path,"BBS_pseudoabsences_P95_decay.csv"),
    "Pterocles orientalis",
    "P95"
  ),
  
  read_data(
    paste0(data_path,"PTS_pseudoabsences_P95_decay.csv"),
    "Pterocles alchata",
    "P95"
  ),
  
  read_data(
    paste0(data_path,"BBS_pseudoabsences_MCP40_decay.csv"),
    "Pterocles orientalis",
    "MCP40km"
  ),
  
  read_data(
    paste0(data_path,"PTS_pseudoabsences_MCP40_decay.csv"),
    "Pterocles alchata",
    "MCP40km"
  )
  
)

#-----------------------------------------
# CONVERT TO sf
#-----------------------------------------

data_sf <- st_as_sf(
  data,
  coords = c("X_25830","Y_25830"),
  crs = 25830
)

#-----------------------------------------
# FACET ORDER
#-----------------------------------------

data_sf$Method <- factor(
  data_sf$Method,
  levels = c(
    "Random",
    "P95",
    "MCP40km"
  )
)

data_sf$Species <- factor(
  data_sf$Species,
  levels = c(
    "Pterocles alchata",
    "Pterocles orientalis"
  )
)

#-----------------------------------------
# ITALIC SPECIES LABELS
#-----------------------------------------

species_labels <- as_labeller(c(
  "Pterocles alchata" = "Pterocles alchata",
  "Pterocles orientalis" = "Pterocles orientalis"
))

#-----------------------------------------
# FIGURE
#-----------------------------------------

p <- ggplot() +
  
  geom_sf(
    data = iberia_mask,
    fill = "grey94",
    colour = "grey55",
    linewidth = 0.25
  ) +
  
  geom_sf(
    data = subset(data_sf, Species == "Pterocles alchata"),
    colour = "#3B6FB6",
    size = 0.12,
    alpha = 0.65
  ) +
  
  geom_sf(
    data = subset(data_sf, Species == "Pterocles orientalis"),
    colour = "#D95F02",
    size = 0.12,
    alpha = 0.65
  ) +
  
  facet_grid(
    Method ~ Species,
    labeller = labeller(
      Species = label_value
    )
  ) +
  
  coord_sf(expand = FALSE) +
  
  theme_void() +
  
  theme(
    
    strip.background = element_rect(
      fill = "grey90",
      colour = "grey60"
    ),
    
    strip.text.y = element_text(
      face = "bold",
      size = 12
    ),
    
    strip.text.x = element_text(
      face = "italic",
      size = 12
    ),
    
    panel.spacing = unit(0.5, "lines")
    
  )

print(p)

ggsave(
  filename = "Pseudoabsence_6panel_Figure.png",
  plot = p,
  width = 8,
  height = 10,
  dpi = 600,
  bg = "white"
)
