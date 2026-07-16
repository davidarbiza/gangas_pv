############################################
# SDM PROJECTION — INTERANNUAL ENSEMBLE
############################################

library(terra)
library(sf)
library(stringr)
library(rnaturalearth)
library(randomForest)

rm(list=ls())
gc()

dir.create("E:/temp_terra", showWarnings = FALSE)

terraOptions(
  memfrac = 0.4,
  tempdir = "E:/temp_terra",
  progress = 1
)

base_path  <- "E:/TFM_gangas"
model_path <- "E:/TFM_gangas/GPS/ExtractedV.4/CV_models"

out_dir <- file.path(
  model_path,
  "PROJECTIONS_INTERANNUAL"
)

dir.create(out_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# TARGET YEARS / DEKADS
# ------------------------------------------------------------

target_years <- c(
  2016,
  2018,
  2020,
  2022,
  2024
)

species_dekads <- list(
  PTS = c(14,24),
  BBS = c(12,24)
)

# ------------------------------------------------------------
# MODELS
# ------------------------------------------------------------

methods <- c("Random")

load_models <- function(species, method){
  
  files <- sort(
    list.files(
      model_path,
      pattern = paste0(
        "RF_",
        species,
        "_",
        method,
        "_fold[1-5]\\.rds"
      ),
      full.names = TRUE
    )
  )
  
  lapply(files, readRDS)
  
}

models <- list()

for(sp in c("PTS","BBS")){
  
  for(met in methods){
    
    models[[paste(sp, met, sep="_")]] <-
      load_models(sp, met)
    
  }
  
}

# ------------------------------------------------------------
# STATIC VARIABLES
# ------------------------------------------------------------

dem <- rast(
  file.path(
    base_path,
    "Topograficas/300m/Spain_DEM_reproject_300m.tif"
  )
)

slope <- rast(
  file.path(
    base_path,
    "Topograficas/300m/Slope_map_Spain_300m.tif"
  )
)

hetero <- rast(
  file.path(
    base_path,
    "Heterogeneidad/300m/shannon_01_05_1km_Spain_25830_300m.tif"
  )
)

roads <- rast(
  file.path(
    base_path,
    "DistanciaCarreteras/300m/Distroads_spain_merged_300m.tif"
  )
)

names(dem)    <- "Altitude"
names(slope)  <- "Slope"
names(hetero) <- "Heterogeneity"
names(roads)  <- "DistRoad"

# ------------------------------------------------------------
# YEAR-DEPENDENT VARIABLES
# ------------------------------------------------------------

get_pop_year <- function(year){
  
  if(year == 2016) return(2015)
  if(year == 2018) return(2020)
  if(year == 2020) return(2020)
  if(year == 2022) return(2020)
  if(year == 2024) return(2025)
  
}

get_hfp_year <- function(year){
  
  if(year <= 2020){
    return(year)
  }
  
  return(2020)
  
}

get_lulucf_year <- function(year){
  
  if(year == 2016){
    return(2015)
  }
  
  if(year == 2018){
    return(2018)
  }
  
  return(2021)
  
}

# ------------------------------------------------------------
# LAND COVER
# ------------------------------------------------------------

cos <- rast(
  file.path(
    base_path,
    "UsosSuelo/COS2023/300m/COS2023_LC_300m.tif"
  )
)

lc_names <- c(
  "LC_Forest","LC_Vineyards","LC_TreeCrops","LC_RiceFields",
  "LC_Greenhouses","LC_AnnualCrops","LC_TreePasture","LC_ShrubPasture",
  "LC_HerbPasture","LC_WaterBodies","LC_Marshes","LC_Artificial",
  "LC_OtherLand","LC_AgriMosaic"
)

names(cos) <- lc_names

# ============================================================
# BUILD LULUCF
# ============================================================

build_lulucf <- function(year){
  
  lulucf_year <- get_lulucf_year(year)
  
  lulucf <- rast(
    file.path(
      base_path,
      "UsosSuelo/LULUCF/300m",
      paste0(
        "LULUCF_LC_",
        lulucf_year,
        "_300m.tif"
      )
    )
  )
  
  names(lulucf) <- lc_names
  
  lulucf_files <- c()
  
  for(i in 1:nlyr(lulucf)){
    
    r_port <- mask(
      cos[[i]],
      por_vect
    )
    
    r_final <- cover(
      r_port,
      lulucf[[i]]
    )
    
    out_file <- file.path(
      "E:/temp_terra",
      paste0(
        "lulucf_",
        year,
        "_",
        i,
        ".tif"
      )
    )
    
    writeRaster(
      r_final,
      out_file,
      overwrite = TRUE
    )
    
    lulucf_files[i] <- out_file
    
    rm(r_port, r_final)
    
    gc()
    
  }
  
  out <- rast(lulucf_files)
  
  names(out) <- lc_names
  
  out
  
}

# ------------------------------------------------------------
# PORTUGAL
# ------------------------------------------------------------

por <- ne_countries(
  country = "Portugal",
  scale = "medium",
  returnclass = "sf"
)

por <- st_transform(por, 25830)
por <- st_cast(por, "POLYGON")

por$area <- st_area(por)

por <- por[which.max(por$area), ]

por_vect <- vect(por)

# ------------------------------------------------------------
# CLIMATE
# ------------------------------------------------------------

ndvi_path <- file.path(
  base_path,
  "NDVI/SpainReprojected/300m"
)

ndvi_files <- sort(
  list.files(
    ndvi_path,
    full.names = TRUE
  )
)

dates <- as.Date(
  str_extract(
    basename(ndvi_files),
    "[0-9]{8}"
  ),
  "%Y%m%d"
)

# ------------------------------------------------------------
# YEAR-SPECIFIC LOADERS
# ------------------------------------------------------------

load_population <- function(year){
  
  pop_year <- get_pop_year(year)
  
  r <- rast(
    file.path(
      base_path,
      "DensidadPoblacion/300m",
      paste0(
        "GHS_POP_",
        pop_year,
        "_25830_300m.tif"
      )
    )
  )
  
  names(r) <- "Population"
  
  r
  
}

load_hfp <- function(year){
  
  hfp_year <- get_hfp_year(year)
  
  r <- rast(
    file.path(
      base_path,
      "HumanFootprint/300m",
      paste0(
        "hfp_",
        hfp_year,
        "_100m_25830_300m.tif"
      )
    )
  )
  
  names(r) <- "HFP"
  
  r
  
}

load_tmean <- function(year){
  
  rast(
    file.path(
      base_path,
      "Climaticas/10_days/Tmean/300m",
      paste0(
        "Tmean_mean_",
        year,
        "_10d_300m.tif"
      )
    )
  )
  
}

load_tmeansd <- function(year){
  
  rast(
    file.path(
      base_path,
      "Climaticas/10_days/Tmean/300m",
      paste0(
        "Tmean_sd_",
        year,
        "_10d_300m.tif"
      )
    )
  )
  
}

load_prcp <- function(year){
  
  rast(
    file.path(
      base_path,
      "Climaticas/10_days/Prcp/300m",
      paste0(
        "Prcp_sum_",
        year,
        "_10d_300m.tif"
      )
    )
  )
  
}

# ============================================================
# MAIN LOOP
# ============================================================

for(year in target_years){
  
  cat("\n====================================\n")
  cat("YEAR:", year, "\n")
  cat("====================================\n")
  
  pop <- load_population(year)
  hfp <- load_hfp(year)
  
  lulucf_final <- build_lulucf(year)
  
  tmean_stack   <- load_tmean(year)
  tmeansd_stack <- load_tmeansd(year)
  prcp_stack    <- load_prcp(year)
  
  for(model_name in names(models)){
    
    parts <- strsplit(model_name, "_")[[1]]
    
    sp  <- parts[1]
    met <- parts[2]
    
    dekads <- species_dekads[[sp]]
    
    for(band_i in dekads){
      
      t0 <- Sys.time()
      
      cat(
        "\nPROJECTING:",
        sp,
        "| YEAR:",
        year,
        "| DEKAD:",
        band_i,
        "\n"
      )
      
      out_mean <- file.path(
        out_dir,
        paste0(
          sp,
          "_",
          met,
          "_YEAR_",
          year,
          "_DEKAD_",
          sprintf("%02d", band_i),
          "_INTERANNUAL.tif"
        )
      )
      
      if(file.exists(out_mean)){
        
        cat("Already exists. Skipping.\n")
        
        next
        
      }
      
      ndvi_idx <- which(
        format(dates,"%Y") == as.character(year) &
          (
            ((as.numeric(format(dates,"%m")) - 1) * 3) +
              ceiling(as.numeric(format(dates,"%d")) / 10)
          ) == band_i
      )
      
      ndvi_m <- rast(ndvi_files[ndvi_idx])
      
      names(ndvi_m) <- "NDVI"
      
      tmean_m   <- tmean_stack[[band_i]]
      tmeansd_m <- tmeansd_stack[[band_i]]
      prcp_m    <- prcp_stack[[band_i]]
      
      names(tmean_m)   <- "Tmean"
      names(tmeansd_m) <- "TmeanSD100"
      names(prcp_m)    <- "Prcp"
      
      env_stack <- c(
        
        dem,
        slope,
        hetero,
        ndvi_m,
        pop,
        hfp,
        roads,
        
        lulucf_final[[c(
          "LC_Forest",
          "LC_Vineyards",
          "LC_TreeCrops",
          "LC_AnnualCrops",
          "LC_TreePasture",
          "LC_ShrubPasture",
          "LC_HerbPasture",
          "LC_WaterBodies",
          "LC_Marshes",
          "LC_Artificial",
          "LC_OtherLand",
          "LC_AgriMosaic"
        )]],
        
        prcp_m,
        tmean_m,
        tmeansd_m
        
      )
      
      fold_files <- c()
      
      for(i in 1:5){
        
        cat("   Fold", i, "\n")
        
        out_fold <- file.path(
          "E:/temp_terra",
          paste0(
            sp,
            "_",
            met,
            "_YEAR_",
            year,
            "_DEKAD_",
            sprintf("%02d", band_i),
            "_fold_",
            i,
            ".tif"
          )
        )
        
        terra::predict(
          
          env_stack,
          
          models[[model_name]][[i]],
          
          fun = function(model, data, ...){
            
            predict(
              model,
              newdata = data,
              type = "prob"
            )[,2]
            
          },
          
          filename = out_fold,
          overwrite = TRUE
          
        )
        
        fold_files[i] <- out_fold
        
        gc()
        
      }
      
      pred_stack <- rast(fold_files)
      
      pred_mean <- app(
        
        pred_stack,
        
        mean,
        
        filename = file.path(
          "E:/temp_terra",
          paste0(
            sp,
            "_",
            met,
            "_",
            year,
            "_mean_tmp.tif"
          )
        ),
        
        overwrite = TRUE
        
      )
      
      writeRaster(
        pred_mean,
        out_mean,
        overwrite = TRUE
      )
      
      file.remove(fold_files)
      
      file.remove(
        file.path(
          "E:/temp_terra",
          paste0(
            sp,
            "_",
            met,
            "_",
            year,
            "_mean_tmp.tif"
          )
        )
      )
      
      elapsed <- round(
        as.numeric(
          difftime(
            Sys.time(),
            t0,
            units = "secs"
          )
        ),
        1
      )
      
      cat(
        "Finished:",
        sp,
        "| Year:",
        year,
        "| Dekad:",
        band_i,
        "|",
        elapsed,
        "sec\n"
      )
      
      rm(
        pred_stack,
        pred_mean,
        ndvi_m,
        tmean_m,
        tmeansd_m,
        prcp_m,
        env_stack
      )
      
      gc()
      
    }
    
  }
  
  rm(
    pop,
    hfp,
    lulucf_final,
    tmean_stack,
    tmeansd_stack,
    prcp_stack
  )
  
  gc()
  
}

cat("\nINTERANNUAL ENSEMBLE PROJECTIONS FINISHED\n")



############################################

# FIGURE 2 — INTERANNUAL MAPS

############################################

library(terra)
library(ggplot2)
library(tidyterra)
library(sf)
library(mapSpain)
library(rnaturalearth)
library(patchwork)

rm(list = ls())
gc()

dir.create("E:/terra_temp", showWarnings = FALSE)

terraOptions(
  memfrac = 0.6,
  tempdir = "E:/terra_temp",
  progress = 1
)

# ------------------------------------------------------------

# PATHS

# ------------------------------------------------------------

base_path <- "E:/TFM_gangas/GPS/ExtractedV.4/CV_models/PROJECTIONS_INTERANNUAL"

out_dir <- file.path(
  base_path,
  "FIGURE2_INTERANNUAL"
)

dir.create(
  out_dir,
  showWarnings = FALSE
)

# ------------------------------------------------------------

# MASK

# ------------------------------------------------------------

provinces <- esp_get_prov()

provinces <- provinces[
  !provinces$iso2.prov.name.es %in%
    c(
      "Baleares",
      "Las Palmas",
      "Santa Cruz de Tenerife",
      "Ceuta",
      "Melilla"
    ),
]

provinces <- st_transform(
  provinces,
  25830
)

mask_spain <- st_union(
  provinces
)

por <- ne_countries(
  country = "Portugal",
  scale = "medium",
  returnclass = "sf"
)

por <- st_transform(
  por,
  25830
)

por <- st_cast(
  por,
  "POLYGON"
)

por$area <- st_area(
  por
)

por <- por[
  which.max(por$area),
]

iberia_mask <- st_union(
  mask_spain,
  por
)

mask_vect <- vect(
  iberia_mask
)

# ------------------------------------------------------------

# GLOBAL SCALE

# ------------------------------------------------------------

all_files <- list.files(
  base_path,
  pattern = "_INTERANNUAL\\.tif$",
  full.names = TRUE
)

all_stack <- rast(
  all_files
)

global_min <- minmax(all_stack)[1]
global_max <- minmax(all_stack)[2]

rm(all_stack)

gc()

# ------------------------------------------------------------

# MAP FUNCTION

# ------------------------------------------------------------

plot_map <- function(r, title){
  
  r <- focal(
    r,
    w = 3,
    fun = mean,
    na.rm = TRUE
  )
  
  ggplot() +
    
    geom_spatraster(
      data = r
    ) +
    
    geom_sf(
      data = iberia_mask,
      fill = NA,
      color = "black",
      linewidth = 0.2
    ) +
    
    scale_fill_viridis_c(
      option = "inferno",
      limits = c(
        global_min,
        global_max
      ),
      name = "Suitability"
    ) +
    
    theme_void(
      base_size = 14
    ) +
    
    theme(
      plot.background = element_rect(
        fill = "white",
        color = "white"
      ),
      panel.background = element_rect(
        fill = "white",
        color = "white"
      ),
      legend.position = "right",
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      )
    ) +
    
    labs(
      title = title
    )
  
}

# ------------------------------------------------------------

# YEARS

# ------------------------------------------------------------

years <- c(
  2016,
  2018,
  2020,
  2022,
  2024
)

# ============================================================

# PTS

# ============================================================

for(year in years){
  
  f14 <- list.files(
    base_path,
    pattern = paste0(
      "^PTS_Random_YEAR_",
      year,
      "_DEKAD_14_INTERANNUAL\\.tif$"
    ),
    full.names = TRUE
  )
  
  f24 <- list.files(
    base_path,
    pattern = paste0(
      "^PTS_Random_YEAR_",
      year,
      "_DEKAD_24_INTERANNUAL\\.tif$"
    ),
    full.names = TRUE
  )
  
  r14 <- mask(
    crop(
      rast(f14),
      mask_vect
    ),
    mask_vect
  )
  
  r24 <- mask(
    crop(
      rast(f24),
      mask_vect
    ),
    mask_vect
  )
  
  p1 <- plot_map(
    r14,
    paste0(
      "P. alchata — Dekad 14 (",
      year,
      ")"
    )
  )
  
  p2 <- plot_map(
    r24,
    paste0(
      "P. alchata — Dekad 24 (",
      year,
      ")"
    )
  )
  
  p_final <- p1 + p2
  
  ggsave(
    file.path(
      out_dir,
      paste0(
        "PTS_",
        year,
        ".png"
      )
    ),
    p_final,
    width = 10,
    height = 5,
    dpi = 300,
    bg = "white"
  )
  
}

# ============================================================

# BBS

# ============================================================

for(year in years){
  
  f12 <- list.files(
    base_path,
    pattern = paste0(
      "^BBS_Random_YEAR_",
      year,
      "_DEKAD_12_INTERANNUAL\\.tif$"
    ),
    full.names = TRUE
  )
  
  f24 <- list.files(
    base_path,
    pattern = paste0(
      "^BBS_Random_YEAR_",
      year,
      "_DEKAD_24_INTERANNUAL\\.tif$"
    ),
    full.names = TRUE
  )
  
  r12 <- mask(
    crop(
      rast(f12),
      mask_vect
    ),
    mask_vect
  )
  
  r24 <- mask(
    crop(
      rast(f24),
      mask_vect
    ),
    mask_vect
  )
  
  p1 <- plot_map(
    r12,
    paste0(
      "P. orientalis — Dekad 12 (",
      year,
      ")"
    )
  )
  
  p2 <- plot_map(
    r24,
    paste0(
      "P. orientalis — Dekad 24 (",
      year,
      ")"
    )
  )
  
  p_final <- p1 + p2
  
  ggsave(
    file.path(
      out_dir,
      paste0(
        "BBS_",
        year,
        ".png"
      )
    ),
    p_final,
    width = 10,
    height = 5,
    dpi = 300,
    bg = "white"
  )
  
}

cat("\nFIGURE 2 INTERANNUAL READY\n")
