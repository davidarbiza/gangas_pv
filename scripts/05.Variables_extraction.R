# ============================================================
# VARIABLE EXTRACTION SCRIPT – TFM GANGAS
# ============================================================

### NON-CLIMATIC VARIABLES

library(terra)
library(sf)
library(dplyr)
library(lubridate)
library(rnaturalearth)
library(tictoc)

# ---- Terra temp & memory settings ----
dir.create("C:/Users/andre/AppData/Local/Temp/terra_tmpC", showWarnings = FALSE)
terraOptions(
  tempdir  = "C:/Users/andre/AppData/Local/Temp/terra_tmpC",
  memfrac  = 0.6,
  progress = 1
)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
base_dir <- "E:/TFM_gangas"
gps_dir  <- file.path(base_dir, "GPS", "MergedV.2")
out_dir  <- file.path(base_dir, "GPS", "ExtractedV.4")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

csv_files <- file.path(
  gps_dir,
  c(
    "BBS_pseudoabsences_MCP40_decay.csv",
    "BBS_pseudoabsences_P95_decay.csv",
    "BBS_pseudoabsences_Random.csv",
    "PTS_pseudoabsences_MCP40_decay.csv",
    "PTS_pseudoabsences_P95_decay.csv",
    "PTS_pseudoabsences_Random.csv"
  )
)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
nearest_year <- function(y, years){
  years[which.min(abs(years - y))]
}

nearest_index <- function(x, ref){
  which.min(abs(ref - x))
}

# ------------------------------------------------------------
# Portugal polygon (continental)
# ------------------------------------------------------------
por <- ne_countries(country = "Portugal", scale = "medium", returnclass = "sf")
por_polys <- st_cast(por, "POLYGON")
por_polys$area <- st_area(por_polys)
por_cont <- por_polys %>% slice_max(area, n = 1)
por_cont <- st_transform(por_cont, 25830)

# ============================================================
# MAIN LOOP
# ============================================================
for(csv in csv_files){
  
  tic(paste("Total time:", basename(csv)))
  cat("\nProcessing:", basename(csv), "\n")
  
  # ----------------------------------------------------------
  # Read points
  # ----------------------------------------------------------
  pts <- read.csv(csv, stringsAsFactors = FALSE)
  pts$date <- as.Date(pts$date)
  pts$year <- year(pts$date)
  
  pts_vect <- vect(pts, geom = c("X_25830","Y_25830"), crs = "EPSG:25830")
  pts_sf   <- st_as_sf(pts_vect)
  
  # ----------------------------------------------------------
  # Define spatial extent (buffered)
  # ----------------------------------------------------------
  ext_pts  <- ext(pts_vect)
  ext_crop <- ext(
    ext_pts$xmin - 5000, ext_pts$xmax + 5000,
    ext_pts$ymin - 5000, ext_pts$ymax + 5000
  )
  
  # ==========================================================
  # CROP RASTERS ONCE (NON-CLIMATIC ONLY)
  # ==========================================================
  tic("Cropping rasters")
  
  dem  <- crop(rast("E:/TFM_gangas/Topograficas/300m/Spain_DEM_reproject_300m.tif"), ext_crop)
  slp  <- crop(rast("E:/TFM_gangas/Topograficas/300m/Slope_map_Spain_300m.tif"), ext_crop)
  asp  <- crop(rast("E:/TFM_gangas/Topograficas/300m/Orientation_map_Spain_300m.tif"), ext_crop)
  rng  <- crop(rast("E:/TFM_gangas/Topograficas/300m/Altitudinal_range_Spain_300m.tif"), ext_crop)
  
  topo_stack <- c(dem, slp, asp, rng)
  names(topo_stack) <- c("Altitude","Slope","Aspect","AltRange")
  
  dist_roads <- crop(
    rast(list.files(file.path(base_dir,"DistanciaCarreteras","300m"),
                    pattern="\\.tif$", full.names=TRUE)),
    ext_crop
  )
  names(dist_roads) <- "DistRoad"
  
  heterogeneity <- crop(
    rast(list.files(file.path(base_dir,"Heterogeneidad","300m"),
                    pattern="\\.tif$", full.names=TRUE)),
    ext_crop
  )
  names(heterogeneity) <- "Heterogeneity"
  
  pop_files <- list.files(file.path(base_dir,"DensidadPoblacion","300m"),
                          pattern="\\.tif$", full.names=TRUE)
  pop_years <- as.numeric(regmatches(pop_files, regexpr("[0-9]{4}", pop_files)))
  pop_stack <- crop(rast(pop_files), ext_crop)
  
  hfp_files <- list.files(file.path(base_dir,"HumanFootprint","300m"),
                          pattern="\\.tif$", full.names=TRUE)
  hfp_years <- as.numeric(regmatches(hfp_files, regexpr("[0-9]{4}", hfp_files)))
  hfp_stack <- crop(rast(hfp_files), ext_crop)
  
  ndvi_files <- list.files(file.path(base_dir,"NDVI","SpainReprojected","300m"),
                           pattern="\\.tif$", full.names=TRUE)
  ndvi_dates <- as.Date(regmatches(ndvi_files, regexpr("[0-9]{8}", ndvi_files)), "%Y%m%d")
  
  toc()
  
  # ==========================================================
  # STATIC VARIABLES
  # ==========================================================
  tic("Static variables")
  
  static_stack <- c(topo_stack, dist_roads, heterogeneity)
  static_vals  <- terra::extract(static_stack, pts_vect)[,-1]
  
  static_vals$Heterogeneity <- static_vals$Heterogeneity / 10000
  pts <- bind_cols(pts, static_vals)
  
  toc()
  
  # ==========================================================
  # Population
  # ==========================================================
  tic("Population")
  
  pts$Population <- NA_real_
  pop_use_year <- sapply(pts$year, nearest_year, years = pop_years)
  
  for(yr in unique(pop_use_year)){
    idx <- which(pop_use_year == yr)
    r   <- pop_stack[[which(pop_years == yr)]]
    pts$Population[idx] <- terra::extract(r, pts_vect[idx,])[,2]
  }
  
  toc()
  
  # ==========================================================
  # Human Footprint
  # ==========================================================
  tic("Human Footprint")
  
  pts$HFP <- NA_real_
  hfp_use_year <- pts$year
  hfp_use_year[hfp_use_year < min(hfp_years)] <- min(hfp_years)
  hfp_use_year[hfp_use_year > max(hfp_years)] <- max(hfp_years)
  
  for(yr in unique(hfp_use_year)){
    idx <- which(hfp_use_year == yr)
    r   <- hfp_stack[[which(hfp_years == yr)]]
    pts$HFP[idx] <- terra::extract(r, pts_vect[idx,])[,2] / 1000
  }
  
  toc()
  
  # ==========================================================
  # NDVI
  # ==========================================================
  tic("NDVI")
  
  pts$NDVI <- NA_real_
  
  # Extract year and 10-day block
  pts$year  <- year(pts$date)
  pts$block <- ceiling(yday(pts$date) / 10)
  
  # Extract NDVI dates from filenames
  ndvi_dates <- as.Date(regmatches(ndvi_files, regexpr("[0-9]{8}", ndvi_files)), "%Y%m%d")
  ndvi_year  <- year(ndvi_dates)
  ndvi_block <- ceiling(yday(ndvi_dates) / 10)
  
  for(yr in unique(pts$year)){
    
    idx_year <- which(pts$year == yr)
    
    for(b in unique(pts$block[idx_year])){
      
      idx <- idx_year[pts$block[idx_year] == b]
      
      # Find matching NDVI file for year + block
      match_file <- which(ndvi_year == yr & ndvi_block == b)
      
      if(length(match_file) == 0) next
      
      r <- crop(rast(ndvi_files[match_file[1]]), ext_crop)
      
      pts$NDVI[idx] <- terra::extract(r, pts_vect[idx,])[,2]
      
      rm(r); gc()
    }
  }
  
  toc()
  
  # ==========================================================
  # Land use
  # ==========================================================
  tic("Land use")
  
  is_portugal <- lengths(st_intersects(pts_sf, por_cont)) > 0
  
  # ---- COS2023 (Portugal) ----
  cos_files <- list.files(file.path(base_dir,"UsosSuelo","COS2023","300m"),
                          pattern="\\.tif$", full.names=TRUE)
  cos_stack <- crop(rast(cos_files), ext_crop)
  
  lc_names <- c(
    "LC_Forest","LC_Vineyards","LC_TreeCrops","LC_RiceFields",
    "LC_Greenhouses","LC_AnnualCrops","LC_TreePasture","LC_ShrubPasture","LC_HerbPasture",
    "LC_WaterBodies","LC_Marshes","LC_Artificial","LC_OtherLand","LC_AgriMosaic"
  )
  names(cos_stack) <- lc_names
  
  for(nm in lc_names){
    pts[[nm]] <- NA_real_
  }
  
  pts[is_portugal, lc_names] <-
    terra::extract(cos_stack, pts_vect[is_portugal,])[, -1]
  
  # ---- LULUCF (Spain) ----
  lulucf_files <- list.files(file.path(base_dir,"UsosSuelo","LULUCF","300m"),
                             pattern="\\.tif$", full.names=TRUE)
  lulucf_years <- as.numeric(regmatches(lulucf_files, regexpr("[0-9]{4}", lulucf_files)))
  
  for(yr in unique(pts$year[!is_portugal])){
    idx <- which(!is_portugal & pts$year == yr)
    use_yr <- nearest_year(yr, lulucf_years)
    r <- crop(rast(lulucf_files[lulucf_years == use_yr]), ext_crop)
    names(r) <- lc_names
    pts[idx, lc_names] <- terra::extract(r, pts_vect[idx,])[, -1]
    rm(r); gc()
  }
  
  toc()
  
  # ==========================================================
  # SAVE
  # ==========================================================
  pts_final <- dplyr::select(pts,
  birdID, date, X_25830, Y_25830, species, presence,
  Altitude, Slope, Aspect, AltRange,
  Heterogeneity, NDVI,
  Population, HFP, DistRoad, all_of(lc_names)
)
  
  out_file <- file.path(
    out_dir,
    paste0(tools::file_path_sans_ext(basename(csv)), "_env.csv")
  )
  write.csv(pts_final, out_file, row.names = FALSE)
  
  cat("Saved:", basename(out_file), "\n")
  toc()
  
  rm(list = ls()[!ls() %in% c("base_dir","gps_dir","out_dir","csv_files",
                              "nearest_year","nearest_index","por_cont")])
  gc()
}




# ============================================================
# CLIMATIC VARIABLES EXTRACTION – TFM GANGAS
# ============================================================

library(terra)
library(lubridate)
library(tictoc)

base_dir <- "E:/TFM_gangas"
in_dir   <- file.path(base_dir,"GPS","ExtractedV.4")

csv_files <- list.files(in_dir, pattern="_env\\.csv$", full.names=TRUE)

clim_base <- file.path(base_dir,"Climaticas","10_days")

clim_files <- list(

  Tmin = list.files(
    file.path(clim_base,"Tmin","300m"),
    pattern = "^Tmin_mean_.*\\.tif$",
    full.names = TRUE
  ),

  TminSD100 = list.files(
    file.path(clim_base,"Tmin","300m"),
    pattern = "^Tmin_sd_.*\\.tif$",
    full.names = TRUE
  ),

  Tmax = list.files(
    file.path(clim_base,"Tmax","300m"),
    pattern = "^Tmax_mean_.*\\.tif$",
    full.names = TRUE
  ),

  TmaxSD100 = list.files(
    file.path(clim_base,"Tmax","300m"),
    pattern = "^Tmax_sd_.*\\.tif$",
    full.names = TRUE
  ),

  Prcp = list.files(
    file.path(clim_base,"Prcp","300m"),
    pattern = "^Prcp_sum_.*\\.tif$",
    full.names = TRUE
  )
)

for(csv in csv_files){
  
  tic(basename(csv))
  
  pts <- read.csv(csv, stringsAsFactors = FALSE)
  pts$date <- as.Date(pts$date)
  
  pts_vect <- vect(pts, geom = c("X_25830","Y_25830"), crs = "EPSG:25830")
  band_idx <- ceiling(yday(pts$date) / 10)
  
  for(v in names(clim_files)){
    pts[[v]] <- NA_real_
    years_pts <- unique(year(pts$date))
    
    for(yr in years_pts){
      
      # Select raster for correct year
      r_file <- clim_files[[v]][grepl(yr, clim_files[[v]])]
      
      r <- rast(r_file)
      
      idx_year <- which(year(pts$date) == yr)
      
      bidx <- band_idx[idx_year]
      
      bidx[bidx > nlyr(r)] <- nlyr(r)
      
      for(b in unique(bidx)){
        
        idx <- idx_year[bidx == b]
        
        pts[[v]][idx] <- terra::extract(
          r[[b]],
          pts_vect[idx,]
        )[,2] / 100
      }
      
      rm(r)
      gc()
    }
  }
  write.csv(pts, csv, row.names = FALSE)
  toc()
}




# ============================================================
# CLIMATIC VARIABLES EXTRACTION – TFM GANGAS (TMEAN ONLY)
# ============================================================

library(terra)
library(lubridate)
library(tictoc)

base_dir <- "E:/TFM_gangas"
in_dir   <- file.path(base_dir,"GPS","ExtractedV.4")

csv_files <- file.path(
  in_dir,
  c("BBS_pseudoabsences_P95_decay2_env.csv",
    "PTS_pseudoabsences_P95_decay2_env.csv"
  )
)

clim_base <- file.path(base_dir,"Climaticas","10_days")

clim_files <- list(
  Tmean      = list.files(file.path(clim_base,"Tmean","300m"),
                          pattern="Tmean_mean_.*_300m\\.tif$",
                          full.names=TRUE),
  
  TmeanSD100 = list.files(file.path(clim_base,"Tmean","300m"),
                          pattern="Tmean_sd_.*_300m\\.tif$",
                          full.names=TRUE)
)

for(csv in csv_files){
  
  tic(basename(csv))
  
  pts <- read.csv(csv, stringsAsFactors = FALSE)
  pts$date <- as.Date(pts$date)
  
  pts_vect <- vect(pts, geom = c("X_25830","Y_25830"), crs = "EPSG:25830")
  
  band_idx <- ceiling(yday(pts$date) / 10)
  
  for(v in names(clim_files)){
    
    pts[[v]] <- NA_real_
    
    years_pts <- unique(year(pts$date))
    
    for(yr in years_pts){
      
      r_file <- clim_files[[v]][grepl(yr, clim_files[[v]])]
      if(length(r_file) == 0) next
      
      r <- rast(r_file)
      
      idx_year <- which(year(pts$date) == yr)
      bidx <- band_idx[idx_year]
      
      bidx[bidx > nlyr(r)] <- nlyr(r)
      
      for(b in unique(bidx)){
        idx <- idx_year[which(bidx == b)]
        pts[[v]][idx] <- terra::extract(r[[b]], pts_vect[idx,])[,2] / 100
      }
      
      rm(r); gc()
    }
  }
  
  write.csv(pts, csv, row.names = FALSE)
  toc()
}






# ============================================================
# CHECK CORRELATION AND VIF
# ============================================================
library(dplyr)
library(openxlsx)
library(ggplot2)
library(car)
library(usdm)
library(patchwork)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
base_path <- "E:/TFM_gangas/GPS/ExtractedV.4"

files <- list(
  PTS = file.path(base_path, "PTS_pseudoabsences_Random_env.csv"),
  BBS = file.path(base_path, "BBS_pseudoabsences_Random_env.csv")
)

# ------------------------------------------------------------
# Create workbook
# ------------------------------------------------------------
wb <- createWorkbook()

plots_heatmap <- list()

for(sp in names(files)) {
  
  cat("\nProcessing:", sp, "\n")
  
  env <- read.csv(files[[sp]])
  
  # Remove non-predictor variables
  env_clean <- env %>%
    dplyr::select(-birdID,
                  -date,
                  -species,
                  -X_25830,
                  -Y_25830,
                  -presence
    )
  
  env_clean <- na.omit(env_clean)
  
  # ------------------------------------------------------------
  # CORRELATION MATRIX
  # ------------------------------------------------------------
  
  cor_matrix <- cor(env_clean, method = "pearson")
  
  cor_matrix[lower.tri(cor_matrix)] <- NA
  
  addWorksheet(wb, paste0(sp, "_Correlation"))
  writeData(wb, paste0(sp, "_Correlation"), cor_matrix)
  
  # ------------------------------------------------------------
  # HEATMAP
  # ------------------------------------------------------------
  
  cor_df <- as.data.frame(as.table(cor_matrix))
  
  cor_df <- cor_df[!is.na(cor_df$Freq), ]
  
  p <- ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
    
    geom_tile() +
    
    scale_fill_gradient2(
      low = "#3B4CC0",
      mid = "white",
      high = "#B40426",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Pearson r"
    ) +
    
    coord_fixed() +
    
    theme_minimal(base_size = 12) +
    
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right",
      plot.title = element_text(face = "bold", hjust = 0.5)
    ) +
    
    labs(title = if(sp == "PTS") expression(italic("P. alchata"))
         else expression(italic("P. orientalis")))
  
  plots_heatmap[[sp]] <- p
  
  # ------------------------------------------------------------
  # VIF
  # ------------------------------------------------------------
  
  vif_values <- vif(env_clean)
  
  vif_df <- data.frame(
    Variable = vif_values$Variables,
    VIF = vif_values$VIF
  )
  
  addWorksheet(wb, paste0(sp, "_VIF"))
  writeData(wb, paste0(sp, "_VIF"), vif_df)
}

# ------------------------------------------------------------
# COMBINE HEATMAPS
# ------------------------------------------------------------

p_combined <- plots_heatmap[[1]] + plots_heatmap[[2]] +
  plot_layout(ncol = 2)

ggsave(
  file.path(base_path, "Correlation_heatmaps_combined.png"),
  p_combined,
  width = 12,
  height = 6,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------
# Save Excel
# ------------------------------------------------------------
saveWorkbook(
  wb,
  file.path(base_path, "Correlation_VIF_Random.xlsx"),
  overwrite = TRUE
)

cat("\nCorrelation and VIF analysis completed\n")





####################################################################
# Environmental distributions of presences and pseudoabsences
####################################################################

library(dplyr)
library(ggplot2)
library(tidyr)
library(openxlsx)

set.seed(123)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

base_path <- "E:/TFM_gangas/GPS/ExtractedV.4"

plot_dir <- file.path(
  base_path,
  "Environmental_niche_plots"
)

dir.create(plot_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# Species and pseudoabsence methods
# ------------------------------------------------------------

species_list <- c("PTS", "BBS")

methods <- data.frame(
  
  method = c(
    "Random",
    "P95",
    "MCP40"
  ),
  
  file = c(
    "pseudoabsences_Random_env.csv",
    "pseudoabsences_P95_decay_env.csv",
    "pseudoabsences_MCP40_decay_env.csv"
  ),
  
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Excel workbook
# ------------------------------------------------------------

wb <- createWorkbook()

############################################
# MAIN LOOP
############################################

for (sp in species_list) {
  
  for (m in 1:nrow(methods)) {
    
    cat("\n====================================\n")
    cat("Species:", sp, "\n")
    cat("Method:", methods$method[m], "\n")
    cat("====================================\n")
    
    # ------------------------------------------------------------
    # Load data
    # ------------------------------------------------------------
    
    env <- read.csv(
      file.path(
        base_path,
        paste0(sp, "_", methods$file[m])
      )
    )
    
    # ------------------------------------------------------------
    # Prepare data
    # ------------------------------------------------------------
    
    env <- env %>%
      
      mutate(
        presence = factor(
          presence,
          levels = c(0,1),
          labels = c(
            "Pseudoabsence",
            "Presence"
          )
        )
      ) %>%
      
      dplyr::select(
        -birdID,
        -date,
        -species,
        -X_25830,
        -Y_25830
      ) %>%
      
      na.omit()
    
    # ------------------------------------------------------------
    # Keep only predictors
    # ------------------------------------------------------------
    
    predictors <- env %>%
      dplyr::select(-presence)
    
    # ------------------------------------------------------------
    # Summary statistics
    # ------------------------------------------------------------
    
    summary_table <- data.frame(
      Variable = names(predictors),
      Mean_presence = NA,
      SD_presence = NA,
      Mean_absence = NA,
      SD_absence = NA,
      Difference_mean = NA
    )
    
    for (i in seq_along(predictors)) {
      
      var_name <- names(predictors)[i]
      
      pres_vals <- predictors[[var_name]][env$presence == "Presence"]
      
      abs_vals  <- predictors[[var_name]][env$presence == "Pseudoabsence"]
      
      summary_table$Mean_presence[i] <- mean(pres_vals)
      
      summary_table$SD_presence[i] <- sd(pres_vals)
      
      summary_table$Mean_absence[i] <- mean(abs_vals)
      
      summary_table$SD_absence[i] <- sd(abs_vals)
      
      summary_table$Difference_mean[i] <-
        summary_table$Mean_presence[i] -
        summary_table$Mean_absence[i]
    }
    
    # ------------------------------------------------------------
    # Save Excel sheet
    # ------------------------------------------------------------
    
    sheet_name <- paste(
      sp,
      methods$method[m],
      sep = "_"
    )
    
    addWorksheet(
      wb,
      sheet_name
    )
    
    writeData(
      wb,
      sheet_name,
      summary_table
    )
    
    # ------------------------------------------------------------
    # Long format for plots
    # ------------------------------------------------------------
    
    env_long <- env %>%
      pivot_longer(
        cols = -presence,
        names_to = "variable",
        values_to = "value"
      )
    
    # ------------------------------------------------------------
    # Plot
    # ------------------------------------------------------------
    
    p <- ggplot(
      env_long,
      aes(
        x = value,
        fill = presence
      )
    ) +
      
      geom_density(
        alpha = 0.5,
        color = NA
      ) +
      
      facet_wrap(
        ~variable,
        scales = "free",
        ncol = 4
      ) +
      
      scale_fill_manual(
        values = c(
          "Pseudoabsence" = "#BDBDBD",
          "Presence" = "#2C7FB8"
        )
      ) +
      
      theme_classic(base_size = 14) +
      
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        
        strip.text = element_text(
          face = "bold",
          size = 11
        ),
        
        axis.title = element_text(
          face = "bold"
        ),
        
        axis.text = element_text(
          color = "black"
        ),
        
        plot.title = element_text(
          face = "bold",
          hjust = 0.5,
          size = 14
        )
      ) +
      
      labs(
        title = paste(
          "Environmental niche distributions -",
          sp,
          "-",
          methods$method[m]
        ),
        
        subtitle = "Presence vs pseudoabsence distributions",
        
        x = "Environmental gradient",
        
        y = "Density"
      )
    
    # ------------------------------------------------------------
    # Save plot
    # ------------------------------------------------------------
    
    ggsave(
      filename = paste0(
        "Environmental_niche_",
        sp,
        "_",
        methods$method[m],
        ".png"
      ),
      
      plot = p,
      
      path = plot_dir,
      
      width = 15,
      height = 10,
      dpi = 300
    )
    
    rm(
      env,
      predictors,
      summary_table,
      env_long,
      p
    )
    
    gc()
  }
}

# ------------------------------------------------------------
# Save Excel workbook
# ------------------------------------------------------------

saveWorkbook(
  wb,
  file.path(
    plot_dir,
    "Environmental_niche_summary.xlsx"
  ),
  overwrite = TRUE
)

cat("\nFINISHED — Excel and plots saved\n")







# ============================================================
# COUNT DATASETS AND CREATE SUMMARY TABLES
# ============================================================

library(dplyr)
library(openxlsx)

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------
base_gps <- "E:/TFM_gangas/GPS"
merged_path <- file.path(base_gps, "MergedV.2")
output_path <- file.path(base_gps, "ExtractedV.4")

# ------------------------------------------------------------
# FUNCTION
# ------------------------------------------------------------
count_rows <- function(file){
  nrow(read.csv(file))
}

# ============================================================
# TABLE 1 — FILTERING AND MERGING
# ============================================================

# -------------------------------
# 1. Original data
# -------------------------------
files_gps <- list.files(base_gps, pattern = "\\.csv$", full.names = TRUE)

files_pts <- files_gps[grepl("PTS", files_gps)]
files_bbs <- files_gps[grepl("BBS", files_gps)]

n_pts_raw <- sum(sapply(files_pts, count_rows))
n_bbs_raw <- sum(sapply(files_bbs, count_rows))

# -------------------------------
# 2. Subsampling
# -------------------------------
n_pts_sub <- count_rows(file.path(merged_path, "PTS_filtered_merged.csv"))
n_bbs_sub <- count_rows(file.path(merged_path, "BBS_filtered_merged.csv"))

# -------------------------------
# 3. No pseudoreplication
# -------------------------------
n_pts_final <- count_rows(file.path(merged_path, "PTS_filtered_NoPseudoreplication.csv"))
n_bbs_final <- count_rows(file.path(merged_path, "BBS_filtered_NoPseudoreplication.csv"))

# -------------------------------
# Table 1
# -------------------------------
table1 <- data.frame(
  Stage = rep(c("Original", "Subsampling", "Final"), 2),
  Species = rep(c("PTS","BBS"), each = 3),
  Presences = c(n_pts_raw, n_pts_sub, n_pts_final,
                n_bbs_raw, n_bbs_sub, n_bbs_final),
  Total = c(n_pts_raw, n_pts_sub, n_pts_final,
            n_bbs_raw, n_bbs_sub, n_bbs_final)
)

# ============================================================
# TABLE 2 — PSEUDOABSENCES
# ============================================================

methods <- c("Random", "P95_decay", "MCP40_decay")

table2_list <- list()

for(sp in c("PTS","BBS")){
  
  for(m in methods){
    
    file <- file.path(merged_path,
                      paste0(sp, "_pseudoabsences_", m, ".csv"))
    
    df <- read.csv(file)
    
    n_pres <- sum(df$presence == 1)
    n_abs  <- sum(df$presence == 0)
    
    table2_list[[paste(sp, m, sep="_")]] <- data.frame(
      Method = m,
      Species = sp,
      Presences = n_pres,
      Pseudoabsences = n_abs,
      Total = n_pres + n_abs
    )
  }
}

table2 <- bind_rows(table2_list)

# ============================================================
# SAVE EXCEL
# ============================================================

wb <- createWorkbook()

addWorksheet(wb, "Filter")
writeData(wb, "Filter", table1)

addWorksheet(wb, "Pseudoabsences")
writeData(wb, "Pseudoabsences", table2)

saveWorkbook(
  wb,
  file.path(output_path, "COUNTING_DATA.xlsx"),
  overwrite = TRUE
)

cat("\nTables finished\n")