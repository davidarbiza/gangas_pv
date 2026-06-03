############################################
# NATURA2000 PROPORTIONAL COVER (300 m)
############################################

library(terra)

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

base_dir <- "E:/TFM_gangas/PA"

pa_file <- file.path(
  base_dir,
  "Natura2000_end2024.gpkg"
)

ndvi_template_file <- "E:/TFM_gangas/NDVI/SpainReprojected/300m/c_gls_NDVI300_201601010000_GLOBE_PROBAV_V1.0.1_25830_300m.tif"

out_dir <- file.path(base_dir, "300m")

dir.create(out_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# LOAD NDVI TEMPLATE
# ------------------------------------------------------------

cat("\nLoading NDVI template...\n")

ndvi_template <- rast(ndvi_template_file)

# ------------------------------------------------------------
# LOAD NATURA2000
# ------------------------------------------------------------

cat("\nLoading Natura2000...\n")

v_pa <- vect(pa_file)

# ------------------------------------------------------------
# REPROJECT
# ------------------------------------------------------------

cat("\nReprojecting to EPSG:25830...\n")

v_pa <- project(v_pa, "EPSG:25830")

# ------------------------------------------------------------
# KEEP GEOMETRY ONLY
# ------------------------------------------------------------

v_pa <- v_pa[,0]

# ------------------------------------------------------------
# ADD FIELD
# ------------------------------------------------------------

v_pa$PA <- 1

# ------------------------------------------------------------
# RASTERIZE TO NDVI GRID
# ------------------------------------------------------------

cat("\nCalculating proportional protected cover...\n")

r_pa <- rasterize(
  v_pa,
  ndvi_template,
  field = "PA",
  background = 0,
  cover = TRUE
)

names(r_pa) <- "PA_PROP"

# ------------------------------------------------------------
# SAVE
# ------------------------------------------------------------

out_file <- file.path(
  out_dir,
  "Natura2000_PA_PROP_300m.tif"
)

writeRaster(
  r_pa,
  out_file,
  overwrite = TRUE
)

cat("\nSaved:\n")
print(out_file)

cat("\nNATURA2000 PROCESSING COMPLETED\n")


############################################
# SOLAR FARMS PROPORTIONAL COVER (300 m)
############################################

library(terra)

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

base_dir <- "E:/TFM_gangas/SolarFarms"

osm_file <- file.path(
  base_dir,
  "fotovoltaica_osm_con_fechas_160725_clustered_clasificada_actualizada.gpkg"
)

allsolar_file <- file.path(
  base_dir,
  "allsolar_cut_diss.shp"
)

ndvi_template_file <- "E:/TFM_gangas/NDVI/SpainReprojected/300m/c_gls_NDVI300_201601010000_GLOBE_PROBAV_V1.0.1_25830_300m.tif"

out_dir <- file.path(base_dir, "300m")

dir.create(out_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# LOAD NDVI TEMPLATE
# ------------------------------------------------------------

cat("\nLoading NDVI template...\n")

ndvi_template <- rast(ndvi_template_file)

# ------------------------------------------------------------
# LOAD DATASETS
# ------------------------------------------------------------

cat("\nLoading OSM dataset...\n")

v_osm <- vect(osm_file)

cat("\nLoading ALLSOLAR dataset...\n")

v_allsolar <- vect(allsolar_file)

# ------------------------------------------------------------
# REPROJECT
# ------------------------------------------------------------

cat("\nReprojecting datasets to EPSG:25830...\n")

v_osm <- project(v_osm, "EPSG:25830")

v_allsolar <- project(v_allsolar, "EPSG:25830")

# ------------------------------------------------------------
# KEEP GEOMETRY ONLY
# ------------------------------------------------------------

v_osm <- v_osm[,0]

v_allsolar <- v_allsolar[,0]

# ------------------------------------------------------------
# MERGE DATASETS
# ------------------------------------------------------------

cat("\nMerging datasets...\n")

v_solar <- rbind(v_osm, v_allsolar)

rm(v_osm, v_allsolar)

gc()

# ------------------------------------------------------------
# DISSOLVE OVERLAPS
# ------------------------------------------------------------

cat("\nDissolving overlaps...\n")

v_solar$ID <- 1

v_solar <- aggregate(v_solar, by = "ID")

# ------------------------------------------------------------
# RASTERIZE TO NDVI GRID
# ------------------------------------------------------------

cat("\nCalculating proportional PV cover...\n")

r_solar <- rasterize(
  v_solar,
  ndvi_template,
  field = "ID",
  background = 0,
  cover = TRUE
)

names(r_solar) <- "PV_PROP"

# ------------------------------------------------------------
# SAVE
# ------------------------------------------------------------

out_file <- file.path(
  out_dir,
  "SolarFarms_PV_PROP_300m.tif"
)

writeRaster(
  r_solar,
  out_file,
  overwrite = TRUE
)

cat("\nSaved:\n")
print(out_file)

cat("\nSOLAR PROCESSING COMPLETED\n")





############################################
# BUILD MASTER TABLE FOR GLM/GAM
############################################

library(terra)
library(data.table)

rm(list = ls())
gc()

dir.create("E:/temp_terra", showWarnings = FALSE)

terraOptions(
  memfrac = 0.7,
  progress = 1,
  tempdir = "E:/temp_terra"
)

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

base_path <- "E:/TFM_gangas/GPS/ExtractedV.4"

proj_dir <- file.path(
  base_path,
  "CALIBRATED_PROJECTIONS"
)

pa_file <- "E:/TFM_gangas/PA/300m/Natura2000_PA_PROP_300m.tif"

pv_file <- "E:/TFM_gangas/SolarFarms/300m/SolarFarms_PV_PROP_300m.tif"

out_dir <- file.path(
  base_path,
  "GLM_GAM_tables"
)

dir.create(out_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# SETTINGS
# ------------------------------------------------------------

species_list <- c("PTS", "BBS")

n_sample <- 250000

# ------------------------------------------------------------
# LOAD STATIC
# ------------------------------------------------------------

cat("\nLoading static rasters...\n")

r_pa <- rast(pa_file)
r_pv <- rast(pv_file)

# ------------------------------------------------------------
# BUILD GLOBAL VALID MASK
# ------------------------------------------------------------

cat("\nBuilding valid mask...\n")

mask_files <- sort(
  list.files(
    proj_dir,
    pattern = "^PTS_Random_dekad_.*CALIBRATED\\.tif$",
    full.names = TRUE
  )
)

r_mask <- rast(mask_files)

valid_mask <- app(
  !is.na(r_mask),
  sum
)

valid_mask <- valid_mask == nlyr(r_mask)

valid_mask[valid_mask == 0] <- NA
valid_mask[valid_mask == 1] <- 1

rm(r_mask)
gc()

# ------------------------------------------------------------
# SAMPLE PIXELS
# ------------------------------------------------------------

cat("\nSampling pixels...\n")

pts <- spatSample(
  valid_mask,
  size = n_sample,
  method = "random",
  as.points = TRUE,
  na.rm = TRUE
)

coords <- crds(pts)

cells <- cellFromXY(
  r_pa,
  coords
)

rm(valid_mask)
gc()

# ------------------------------------------------------------
# STATIC VARIABLES
# ------------------------------------------------------------

cat("\nExtracting static variables...\n")

static_vals <- extract(
  c(r_pa, r_pv),
  pts,
  ID = FALSE
)

pa_vals <- static_vals[,1]
pv_vals <- static_vals[,2]

rm(static_vals)
gc()

# ------------------------------------------------------------
# LOOP SPECIES
# ------------------------------------------------------------

for(sp in species_list){
  
  cat("\n=================================\n")
  cat("Species:", sp, "\n")
  cat("=================================\n")
  
  files <- sort(
    list.files(
      proj_dir,
      pattern = paste0(
        "^",
        sp,
        "_Random_dekad_.*CALIBRATED\\.tif$"
      ),
      full.names = TRUE
    )
  )
  
  all_dt <- vector(
    "list",
    length(files)
  )
  
  for(i in seq_along(files)){
    
    cat("\nDekad:", i, "\n")
    
    r <- rast(files[i])
    
    suit_vals <- extract(
      r,
      pts,
      ID = FALSE
    )[,1]
    
    dt <- data.table(
      cell = cells,
      x = coords[,1],
      y = coords[,2],
      species = sp,
      dekad = i,
      suitability = suit_vals,
      PA = pa_vals,
      PV = pv_vals
    )
    
    all_dt[[i]] <- dt
    
    rm(r, suit_vals, dt)
    gc()
  }
  
  final_dt <- rbindlist(all_dt)
  
  fwrite(
    final_dt,
    file.path(
      out_dir,
      paste0(
        sp,
        "_Random_master_table.csv"
      )
    )
  )
  
  rm(all_dt, final_dt)
  gc()
}

cat("\nMASTER TABLES READY\n")




############################################
# FINAL EXPLORATORY PIPELINE
############################################
library(data.table)
library(mgcv)
library(openxlsx)
library(ggplot2)

rm(list = ls())
gc()

# ----------------------------------------------------------
# PATHS
# ----------------------------------------------------------

base_dir <- "E:/TFM_gangas/GPS/ExtractedV.4"

table_dir <- file.path(
  base_dir,
  "GLM_GAM_tables"
)

out_dir <- file.path(
  table_dir,
  "Exploration"
)

dir.create(
  out_dir,
  showWarnings = FALSE
)

species_list <- c(
  "PTS",
  "BBS"
)

# ----------------------------------------------------------
# SETTINGS
# ----------------------------------------------------------

model_sample <- 250000

moran_sample <- 10000

eps <- 0.0001

# ----------------------------------------------------------
# LOOP
# ----------------------------------------------------------

for(sp in species_list){
  
  cat("\n====================================\n")
  cat("SPECIES:", sp, "\n")
  cat("====================================\n")
  
  # --------------------------------------------------------
  # LOAD
  # --------------------------------------------------------
  
  table_file <- file.path(
    table_dir,
    paste0(sp, "_Random_master_table.csv")
  )
  
  tbl <- fread(table_file)
  
  # --------------------------------------------------------
  # MODEL SAMPLE
  # --------------------------------------------------------
  
  set.seed(123)
  
  tbl_model <- tbl[
    sample(
      seq_len(.N),
      min(model_sample, .N)
    )
  ]
  
  # --------------------------------------------------------
  # RESPONSE
  # --------------------------------------------------------
  
  tbl_model[
    ,
    suitability_beta :=
      pmin(
        pmax(
          suitability,
          eps
        ),
        1 - eps
      )
  ]
  
  # --------------------------------------------------------
  # SCALE
  # --------------------------------------------------------
  
  tbl_model[
    ,
    PA_s := as.numeric(scale(PA))
  ]
  
  tbl_model[
    ,
    PV_s := as.numeric(scale(PV))
  ]
  
  tbl_model[
    ,
    dekad_c := dekad
  ]
  
  # --------------------------------------------------------
  # SUMMARY
  # --------------------------------------------------------
  
  summary_tbl <- data.frame(
    
    Variable = c(
      "Suitability",
      "PA",
      "PV"
    ),
    
    Min = c(
      min(tbl$suitability),
      min(tbl$PA),
      min(tbl$PV)
    ),
    
    Mean = c(
      mean(tbl$suitability),
      mean(tbl$PA),
      mean(tbl$PV)
    ),
    
    Median = c(
      median(tbl$suitability),
      median(tbl$PA),
      median(tbl$PV)
    ),
    
    Max = c(
      max(tbl$suitability),
      max(tbl$PA),
      max(tbl$PV)
    )
  )
  
  # --------------------------------------------------------
  # CORRELATION
  # --------------------------------------------------------
  
  cor_tbl <- data.frame(
    Correlation_PA_PV =
      cor(
        tbl$PA,
        tbl$PV
      )
  )
  
  # --------------------------------------------------------
  # GLM
  # --------------------------------------------------------
  
  glm_pa <- glm(
    suitability ~ PA_s,
    data = tbl_model,
    family = gaussian()
  )
  
  glm_pv <- glm(
    suitability ~ PV_s,
    data = tbl_model,
    family = gaussian()
  )
  
  glm_both <- glm(
    suitability ~ PA_s + PV_s,
    data = tbl_model,
    family = gaussian()
  )
  
  # --------------------------------------------------------
  # GAM
  # --------------------------------------------------------
  
  gam_pa <- bam(
    suitability_beta ~ s(PA_s),
    family = betar(link = "logit"),
    data = tbl_model,
    method = "fREML",
    discrete = TRUE
  )
  
  gam_pv <- bam(
    suitability_beta ~ s(PV_s),
    family = betar(link = "logit"),
    data = tbl_model,
    method = "fREML",
    discrete = TRUE
  )
  
  gam_both <- bam(
    suitability_beta ~
      s(PA_s) +
      s(PV_s),
    
    family = betar(link = "logit"),
    data = tbl_model,
    method = "fREML",
    discrete = TRUE
  )
  
  gam_pa_time <- bam(
    suitability_beta ~
      s(PA_s) +
      s(dekad_c, bs = "cc", k = 12),
    
    family = betar(link = "logit"),
    data = tbl_model,
    method = "fREML",
    discrete = TRUE
  )
  
  gam_pv_time <- bam(
    suitability_beta ~
      s(PV_s) +
      s(dekad_c, bs = "cc", k = 12),
    
    family = betar(link = "logit"),
    data = tbl_model,
    method = "fREML",
    discrete = TRUE
  )
  
  gam_time <- bam(
    suitability_beta ~
      s(PA_s) +
      s(PV_s) +
      s(dekad_c, bs = "cc", k = 12),
    
    family = betar(link = "logit"),
    data = tbl_model,
    method = "fREML",
    discrete = TRUE
  )
  
  gam_papv <- bam(
    suitability_beta ~
      s(PA_s) +
      s(PV_s) +
      ti(PA_s, PV_s),
    
    family = betar(link = "logit"),
    data = tbl_model,
    method = "fREML",
    discrete = TRUE
  )
  
  # --------------------------------------------------------
  # MODEL TABLE
  # --------------------------------------------------------
  
  model_tbl <- data.frame(
    
    Model = c(
      "GLM_PA",
      "GLM_PV",
      "GLM_BOTH",
      "GAM_PA",
      "GAM_PV",
      "GAM_BOTH",
      "GAM_PA_TIME",
      "GAM_PV_TIME",
      "GAM_TIME",
      "GAM_PAPV"
    ),
    
    AIC = c(
      AIC(glm_pa),
      AIC(glm_pv),
      AIC(glm_both),
      AIC(gam_pa),
      AIC(gam_pv),
      AIC(gam_both),
      AIC(gam_pa_time),
      AIC(gam_pv_time),
      AIC(gam_time),
      AIC(gam_papv)
    ),
    
    Deviance_Explained = c(
      
      1 - glm_pa$deviance / glm_pa$null.deviance,
      1 - glm_pv$deviance / glm_pv$null.deviance,
      1 - glm_both$deviance / glm_both$null.deviance,
      
      summary(gam_pa)$dev.expl,
      summary(gam_pv)$dev.expl,
      summary(gam_both)$dev.expl,
      summary(gam_pa_time)$dev.expl,
      summary(gam_pv_time)$dev.expl,
      summary(gam_time)$dev.expl,
      summary(gam_papv)$dev.expl
    )
  )
  
  # --------------------------------------------------------
  # EDF
  # --------------------------------------------------------
  
  edf_tbl <- data.frame(
    
    Model = c(
      "PA",
      "PV",
      "TIME_PA",
      "TIME_PV",
      "TIME_DEKAD"
    ),
    
    EDF = c(
      summary(gam_pa)$s.table[1,"edf"],
      summary(gam_pv)$s.table[1,"edf"],
      summary(gam_time)$s.table[1,"edf"],
      summary(gam_time)$s.table[2,"edf"],
      summary(gam_time)$s.table[3,"edf"]
    )
  )
  
  # --------------------------------------------------------
  # GAM SMOOTH TABLE
  # --------------------------------------------------------
  
  smooth_tbl <- as.data.frame(
    summary(gam_time)$s.table
  )
  
  smooth_tbl$Term <- rownames(
    summary(gam_time)$s.table
  )
  
  rownames(smooth_tbl) <- NULL
  
  # --------------------------------------------------------
  # GAM CHECK
  # --------------------------------------------------------
  
  capture.output(
    
    gam.check(
      gam_time
    ),
    
    file = file.path(
      out_dir,
      paste0(
        sp,
        "_GAM_CHECK.txt"
      )
    )
  )
  
 
  # --------------------------------------------------------
  # EXCEL
  # --------------------------------------------------------
  
  wb <- createWorkbook()
  
  addWorksheet(wb, "Summary")
  writeData(wb, "Summary", summary_tbl)
  
  addWorksheet(wb, "Correlation")
  writeData(wb, "Correlation", cor_tbl)
  
  addWorksheet(wb, "Models")
  writeData(wb, "Models", model_tbl)
  
  addWorksheet(wb, "EDF")
  writeData(wb, "EDF", edf_tbl)
  
  addWorksheet(wb, "Smooths")
  writeData(wb, "Smooths", smooth_tbl)

  saveWorkbook(
    wb,
    file.path(
      out_dir,
      paste0(
        sp,
        "_Exploration.xlsx"
      )
    ),
    overwrite = TRUE
  )
  
  # --------------------------------------------------------
  # RELATIONSHIPS
  # --------------------------------------------------------
  
  p_pa <- ggplot(
    tbl_model,
    aes(
      PA,
      suitability
    )
  ) +
    geom_point(
      alpha = 0.01,
      size = 0.15
    ) +
    geom_smooth(
      method = "gam"
    ) +
    theme_bw()
  
  p_pv <- ggplot(
    tbl_model,
    aes(
      PV,
      suitability
    )
  ) +
    geom_point(
      alpha = 0.01,
      size = 0.15
    ) +
    geom_smooth(
      method = "gam"
    ) +
    theme_bw()
  
  ggsave(
    file.path(
      out_dir,
      paste0(
        sp,
        "_Relationship_PA.png"
      )
    ),
    p_pa,
    width = 7,
    height = 5
  )
  
  ggsave(
    file.path(
      out_dir,
      paste0(
        sp,
        "_Relationship_PV.png"
      )
    ),
    p_pv,
    width = 7,
    height = 5
  )
  
  # --------------------------------------------------------
  # SMOOTHS
  # --------------------------------------------------------
  
  png(
    file.path(
      out_dir,
      paste0(
        sp,
        "_Smooths.png"
      )
    ),
    width = 2400,
    height = 1800,
    res = 250
  )
  
  par(
    mfrow = c(3,2)
  )
  
  plot(
    gam_pa,
    pages = 1
  )
  
  plot(
    gam_pv,
    pages = 1
  )
  
  plot(
    gam_time,
    pages = 1
  )
  
  dev.off()
}

cat(
  "\nFINAL EXPLORATION COMPLETED\n"
)



############################################
# GAM MARGINAL EFFECTS
############################################

library(data.table)
library(mgcv)
library(ggplot2)

rm(list = ls())
gc()

# ----------------------------------------------------------
# PATHS
# ----------------------------------------------------------

base_dir <- "E:/TFM_gangas/GPS/ExtractedV.4"

table_dir <- file.path(
  base_dir,
  "GLM_GAM_tables"
)

out_dir <- file.path(
  table_dir,
  "Marginal_Effects"
)

dir.create(
  out_dir,
  showWarnings = FALSE
)

species_list <- c(
  "PTS",
  "BBS"
)

# ----------------------------------------------------------
# SETTINGS
# ----------------------------------------------------------

model_sample <- 250000

eps <- 0.0001

# ----------------------------------------------------------
# LOOP
# ----------------------------------------------------------

for(sp in species_list){
  
  cat("\n========================\n")
  cat(sp,"\n")
  cat("========================\n")
  
  tbl <- fread(
    file.path(
      table_dir,
      paste0(
        sp,
        "_Random_master_table.csv"
      )
    )
  )
  
  set.seed(123)
  
  tbl <- tbl[
    sample(
      .N,
      min(model_sample,.N)
    )
  ]
  
  tbl[
    ,
    suitability_beta :=
      pmin(
        pmax(
          suitability,
          eps
        ),
        1-eps
      )
  ]
  
  tbl[
    ,
    PA_s := as.numeric(scale(PA))
  ]
  
  tbl[
    ,
    PV_s := as.numeric(scale(PV))
  ]
  
  tbl[
    ,
    dekad_c := dekad
  ]
  
  # ------------------------------------------------------
  # MODEL
  # ------------------------------------------------------
  
  gam_time <- bam(
    suitability_beta ~
      s(PA_s) +
      s(PV_s) +
      s(dekad_c, bs="cc", k=12),
    family = betar(link="logit"),
    data = tbl,
    method = "fREML",
    discrete = TRUE
  )
  
  # ------------------------------------------------------
  # PA EFFECT
  # ------------------------------------------------------
  
  pa_seq <- seq(
    min(tbl$PA_s),
    max(tbl$PA_s),
    length.out = 200
  )
  
  pa_new <- data.frame(
    PA_s = pa_seq,
    PV_s = mean(tbl$PV_s),
    dekad_c = mean(tbl$dekad_c)
  )
  
  pa_pred <- predict(
    gam_time,
    newdata = pa_new,
    se.fit = TRUE,
    type = "response"
  )
  
  pa_new$fit <- pa_pred$fit
  pa_new$lwr <- pa_pred$fit - 1.96*pa_pred$se.fit
  pa_new$upr <- pa_pred$fit + 1.96*pa_pred$se.fit
  
  p_pa <- ggplot(
    pa_new,
    aes(PA_s, fit)
  ) +
    geom_ribbon(
      aes(
        ymin = lwr,
        ymax = upr
      ),
      alpha = 0.3
    ) +
    geom_line(
      linewidth = 1
    ) +
    theme_bw() +
    labs(
      x = "Protected Area",
      y = "Predicted suitability"
    )
  
  # ------------------------------------------------------
  # PV EFFECT
  # ------------------------------------------------------
  
  pv_seq <- seq(
    min(tbl$PV_s),
    max(tbl$PV_s),
    length.out = 200
  )
  
  pv_new <- data.frame(
    PA_s = mean(tbl$PA_s),
    PV_s = pv_seq,
    dekad_c = mean(tbl$dekad_c)
  )
  
  pv_pred <- predict(
    gam_time,
    newdata = pv_new,
    se.fit = TRUE,
    type = "response"
  )
  
  pv_new$fit <- pv_pred$fit
  pv_new$lwr <- pv_pred$fit - 1.96*pv_pred$se.fit
  pv_new$upr <- pv_pred$fit + 1.96*pv_pred$se.fit
  
  p_pv <- ggplot(
    pv_new,
    aes(PV_s, fit)
  ) +
    geom_ribbon(
      aes(
        ymin = lwr,
        ymax = upr
      ),
      alpha = 0.3
    ) +
    geom_line(
      linewidth = 1
    ) +
    theme_bw() +
    labs(
      x = "PV cover",
      y = "Predicted suitability"
    )
  
  # ------------------------------------------------------
  # DEKAD EFFECT
  # ------------------------------------------------------
  
  dekad_new <- data.frame(
    PA_s = mean(tbl$PA_s),
    PV_s = mean(tbl$PV_s),
    dekad_c = 1:36
  )
  
  dekad_pred <- predict(
    gam_time,
    newdata = dekad_new,
    se.fit = TRUE,
    type = "response"
  )
  
  dekad_new$fit <- dekad_pred$fit
  dekad_new$lwr <- dekad_pred$fit - 1.96*dekad_pred$se.fit
  dekad_new$upr <- dekad_pred$fit + 1.96*dekad_pred$se.fit
  
  p_dekad <- ggplot(
    dekad_new,
    aes(dekad_c, fit)
  ) +
    geom_ribbon(
      aes(
        ymin = lwr,
        ymax = upr
      ),
      alpha = 0.3
    ) +
    geom_line(
      linewidth = 1
    ) +
    theme_bw() +
    labs(
      x = "Dekad",
      y = "Predicted suitability"
    )
  
  ggsave(
    file.path(
      out_dir,
      paste0(sp,"_PA_effect.png")
    ),
    p_pa,
    width = 7,
    height = 5
  )
  
  ggsave(
    file.path(
      out_dir,
      paste0(sp,"_PV_effect.png")
    ),
    p_pv,
    width = 7,
    height = 5
  )
  
  ggsave(
    file.path(
      out_dir,
      paste0(sp,"_Dekad_effect.png")
    ),
    p_dekad,
    width = 7,
    height = 5
  )
}

cat("\nMARGINAL EFFECTS COMPLETED\n")











############################################
# CONSERVATION SUMMARY TABLE
############################################

library(terra)
library(data.table)
library(openxlsx)

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

proj_dir <- "E:/TFM_gangas/GPS/ExtractedV.4/CALIBRATED_PROJECTIONS"

pa_file <- "E:/TFM_gangas/PA/300m/Natura2000_PA_PROP_300m.tif"

pv_file <- "E:/TFM_gangas/SolarFarms/300m/SolarFarms_PV_PROP_300m.tif"

out_dir <- file.path(
  proj_dir,
  "FINAL_PA_PV_RESULTS"
)

dir.create(
  out_dir,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# SPECIES
# ------------------------------------------------------------

species_list <- c(
  "PTS",
  "BBS"
)

species_names <- c(
  PTS = "Pterocles alchata",
  BBS = "Pterocles orientalis"
)

# ------------------------------------------------------------
# LOAD STATIC RASTERS
# ------------------------------------------------------------

cat("\nLoading static rasters...\n")

r_pa <- rast(pa_file)

r_pv <- rast(pv_file)

# ------------------------------------------------------------
# RESULTS
# ------------------------------------------------------------

results <- list()

# ------------------------------------------------------------
# LOOP SPECIES
# ------------------------------------------------------------

for(sp in species_list){
  
  cat(
    "\n=====================================\n",
    species_names[sp],
    "\n=====================================\n"
  )
  
  files <- sort(
    list.files(
      proj_dir,
      pattern = paste0(
        "^",
        sp,
        "_Random_.*CALIBRATED\\.tif$"
      ),
      full.names = TRUE
    )
  )
  
  # ----------------------------------------------------------
  # DETECT MIN / MAX PERIOD
  # ----------------------------------------------------------
  
  mean_suit_all <- numeric(
    length(files)
  )
  
  for(i in seq_along(files)){
    
    r <- rast(files[i])
    
    mean_suit_all[i] <- global(
      r,
      mean,
      na.rm = TRUE
    )[1,1]
    
    rm(r)
    gc()
  }
  
  min_dekad <- which.min(
    mean_suit_all
  )
  
  max_dekad <- which.max(
    mean_suit_all
  )
  
  cat(
    "\nMinimum suitability period:",
    min_dekad
  )
  
  cat(
    "\nMaximum suitability period:",
    max_dekad,
    "\n"
  )
  
  # ----------------------------------------------------------
  # LOOP DEKADS
  # ----------------------------------------------------------
  
  for(i in seq_along(files)){
    
    cat(
      "\nDekad:",
      i
    )
    
    r <- rast(
      files[i]
    )
    
    # --------------------------------------------------------
    # SUITABILITY
    # --------------------------------------------------------
    
    mean_suit <- global(
      r,
      mean,
      na.rm = TRUE
    )[1,1]
    
    # --------------------------------------------------------
    # FULL RASTER VALUES
    # --------------------------------------------------------
    
    suit_vals <- values(
      r,
      mat = FALSE
    )
    
    # remove NAs
    
    valid <- !is.na(suit_vals)
    
    suit_vals <- suit_vals[valid]
    
    pa_full <- values(
      r_pa,
      mat = FALSE
    )[valid]
    
    pv_full <- values(
      r_pv,
      mat = FALSE
    )[valid]
    
    # --------------------------------------------------------
    # MEAN SUITABILITY BY PROTECTION
    # --------------------------------------------------------
    
    mean_suit_PA <- mean(
      suit_vals[pa_full > 0],
      na.rm = TRUE
    )
    
    mean_suit_nonPA <- mean(
      suit_vals[pa_full == 0],
      na.rm = TRUE
    )
    
    # --------------------------------------------------------
    # MEAN SUITABILITY BY PV
    # --------------------------------------------------------
    
    mean_suit_PV <- mean(
      suit_vals[pv_full > 0],
      na.rm = TRUE
    )
    
    mean_suit_nonPV <- mean(
      suit_vals[pv_full == 0],
      na.rm = TRUE
    )
    
    # --------------------------------------------------------
    # HOTSPOTS
    # --------------------------------------------------------
    
    thr <- quantile(
      values(r),
      0.90,
      na.rm = TRUE
    )
    
    hotspot <- r >= thr
    
    hotspot_vals <- values(
      hotspot,
      mat = FALSE
    )
    
    pa_vals <- values(
      r_pa,
      mat = FALSE
    )
    
    pv_vals <- values(
      r_pv,
      mat = FALSE
    )
    
    idx <- which(
      hotspot_vals == 1
    )
    
    total_hotspots <- length(
      idx
    )
    
    pa_bin <- pa_vals[idx] > 0
    
    pv_bin <- pv_vals[idx] > 0
    
    # --------------------------------------------------------
    # CATEGORIES
    # --------------------------------------------------------
    
    pa_only <- sum(
      pa_bin & !pv_bin,
      na.rm = TRUE
    )
    
    pv_only <- sum(
      !pa_bin & pv_bin,
      na.rm = TRUE
    )
    
    both <- sum(
      pa_bin & pv_bin,
      na.rm = TRUE
    )
    
    outside <- sum(
      !pa_bin & !pv_bin,
      na.rm = TRUE
    )
    
    # --------------------------------------------------------
    # SAVE
    # --------------------------------------------------------
    
    results[[length(results)+1]] <-
      data.frame(
        
        species = sp,
        
        species_name =
          species_names[sp],
        
        dekad = i,
        
        mean_suitability =
          mean_suit,
        
        mean_suit_PA =
          mean_suit_PA,
        
        mean_suit_nonPA =
          mean_suit_nonPA,
        
        mean_suit_PV =
          mean_suit_PV,
        
        mean_suit_nonPV =
          mean_suit_nonPV,
        
        hotspot_cells =
          total_hotspots,
        
        perc_PA =
          pa_only / total_hotspots * 100,
        
        perc_PV =
          pv_only / total_hotspots * 100,
        
        perc_BOTH =
          both / total_hotspots * 100,
        
        perc_OUTSIDE =
          outside / total_hotspots * 100,
        
        max_dekad =
          max_dekad,
        
        min_dekad =
          min_dekad
      )
    
    rm(
      r,
      hotspot,
      hotspot_vals,
      pa_vals,
      pv_vals,
      idx
    )
    
    gc()
  }
}

# ------------------------------------------------------------
# COMBINE
# ------------------------------------------------------------

summary_table <- rbindlist(
  results
)

# ------------------------------------------------------------
# SAVE CSV
# ------------------------------------------------------------

fwrite(
  summary_table,
  file.path(
    out_dir,
    "Conservation_summary.csv"
  )
)

# ------------------------------------------------------------
# SAVE EXCEL
# ------------------------------------------------------------

write.xlsx(
  summary_table,
  file.path(
    out_dir,
    "Conservation_summary.xlsx"
  ),
  overwrite = TRUE
)


############################################
# FIGURE 1 - VIOLIN PLOTS
# Habitat suitability inside and outside
# Protected Areas and Solar Farms
############################################

library(terra)
library(data.table)
library(ggplot2)
library(dplyr)
library(patchwork)


rm(list = ls())
gc()

set.seed(123)

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

proj_dir <- "E:/TFM_gangas/GPS/ExtractedV.4/CALIBRATED_PROJECTIONS"

pa_file <- "E:/TFM_gangas/PA/300m/Natura2000_PA_PROP_300m.tif"

pv_file <- "E:/TFM_gangas/SolarFarms/300m/SolarFarms_PV_PROP_300m.tif"

out_dir <- file.path(
  proj_dir,
  "FINAL_PA_PV_RESULTS"
)

dir.create(
  out_dir,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# LOAD STATIC
# ------------------------------------------------------------

r_pa <- rast(pa_file)

r_pv <- rast(pv_file)

# ------------------------------------------------------------
# SPECIES
# ------------------------------------------------------------

species_list <- c(
  "PTS",
  "BBS"
)

species_names <- c(
  PTS = "P. alchata",
  BBS = "P. orientalis"
)

cols_species <- c(
  "P. alchata"    = "#4DBBD5",
  "P. orientalis" = "#E64B35"
)

# ------------------------------------------------------------
# BUILD DATASET
# ------------------------------------------------------------

all_data <- list()

for(sp in species_list){
  
  cat("\nProcessing:", sp, "\n")
  
  files <- sort(
    list.files(
      proj_dir,
      pattern = paste0(
        "^",
        sp,
        "_Random_.*CALIBRATED\\.tif$"
      ),
      full.names = TRUE
    )
  )
  
  r_stack <- rast(files)
  
  mean_r <- app(
    r_stack,
    mean,
    na.rm = TRUE
  )
  
  pts <- spatSample(
    mean_r,
    size = 250000,
    method = "random",
    as.points = TRUE,
    na.rm = TRUE
  )
  
  suit <- extract(
    mean_r,
    pts,
    ID = FALSE
  )[,1]
  
  pa <- extract(
    r_pa,
    pts,
    ID = FALSE
  )[,1]
  
  pv <- extract(
    r_pv,
    pts,
    ID = FALSE
  )[,1]
  
  all_data[[sp]] <- data.table(
    species = species_names[sp],
    suitability = suit,
    PA = ifelse(pa > 0,
                "Protected Area",
                "Outside Protected Area"),
    PV = ifelse(pv > 0,
                "Solar Farm",
                "Outside Solar Farm")
  )
  
  rm(
    r_stack,
    mean_r,
    pts,
    suit,
    pa,
    pv
  )
  
  gc()
}

violin_dt <- rbindlist(all_data)

# ------------------------------------------------------------
# PANEL A
# Protected Areas
# ------------------------------------------------------------

p_pa <- ggplot(
  violin_dt,
  aes(
    x = PA,
    y = suitability,
    fill = species
  )
) +
  
  geom_violin(
    trim = FALSE,
    alpha = 0.8,
    colour = NA
  ) +
  
  geom_boxplot(
    aes(
      group = interaction(PA, species)
    ),
    position = position_dodge(width = 0.9),
    width = 0.12,
    outlier.shape = NA,
    fill = "white"
  ) +
  
  scale_fill_manual(
    values = cols_species
  ) +
  
  labs(
    x = NULL,
    y = "Habitat suitability",
    title = "Protected Areas"
  ) +
  
  theme_bw(base_size = 14) +
  
  theme(
    plot.background =
      element_rect(fill = "white"),
    
    panel.background =
      element_rect(fill = "white"),
    
    legend.position = "bottom",
    
    legend.title = element_blank()
  )

# ------------------------------------------------------------
# PANEL B
# Solar Farms
# ------------------------------------------------------------

p_pv <- ggplot(
  violin_dt,
  aes(
    x = PV,
    y = suitability,
    fill = species
  )
) +
  
  geom_violin(
    trim = FALSE,
    alpha = 0.8,
    colour = NA
  ) +
  
  geom_boxplot(
    aes(
      group = interaction(PV, species)
    ),
    position = position_dodge(width = 0.9),
    width = 0.12,
    outlier.shape = NA,
    fill = "white"
  ) +
  
  scale_fill_manual(
    values = cols_species
  ) +
  
  labs(
    x = NULL,
    y = "Habitat suitability",
    title = "Solar Farms"
  ) +
  
  theme_bw(base_size = 14) +
  
  theme(
    plot.background =
      element_rect(fill = "white"),
    
    panel.background =
      element_rect(fill = "white"),
    
    legend.position = "bottom",
    
    legend.title = element_blank()
  )

# ------------------------------------------------------------
# COMBINE
# ------------------------------------------------------------

final_fig <- p_pa + p_pv +
  plot_layout(ncol = 2)

# ------------------------------------------------------------
# SAVE
# ------------------------------------------------------------

ggsave(
  file.path(
    out_dir,
    "Figure1_Violin.png"
  ),
  final_fig,
  width = 12,
  height = 6,
  dpi = 300,
  bg = "white"
)

cat(
  "\nFIGURE 1 SAVED\n"
)




############################################
# FIGURE 2
# Temporal dynamics of habitat suitability
############################################

library(tidyr)


rm(list = ls())

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

out_dir <- "E:/TFM_gangas/GPS/ExtractedV.4/CALIBRATED_PROJECTIONS/FINAL_PA_PV_RESULTS"

summary_table <- fread(
  file.path(
    out_dir,
    "Conservation_summary.csv"
  )
)

# ------------------------------------------------------------
# SPECIES LABELS
# ------------------------------------------------------------

summary_table$species_name <- factor(
  summary_table$species_name,
  levels = c(
    "Pterocles alchata",
    "Pterocles orientalis"
  ),
  labels = c(
    "P. alchata",
    "P. orientalis"
  )
)

# ------------------------------------------------------------
# MONTH LABELS
# ------------------------------------------------------------

month_pos <- c(
  2,5,8,11,14,17,
  20,23,26,29,32,35
)

month_lab <- c(
  "Jan","Feb","Mar","Apr",
  "May","Jun","Jul","Aug",
  "Sep","Oct","Nov","Dec"
)

# ------------------------------------------------------------
# PROTECTED AREAS
# ------------------------------------------------------------

plot_pa <- summary_table %>%
  select(
    species_name,
    dekad,
    mean_suit_PA,
    mean_suit_nonPA
  ) %>%
  pivot_longer(
    cols = c(
      mean_suit_PA,
      mean_suit_nonPA
    ),
    names_to = "Group",
    values_to = "Suitability"
  )

plot_pa$Group <- recode(
  plot_pa$Group,
  mean_suit_PA = "Protected Area",
  mean_suit_nonPA = "Outside Protected Area"
)

# ------------------------------------------------------------
# SOLAR FARMS
# ------------------------------------------------------------

plot_pv <- summary_table %>%
  select(
    species_name,
    dekad,
    mean_suit_PV,
    mean_suit_nonPV
  ) %>%
  pivot_longer(
    cols = c(
      mean_suit_PV,
      mean_suit_nonPV
    ),
    names_to = "Group",
    values_to = "Suitability"
  )

plot_pv$Group <- recode(
  plot_pv$Group,
  mean_suit_PV = "Solar Farm",
  mean_suit_nonPV = "Outside Solar Farm"
)

# ------------------------------------------------------------
# PANEL A
# PROTECTED AREAS
# ------------------------------------------------------------

p1 <- ggplot(
  plot_pa,
  aes(
    dekad,
    Suitability,
    colour = Group
  )
) +
  
  geom_line(
    linewidth = 1.2
  ) +
  
  facet_wrap(
    ~species_name,
    ncol = 1
  ) +
  
  scale_colour_manual(
    values = c(
      "Protected Area" = "#1B9E77",
      "Outside Protected Area" = "black"
    )
  ) +
  
  scale_x_continuous(
    breaks = month_pos,
    labels = month_lab
  ) +
  
  labs(
    x = NULL,
    y = "Habitat suitability",
    colour = NULL,
    title = "Protected Areas"
  ) +
  
  theme_bw(base_size = 14) +
  
  theme(
    legend.position = "top",
    plot.background =
      element_rect(fill = "white"),
    panel.background =
      element_rect(fill = "white"),
    strip.background =
      element_rect(fill = "grey95"),
    axis.text.x =
      element_blank(),
    axis.ticks.x =
      element_blank()
  )

# ------------------------------------------------------------
# PANEL B
# SOLAR FARMS
# ------------------------------------------------------------

p2 <- ggplot(
  plot_pv,
  aes(
    dekad,
    Suitability,
    colour = Group
  )
) +
  
  geom_line(
    linewidth = 1.2
  ) +
  
  facet_wrap(
    ~species_name,
    ncol = 1
  ) +
  
  scale_colour_manual(
    values = c(
      "Solar Farm" = "#D95F02",
      "Outside Solar Farm" = "black"
    )
  ) +
  
  scale_x_continuous(
    breaks = month_pos,
    labels = month_lab
  ) +
  
  labs(
    x = NULL,
    y = "Habitat suitability",
    colour = NULL,
    title = "Solar Farms"
  ) +
  
  theme_bw(base_size = 14) +
  
  theme(
    legend.position = "top",
    plot.background =
      element_rect(fill = "white"),
    panel.background =
      element_rect(fill = "white"),
    strip.background =
      element_rect(fill = "grey95")
  )

# ------------------------------------------------------------
# COMBINE
# ------------------------------------------------------------

final_fig <- p1 / p2 +
  plot_layout(
    heights = c(1,1)
  )

# ------------------------------------------------------------
# SAVE
# ------------------------------------------------------------

ggsave(
  file.path(
    out_dir,
    "Figure2_TemporalSuitability.png"
  ),
  final_fig,
  width = 11,
  height = 10,
  dpi = 300,
  bg = "white"
)

cat(
  "\nFIGURE 2 SAVED\n"
)
############################################
# FIGURE 3
# OVERLAP MAPS
############################################

library(terra)
library(sf)
library(ggplot2)
library(tidyterra)
library(patchwork)
library(mapSpain)
library(rnaturalearth)

rm(list = ls())
gc()

dir.create("E:/terra_temp", showWarnings = FALSE)

terraOptions(
  memfrac = 0.7,
  tempdir = "E:/terra_temp",
  progress = 1
)

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

proj_dir <- "E:/TFM_gangas/GPS/ExtractedV.4/CALIBRATED_PROJECTIONS"
pa_file  <- "E:/TFM_gangas/PA/300m/Natura2000_PA_PROP_300m.tif"
pv_file  <- "E:/TFM_gangas/SolarFarms/300m/SolarFarms_PV_PROP_300m.tif"

out_dir <- file.path(proj_dir, "FINAL_PA_PV_RESULTS")
dir.create(out_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# IBERIA MASK
# ------------------------------------------------------------

provinces <- esp_get_prov()
provinces <- provinces[!provinces$iso2.prov.name.es %in% 
                         c("Baleares", "Las Palmas", "Santa Cruz de Tenerife", "Ceuta", "Melilla"), ]

provinces <- st_transform(provinces, 25830)
mask_spain <- st_union(provinces)

por <- ne_countries(country = "Portugal", scale = "medium", returnclass = "sf")
por <- st_transform(por, 25830)
por <- st_cast(por, "POLYGON")
por$area <- st_area(por)
por <- por[which.max(por$area), ]

iberia <- st_union(mask_spain, por)
iberia_sf <- st_as_sf(iberia)
iberia_vect <- vect(iberia)

# ------------------------------------------------------------
# STATIC RASTERS
# ------------------------------------------------------------

r_pa <- rast(pa_file)
r_pv <- rast(pv_file)

# ------------------------------------------------------------
# FUNCTION
# ------------------------------------------------------------

make_panel <- function(species, dekad, title_text){
  
  file <- file.path(proj_dir, paste0(species, "_Random_dekad_", dekad, "_CALIBRATED.tif"))
  r <- rast(file)
  r <- mask(r, iberia_vect)
  
  thr <- quantile(values(r), probs = 0.90, na.rm = TRUE)
  hotspot <- r >= thr
  hotspot <- mask(hotspot, iberia_vect)
  
  hotspot_vals <- values(hotspot, mat = FALSE)
  pa_vals      <- values(r_pa, mat = FALSE)
  pv_vals      <- values(r_pv, mat = FALSE)
  
  class_vals <- rep(NA, length(hotspot_vals))
  idx <- which(hotspot_vals == 1)
  
  pa_bin <- pa_vals[idx] > 0
  pv_bin <- pv_vals[idx] > 0
  
  class_vals[idx] <- ifelse(
    pa_bin & !pv_bin, 1,
    ifelse(!pa_bin & pv_bin, 2,
           ifelse(pa_bin & pv_bin, 2, 4)
    )
  )
  
  class_r <- hotspot
  values(class_r) <- class_vals
  
  class_df <- as.data.frame(class_r, xy = TRUE, na.rm = TRUE)
  names(class_df)[3] <- "Class"
  
  class_df$Class <- factor(
    class_df$Class,
    levels = c(1, 2, 4),
    labels = c("Protected only", "Solar farms only", "Neither")
  )
  
  p <- ggplot() +
    geom_tile(
      data = class_df,
      aes(x = x, y = y, fill = Class)
    ) +
    
    geom_sf(
      data = iberia_sf,
      fill = NA,
      colour = "black",
      linewidth = 0.3
    ) +
    
    coord_sf() +
    
    scale_fill_manual(
      values = c(
        "Protected only"   = "#2E8B57",
        "Solar farms only" = "#FF0033",
        "Neither"          = "grey70"
      ),
      name = NULL
    ) +
    
    labs(title = title_text) +
    theme_bw(base_size = 13) +
    
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      legend.position = "bottom"
    )
  
  return(p)
}

# ------------------------------------------------------------
# PANELS
# ------------------------------------------------------------

p1 <- make_panel("PTS", 14, "P. alchata\nMinimum suitability\n(Mid May)")
p2 <- make_panel("PTS", 24, "P. alchata\nMaximum suitability\n(Late August)")
p3 <- make_panel("BBS", 12, "P. orientalis\nMinimum suitability\n(Late April)")
p4 <- make_panel("BBS", 24, "P. orientalis\nMaximum suitability\n(Late August)")

# ------------------------------------------------------------
# COMBINE
# ------------------------------------------------------------

final_plot <- (p1 | p2) / (p3 | p4)
# ------------------------------------------------------------
# SAVE
# ------------------------------------------------------------

ggsave(
  file.path(out_dir, "Figure_3_Overlap_Maps.png"),
  final_plot,
  width = 14,
  height = 13,
  dpi = 600,
  bg = "white"
)

cat("\nFIGURE 3 READY\n")



























############################################
# FIGURE 2
# Temporal dynamics
############################################

# ------------------------------------------------------------
# LOAD SUMMARY
# ------------------------------------------------------------

summary_table <- fread(
  "E:/TFM_gangas/GPS/ExtractedV.4/CALIBRATED_PROJECTIONS/FINAL_PA_PV_RESULTS/Conservation_summary.csv"
)

# ------------------------------------------------------------
# MONTH LABELS
# ------------------------------------------------------------

month_pos <- c(
  2,5,8,11,14,17,
  20,23,26,29,32,35
)

month_lab <- c(
  "Jan","Feb","Mar","Apr",
  "May","Jun","Jul","Aug",
  "Sep","Oct","Nov","Dec"
)

# ------------------------------------------------------------
# TOTAL PA / PV
# ------------------------------------------------------------

summary_table <- summary_table %>%
  mutate(
    
    hotspot_PA =
      perc_PA + perc_BOTH,
    
    hotspot_PV =
      perc_PV + perc_BOTH
  )

# ------------------------------------------------------------
# NORMALISE
# ------------------------------------------------------------

norm_fun <- function(x){
  
  (x - min(x)) /
    (max(x) - min(x))
}

plot_dt <- summary_table %>%
  group_by(species_name) %>%
  mutate(
    
    Suitability =
      norm_fun(mean_suitability),
    
    `Protected Areas` =
      norm_fun(hotspot_PA),
    
    `Solar Farms` =
      norm_fun(hotspot_PV)
    
  ) %>%
  ungroup() %>%
  tidyr::pivot_longer(
    
    cols = c(
      Suitability,
      `Protected Areas`,
      `Solar Farms`
    ),
    
    names_to = "Variable",
    values_to = "Value"
  )

# ------------------------------------------------------------
# SPECIES LABELS
# ------------------------------------------------------------

plot_dt$species_name <-
  factor(
    plot_dt$species_name,
    levels = c(
      "Pterocles alchata",
      "Pterocles orientalis"
    ),
    labels = c(
      "P. alchata",
      "P. orientalis"
    )
  )

# ------------------------------------------------------------
# FIGURE
# ------------------------------------------------------------

p <- ggplot(
  plot_dt,
  aes(
    dekad,
    Value,
    colour = Variable
  )
) +
  
  geom_line(
    linewidth = 1.2
  ) +
  
  facet_wrap(
    ~species_name,
    ncol = 1
  ) +
  
  scale_colour_manual(
    values = c(
      "Suitability" = "black",
      "Protected Areas" = "#1B9E77",
      "Solar Farms" = "#D95F02"
    )
  ) +
  
  scale_x_continuous(
    breaks = month_pos,
    labels = month_lab
  ) +
  
  labs(
    x = NULL,
    y = "Relative value (0–1)",
    colour = NULL
  ) +
  
  theme_bw(base_size = 14) +
  
  theme(
    
    legend.position = "top",
    
    plot.background =
      element_rect(
        fill = "white",
        colour = "white"
      ),
    
    panel.background =
      element_rect(
        fill = "white",
        colour = "white"
      ),
    
    strip.background =
      element_rect(
        fill = "grey95"
      )
  )

# ------------------------------------------------------------
# SAVE
# ------------------------------------------------------------

ggsave(
  "E:/TFM_gangas/GPS/ExtractedV.4/CALIBRATED_PROJECTIONS/FINAL_PA_PV_RESULTS/Figure2_TemporalDynamics.png",
  p,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)