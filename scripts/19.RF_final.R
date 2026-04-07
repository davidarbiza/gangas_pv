############################################
# FINAL RANDOM FOREST MODEL
# + Variable importance
# + Partial Dependence Plots
############################################

# -------------------------------
# Libraries
# -------------------------------
library(randomForest)
library(dplyr)
library(pdp)
library(ggplot2)
library(openxlsx)

set.seed(999)

# -------------------------------
# Paths
# -------------------------------
base_path <- "E:/TFM_gangas/GPS/ExtractedV.2"

# -------------------------------
# Species
# -------------------------------
species_list <- c("PTS", "BBS")

# -------------------------------
# Selected pseudoabsence method
# -------------------------------
file_name <- "pseudoabsences_Random_env.csv"

# -------------------------------
# Storage objects
# -------------------------------
importance_final <- data.frame()
all_pdp_final <- data.frame()

############################################
# LOOP BY SPECIES
############################################

for (sp in species_list) {
  
  cat("\n=============================\n")
  cat("FINAL MODEL:", sp, "\n")
  cat("=============================\n")
  
  ##########################################
  # LOAD DATA
  ##########################################
  
  data_pts <- read.csv(
    file.path(base_path, paste0(sp, "_", file_name))
  )
  
  ##########################################
  # DATA PREPARATION
  ##########################################
  
  data_model <- data_pts %>%
    mutate(presence = factor(presence, levels = c(0,1))) %>%
    dplyr::select(
      -birdID,
      -date,
      -species,
      -X_25830,
      -Y_25830,
      -Tmin,
      -Tmax,
      -TminSD100,
      -TmaxSD100,
      -AltRange,
      -Aspect,
      -LC_RiceFields,
      -LC_Greenhouses
    ) %>%
    na.omit()
  
  ##########################################
  # BALANCED SAMPLING
  ##########################################
  
  n_pres <- sum(data_model$presence == 1)
  sampsize <- c("0" = n_pres, "1" = n_pres)
  
  ##########################################
  # FINAL RANDOM FOREST MODEL
  ##########################################
  
  predictors <- setdiff(names(data_model), "presence")
  
  rf_model <- randomForest(
    presence ~ .,
    data = data_model,
    ntree = 500,
    mtry = 11,
    nodesize = 1,
    sampsize = sampsize,
    importance = TRUE
  )
  
  ##########################################
  # SAVE MODEL
  ##########################################
  
  saveRDS(
    rf_model,
    file = file.path(base_path, paste0("RF_final_", sp, ".rds"))
  )
  
  ##########################################
  # VARIABLE IMPORTANCE
  ##########################################
  
  imp <- importance(rf_model)
  
  imp_df <- data.frame(
    variable = rownames(imp),
    MeanDecreaseAccuracy = imp[,1],
    MeanDecreaseGini = imp[,2],
    species = sp
  )
  
  importance_final <- rbind(importance_final, imp_df)
  
  ##########################################
  # PDP (FINAL MODEL)
  ##########################################
  
  grid_list <- lapply(predictors, function(var){
    rng <- range(data_model[[var]], na.rm = TRUE)
    seq(rng[1], rng[2], length.out = 50)
  })
  
  names(grid_list) <- predictors
  
  data_sample <- data_model %>%
    slice_sample(n = min(5000, nrow(data_model)))
  
  pdp_list <- lapply(predictors, function(var){
    
    grid_df <- data.frame(value = grid_list[[var]])
    colnames(grid_df) <- var
    
    pd <- partial(
      rf_model,
      pred.var = var,
      pred.grid = grid_df,
      train = data_sample,
      prob = TRUE,
      which.class = 2
    )
    
    colnames(pd)[1] <- "value"
    
    pd$variable <- var
    pd$species <- sp
    
    pd
  })
  
  pdp_df <- bind_rows(pdp_list)
  
  all_pdp_final <- rbind(all_pdp_final, pdp_df)
}

############################################
# SAVE IMPORTANCE
############################################

wb <- createWorkbook()

addWorksheet(wb, "Final_importance")
writeData(wb, "Final_importance", importance_final)

saveWorkbook(
  wb,
  file.path(base_path, "FINAL_MODEL_RESULTS.xlsx"),
  overwrite = TRUE
)

############################################
# PDP PLOTS (FINAL MODEL)
############################################

plot_dir <- file.path(base_path, "PDP_final")
dir.create(plot_dir, showWarnings = FALSE)

for (sp in unique(all_pdp_final$species)) {
  
  pdp_subset <- all_pdp_final %>%
    filter(species == sp)
  
  p <- ggplot(pdp_subset, aes(x = value, y = yhat)) +
    geom_line(color = "#2C2C2C", linewidth = 1) +
    facet_wrap(~variable, scales = "free_x") +
    theme_classic(base_size = 14) +
    labs(
      title = paste("Partial dependence plots (final model) -", sp),
      x = "Environmental gradient",
      y = "Predicted probability"
    ) +
    coord_cartesian(ylim = c(0, 0.35))
  
  ggsave(
    filename = paste0("PDP_FINAL_", sp, ".png"),
    plot = p,
    path = plot_dir,
    width = 14,
    height = 10,
    dpi = 300
  )
}

cat("\nFINAL MODEL COMPLETED SUCCESSFULLY\n")





############################################
# FINAL PROJECTION SCRIPT - ALL MONTHS (PTS + BBS)
############################################

library(terra)
library(sf)
library(stringr)
library(rnaturalearth)
library(randomForest)

terraOptions(memfrac = 0.6, progress = 1)

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------
base_path  <- "E:/TFM_gangas"
model_path <- "E:/TFM_gangas/GPS/ExtractedV.2"

out_dir <- file.path(model_path, "PROJECTIONS")
dir.create(out_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# MODELS
# ------------------------------------------------------------
models <- list(
  PTS = readRDS(file.path(model_path, "RF_final_PTS.rds")),
  BBS = readRDS(file.path(model_path, "RF_final_BBS.rds"))
)

# ------------------------------------------------------------
# STATIC VARIABLES
# ------------------------------------------------------------
dem  <- rast(file.path(base_path,"Topograficas/300m/Spain_DEM_reproject_300m.tif"))
slope <- rast(file.path(base_path,"Topograficas/300m/Slope_map_Spain_300m.tif"))
hetero <- rast(file.path(base_path,"Heterogeneidad/300m/shannon_01_05_1km_Spain_25830_300m.tif"))
roads <- rast(file.path(base_path,"DistanciaCarreteras/300m/Distroads_spain_merged_300m.tif"))

names(dem)    <- "Altitude"
names(slope)  <- "Slope"
names(hetero) <- "Heterogeneity"
names(roads)  <- "DistRoad"

# ------------------------------------------------------------
# SOCIO-ENVIRONMENTAL
# ------------------------------------------------------------
pop <- rast(file.path(base_path,"DensidadPoblacion/300m/GHS_POP_2020_25830_300m.tif"))
hfp <- rast(file.path(base_path,"HumanFootprint/300m/hfp_2020_100m_25830_300m.tif"))

names(pop) <- "Population"
names(hfp) <- "HFP"

# ------------------------------------------------------------
# LULUCF + COS
# ------------------------------------------------------------
lulucf <- rast(file.path(base_path,
                         "UsosSuelo/LULUCF/300m/LULUCF_LC_2021_300m.tif"))

cos <- rast(file.path(base_path,
                      "UsosSuelo/COS2023/300m/COS2023_LC_300m.tif"))

lc_names <- c(
  "LC_Forest","LC_Vineyards","LC_TreeCrops","LC_RiceFields",
  "LC_Greenhouses","LC_AnnualCrops","LC_TreePasture","LC_ShrubPasture",
  "LC_HerbPasture","LC_WaterBodies","LC_Marshes","LC_Artificial",
  "LC_OtherLand","LC_AgriMosaic"
)

names(lulucf) <- lc_names
names(cos)    <- lc_names

por <- ne_countries(country = "Portugal", scale = "medium", returnclass = "sf")
por <- st_transform(por, 25830)
por_vect <- vect(por)

lulucf_list <- vector("list", nlyr(lulucf))

for(i in 1:nlyr(lulucf)){
  r_port  <- mask(cos[[i]], por_vect)
  r_final <- cover(r_port, lulucf[[i]])
  lulucf_list[[i]] <- r_final
  rm(r_port, r_final); gc()
}

lulucf_final <- rast(lulucf_list)
names(lulucf_final) <- lc_names

# ------------------------------------------------------------
# FILE LISTS
# ------------------------------------------------------------
ndvi_path <- file.path(base_path,"NDVI/SpainReprojected/300m")
ndvi_files <- list.files(ndvi_path, full.names = TRUE)
dates <- as.Date(str_extract(ndvi_files, "\\d{8}"), "%Y%m%d")

tmean_files <- list.files(file.path(base_path,"Climaticas/10_days/Tmean/300m"),
                          pattern="mean_.*\\.tif$", full.names=TRUE)

tmeansd_files <- list.files(file.path(base_path,"Climaticas/10_days/Tmean/300m"),
                            pattern="sd_.*\\.tif$", full.names=TRUE)

prcp_files <- list.files(file.path(base_path,"Climaticas/10_days/Prcp/300m"),
                         pattern="\\.tif$", full.names=TRUE)

# ------------------------------------------------------------
# MONTH LOOP
# ------------------------------------------------------------
months_vec <- sprintf("%02d", 1:12)

for(m in months_vec){
  
  out_files <- file.path(out_dir, paste0(names(models), "_month_", m, ".tif"))
  
  if(all(file.exists(out_files))){
    cat("Month", m, "already done → skipping everything\n")
    next
  }
  
  cat("\nMONTH:", m, "\n")
  m_num <- as.numeric(m)
  
  # ---------------- NDVI ----------------
  ndvi_m <- rast(ndvi_files[format(dates,"%m")==m])
  ndvi_m <- mean(ndvi_m, na.rm=TRUE)
  names(ndvi_m) <- "NDVI"
  
  # ---------------- TMEAN ----------------
  tmean_sum <- NULL; n <- 0
  for(f in tmean_files){
    r <- rast(f)
    idx <- ((m_num-1)*3+1):(m_num*3)
    r_m <- mean(r[[idx]], na.rm=TRUE)
    if(is.null(tmean_sum)) tmean_sum <- r_m else tmean_sum <- tmean_sum + r_m
    n <- n + 1
    rm(r, r_m); gc()
  }
  tmean_m <- tmean_sum / n
  names(tmean_m) <- "Tmean"
  
  # ---------------- TMEAN SD ----------------
  tmeansd_sum <- NULL; n <- 0
  for(f in tmeansd_files){
    r <- rast(f)
    idx <- ((m_num-1)*3+1):(m_num*3)
    r_m <- mean(r[[idx]], na.rm=TRUE)
    if(is.null(tmeansd_sum)) tmeansd_sum <- r_m else tmeansd_sum <- tmeansd_sum + r_m
    n <- n + 1
    rm(r, r_m); gc()
  }
  tmeansd_m <- tmeansd_sum / n
  names(tmeansd_m) <- "TmeanSD100"
  
  # ---------------- PRCP ----------------
  prcp_sum <- NULL; n <- 0
  for(f in prcp_files){
    r <- rast(f)
    idx <- ((m_num-1)*3+1):(m_num*3)
    r_m <- mean(r[[idx]], na.rm=TRUE)
    if(is.null(prcp_sum)) prcp_sum <- r_m else prcp_sum <- prcp_sum + r_m
    n <- n + 1
    rm(r, r_m); gc()
  }
  prcp_m <- prcp_sum / n
  names(prcp_m) <- "Prcp"
  
  # ---------------- STACK ----------------
  env_stack <- c(
    dem, slope, hetero, roads,
    pop, hfp,
    lulucf_final,
    ndvi_m,
    prcp_m,
    tmean_m,
    tmeansd_m
  )
  
  # --------------------------------------------------------
  # MODEL LOOP
  # --------------------------------------------------------
  for(model_name in names(models)){
    
    rf_model <- models[[model_name]]
    out_file <- file.path(out_dir, paste0(model_name, "_month_", m, ".tif"))
    
    if(file.exists(out_file)){
      cat("Skipping:", out_file, "\n")
      next
    }
    
    cat("Running:", model_name, m, "\n")
    
    terra::predict(
      env_stack,
      rf_model,
      fun = function(model, data, ...) {
        predict(model, newdata = data, type = "prob")[,2]
      },
      na.rm = TRUE,
      filename = out_file,
      overwrite = TRUE
    )
  }
}




######  SUITABILITY GIFTS  #########

library(terra)
library(ggplot2)
library(tidyterra)
library(magick)
library(scales)

base_path <- "E:/TFM_gangas/PROJECTIONS"

make_gif_prob <- function(pattern, out_name){
  
  files <- list.files(base_path, pattern = pattern, full.names = TRUE)
  files <- sort(files)
  
  # escala global
  all_vals <- c()
  for(f in files){
    r <- rast(f)
    all_vals <- c(all_vals, values(r))
  }
  all_vals <- all_vals[!is.na(all_vals)]
  q <- quantile(all_vals, c(0.01, 0.99))
  
  tmp_imgs <- c()
  
  for(i in seq_along(files)){
    
    r <- rast(files[i])
    
    r_fill <- focal(r, w=3, fun=mean, na.rm=TRUE)
    r[is.na(r)] <- r_fill[is.na(r)]
    
    tmp <- file.path(tempdir(), paste0(out_name, "_", i, ".png"))
    
    png(tmp, 900, 700)
    print(
      ggplot() +
        geom_spatraster(data = r) +
        scale_fill_viridis_c(
          option = "plasma",
          limits = q,
          oob = squish,
          name = "Suitability"
        ) +
        theme_void() +
        ggtitle(paste("Month", i))
    )
    dev.off()
    
    tmp_imgs <- c(tmp_imgs, tmp)
  }
  
  image_write(image_animate(image_read(tmp_imgs), fps = 2),
              file.path(base_path, out_name))
}

make_gif_prob("PTS_month_.*\\.tif$", "PTS_prob.gif")
make_gif_prob("BBS_month_.*\\.tif$", "BBS_prob.gif")

######## BINARY GIFTS  #########

library(terra)
library(ggplot2)
library(tidyterra)
library(magick)

base_path <- "E:/TFM_gangas/PROJECTIONS"

# ------------------------------------------------------------
# THRESHOLD (ajústalo si calculas TSS)
# ------------------------------------------------------------
threshold <- 0.3

# ------------------------------------------------------------
# FUNCTION
# ------------------------------------------------------------
make_gif_binary <- function(pattern, out_name){
  
  files <- list.files(base_path, pattern = pattern, full.names = TRUE)
  files <- sort(files)
  
  tmp_imgs <- c()
  
  for(i in seq_along(files)){
    
    cat("Processing:", files[i], "\n")
    
    r <- rast(files[i])
    
    # --------------------------------------------------------
    # FILL NA (ONLY NA, NO SMOOTHING)
    # --------------------------------------------------------
    r_fill <- focal(r, w=3, fun=mean, na.rm=TRUE)
    r[is.na(r)] <- r_fill[is.na(r)]
    
    # --------------------------------------------------------
    # BINARY
    # --------------------------------------------------------
    r_bin <- r > threshold
    
    # --------------------------------------------------------
    # SAVE FRAME
    # --------------------------------------------------------
    tmp <- file.path(tempdir(), paste0(out_name, "_", i, ".png"))
    
    png(tmp, width = 900, height = 700)
    
    print(
      ggplot() +
        geom_spatraster(data = r_bin) +
        scale_fill_manual(
          values = c("white", "darkred"),
          labels = c("Absence", "Presence"),
          name = ""
        ) +
        theme_void() +
        ggtitle(paste("Month", sprintf("%02d", i)))
    )
    
    dev.off()
    
    tmp_imgs <- c(tmp_imgs, tmp)
  }
  
  # ------------------------------------------------------------
  # CREATE GIF
  # ------------------------------------------------------------
  imgs <- image_read(tmp_imgs)
  animation <- image_animate(imgs, fps = 2)
  
  image_write(animation, file.path(base_path, out_name))
}

# ------------------------------------------------------------
# RUN
# ------------------------------------------------------------
make_gif_binary("PTS_month_.*\\.tif$", "PTS_binary.gif")
make_gif_binary("BBS_month_.*\\.tif$", "BBS_binary.gif")








############################################
# SDM FINAL RESULTS — CLEAN VERSION
############################################

library(terra)
library(ggplot2)
library(tidyterra)
library(dplyr)
library(sf)
library(mapSpain)
library(rnaturalearth)
library(patchwork)

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------
base_path <- "E:/TFM_gangas/GPS/ExtractedV.2/PROJECTIONS"
out_dir <- file.path(base_path, "FINAL_RESULTS_CLEAN")
dir.create(out_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# THRESHOLDS
# ------------------------------------------------------------
threshold_pts <- 0.195
threshold_bbs <- 0.203

# ------------------------------------------------------------
# FILES
# ------------------------------------------------------------
files_pts <- sort(list.files(base_path, pattern="PTS_month_.*\\.tif$", full.names=TRUE))
files_bbs <- sort(list.files(base_path, pattern="BBS_month_.*\\.tif$", full.names=TRUE))

# ------------------------------------------------------------
# IBERIA MASK
# ------------------------------------------------------------
provinces <- esp_get_prov()
provinces <- provinces[!provinces$iso2.prov.name.es %in% 
                         c("Baleares","Las Palmas","Santa Cruz de Tenerife","Ceuta","Melilla"), ]
provinces <- st_transform(provinces, 25830)

mask_spain <- st_union(provinces)

por <- ne_countries(country = "Portugal", scale = "medium", returnclass = "sf")
por <- st_transform(por, 25830)
por <- st_cast(por, "POLYGON")
por$area <- st_area(por)
por <- por[which.max(por$area), ]

iberia_mask <- st_union(mask_spain, por)
mask_vect <- vect(iberia_mask)

# ------------------------------------------------------------
# LOAD STACKS
# ------------------------------------------------------------
stack_pts <- rast(files_pts)
stack_bbs <- rast(files_bbs)

stack_pts <- crop(stack_pts, mask_vect)
stack_pts <- mask(stack_pts, mask_vect)

stack_bbs <- crop(stack_bbs, mask_vect)
stack_bbs <- mask(stack_bbs, mask_vect)

stack_pts[is.na(stack_pts)] <- NA
stack_bbs[is.na(stack_bbs)] <- NA

# ------------------------------------------------------------
# GLOBAL SCALE
# ------------------------------------------------------------
all_vals <- c(values(stack_pts), values(stack_bbs))
all_vals <- all_vals[!is.na(all_vals)]

global_min <- min(all_vals)
global_max <- max(all_vals)

# ------------------------------------------------------------
# MAP FUNCTION
# ------------------------------------------------------------
plot_map <- function(r, title){
  
  r <- focal(r, w = 3, fun = mean, na.rm = TRUE)
  
  ggplot() +
    geom_spatraster(data = r) +
    geom_sf(data = iberia_mask, fill = NA, color = "black", linewidth = 0.2) +
    
    scale_fill_viridis_c(
      option = "inferno",
      limits = c(global_min, global_max),
      na.value = NA,
      name = "Suitability"
    ) +
    
    theme_void(base_size = 13) +
    labs(title = title) +
    
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "right"
    )
}

# ------------------------------------------------------------
# 1. TEMPORAL CURVE
# ------------------------------------------------------------
get_curve <- function(stack, label){
  data.frame(
    month = 1:nlyr(stack),
    mean = global(stack, mean, na.rm=TRUE)[,1],
    species = label
  )
}

df_mean <- rbind(
  get_curve(stack_pts,"P. alchata"),
  get_curve(stack_bbs,"P. orientalis")
)

p_mean <- ggplot(df_mean, aes(month, mean, color = species)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = 1:12,
    labels = c("Jan","Feb","Mar","Apr","May","Jun",
               "Jul","Aug","Sep","Oct","Nov","Dec")
  ) +
  theme_classic(base_size = 14) +
  labs(
    x = "Month",
    y = "Mean habitat suitability",
    color = "Species"
  )

ggsave(file.path(out_dir,"Fig1_mean_suitability.png"),
       p_mean, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------
# 2. SUITABLE AREA
# ------------------------------------------------------------
get_area <- function(stack, threshold, label){
  data.frame(
    month = 1:nlyr(stack),
    area = global(stack > threshold, mean, na.rm=TRUE)[,1],
    species = label
  )
}

df_area <- rbind(
  get_area(stack_pts, threshold_pts, "P. alchata"),
  get_area(stack_bbs, threshold_bbs, "P. orientalis")
)

p_area <- ggplot(df_area, aes(month, area, color = species)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = 1:12,
    labels = c("Jan","Feb","Mar","Apr","May","Jun",
               "Jul","Aug","Sep","Oct","Nov","Dec")
  ) +
  theme_classic(base_size = 14) +
  labs(
    x = "Month",
    y = "Proportion of suitable habitat",
    color = "Species"
  )

ggsave(file.path(out_dir,"Fig2_suitable_area.png"),
       p_area, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------
# 3. MIN vs MAX MAPS
# ------------------------------------------------------------
mean_pts <- global(stack_pts, mean, na.rm=TRUE)[,1]
mean_bbs <- global(stack_bbs, mean, na.rm=TRUE)[,1]

p_min_pts <- plot_map(stack_pts[[which.min(mean_pts)]], "P. alchata — Minimum")
p_max_pts <- plot_map(stack_pts[[which.max(mean_pts)]], "P. alchata — Maximum")

p_min_bbs <- plot_map(stack_bbs[[which.min(mean_bbs)]], "P. orientalis — Minimum")
p_max_bbs <- plot_map(stack_bbs[[which.max(mean_bbs)]], "P. orientalis — Maximum")

p_minmax <- (p_min_pts + p_max_pts) / (p_min_bbs + p_max_bbs)

ggsave(file.path(out_dir,"Fig3_min_max.png"),
       p_minmax, width = 10, height = 10, dpi = 300)

# ------------------------------------------------------------
# 4. OVERLAP (TOP 10%)
# ------------------------------------------------------------
mean_pts_r <- app(stack_pts, mean, na.rm=TRUE)
mean_bbs_r <- app(stack_bbs, mean, na.rm=TRUE)

thr_pts <- quantile(values(mean_pts_r), 0.9, na.rm=TRUE)
thr_bbs <- quantile(values(mean_bbs_r), 0.9, na.rm=TRUE)

hot_pts <- mean_pts_r > thr_pts
hot_bbs <- mean_bbs_r > thr_bbs

overlap <- hot_pts*1 + hot_bbs*2
overlap <- as.factor(overlap)

levels(overlap) <- data.frame(
  ID = c(1,2,3),
  class = c("P. alchata","P. orientalis","Overlap")
)

p_overlap <- ggplot() +
  geom_spatraster(data = overlap) +
  scale_fill_manual(
    values = c(
      "P. alchata" = "#E64B35",
      "P. orientalis" = "#4DBBD5",
      "Overlap" = "#7E6148"
    ),
    name = "Species", 
    na.value = NA,
    na.translate = FALSE
  ) +
  geom_sf(data = iberia_mask, fill = NA, color = "black", linewidth = 0.17) +
  theme_void() +
  labs(title = "Core habitat overlap (top 10%)", fill = "") +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(25, 70, 10, 40),
    plot.title = element_text(
      face = "bold",
      hjust = -0.08,
      vjust = 4,
      size = 14
    ),
    legend.position = c(1.05, 0.5),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  )

ggsave(file.path(out_dir,"Fig4_overlap.png"),
       p_overlap, width = 6, height = 5, dpi = 300, bg = "white")

cat("\nFINAL CLEAN RESULTS READY\n")
