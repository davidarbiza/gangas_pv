###########################
# SDM x NATURA2000
###########################

rm(list=ls())
gc()

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(mapSpain)
library(rnaturalearth)
library(cowplot)

# ---------------- PATHS ----------------
proj_path <- "E:/TFM_gangas/GPS/ExtractedV.2/CV_models/PROJECTIONS_ENSEMBLE_10DAYS"
pa_path   <- "C:/Users/andre/Downloads/Natura2000_end2024.gpkg"

out_dir <- file.path(proj_path, "PA_FINAL_CLEAN")
dir.create(out_dir, showWarnings = FALSE)

# ---------------- FILES ----------------
files_pts <- sort(list.files(proj_path, pattern="PTS_.*ENSEMBLE\\.tif$", full.names=TRUE))
files_bbs <- sort(list.files(proj_path, pattern="BBS_.*ENSEMBLE\\.tif$", full.names=TRUE))

idx <- c(1,2)

# ---------------- MASK ----------------
provinces <- esp_get_prov()
provinces <- provinces[!provinces$iso2.prov.name.es %in% 
                         c("Baleares","Las Palmas","Santa Cruz de Tenerife","Ceuta","Melilla"), ]
provinces <- st_transform(provinces, 25830)

mask_spain <- st_union(provinces)

por <- ne_countries(country="Portugal", scale="medium", returnclass="sf")
por <- st_transform(por, 25830)
por <- st_cast(por, "POLYGON")
por <- por[which.max(st_area(por)), ]

iberia <- st_union(mask_spain, por)
iberia_vect <- vect(iberia)

# ---------------- NATURA ----------------
pa <- st_read(pa_path, layer="NaturaSite_polygon", quiet=TRUE)
pa <- st_transform(pa, 25830)
pa <- st_intersection(pa, iberia)

pa$ZEPA <- ifelse(pa$SITETYPE %in% c("A","C"), 1, 0)
pa$SAC  <- ifelse(pa$SITETYPE %in% c("B","C"), 1, 0)

pa_union <- st_union(pa)

# ---------------- FUNCTION ----------------
analyze <- function(file, species, period){
  
  cat("\nProcessing:", species, period, "\n")
  
  r <- rast(file)
  r <- crop(r, iberia_vect)
  r <- mask(r, iberia_vect)
  
  # raster layers
  pa_r   <- rasterize(vect(pa_union), r, field=1, background=0)
  zepa_r <- rasterize(vect(pa[pa$ZEPA==1,]), r, field=1, background=0)
  sac_r  <- rasterize(vect(pa[pa$SAC==1,]),  r, field=1, background=0)
  
  # ---------------- TOP 10 ----------------
  thr <- quantile(values(r), 0.9, na.rm=TRUE)
  r_top <- r >= thr
  
  # ---------------- DATA ----------------
  df <- as.data.frame(c(r_top, zepa_r, sac_r), na.rm=TRUE)
  colnames(df) <- c("top10","zepa","sac")
  
  # Protection type
  df$prot <- "outside"
  df$prot[df$zepa==1 & df$sac==0] <- "ZEPA"
  df$prot[df$zepa==0 & df$sac==1] <- "SAC"
  df$prot[df$zepa==1 & df$sac==1] <- "both"
  
  # =========================
  # EXCELL
  # =========================
  
  df_top <- df[df$top10==1, ]
  total <- nrow(df_top)
  
  tab <- df_top %>%
    group_by(prot) %>%
    summarise(n_cells = n(), .groups="drop")
  
  tab$perc <- (tab$n_cells / total) * 100
  tab$species <- species
  tab$period  <- period
  
  # =========================
  # BIVARIATE
  # =========================
  
  df_map <- as.data.frame(c(r_top, pa_r), xy=TRUE, na.rm=TRUE)
  colnames(df_map) <- c("x","y","top10","protected")
  
  df_map$cat <- "Other"
  df_map$cat[df_map$top10==1 & df_map$protected==0] <- "Top10_not_protected"
  df_map$cat[df_map$top10==1 & df_map$protected==1] <- "Top10_protected"
  df_map$cat[df_map$top10==0 & df_map$protected==1] <- "Protected_only"
  
  cols <- c(
    "Other" = "white",
    "Protected_only" = "#4DBBD5",
    "Top10_not_protected" = "#E64B35",
    "Top10_protected" = "#000000"
  )
  
  p_map <- ggplot(df_map) +
    geom_tile(aes(x=x,y=y,fill=cat)) +
    scale_fill_manual(values=cols, guide="none") +
    geom_sf(data=iberia, fill=NA, color="black", linewidth=0.2) +
    theme_void() +
    theme(
      plot.background = element_rect(fill="white"),
      plot.title = element_text(size=12, hjust=0.5)
    ) +
    labs(title = paste(species, period, "- Protection vs Suitability"))
  
  # Leyend
  legend_df <- expand.grid(x=1:2, y=1:2)
  legend_df$cat <- c(
    "Other","Top10_not_protected",
    "Protected_only","Top10_protected"
  )
  
  p_leg <- ggplot(legend_df) +
    geom_tile(aes(x=x,y=y,fill=cat)) +
    scale_fill_manual(values=cols, guide="none") +
    annotate("text", x=1.5, y=0.2, label="Suitability →", size=3) +
    annotate("text", x=0.2, y=1.5, label="Protection →", angle=90, size=3) +
    
    coord_fixed() +
    theme_void()
  
  final_map <- ggdraw() +
    draw_plot(p_map) +
    draw_plot(p_leg, 
              x=0.76, y=0.1,
              width=0.18,
              height=0.18)
  
  ggsave(file.path(out_dir, paste0("bivar2x2_",species,"_",period,".png")),
         final_map, width=6, height=5, dpi=300, bg="white")
  
  # =========================
  # PROTECTION TYPES MAPS
  # =========================
  
  df_top_map <- as.data.frame(c(r_top, zepa_r, sac_r), xy=TRUE, na.rm=TRUE)
  colnames(df_top_map) <- c("x","y","top10","zepa","sac")
  
  df_top_map <- df_top_map[df_top_map$top10==1, ]
  
  df_top_map$prot <- "outside"
  df_top_map$prot[df_top_map$zepa==1 & df_top_map$sac==0] <- "ZEPA"
  df_top_map$prot[df_top_map$zepa==0 & df_top_map$sac==1] <- "SAC"
  df_top_map$prot[df_top_map$zepa==1 & df_top_map$sac==1] <- "both"
  
  p_top <- ggplot(df_top_map) +
    geom_tile(aes(x=x,y=y,fill=prot)) +
    scale_fill_manual(values=c(
      "outside"="grey80",
      "SAC"="#E69F00",
      "ZEPA"="#56B4E9",
      "both"="#000000"
    )) +
    geom_sf(data=iberia, fill=NA, color="black", linewidth=0.2) +
    theme_void() +
    theme(plot.background = element_rect(fill="white")) +
    labs(title=paste(species, period, "- Top 10% protection types"))
  
  ggsave(file.path(out_dir, paste0("top10_types_",species,"_",period,".png")),
         p_top, width=6, height=5, dpi=300, bg="white")
  
  return(tab)
}

# ---------------- RUN ----------------
results <- list()

for(i in idx){
  results[[length(results)+1]] <- analyze(files_pts[i], "PTS", paste0("T",i))
  results[[length(results)+1]] <- analyze(files_bbs[i], "BBS", paste0("T",i))
}

final <- do.call(rbind, results)

# ---------------- SAVE EXCEL ----------------
write.xlsx(final, file.path(out_dir, "Top10_results.xlsx"))

cat("\nDONE\n")




############################################
# ANALYSIS TOP10 NATURA2000
############################################

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(openxlsx)

# ---------------- PATH ----------------
file_excel <- "E:/TFM_gangas/GPS/ExtractedV.2/CV_models/PROJECTIONS_ENSEMBLE_10DAYS/PA_FINAL_CLEAN/Top10_results.xlsx"

out_dir <- dirname(file_excel)

analysis_dir <- file.path(out_dir, "ANALYSIS")
dir.create(analysis_dir, showWarnings = FALSE)

# ---------------- LOAD ----------------
df <- read_excel(file_excel)

# rename species
df$species <- recode(df$species,
                     "BBS" = "P. orientalis",
                     "PTS" = "P. alchata")

df$period <- factor(df$period, levels=unique(df$period))

# ---------------- METRICS ----------------

df_wide <- df %>%
  select(species, period, prot, perc) %>%
  pivot_wider(names_from = prot, values_from = perc)

df_wide[is.na(df_wide)] <- 0

df_wide <- df_wide %>%
  mutate(
    protected = SAC + ZEPA + both,
  )

# ---------------- SAVE SUMMARY ----------------
write.xlsx(df_wide, file.path(analysis_dir, "analysis_summary.xlsx"), overwrite=TRUE)

# colors species
cols_species <- c("P. orientalis" = "#E64B35",
                  "P. alchata"   = "#4DBBD5")

# =========================================================
# 1. % PROTECTED
# =========================================================

p1 <- ggplot(df_wide, aes(x=period, y=protected, color=species, group=species)) +
  geom_line(linewidth=1.2) +
  geom_point(size=2) +
  scale_color_manual(values=cols_species) +
  labs(y="% Protected (Top10)", x="Period") +
  theme_minimal()

ggsave(file.path(analysis_dir, "line_protected.png"),
       p1, width=7, height=5, dpi=300)

# =========================================================
# 2. STACKED AREA
# =========================================================

df_area <- df %>%
  mutate(prot = factor(prot, levels=c("outside","SAC","ZEPA","both")))

p2 <- ggplot(df_area, aes(x=period, y=perc, fill=prot, group=prot)) +
  geom_area(alpha=0.9) +
  facet_wrap(~species) +
  scale_fill_manual(values=c(
    "outside"="grey80",
    "SAC"="#E69F00",
    "ZEPA"="#56B4E9",
    "both"="#000000"
  )) +
  labs(y="% of Top10", x="Period") +
  theme_minimal()

ggsave(file.path(analysis_dir, "stacked_area.png"),
       p2, width=8, height=5, dpi=300)

# =========================================================
# 3. BARPLOT — COMPARISON
# =========================================================

p3 <- ggplot(df_wide, aes(x=period, y=protected, fill=species)) +
  geom_col(position="dodge") +
  scale_fill_manual(values=cols_species) +
  labs(y="% Protected", x="Period") +
  theme_minimal()

ggsave(file.path(analysis_dir, "barplot_protected.png"),
       p3, width=7, height=5, dpi=300)

# =========================================================
# 4. HEATMAP — FULL DISTRIBUTION
# =========================================================

p4 <- ggplot(df, aes(x=prot, y=period, fill=perc)) +
  geom_tile(color="white") +
  facet_wrap(~species) +

  scale_fill_gradientn(
    colors = c("#f7fbff", "#6baed6", "#08306b")
  ) +
  
  labs(x="Category", y="Period", fill="%") +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(fill="white"),
    panel.background = element_rect(fill="white")
  )

ggsave(file.path(analysis_dir, "heatmap.png"),
       p4, width=7, height=5, dpi=300)

# =========================================================
# 5. OUTSIDE EVOLUTION
# =========================================================

p5 <- ggplot(df_wide, aes(x=period, y=outside, color=species, group=species)) +
  geom_line(linewidth=1.2) +
  geom_point(size=2) +
  scale_color_manual(values=cols_species) +
  labs(y="% Outside Protection", x="Period") +
  theme_minimal()

ggsave(file.path(analysis_dir, "outside.png"),
       p5, width=7, height=5, dpi=300)

# =========================================================
# 6. PIE CHARTS
# =========================================================

for(p in unique(df$period)){
  
  tmp <- df[df$period == p, ]
  
  p_pie <- ggplot(tmp, aes(x="", y=perc, fill=prot)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y") +
    
    geom_text(aes(
      label = paste0(round(perc,1), "%"),
      color = prot
    ),
    position = position_stack(vjust = 0.5),
    size=3,
    show.legend = FALSE) +
    
    facet_wrap(~species) +
    
    scale_fill_manual(values=c(
      "outside"="grey80",
      "SAC"="#E69F00",
      "ZEPA"="#56B4E9",
      "both"="#000000"
    )) +
    
    scale_color_manual(values=c(
      "outside"="black",
      "SAC"="black",
      "ZEPA"="black",
      "both"="white"
    )) +
    
    theme_void() +
    theme(
      plot.background = element_rect(fill="white"),
      panel.background = element_rect(fill="white"),
      legend.position = "right"
    )
  
  ggsave(file.path(analysis_dir, paste0("pie_",p,".png")),
         p_pie, width=6, height=4, dpi=300, bg="white")
}
cat("\nALL ANALYSIS DONE\n")