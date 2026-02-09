################################ 
# LULUCF RECLASSIFICATION SCRIPT
################################

library(terra)

# -----------------------
# PATHS
# -----------------------
base_folder <- "E:/TFM_gangas/LULUCF/"
lulucf_files <- c("LULUCF2015_PB.tif", "LULUCF2018_PB.tif", "LULUCF2021_PB.tif")

# -----------------------
# FINAL CLASS VALUES
# -----------------------
classes <- c(
  "Bosques" = 1,
  "Olivares" = 2,
  "Viñedos" = 3,
  "Otros cultivos perennes" = 4,
  "Arrozales" = 5,
  "Invernaderos" = 6,
  "Cultivos anuales" = 7,
  "Pastizales con árboles" = 8,
  "Pastizales arbustivos" = 9,
  "Pastizales herbáceos" = 10,
  "Zonas acuáticas" = 11,
  "Marismas" = 12,
  "Áreas artificiales" = 13,
  "Otras tierras" = 14,
  "Mosaico de cultivos complejos" = 15
)

class_table <- data.frame(value = unname(classes), class = names(classes))

# -----------------------
# LULUCF RECLASS MATRIX
# -----------------------
lulucf_matrix <- matrix(c(
  # Bosques
  100,100,classes["Bosques"],
  110,110,classes["Bosques"],
  111,111,classes["Bosques"],
  112,112,classes["Bosques"],
  120,120,classes["Bosques"],
  121,121,classes["Bosques"],
  122,122,classes["Bosques"],
  130,130,classes["Bosques"],
  131,131,classes["Bosques"],
  132,132,classes["Bosques"],
  140,140,classes["Bosques"],
  141,141,classes["Bosques"],
  142,142,classes["Bosques"],
  
  # Cultivos perennes
  711,711,classes["Olivares"],
  712,712,classes["Viñedos"],
  713,713,classes["Otros cultivos perennes"],
  714,714,classes["Otros cultivos perennes"],
  715,715,classes["Mosaico de cultivos complejos"],
  719,719,classes["Otros cultivos perennes"],
  
  # Cultivos anuales / arrozales / invernaderos
  721,721,classes["Arrozales"],
  722,722,classes["Invernaderos"],
  729,729,classes["Cultivos anuales"],
  730,730,classes["Cultivos anuales"],
  
  # Pastizales
  210,210,classes["Pastizales con árboles"],
  220,220,classes["Pastizales arbustivos"],
  230,230,classes["Pastizales herbáceos"],
  240,240,classes["Otras tierras"],
  
  # Superficies de agua
  500,500,classes["Zonas acuáticas"],
  510,510,classes["Zonas acuáticas"],
  511,511,classes["Zonas acuáticas"],
  512,512,classes["Zonas acuáticas"],
  520,520,classes["Zonas acuáticas"],
  521,521,classes["Zonas acuáticas"],
  522,522,classes["Zonas acuáticas"],
  523,523,classes["Zonas acuáticas"],
  524,524,classes["Zonas acuáticas"],
  525,525,classes["Zonas acuáticas"],
  526,526,classes["Zonas acuáticas"],
  527,527,classes["Zonas acuáticas"],
  530,530,classes["Zonas acuáticas"],
  531,531,classes["Zonas acuáticas"],
  532,532,classes["Zonas acuáticas"],
  533,533,classes["Zonas acuáticas"],
  534,534,classes["Zonas acuáticas"],
  535,535,classes["Zonas acuáticas"],
  536,536,classes["Marismas"],
  537,537,classes["Zonas acuáticas"],
  
  # Áreas artificiales
  800,800,classes["Áreas artificiales"],
  810,810,classes["Áreas artificiales"],
  820,820,classes["Áreas artificiales"],
  821,821,classes["Áreas artificiales"],
  822,822,classes["Áreas artificiales"],
  823,823,classes["Áreas artificiales"],
  824,824,classes["Áreas artificiales"],
  830,830,classes["Áreas artificiales"],
  831,831,classes["Áreas artificiales"],
  832,832,classes["Áreas artificiales"],
  840,840,classes["Áreas artificiales"],
  841,841,classes["Áreas artificiales"],
  842,842,classes["Áreas artificiales"],
  849,849,classes["Áreas artificiales"],
  850,850,classes["Áreas artificiales"],
  860,860,classes["Áreas artificiales"],
  870,870,classes["Áreas artificiales"],
  880,880,classes["Áreas artificiales"],
  881,881,classes["Áreas artificiales"],
  882,882,classes["Áreas artificiales"],
  883,883,classes["Áreas artificiales"],
  
  # Otras tierras
  400,400,classes["Otras tierras"]
  
), ncol=3, byrow=TRUE)

# -----------------------
# PROCESS LULUCF
# -----------------------
for (f in lulucf_files) {
  cat("Processing", f, "...\n")
  r <- rast(paste0(base_folder, f))
  lulucf_lookup <- lulucf_matrix[, c(1,3)]
  
  r_reclass <- classify(r, lulucf_lookup, others=NA)
  
  levels(r_reclass) <- class_table
  
  out_name <- paste0(base_folder, tools::file_path_sans_ext(f), "_Reclass.tif")
  writeRaster(r_reclass, out_name, overwrite=TRUE)
  
  rm(r, r_reclass); gc()
  cat("Saved:", out_name, "\n")
}



################################ 
# COS PORTUGAL RECLASS SCRIPT
################################

# -----------------------
# PATHS
# -----------------------
base_folder <- "E:/TFM_gangas/LULUCF/"
portugal_file <- "COS2023v1-S2.gpkg"


# -----------------------
# PORTUGAL CLASS TABLE (Nível 4)
# -----------------------
pt_table <- data.frame(
  code = c(
    "1.1.1.1","1.1.1.2","1.1.2.1","1.1.2.2","1.2.1.1","1.2.1.2","1.2.2.1",
    "1.3.1.1","1.3.2.1","1.3.2.2","1.3.2.3","1.3.2.4","1.3.3.1","1.3.4.1",
    "1.4.1.1","1.4.1.2","1.4.1.3","1.4.2.1","1.4.3.1","1.4.4.1","1.4.4.2",
    "1.4.5.1","1.4.5.2","1.4.6.1","1.5.1.1","1.5.1.2","1.5.2.1","1.5.2.2",
    "1.5.2.3","1.5.3.1","1.5.3.2","1.5.4.1","1.6.1.1","1.6.1.2","1.7.1.1",
    "1.7.1.2","1.8.1.1","2.1.1.1","2.1.1.2","2.2.1.1","2.2.2.1","2.2.3.1",
    "2.3.1.1","2.3.1.2","2.3.1.3","2.3.2.1","2.3.3.1","2.4.1.1","3.1.1.1",
    "3.1.2.1","4.1.1.1","4.1.1.2","4.1.1.3","4.1.1.4","4.1.2.1","4.1.2.2",
    "4.2.1.1","4.2.1.2","4.2.1.3","4.2.1.4","4.2.2.1","4.2.2.2","5.1.1.1",
    "5.1.1.2","5.1.1.3","5.1.1.4","5.1.1.5","5.1.1.6","5.1.1.7","5.1.1.8",
    "5.1.2.1","5.1.2.2","5.1.2.3","6.1.1.1","7.1.1.1","7.1.1.2","7.1.2.1",
    "7.1.3.1","8.1.1.1","8.1.2.1","8.1.2.2","9.1.1.1","9.1.1.2","9.1.2.1",
    "9.1.2.2","9.1.2.3","9.1.2.4","9.1.2.5","9.2.1.1","9.3.1.1","9.3.2.1",
    "9.3.3.1","9.3.4.1"
  ),
  class <- c(
    "Áreas artificiales","Áreas artificiales","Áreas artificiales","Áreas artificiales",
    "Áreas artificiales","Áreas artificiales","Áreas artificiales","Áreas artificiales",
    "Áreas artificiales","Áreas artificiales","Áreas artificiales","Áreas artificiales",
    "Áreas artificiales","Áreas artificiales","Áreas artificiales","Áreas artificiales",
    "Áreas artificiales","Áreas artificiales","Áreas artificiales","Áreas artificiales",
    "Áreas artificiales","Áreas artificiales","Áreas artificiales","Áreas artificiales",
    "Áreas artificiales","Áreas artificiales","Áreas artificiales","Áreas artificiales",
    "Áreas artificiales","Áreas artificiales","Áreas artificiales","Áreas artificiales",
    "Áreas artificiales","Áreas artificiales","Otras tierras","Áreas artificiales",
    "Áreas artificiales","Cultivos anuales","Arrozales","Viñedos",
    "Otros cultivos perennes","Olivares","Mosaico de cultivos complejos","Mosaico de cultivos complejos",
    "Mosaico de cultivos complejos","Mosaico de cultivos complejos","Mosaico de cultivos complejos","Invernaderos",
    "Pastizales herbáceos","Pastizales herbáceos","Bosques","Bosques","Bosques","Bosques",
    "Bosques","Bosques","Pastizales con árboles","Pastizales con árboles",
    "Pastizales con árboles","Pastizales con árboles","Pastizales con árboles",
    "Pastizales con árboles","Bosques","Bosques","Bosques","Bosques","Bosques",
    "Bosques","Bosques","Bosques","Bosques","Bosques","Bosques","Pastizales arbustivos",
    "Otras tierras","Otras tierras","Otras tierras","Pastizales herbáceos",
    "Zonas acuáticas","Marismas","Marismas","Zonas acuáticas",
    "Zonas acuáticas","Zonas acuáticas","Zonas acuáticas","Zonas acuáticas",
    "Zonas acuáticas","Zonas acuáticas","Zonas acuáticas","Zonas acuáticas",
    "Zonas acuáticas","Zonas acuáticas", "Zonas acuáticas"
  )
  
)

# -----------------------
# PROCESS PORTUGAL VECTOR
# -----------------------
cat("Processing Portugal layer...\n")

v <- vect(paste0(base_folder, portugal_file))
v$class_name <- pt_table$class[match(v$COS23_n4_C, pt_table$code)]
v$class_val  <- unname(classes[v$class_name])
v_proj <- project(v, "EPSG:25830")

r_template <- rast(ext(v_proj), resolution=300, crs="EPSG:25830")
r_pt <- rasterize(v_proj, r_template, field="class_val", touches=TRUE)

levels(r_pt) <- class_table


out_name <- paste0(base_folder, "COS2023v1-S2_Reclass.tif")
writeRaster(r_pt, out_name, overwrite=TRUE)

rm(v, v_proj, r_pt, r_template); gc()

cat("Portugal raster saved:", out_name, "\n")
cat("All processes completed successfully!\n")










################################ 
# CLC2018 CROP AND REPROJECT SCRIPT
################################

library(terra)
library(sf)

# -----------------------
# PATHS
# -----------------------
base_folder <- "E:/TFM_gangas/LULUCF/CLC2018/"
clc_file <- "U2018_CLC2018_V2020_20u1.tif"

# -----------
# MASK BBOX 
# -----------
peninsula_bbox <- st_bbox(c(xmin = -10, ymin = 35.5, xmax = 5, ymax = 44.5), crs = 4326)
mask <- vect(st_as_sfc(peninsula_bbox))

# -----------------------
# CROP AND REPROJECT TO EPSG:25830
# -----------------------
cat("Cropping and reprojecting CLC raster to EPSG:25830...\n")

r <- rast(paste0(base_folder, clc_file))

# Reproject mask to raster CRS
mask_proj <- project(mask, crs(r))

# Crop raster to mask
r_crop <- crop(r, mask_proj)

# Reproject cropped raster to EPSG:25830 using nearest neighbor
r_25830 <- project(r_crop, "EPSG:25830", method="near")

# Save reprojected raster
reprojected_file <- paste0(base_folder, "CLC2018_PB.tif")
writeRaster(r_25830, reprojected_file, overwrite=TRUE)

cat("Reprojected raster saved:", reprojected_file, "\n")



################################ 
# CLC2018 FINAL RECLASSIFICATION SCRIPT
################################

library(terra)

# -----------------------
# PATHS
# -----------------------
base_folder <- "E:/TFM_gangas/LULUCF/CLC2018/"
intermediate_file <- "CLC2018_PB.tif"  # raster ya recortado

# -----------------------
# FINAL CLASS VALUES (ORDERED)
# -----------------------
classes <- c(
  "Bosques" = 1,
  "Olivares" = 2,
  "Viñedos" = 3,
  "Otros cultivos perennes" = 4,
  "Arrozales" = 5,
  "Invernaderos" = 6,
  "Cultivos anuales de secano" = 7,
  "Pastizales con árboles" = 8,
  "Pastizales arbustivos" = 9,
  "Pastizales herbáceos" = 10,
  "Zonas acuáticas" = 11,
  "Marismas" = 12,
  "Áreas artificiales" = 13,
  "Otras tierras" = 14,
  "Mosaico de cultivos complejos" = 15,
  "Cultivos anuales de regadío" = 16,
  "NODATA" = 999
)

class_table <- data.frame(
  value = unname(classes),
  class = names(classes)
)

# -----------------------
# CLC RECLASS MATRIX
# (FROM raster values → final class values)
# -----------------------
clc_matrix <- matrix(c(
  1,13,  2,13,  3,13,  4,13,  5,13,  6,13,
  7,13,  8,13,  9,13, 10,13, 11,13,
  12,7, 13,16, 14,5, 15,3, 16,4, 17,2,
  18,10, 19,15, 20,15, 21,15, 22,15,
  23,1, 24,1, 25,1, 26,10, 27,9, 28,10, 29,8,
  30,14, 31,14, 32,14, 33,14, 34,14,
  35,11, 36,11, 37,12, 38,11, 39,11,
  40,11, 41,11, 42,11, 43,11, 44,11,
  48,999
), ncol=2, byrow=TRUE)

# -----------------------
# PROCESS CLC FINAL RECLASS
# -----------------------
cat("Loading intermediate CLC raster...\n")
r <- rast(paste0(base_folder, intermediate_file))

cat("Reclassifying CLC raster...\n")
r_reclass <- classify(r, clc_matrix, others=999)

# Assign ordered class labels
levels(r_reclass) <- class_table

# Save final raster
out_name <- paste0(base_folder, "CLC2018_PB_Reclass.tif")
writeRaster(r_reclass, out_name, overwrite=TRUE)

rm(r, r_reclass); gc()
cat("CLC2018 reclassified raster saved:", out_name, "\n")
cat("CLC2018 processing completed successfully!\n")




#############################
# RECLASS RASTER CHECK SCRIPT
#############################

library(terra)

check_reclass <- function(path) {
  
  cat("\n==============================\n")
  cat("Checking:", basename(path), "\n")
  
  r <- rast(path)
  r_num <- as.numeric(r)
  
  f <- freq(r_num)
  values_present <- sort(f$value)
  
  cat("Values found:\n")
  print(values_present)
  
  # Expected class system
  expected <- c(1:16, 999)
  
  missing <- setdiff(expected, values_present)
  extra   <- setdiff(values_present, expected)
  
  if (length(missing)==0 & length(extra)==0) {
    cat("✔ Class values are correct\n")
  } else {
    if (length(missing)>0) cat("❌ Missing classes:", missing, "\n")
    if (length(extra)>0)   cat("❌ Unexpected classes:", extra, "\n")
  }
  
  # Class distribution
  cat("\nCell count per class:\n")
  print(f)
  
  total_cells <- sum(f$count)
  
  if (999 %in% values_present) {
    nodata_cells <- f$count[f$value == 999]
    cat("\nNODATA cells:", nodata_cells,
        sprintf("(%.2f%% of raster)\n", 100 * nodata_cells / total_cells))
  }
  
  # Red flags
  if (length(values_present) == 1 && values_present == 999) {
    cat("🚨 WARNING: Raster is entirely NODATA\n")
  }
  
  if (any(values_present > 100 & !values_present %in% expected)) {
    cat("🚨 WARNING: Looks like original land cover codes remain\n")
  }
  
  cat("==============================\n")
}

base <- "E:/TFM_gangas/LULUCF/"

files <- c(
  "LULUCF2015_PB_Reclass.tif",
  "LULUCF2018_PB_Reclass.tif",
  "LULUCF2021_PB_Reclass.tif",
  "COS2023v1-S2_Reclass.tif",
  "CLC2018/CLC2018_PB_Reclass.tif"
)

for (f in files) {
  check_reclass(paste0(base, f))
}





############################
# AREA PERCENTAGE PER CLASS
############################

library(terra)
library(openxlsx)

base_folder <- "E:/TFM_gangas/LULUCF/"

files <- list(
  LULUCF2015 = paste0(base_folder, "LULUCF2015_PB_Reclass.tif"),
  LULUCF2018 = paste0(base_folder, "LULUCF2018_PB_Reclass.tif"),
  LULUCF2021 = paste0(base_folder, "LULUCF2021_PB_Reclass.tif"),
  COS2023    = paste0(base_folder, "COS2023v1-S2_Reclass.tif"),
  CLC2018    = paste0(base_folder, "CLC2018/CLC2018_PB_Reclass.tif")
)

class_names <- c(
  "1"="Bosques","2"="Olivares","3"="Viñedos","4"="Otros cultivos perennes",
  "5"="Arrozales","6"="Invernaderos","7"="Cultivos anuales de secano",
  "8"="Pastizales con árboles","9"="Pastizales arbustivos",
  "10"="Pastizales herbáceos","11"="Zonas acuáticas","12"="Marismas",
  "13"="Áreas artificiales","14"="Otras tierras",
  "15"="Mosaico de cultivos complejos","16"="Cultivos anuales de regadío",
  "999"="NODATA"
)

wb <- createWorkbook()

for (name in names(files)) {
  
  cat("Processing", name, "\n")
  r <- rast(files[[name]])
  f <- as.data.frame(freq(r))
  f <- f[!is.na(f$value), ]
  f$class_name <- class_names[as.character(f$value)]
  
  addWorksheet(wb, name)
  
  # =========================================================
  # TABLE 1 — INCLUDING NODATA (real raster proportions)
  # =========================================================
  total_all <- sum(f$count)
  tab_all <- f
  tab_all$percentage <- round((tab_all$count / total_all) * 100, 2)
  tab_all <- tab_all[order(as.numeric(tab_all$value)), c("value","class_name","count","percentage")]
  names(tab_all) <- c("Class value","Class name","Cell count","Percentage (%)")
  
  writeData(wb, name, "Percentages INCLUDING NODATA", startRow=1, startCol=1)
  writeData(wb, name, tab_all, startRow=2, startCol=1)
  
  # =========================================================
  # TABLE 2 — EXCLUDING NODATA (CLC only)
  # =========================================================
  if (name == "CLC2018") {
    
    tab_land <- f[f$value != 999, ]   # REMOVE NODATA ROWS
    total_land <- sum(tab_land$count) # NEW denominator
    
    tab_land$percentage <- round((tab_land$count / total_land) * 100, 2)
    tab_land <- tab_land[order(as.numeric(tab_land$value)), c("value","class_name","count","percentage")]
    names(tab_land) <- c("Class value","Class name","Cell count","Percentage (%)")
    
    start_row <- nrow(tab_all) + 5
    
    writeData(wb, name, "Percentages EXCLUDING NODATA (Land only)", startRow=start_row, startCol=1)
    writeData(wb, name, tab_land, startRow=start_row + 1, startCol=1)
  }
}

saveWorkbook(wb, paste0(base_folder, "Class_percentage.xlsx"), overwrite=TRUE)

cat("Excel saved in:", paste0(base_folder, "Class_percentage.xlsx"), "\n")
