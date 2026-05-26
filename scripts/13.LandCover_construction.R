################################ 
# LULUCF RECLASSIFICATION SCRIPT
################################

library(terra)

# -----------------------
# PATHS
# -----------------------
base_folder <- "E:/TFM_gangas/UsosSuelo/LULUCF"
lulucf_files <- c("/Originales/LULUCF2015_PB.tif", "/Originales/LULUCF2018_PB.tif", "/Originales/LULUCF2021_PB.tif")

# -----------------------
# FINAL CLASS VALUES
# -----------------------
classes <- c(
  "Bosques" = 1,
  "Viñedos" = 2,
  "Otros cultivos permanentes" = 3,
  "Arrozales" = 4,
  "Invernaderos" = 5,
  "Cultivos anuales" = 6,
  "Pastizales con árboles" = 7,
  "Pastizales arbustivos" = 8,
  "Pastizales herbáceos" = 9,
  "Zonas acuáticas" = 10,
  "Marismas" = 11,
  "Áreas artificiales" = 12,
  "Otras tierras" = 13,
  "Mosaico de cultivos complejos" = 14
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
  711,711,classes["Otros cultivos permanentes"],
  712,712,classes["Viñedos"],
  713,713,classes["Otros cultivos permanentes"],
  714,714,classes["Otros cultivos permanentes"],
  715,715,classes["Mosaico de cultivos complejos"],
  719,719,classes["Otros cultivos permanentes"],
  
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




############################################
# LULUCF PROPORTIONAL LAND COVER (NDVI GRID)
############################################

library(terra)

# -----------------------
# PATHS
# -----------------------
base_dir <- "E:/TFM_gangas/UsosSuelo/LULUCF/"
ndvi_template_file <- "E:/TFM_gangas/NDVI/SpainReprojected/300m/c_gls_NDVI300_201601010000_GLOBE_PROBAV_V1.0.1_25830_300m.tif"

out_dir <- file.path(base_dir, "300m")
dir.create(out_dir, showWarnings = FALSE)

# -----------------------
# INPUT FILES
# -----------------------
lulucf_files <- c(
  "LULUCF2015_PB_Reclass.tif",
  "LULUCF2018_PB_Reclass.tif",
  "LULUCF2021_PB_Reclass.tif"
)

# -----------------------
# CLASS DEFINITIONS
# -----------------------
classes <- c(
  "Bosques" = 1,
  "Viñedos" = 2,
  "Otros cultivos permanentes" = 3,
  "Arrozales" = 4,
  "Invernaderos" = 5,
  "Cultivos anuales" = 6,
  "Pastizales con árboles" = 7,
  "Pastizales arbustivos" = 8,
  "Pastizales herbáceos" = 9,
  "Zonas acuáticas" = 10,
  "Marismas" = 11,
  "Áreas artificiales" = 12,
  "Otras tierras" = 13,
  "Mosaico de cultivos complejos" = 14
)

# -----------------------
# LOAD NDVI TEMPLATE
# -----------------------
ndvi_template <- rast(ndvi_template_file)

# -----------------------
# PROCESS EACH YEAR
# -----------------------
for (f in lulucf_files) {
  
  cat("\nProcessing", f, "...\n")
  
  r <- rast(file.path(base_dir, f))
  fact <- 300 / res(r)[1]
  
  lc_stack <- rast()
  
  for (i in seq_along(classes)) {
    
    class_name <- names(classes)[i]
    class_val  <- unname(classes[i])
    
    cat("  →", class_name, "\n")
    
    r_bin <- r == class_val
    
    r_prop <- aggregate(
      r_bin,
      fact = fact,
      fun = mean,
      na.rm = TRUE
    )
    
    names(r_prop) <- paste0("LC_", gsub(" ", "_", class_name))
    
    lc_stack <- c(lc_stack, r_prop)
    
    rm(r_bin, r_prop); gc()
  }
  
  # -----------------------
  # ALIGN TO NDVI GRID
  # -----------------------
  lc_stack <- resample(
    lc_stack,
    ndvi_template,
    method = "bilinear"
  )
  
  year_tag <- gsub(".*(2015|2018|2021).*", "\\1", f)
  out_file <- file.path(out_dir, paste0("LULUCF_LC_", year_tag, "_300m.tif"))
  
  writeRaster(lc_stack, out_file, overwrite = TRUE)
  cat("Saved:", out_file, "\n")
  
  rm(r, lc_stack); gc()
}

cat("\nLULUCF processing completed.\n")






############################################
# COS2023 PROPORTIONAL LAND COVER (NDVI GRID)
############################################

library(terra)

# -----------------------
# PATHS
# -----------------------
base_dir <- "E:/TFM_gangas/UsosSuelo/COS2023/"
gpkg_file <- file.path(base_dir, "Originales/COS2023v1-S2.gpkg")
ndvi_template_file <- "E:/TFM_gangas/NDVI/SpainReprojected/300m/c_gls_NDVI300_201601010000_GLOBE_PROBAV_V1.0.1_25830_300m.tif"

out_dir <- file.path(base_dir, "300m")
dir.create(out_dir, showWarnings = FALSE)

# -----------------------
# CLASS DEFINITIONS
# -----------------------
classes <- c(
  "Bosques" = 1,
  "Viñedos" = 2,
  "Otros cultivos permanentes" = 3,
  "Arrozales" = 4,
  "Invernaderos" = 5,
  "Cultivos anuales" = 6,
  "Pastizales con árboles" = 7,
  "Pastizales arbustivos" = 8,
  "Pastizales herbáceos" = 9,
  "Zonas acuáticas" = 10,
  "Marismas" = 11,
  "Áreas artificiales" = 12,
  "Otras tierras" = 13,
  "Mosaico de cultivos complejos" = 14
)

# -----------------------
# PORTUGAL CLASS TABLE
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
  class = c(
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
    "Otros cultivos permanentes","Otros cultivos permanentes","Mosaico de cultivos complejos","Mosaico de cultivos complejos",
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
    "Zonas acuáticas","Zonas acuáticas","Zonas acuáticas"
  )
)

# -----------------------
# LOAD NDVI TEMPLATE
# -----------------------
ndvi_template <- rast(ndvi_template_file)

# -----------------------
# LOAD AND RECLASSIFY VECTOR
# -----------------------
v <- vect(gpkg_file)
v$class_name <- pt_table$class[match(v$COS23_n4_C, pt_table$code)]
v$class_val  <- unname(classes[v$class_name])
v <- project(v, "EPSG:25830")

# -----------------------
# RASTERIZE AT FINE RESOLUTION
# -----------------------
fine_res <- 25
r_fine <- rast(ext(v), resolution = fine_res, crs = "EPSG:25830")

r_cos <- rasterize(v, r_fine, field = "class_val", touches = TRUE)

rm(v, r_fine); gc()

# -----------------------
# PROPORTIONS + ALIGNMENT
# -----------------------
fact <- 300 / fine_res
lc_stack <- rast()

for (i in seq_along(classes)) {
  
  class_name <- names(classes)[i]
  class_val  <- unname(classes[i])
  
  cat("  →", class_name, "\n")
  
  r_bin <- r_cos == class_val
  
  r_prop <- aggregate(
    r_bin,
    fact = fact,
    fun = mean,
    na.rm = TRUE
  )
  
  names(r_prop) <- paste0("LC_", gsub(" ", "_", class_name))
  
  lc_stack <- c(lc_stack, r_prop)
  
  rm(r_bin, r_prop); gc()
}

rm(r_cos); gc()

lc_stack <- resample(
  lc_stack,
  ndvi_template,
  method = "bilinear"
)

out_file <- file.path(out_dir, "COS2023_LC_300m.tif")
writeRaster(lc_stack, out_file, overwrite = TRUE)

cat("Saved:", out_file, "\n")
cat("\nCOS2023 processing completed.\n")




################################ 
# LC FREQUENCY CHECK
################################
library(dplyr)
library(openxlsx)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
base_path <- "E:/TFM_gangas/GPS/Extracted"

files <- list(
  PTS = file.path(base_path, "PTS_pseudoabsences_Random_env.csv"),
  BBS = file.path(base_path, "BBS_pseudoabsences_Random_env.csv")
)

# ------------------------------------------------------------
# Create workbook
# ------------------------------------------------------------
wb <- createWorkbook()

# ------------------------------------------------------------
# Loop species
# ------------------------------------------------------------
for(sp in names(files)) {
  
  cat("\nProcessing:", sp, "\n")
  
  env <- read.csv(files[[sp]])
  
  lc_cols <- grep("^LC_", names(env), value = TRUE)
  
  lc_summary <- data.frame(
    Variable = lc_cols,
    Prop_nonzero = sapply(env[, lc_cols], function(x) mean(x > 0, na.rm = TRUE)),
    Mean = sapply(env[, lc_cols], function(x) mean(x, na.rm = TRUE)),
    SD   = sapply(env[, lc_cols], function(x) sd(x, na.rm = TRUE))
  )
  
  # Add worksheet
  addWorksheet(wb, paste0(sp, "_Random"))
  writeData(wb, paste0(sp, "_Random"), lc_summary)
}

# ------------------------------------------------------------
# Save Excel
# ------------------------------------------------------------
saveWorkbook(
  wb,
  file.path(base_path, "LC_frequency_Random.xlsx"),
  overwrite = TRUE
)

cat("\nLC frequency Excel saved successfully\n")



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
  expected <- 1:14
  
  missing <- setdiff(expected, values_present)
  extra   <- setdiff(values_present, expected)
  
  if (length(missing)==0 & length(extra)==0) {
    cat("✔ Class values are correct\n")
  } else {
    if (length(missing)>0) cat("Missing classes:", missing, "\n")
    if (length(extra)>0)   cat("Unexpected classes:", extra, "\n")
  }
  
  # Class distribution
  cat("\nCell count per class:\n")
  print(f)
  
  # Red flags
  if (any(values_present > 100 & !values_present %in% expected)) {
    cat("WARNING: Looks like original land cover codes remain\n")
  }
  
  cat("==============================\n")
}

base <- "E:/TFM_gangas/UsosSuelo/"

files <- c(
  "LULUCF/300m/LULUCF2015_300m.tif",
  "LULUCF/300m/LULUCF2018_300m.tif",
  "LULUCF/300m/LULUCF2021_300m.tif",
  "COS2023/300m/COS2023v1-S2_300m.tif"
)

for (f in files) {
  check_reclass(paste0(base, f))
}