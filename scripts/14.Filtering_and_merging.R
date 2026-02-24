# =====================================================
# REVIEW NEARET POINT TO EACH TARGET HOUR SCRIPT
# =====================================================

library(data.table)
library(lubridate)
library(tictoc)

base_path <- "E:/TFM_gangas/GPS"

csv_files <- list.files(base_path, pattern = "\\.csv$", full.names = TRUE)

all_results <- list()
file_index <- 1

tic("FULL PROCESS")

for (file in csv_files) {
  
  tic(paste("File", file_index))
  
  dt <- fread(file, encoding = "UTF-8")
  
  # Ensure timestamp is proper POSIXct
  dt[, timestamp_gmt0 := as.POSIXct(timestamp_gmt0, tz = "UTC")]
  
  # Remove rows with missing timestamps
  dt <- dt[!is.na(timestamp_gmt0)]
  
  # Extract date and minutes of day
  dt[, date := as.Date(timestamp_gmt0)]
  dt[, minutes_of_day := hour(timestamp_gmt0) * 60 + minute(timestamp_gmt0)]
  
  # Target hours in minutes
  targets <- c(8, 12, 16, 20) * 60
  
  # For each bird and day, find nearest point to each target hour
  res <- dt[, {
    
    out <- lapply(targets, function(tg) {
      diffs <- abs(minutes_of_day - tg)
      idx <- which.min(diffs)
      list(
        target_hour = tg / 60,
        real_time = format(timestamp_gmt0[idx], "%H:%M:%S"),
        diff_minutes = diffs[idx]
      )
    })
    
    rbindlist(out)
    
  }, by = .(birdID, date)]
  
  all_results[[file_index]] <- res
  
  rm(dt, res)
  gc()
  
  toc()
  file_index <- file_index + 1
}

toc()

# Combine all files
final_dt <- rbindlist(all_results)

cat("\nSummary of distance to nearest fix (minutes):\n")
print(summary(final_dt$diff_minutes))

cat("\nQuantiles:\n")
print(quantile(final_dt$diff_minutes, probs = c(0.5, 0.75, 0.9, 0.95)))

cat("\nSummary by target hour:\n")
print(final_dt[, .(
  median = median(diff_minutes),
  p75 = quantile(diff_minutes, 0.75),
  p90 = quantile(diff_minutes, 0.9)
), by = target_hour])





# ===========================================
# FILTER AND MERGE SANDGROUSE CSV SCRIPT
# ===========================================

library(data.table)
library(lubridate)
library(tictoc)

base_path <- "E:/TFM_gangas/GPS"

csv_files <- list.files(base_path, pattern = "\\.csv$", full.names = TRUE)

bbs_files <- csv_files[grepl("BBS", basename(csv_files))]
pts_files <- csv_files[grepl("PTS", basename(csv_files))]

process_species <- function(file_list, species_name) {
  
  all_results <- list()
  file_index <- 1
  
  tic(paste("Processing", species_name))
  
  for (file in file_list) {
    
    tic(paste("File", basename(file)))
    
    dt <- fread(file, encoding = "UTF-8")
    
    if (!"timestamp_gmt0" %in% names(dt)) next
    
    dt[, timestamp_gmt0 := as.POSIXct(timestamp_gmt0, tz = "UTC")]
    dt <- dt[!is.na(timestamp_gmt0)]
    
    # Remove outliers
    if ("outliers" %in% names(dt)) {
      dt <- dt[is.na(outliers) | outliers != "YES"]
    }
    
    # Special rule for PTS_12901
    if (grepl("PTS_12901", basename(file))) {
      dt <- dt[timestamp_gmt0 >= as.POSIXct("2023-03-15 00:00:00", tz = "UTC")]
    }
    
    # Remove data after 31 Dec 2024
    dt <- dt[timestamp_gmt0 <= as.POSIXct("2024-12-31 23:59:59", tz = "UTC")]
    
    dt[, date := as.Date(timestamp_gmt0)]
    dt[, minutes_of_day := hour(timestamp_gmt0) * 60 + minute(timestamp_gmt0)]
    
    targets <- c(8, 12, 16, 20)
    window <- 10
    
    file_results <- list()
    
    for (h in targets) {
      
      target_min <- h * 60
      
      temp <- copy(dt)
      temp[, diff := abs(minutes_of_day - target_min)]
      
      # Keep only fixes within ±10 minutes
      temp <- temp[diff <= window]
      
      if (nrow(temp) == 0) next
      
      # Keep closest fix per bird per day per hour
      temp <- temp[order(diff)]
      temp <- temp[, .SD[1], by = .(birdID, date)]
      temp[, target_hour := h]
      
      file_results[[as.character(h)]] <- temp
    }
    
    if (length(file_results) > 0) {
      all_results[[file_index]] <- rbindlist(file_results, fill = TRUE)
    }
    
    rm(dt, temp, file_results)
    gc()
    
    toc()
    file_index <- file_index + 1
  }
  
  toc()
  
  final_dt <- rbindlist(all_results, fill = TRUE)
  return(final_dt)
}

# Run processing
bbs_data <- process_species(bbs_files, "BBS")
pts_data <- process_species(pts_files, "PTS")

# Save outputs
fwrite(bbs_data, file.path(base_path, "BBS_filtered_merged.csv"))
fwrite(pts_data, file.path(base_path, "PTS_filtered_merged.csv"))

cat("\nDONE. Files created:\n")
cat("BBS_filtered_merged.csv\n")
cat("PTS_filtered_merged.csv\n")




# ====================================
# CHECK SCRIPT
# ====================================

library(data.table)
library(lubridate)

base_path <- "E:/TFM_gangas/GPS"

bbs <- fread(file.path(base_path, "BBS_filtered_merged.csv"))
pts <- fread(file.path(base_path, "PTS_filtered_merged.csv"))

# ------------------------------------
# 1. Bird ID completeness check
# ------------------------------------
csv_files <- list.files(base_path, pattern = "\\.csv$", full.names = FALSE)

bbs_ids_files <- unique(sub(".*_(BBS[^\\.]+)\\.csv", "\\1", csv_files[grepl("BBS", csv_files)]))
pts_ids_files <- unique(sub(".*_(PTS[^\\.]+)\\.csv", "\\1", csv_files[grepl("PTS", csv_files)]))

bbs_ids_data <- unique(bbs$birdID)
pts_ids_data <- unique(pts$birdID)

cat("\nBBS birds in files:", length(bbs_ids_files))
cat("\nBBS birds in merged data:", length(bbs_ids_data), "\n")

cat("\nPTS birds in files:", length(pts_ids_files))
cat("\nPTS birds in merged data:", length(pts_ids_data), "\n")

cat("\nBBS missing birds:\n")
print(setdiff(bbs_ids_files, bbs_ids_data))

cat("\nPTS missing birds:\n")
print(setdiff(pts_ids_files, pts_ids_data))

# ------------------------------------
# 2. Day coverage
# ------------------------------------
check_days <- function(dt, name) {
  
  cat("\n====================\n")
  cat("DAY COVERAGE:", name, "\n")
  cat("====================\n")
  
  dt[, timestamp_gmt0 := as.POSIXct(timestamp_gmt0, tz = "UTC")]
  dt[, date := as.Date(timestamp_gmt0)]
  
  daily_counts <- dt[, .N, by = .(birdID, date)]
  
  print(table(daily_counts$N))
  cat("\nMean fixes per day:", mean(daily_counts$N), "\n")
}

check_days(bbs, "BBS")
check_days(pts, "PTS")

# ------------------------------------
# 3. Year coverage
# ------------------------------------
check_years <- function(dt, name) {
  
  cat("\n====================\n")
  cat("YEAR COVERAGE:", name, "\n")
  cat("====================\n")
  
  dt[, timestamp_gmt0 := as.POSIXct(timestamp_gmt0, tz = "UTC")]
  dt[, year := year(timestamp_gmt0)]
  
  print(dt[, .N, by = year][order(year)])
}

check_years(bbs, "BBS")
check_years(pts, "PTS")







# ===========================================
# CREATE INDIVIDUAL GPKG SCRIPT
# ===========================================

library(sf)
library(dplyr)


csv_files <- c(
  "E:/TFM_gangas/GPS/BBS_filtered_merged.csv",
  "E:/TFM_gangas/GPS/PTS_filtered_merged.csv"
)

# Loop to process both CSV files
for (csv_path in csv_files) {
  
  cat("Processing file:", csv_path, "\n")
  
  # Read CSV
  data <- read.csv(csv_path)
  
  # Keep only required columns
  data <- data %>% select(birdID, timestamp_gmt0, X_25830, Y_25830)
  
  # Convert to sf object using EPSG:25830
  data_sf <- st_as_sf(
    data,
    coords = c("X_25830", "Y_25830"),
    crs = 25830,
    remove = FALSE
  )
  
  # Ensure birdID is character
  data_sf$birdID <- as.character(data_sf$birdID)
  
  # Get unique individuals
  individuals <- unique(data_sf$birdID)
  
  # Output path (same folder, add _indi)
  output_gpkg <- gsub(".csv$", "_indi.gpkg", csv_path)
  
  # Remove existing file if present
  if (file.exists(output_gpkg)) file.remove(output_gpkg)
  
  # Write one layer per individual
  for (id in individuals) {
    cat("  Writing individual:", id, "\n")
    
    ind_data <- data_sf %>% filter(birdID == id)
    
    st_write(
      ind_data,
      output_gpkg,
      layer = id,
      append = TRUE,
      quiet = TRUE
    )
  }
  
  cat("Finished file:", output_gpkg, "\n")
  
  # Clean memory
  rm(data, data_sf, individuals, ind_data)
  gc()
}

cat("All files processed successfully ✅\n")







# ======================================================
# AVOID PSEUDOREPLICATION SCRIPT AND SELECT COLS
# ======================================================

library(data.table)
library(lubridate)
library(terra)

# 1. Paths and input files

base_path <- "E:/TFM_gangas/GPS/Merged"

input_files <- list(
  BBS = file.path(base_path, "BBS_filtered_merged.csv"),
  PTS = file.path(base_path, "PTS_filtered_merged.csv")
)

output_files <- list(
  BBS = file.path(base_path, "BBS_filtered_NoPseudoreplication.csv"),
  PTS = file.path(base_path, "PTS_filtered_NoPseudoreplication.csv")
)

# NDVI folder (used as temporal reference)
ndvi_path <- "E:/TFM_gangas/NDVI/SpainReprojected/300m"

ndvi_files <- sort(list.files(ndvi_path, pattern = "\\.tif$", full.names = FALSE))

# 2. Build NDVI decade calendar

ndvi_calendar <- data.table(file = ndvi_files)
ndvi_calendar[, date_str := sub(".*_(\\d{8})0000_.*", "\\1", file)]
ndvi_calendar[, decade_start := as.Date(date_str, format = "%Y%m%d")]
ndvi_calendar <- ndvi_calendar[!is.na(decade_start)]

setorder(ndvi_calendar, decade_start)
ndvi_calendar[, ten_days_id := .I]

# Safety check
cat("\nNDVI temporal reference:\n")
cat("First decade:", as.character(min(ndvi_calendar$decade_start)), "\n")
cat("Last decade:", as.character(max(ndvi_calendar$decade_start)), "\n")
cat("Total decade:", max(ndvi_calendar$ten_days_id), "\n")

# 3. Function to assign ten_days_id to GPS data

assign_ten_days_id <- function(dt, ndvi_cal) {
  
  setkey(ndvi_cal, decade_start)
  
  dt[, ten_days_start := as.Date(
    ifelse(
      day(date) <= 10, paste(year(date), month(date), "01", sep = "-"),
      ifelse(
        day(date) <= 20, paste(year(date), month(date), "11", sep = "-"),
        paste(year(date), month(date), "21", sep = "-")
      )
    )
  )]
  
  dt <- ndvi_cal[dt, on = .(decade_start = ten_days_start)]
  
  dt
}

# 4. Process each species

for (sp in names(input_files)) {
  
  cat("\n====================\n")
  cat("Processing species:", sp, "\n")
  cat("====================\n")
  
  dt <- fread(input_files[[sp]])
  
  # Keep only relevant columns
  cols_keep <- c(
    "birdID",
    "date",
    "X_25830",
    "Y_25830",
    "species"
  )
  
  dt <- dt[, ..cols_keep]
  
  # Assign ten_days_id
  dt[, date := as.Date(date)]
  dt <- assign_ten_days_id(dt, ndvi_calendar)
  
  dt <- dt[!is.na(ten_days_id)]
  
  # Create cell_id (based on NDVI grid)
  ndvi_ref <- rast(file.path(ndvi_path, ndvi_files[1]))
  
  coords <- as.matrix(dt[, .(X_25830, Y_25830)])
  dt[, cell_id := cellFromXY(ndvi_ref, coords)]
  
  # Collapse to unique environmental presences
  n_before <- nrow(dt)
  
  dt_unique <- unique(dt, by = c("cell_id", "ten_days_id"))
  
  n_after <- nrow(dt_unique)
  
  cat("Records before collapse:", n_before, "\n")
  cat("Records after collapse :", n_after, "\n")
  cat("Removed as pseudoreplicates:", n_before - n_after, "\n")
  
  # Select final columns and order
  dt_unique <- dt_unique[, .(
    birdID,
    date,
    X_25830,
    Y_25830,
    cell_id,
    ten_days_id,
    species
  )]
  
  # Save output
  fwrite(dt_unique, output_files[[sp]])
  
  cat("Saved:", basename(output_files[[sp]]), "\n")
  
  rm(dt, dt_unique)
  gc()
}

# ======================================================
# 5. Final sanity checks
# ======================================================

cat("\n====================\n")
cat("FINAL SANITY CHECKS\n")
cat("====================\n")

for (sp in names(output_files)) {
  
  dt <- fread(output_files[[sp]])
  
  dup_check <- dt[, .N, by = .(cell_id, ten_days_id)][N > 1]
  
  cat("\n", sp, "\n")
  cat("Total records:", nrow(dt), "\n")
  cat("Duplicated (cell_id, ten_days_id):", nrow(dup_check), "\n")
  
  if (nrow(dup_check) == 0) {
    cat("✔ No pseudoreplication detected\n")
  } else {
    cat("⚠ WARNING: duplicates found\n")
  }
}
