#===============================================================================
# Description: Script to prepare GAEZ data at the commune level
#===============================================================================

# Clean memory 
rm(list=ls())
gc()

#===============================================================================
# 1). Prepare environment ------
#===============================================================================

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, GAEZr, here, rnaturalearthdata, rnaturalearth, sf, terra)

# Set file path, where to put downloaded GAEZ data
file_gaezdata <- "C:/Users/loihenry/Dropbox/Recherche_Dauphine/DataArchive/GAEZ_data/Data"

# Create list of useful directories 
dir <- list()
dir$root <- here()
dir$file_gaezdata <- file_gaezdata
dir$data_th3 <- here(file_gaezdata, "Theme3")
dir$data_th4 <- here(file_gaezdata, "Theme4")
dir$dataprepared <- here(dir$root, "data_prepared")
dir$output <- here(dir$root, "output")
dir$script <- here(dir$root, "script")
dir$sf <- here(dir$file_gaezdata, "Shapefiles")

# Create non existing directories
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F))

# Execute script "00_functions.R" to load functions
source(here(dir$script, "01_assembledata.R"))

#===============================================================================
# 2). Get data for wheat, maize, soy, barley, sugar beet, olive, tomato, carrot ------
#===============================================================================

# Vector of crop choices

# So far: chose 5 RPG crops with their equivalent in RPG
# GAEZ           ~ RPG
# Gram, chickpea ~ Légumineuse à grain
# Maize          ~ Maïs grain et ensilage
# Wheat          ~ Blé tendre
# Flax           ~ Plantes à fibre
# Dry Pea        ~ Protéagineux
crops <- c("Chickpea", "Flax", "Gram", "Maize", "Dry pea", "Wheat")

# Apply the function which assembles data to each crop
lapply(crops, function(crop_choice) {
  process_crop_yield(crop_choice = crop_choice, folder = dir$dataprepared, crops_theme3 = crops_theme3, crops_theme4 = crops_theme4)
})

#===============================================================================
# 3). Aggregate yield and yield changes at the commune level ------
#===============================================================================

data_files <- list.files(here(dir$dataprepared), pattern = "\\.rds$", recursive=TRUE, full.names=TRUE)

# Load communes sf
communes_sf <- st_read(here(dir$sf, "communes-20220101.shp"))
# Keep communes in metropolitan France + corsica
communes_sf_filt <- communes_sf %>% 
  filter(as.numeric(insee) < 96000 | grepl("^2A|^2B", insee))

# Get centroids
commune_centroids <- communes_sf_filt %>%
  mutate(centroid = st_centroid(geometry)) %>%
  st_set_geometry("centroid") 

# Function to process each file
aggregate_gaez_commune <- function(data_file) {
  # Read the rds file
  gaez_df <- readRDS(data_file)
  # Convert tidy df as a raster
  gaez_sf <- gaez_df %>%
    st_as_sf(coords = c("x", "y"), crs = st_crs(as.numeric(crs_gaez))) %>% 
    st_transform(st_crs(communes_sf)) # Transform to the same CRS as communes
  
  # Per scenario and variable, find for each commune centroid the nearest cell and assign the value and change in yield for this crop
  # Unique scenarios
  scenario_keys <- gaez_sf %>%
    st_drop_geometry() %>%
    distinct(model, rcp, theme_id, variable, crop)
  
  # Function to extract yield per scenario using nearest point
  get_yield_per_commune <- function(scenario_row) {
    
    # Filter yield points for that scenario
    scenario_sf <- gaez_sf %>%
      filter(
        model == scenario_row$model,
        rcp == scenario_row$rcp,
        theme_id == scenario_row$theme_id,
        variable == scenario_row$variable,
        crop == scenario_row$crop
      )
    
    # Perform nearest neighbor join (1 nearest cell per commune)
    matched <- st_nearest_feature(commune_centroids, scenario_sf)
    
    # Extract yield change values
    commune_centroids %>%
      mutate(
        value = scenario_sf$value[matched],
        value_hist = scenario_sf$value_hist[matched],
        change = scenario_sf$change[matched],
        model = scenario_row$model,
        rcp = scenario_row$rcp,
        theme_id = scenario_row$theme_id,
        variable = scenario_row$variable,
        crop = scenario_row$crop
      )
  }
  
  # Apply for all scenarios
  commune_yield_all <- scenario_keys %>%
    split(1:nrow(.)) %>%
    map_dfr(~get_yield_per_commune(.x))

}

# Apply the function to all files and combine results
all_summaries <- map_dfr(data_files, aggregate_gaez_commune)

# Save results
saveRDS(all_summaries, file = here(dir$dataprepared, "GAEZ_yieldchange_communes.rds"))

