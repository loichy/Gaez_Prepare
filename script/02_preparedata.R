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
dir$datafinal <- here(dir$root, "data_final")
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
# crops_filter <- c("Chickpea", "Flax", "Gram", "Maize", "Dry pea", "Wheat")
crops <- c(unique(crops_theme4$crop), "Grass")

# Apply the function which assembles data to each crop
lapply(crops, function(crop_choice) {
  process_crop_yield(crop_choice = crop_choice, folder = dir$dataprepared, crops_theme3 = crops_theme3, crops_theme4 = crops_theme4)
})

# For Gras (manually)s:

yield_list_th3 <- list.files(dir$data_th3, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)

# 2). Get crop codes (with checks)
crop_code_grass <- crops_theme3 %>% filter(crop == "Grass") %>% pull(code)
yield_grass_list_th3 <- yield_list_th3[grepl(crop_code_grass, yield_list_th3)]  
  
# 3). Define info extractor
extract_info <- function(path) {
  
  path <- gsub("\\\\", "/", path)
  parts <- unlist(strsplit(path, "/"))
  idx <- which(grepl("Theme", parts))
  model <- parts[idx + 1]
  rcp   <- parts[idx + 2]
  time  <- parts[idx + 3]
  file  <- basename(path)
  name_parts <- stringr::str_match(file, "^(.*)_(.*)\\.tif$")[-1]
  crop <- "Grass"
  variable <- name_parts[(!grepl(crop_code_grass, name_parts))]
  return(paste(model, rcp, time, crop, variable, sep = "_"))
}
  
# Prepare metropolitan France geometry
france <- rnaturalearth::ne_countries(scale = "medium", country = "France", returnclass = "sf")
metropolitan_bbox <- sf::st_bbox(c(xmin = -5, xmax = 10, ymin = 41, ymax = 52), crs = sf::st_crs(france))
bbox_sf <- sf::st_as_sfc(metropolitan_bbox)
metropolitan_france <- sf::st_intersection(france, bbox_sf)
  
# Prepare results containers
yield_grass_final <- NULL
 
raster_stack_grass <- terra::rast(yield_grass_list_th3)
names(raster_stack_grass) <- sapply(yield_grass_list_th3, extract_info)
metropolitan_france <- sf::st_transform(metropolitan_france, terra::crs(raster_stack_grass))
raster_grass_france <- terra::crop(raster_stack_grass, terra::vect(metropolitan_france)) %>%
  terra::mask(terra::vect(metropolitan_france))
yield_grass_df <- as.data.frame(raster_grass_france, xy = TRUE, na.rm = TRUE)
yield_grass_df_long <- yield_grass_df %>%
  pivot_longer(cols = -c(x, y), names_to = "layer", values_to = "value") %>%
  separate(layer, into = c("model", "rcp", "period", "crop", "variable"), sep = "_") %>%
  mutate(theme_id = "3") %>%
  arrange(variable, crop, model, rcp)
yield_grass_df_final <- yield_grass_df_long %>%
  select(x, y, theme_id, crop, variable, model, rcp, period, value)
historical_grass <- yield_grass_df_final %>%
  filter(rcp == "Hist") %>%
  select(x, y, theme_id, crop, value) %>%
  rename(value_hist = value)
yield_grass_final <- yield_grass_df_final %>%
  left_join(historical_grass, by = c("x", "y", "theme_id", "crop")) %>%
  mutate(change = value / value_hist - 1)

saveRDS(object = yield_grass_final, file = here::here(dir$dataprepared, paste0("GAEZ_yieldchange_", "Grass", ".rds")))

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
all_summaries_df <- all_summaries %>% 
  st_drop_geometry() %>% 
  select(-geometry)
  

# Filter for only rcp8p5 and theme4
all_summaries_df_filtered <- all_summaries_df %>%
  filter(rcp %in% c("Hist", "rcp8p5"), 
         # theme_id == "4",
         model %in% c("CRUTS", "HadGEM2-ES")) 
  # Remove all geometries to have a df

object.size(all_summaries_df_filtered)
# Save results
saveRDS(all_summaries_df_filtered, file = here(dir$datafinal, "GAEZ_yieldchange_communes_filt.rds"))
saveRDS(all_summaries_df, file = here(dir$datafinal, "GAEZ_yieldchange_communes.rds"))


