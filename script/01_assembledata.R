#===============================================================================
# Description: Script to limit downloaded GAEZ data to France, and assemble
# all file in a data frame format
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

# Set file path, where to load downloaded GAEZ data
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


#===============================================================================
# 2). Load dataset containing all crop codes ------
#===============================================================================

### source : Documentation/Variables, Crops and Symbology Spreadsheet
crops_theme <- readxl::read_excel(
  here(dir$file_gaezdata,
       "GAEZ4_DB_Variables_Symbology_Crops.xlsx"),
  sheet = "crops by theme"
)

## Data cleaning
crops_theme3 <- crops_theme |>
  filter(`Theme id` == 3) |>
  rename(crop = `Crop name`, code = `Crop acronym`) |>
  select(-`Theme id`, -`Theme acronym`, -`Theme name`)

crops_theme4 <- crops_theme |>
  filter(`Theme id` == 4) |>
  rename(crop = `Crop name`, code = `Crop acronym`) |>
  select(-`Theme id`, -`Theme acronym`, -`Theme name`)

crs_gaez <- list.files(dir$data_th3, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)[1] %>% 
  terra::rast() %>% 
  crs(, describe =T) %>% 
  pull(code) # GAEZ data should be in WGS84

#===============================================================================
# 3). Function to process data for a given crop ------
#===============================================================================


process_crop_yield <- function(crop_choice, folder, crops_theme3, crops_theme4) {

  crop_choice <- "Rubber"
  
  # 1). Find all .tif file for a given theme and a given crop and load them ------

  yield_list_th3 <- list.files(dir$data_th3, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
  yield_list_th4 <- list.files(dir$data_th4, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
  
  # 2). Get crop codes
  crop_code3 <- crops_theme3 %>% 
    filter(crop == crop_choice) %>% 
    pull(code)
  crop_code4 <- crops_theme4 %>% 
    filter(crop == crop_choice) %>% 
    pull(code)

  # Filter files for chosen crop
  yield_crop_list_th3 <- yield_list_th3[grepl(crop_code3, yield_list_th3)]
  yield_crop_list_th4 <- yield_list_th4[grepl(crop_code4, yield_list_th4)]

  # 3). Stack all tif files into a spatial raster
  raster_stack_th3 <- terra::rast(yield_crop_list_th3)
  raster_stack_th4 <- terra::rast(yield_crop_list_th4)

  # 4). Extract metadata from paths and add them to each layers

  extract_info <- function(path) {
    # path <- yield_crop_list_th3[1]
    # Normalize slashes for consistency
    path <- gsub("\\\\", "/", path)
    # Extract relevant parts
    parts <- unlist(strsplit(path, "/"))
    # Find the index of "Theme"
    idx <- which(grepl("Theme", parts))
    
    # Extract model names, rcp names, and time period
    model <- parts[idx + 1]
    rcp   <- parts[idx + 2]
    time  <- parts[idx + 3]
    file  <- basename(path)
    name_parts <- str_match(file, "^(.*)_(.*)\\.tif$")[-1]
    crop <- crop_choice
    variable <- name_parts[(!grepl(crop_code4, name_parts)) & (!grepl(crop_code3, name_parts))]
    # Return as a character string
    return(paste(model, rcp, time, crop, variable, sep = "_"))
}

  # Generate names for each layer
  layer_names_th3 <- sapply(yield_crop_list_th3, extract_info)
  layer_names_th4 <- sapply(yield_crop_list_th4, extract_info)

  # Assign layer names
  names(raster_stack_th3) <- layer_names_th3
  names(raster_stack_th4) <- layer_names_th4

  # 5). Crop raster data to France borders

  france <- ne_countries(scale = "medium", country = "France", returnclass = "sf")

  # Keep metropolitan france
  metropolitan_bbox <- st_bbox(c(
    xmin = -5,   # Western boundary (Brittany)
    xmax = 10,   # Eastern boundary (Alsace)
    ymin = 41,   # Southern boundary (Corsica included)
    ymax = 52    # Northern boundary (Lille area)
  ), crs = st_crs(france))
  
  # Convert bbox to sf object
  bbox_sf <- st_as_sfc(metropolitan_bbox)
  
  # Clip France polygon to this bbox
  metropolitan_france <- st_intersection(france, bbox_sf)


  # Crop and mask the raster stack to the metropolitan France area
  metropolitan_france <- st_transform(metropolitan_france, terra::crs(raster_stack_th3))
  
  raster_th3_france <- crop(raster_stack_th3, vect(metropolitan_france)) %>% 
    mask(vect(metropolitan_france))
  raster_th4_france <- crop(raster_stack_th4, vect(metropolitan_france)) %>% 
    mask(vect(metropolitan_france))      

  # 6). Convert as data frame and reshape it
  ## Dataframe with coordinates
  yield_th3_df <- as.data.frame(raster_th3_france, xy = TRUE, na.rm = TRUE)
  yield_th4_df <- as.data.frame(raster_th4_france, xy = TRUE, na.rm = TRUE)

  # Pivot to long format (gather all layers into rows)
  yield_th3_df_long <- yield_th3_df %>%
    pivot_longer(
      cols = -c(x, y),
      names_to = "layer",
      values_to = "value"
    ) %>%
    separate(layer, into = c("model", "rcp", "period", "crop", "variable"), sep = "_") %>% 
    mutate(theme_id = "3") %>% 
    arrange(variable, crop, model, rcp)

  yield_th4_df_long <- yield_th4_df %>%
    pivot_longer(
      cols = -c(x, y),
      names_to = "layer",
      values_to = "value"
    ) %>%
    separate(layer, into = c("model", "rcp", "period", "crop", "variable"), sep = "_") %>% 
    mutate(theme_id = "4")%>% 
    arrange(variable, crop, model, rcp)

  # Final tidy format
  yield_th3_df_final <- yield_th3_df_long %>%
    select(x, y, theme_id, crop, variable, model, rcp, period, value)
  yield_th4_df_final <- yield_th4_df_long %>%
    select(x, y, theme_id, crop, variable, model, rcp, period, value)


  # 7). Compute variations relative to historical rcp

  # Filter historical rcp in a separate df
  historical_th3 <- yield_th3_df_final %>%
    filter(rcp == "Hist") %>%
    select(x, y, theme_id, crop, variable, value) %>%
    rename(value_hist = value)
  historical_th4 <- yield_th4_df_final %>%
    filter(rcp == "Hist") %>%
    select(x, y, theme_id, crop, variable, value) %>%
    rename(value_hist = value)

  # Join historical data with the rest
  yield_th3_final <- yield_th3_df_final %>%
    left_join(historical_th3, by = c("x", "y", "theme_id", "crop")) %>%
    mutate(change = value / value_hist - 1) 
  yield_th4_final <- yield_th4_df_final %>%
    left_join(historical_th4, by = c("x", "y", "theme_id", "crop")) %>%
    mutate(change = value / value_hist - 1)

  # Combine data
  yield_final <- bind_rows(yield_th3_final, yield_th4_final) %>% 
    mutate(variable = variable.x) %>% 
    select(-variable.x, -variable.y)
  
  # 8). Save dataset
  saveRDS(object = yield_final, file = here(folder, paste0("GAEZ_yieldchange_", crop_choice,".rds")))

}


process_crop_yield <- function(crop_choice, folder, crops_theme3, crops_theme4) {
  
  # crop_choice <- "Rubber"
  
  # 1). Find all .tif files for a given theme and crop
  yield_list_th3 <- list.files(dir$data_th3, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
  yield_list_th4 <- list.files(dir$data_th4, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
  
  # 2). Get crop codes (with checks)
  crop_code3 <- crops_theme3 %>% filter(crop == crop_choice) %>% pull(code)
  crop_code4 <- crops_theme4 %>% filter(crop == crop_choice) %>% pull(code)
  
  # Handle missing crop codes
  if (length(crop_code3) == 0 || is.na(crop_code3)) {
    warning("No crop code found for Theme 3 crop: ", crop_choice)
    crop_code3 <- NULL
    yield_crop_list_th3 <- character(0)
  } else {
    yield_crop_list_th3 <- yield_list_th3[grepl(crop_code3, yield_list_th3)]
  }
  
  if (length(crop_code4) == 0 || is.na(crop_code4)) {
    warning("No crop code found for Theme 4 crop: ", crop_choice)
    crop_code4 <- NULL
    yield_crop_list_th4 <- character(0)
  } else {
    yield_crop_list_th4 <- yield_list_th4[grepl(crop_code4, yield_list_th4)]
  }
  
  # Define info extractor
  extract_info <- function(path) {
    path <- gsub("\\\\", "/", path)
    parts <- unlist(strsplit(path, "/"))
    idx <- which(grepl("Theme", parts))
    model <- parts[idx + 1]
    rcp   <- parts[idx + 2]
    time  <- parts[idx + 3]
    file  <- basename(path)
    name_parts <- stringr::str_match(file, "^(.*)_(.*)\\.tif$")[-1]
    crop <- crop_choice
    variable <- name_parts[(!grepl(crop_code4, name_parts)) & (!grepl(crop_code3, name_parts))]
    return(paste(model, rcp, time, crop, variable, sep = "_"))
  }
  
  # Prepare metropolitan France geometry
  france <- rnaturalearth::ne_countries(scale = "medium", country = "France", returnclass = "sf")
  metropolitan_bbox <- sf::st_bbox(c(xmin = -5, xmax = 10, ymin = 41, ymax = 52), crs = sf::st_crs(france))
  bbox_sf <- sf::st_as_sfc(metropolitan_bbox)
  metropolitan_france <- sf::st_intersection(france, bbox_sf)
  
  # Prepare results containers
  yield_th3_final <- NULL
  yield_th4_final <- NULL
  
  if (length(yield_crop_list_th3) > 0) {
    raster_stack_th3 <- terra::rast(yield_crop_list_th3)
    names(raster_stack_th3) <- sapply(yield_crop_list_th3, extract_info)
    metropolitan_france <- sf::st_transform(metropolitan_france, terra::crs(raster_stack_th3))
    raster_th3_france <- terra::crop(raster_stack_th3, terra::vect(metropolitan_france)) %>%
      terra::mask(terra::vect(metropolitan_france))
    yield_th3_df <- as.data.frame(raster_th3_france, xy = TRUE, na.rm = TRUE)
    yield_th3_df_long <- yield_th3_df %>%
      pivot_longer(cols = -c(x, y), names_to = "layer", values_to = "value") %>%
      separate(layer, into = c("model", "rcp", "period", "crop", "variable"), sep = "_") %>%
      mutate(theme_id = "3") %>%
      arrange(variable, crop, model, rcp)
    yield_th3_df_final <- yield_th3_df_long %>%
      select(x, y, theme_id, crop, variable, model, rcp, period, value)
    historical_th3 <- yield_th3_df_final %>%
      filter(rcp == "Hist") %>%
      select(x, y, theme_id, crop, variable, value) %>%
      rename(value_hist = value)
    yield_th3_final <- yield_th3_df_final %>%
      left_join(historical_th3, by = c("x", "y", "theme_id", "crop")) %>%
      mutate(change = value / value_hist - 1)
  }
  
  if (length(yield_crop_list_th4) > 0) {
    raster_stack_th4 <- terra::rast(yield_crop_list_th4)
    names(raster_stack_th4) <- sapply(yield_crop_list_th4, extract_info)
    metropolitan_france <- sf::st_transform(metropolitan_france, terra::crs(raster_stack_th4))
    raster_th4_france <- terra::crop(raster_stack_th4, terra::vect(metropolitan_france)) %>%
      terra::mask(terra::vect(metropolitan_france))
    yield_th4_df <- as.data.frame(raster_th4_france, xy = TRUE, na.rm = TRUE)
    yield_th4_df_long <- yield_th4_df %>%
      pivot_longer(cols = -c(x, y), names_to = "layer", values_to = "value") %>%
      separate(layer, into = c("model", "rcp", "period", "crop", "variable"), sep = "_") %>%
      mutate(theme_id = "4") %>%
      arrange(variable, crop, model, rcp)
    yield_th4_df_final <- yield_th4_df_long %>%
      select(x, y, theme_id, crop, variable, model, rcp, period, value)
    historical_th4 <- yield_th4_df_final %>%
      filter(rcp == "Hist") %>%
      select(x, y, theme_id, crop, variable, value) %>%
      rename(value_hist = value)
    yield_th4_final <- yield_th4_df_final %>%
      left_join(historical_th4, by = c("x", "y", "theme_id", "crop")) %>%
      mutate(change = value / value_hist - 1)
  }
  
  # Combine data if available
  if (length(yield_th3_final)>0 | length(yield_th4_final)>0){
    yield_final <- bind_rows(
      yield_th3_final,
      yield_th4_final
    ) %>%
      mutate(variable = coalesce(variable.x, variable.y)) %>%
      select(-variable.x, -variable.y)
  } else{
    warning("No yield data found for crop: ", crop_choice)
  }

  
  # 8). Save dataset if not empty
  if (nrow(yield_final) > 0) {
    saveRDS(object = yield_final, file = here::here(folder, paste0("GAEZ_yieldchange_", crop_choice, ".rds")))
  } else {
    warning("No yield data found for crop: ", crop_choice)
  }
}

