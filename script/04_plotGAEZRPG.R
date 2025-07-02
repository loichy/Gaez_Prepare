#===============================================================================
# Description: Plot GAEZ Raster and RPG Commune Yield
#===============================================================================

# Clean memory 
rm(list=ls())
gc()

#===============================================================================
# 1). Prepare environment ------
#===============================================================================

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, GAEZr, here, rnaturalearthdata, rnaturalearth, sf, terra, cowplot, rmapshaper)

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

#===============================================================================
# 2). Load data ------
#===============================================================================

# 1. Read the shapefile of French communes
# Replace 'path_to_shapefile' with your actual path to the .shp file
communes <- st_read(here(dir$sf, "communes-20220101.shp"), quiet = TRUE)

# 2. Read Gaez data
GAEZ_df <- readRDS(here(dir$dataprepared, "GAEZ_yieldchange_communes_filt.rds"))

GAEZ_df_filt <- GAEZ_df %>% 
  filter(theme_id == 4, variable == "ylHr", 
         model == "HadGEM2-ES", 
         rcp == "rcp8p5", 
         crop %in% c("Oat", "Barley") ) %>% 
  filter(change < 1)

GAEZ_sf <- GAEZ_df_filt %>%
  left_join(communes, by = c("insee")) %>% 
  st_as_sf()

GAEZ_wh <- GAEZ_sf %>% 
  filter(crop == "Oat") 
GAEZ_fl <- GAEZ_sf %>%
  filter(crop == "Barley")

# 3. Read RPG data
RPG_df <- readRDS("C:/Users/loihenry/Dropbox/Recherche_Dauphine/DataArchive/RPG_data/RPG_AnalyzeData/data/final/LongPeriod_AcreageVariations.rds")

RPG_df_filt <- RPG_df %>% 
  filter(LIBELLE_GROUPE_CULTURE %in% c("Blé tendre", "Plantes à fibres")) %>% 
  mutate(crop = case_when(
    LIBELLE_GROUPE_CULTURE == "Blé tendre" ~ "Wheat",
    LIBELLE_GROUPE_CULTURE == "Plantes à fibres" ~ "Flax"
    TRUE ~ NA_character_
  )) 

RPG_sf <- RPG_df_filt %>%
  left_join(communes, by = c("insee")) %>% 
  st_as_sf() %>% 
  mutate(area_change = diff_final_debut_abs * 100)

RPG_wh <- RPG_sf %>% 
  filter(crop == "Wheat")
RPG_fl <- RPG_sf %>%
  filter(crop == "Flax")

#===============================================================================
# 2). Map ------
#===============================================================================

# Plot yield change
GAEZ_wheat_map <- ggplot(GAEZ_wh) +
  geom_sf(aes(fill = change), color = NA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "orange", midpoint = 0) +
  labs(fill = "Yield changes in percentage for wheat") +
  theme_minimal()
# Save the GAEZ map
ggsave(here(dir$output, "GAEZ_yield_change_wheat_map.png"), plot = GAEZ_wheat_map, width = 10, height = 8)
GAEZ_flax_map <- ggplot(GAEZ_fl) +
  geom_sf(aes(fill = change), color = NA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "orange", midpoint = 0) +
  labs(fill = "Yield changes in percentage for flax") +
  theme_minimal()
# Save the GAEZ map
ggsave(here(dir$output, "GAEZ_yield_change_flax_map.png"), plot = GAEZ_flax_map, width = 10, height = 8)

# Plot crop area change
RPG_wheat_map <- ggplot(RPG_wh) +
  geom_sf(aes(fill = area_change), color = NA) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(fill = "Area change in percentage") +
  theme_minimal()
ggsave(here(dir$output, "RPG_area_change_wheat_map.png"), plot = RPG_wheat_map, width = 10, height = 8)
RPG_flax_map <- ggplot(RPG_fl) +
  geom_sf(aes(fill = area_change), color = NA) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(fill = "Area change in percentage") +
  theme_minimal()
ggsave(here(dir$output, "RPG_area_change_flax_map.png"), plot = RPG_flax_map, width = 10, height = 8)


##Combine both plots together on a bivariate chloropleth map
library(cowplot)  # for legends

# 1. Merge the two sf objects (assume they both have `INSEE_COM`)
bivar_wh <- left_join(RPG_wh, GAEZ_wh %>% st_drop_geometry(), by = c("insee","crop"))
bivar_fl <- left_join(RPG_fl, GAEZ_fl %>% st_drop_geometry(), by = c("insee","crop"))

# 2. Create binned versions of each variable (2 bins: neg vs pos)
bivar_wh <- bivar_wh %>%
  filter(!is.na(change), !is.na(area_change)) %>% 
  mutate(
    yield_cat = case_when(
      change < 0 ~ "Yield decrease",
      change >= 0 ~ "Yield increase",
      TRUE ~ NA_character_
    ),
    area_cat = case_when(
      area_change < 0 ~ "Area decrease",
      area_change >= 0 ~ "Area increase",
      TRUE ~ NA_character_
    ),
    bivar_cat = paste(yield_cat, area_cat, sep = "-")
  )
bivar_fl <- bivar_fl %>%
  filter(!is.na(change), !is.na(area_change)) %>% 
  mutate(
    yield_cat = case_when(
      change < 0 ~ "Yield decrease",
      change >= 0 ~ "Yield increase",
      TRUE ~ NA_character_
    ),
    area_cat = case_when(
      area_change < 0 ~ "Area decreae",
      area_change >= 0 ~ "Area increase",
      TRUE ~ NA_character_
    ),
    bivar_cat = paste(yield_cat, area_cat, sep = "-")
  )

# 3. Define a bivariate color scale (can be customized)
bivar_palette <- c(
  "Yield decrease-Area decrease" = "#5e3c99",  # both negative (purple)
  "Yield decrease-Area increase" = "#e66101",  # yield-, area+
  "Yield increase-Area decrease" = "#fdb863",  # yield+, area-
  "Yield increase-Area increase" = "#1b7837"   # both positive (green)
)

# 4. Plot the bivariate map
map_wheat <- ggplot(bivar_wh) +
  geom_sf(aes(fill = bivar_cat), color = NA) +
  scale_fill_manual(values = bivar_palette, name = "Change combo") +
  labs(
    title = "Map of changes in potential yields and crop area",
    caption = "Color encodes joint sign of yield and area change"
  ) +
  theme_minimal()
ggsave(here(dir$output, "RPG_GAEZ_wheat_map.png"), plot = map_wheat, width = 10, height = 8)
map_flax <- ggplot(bivar_fl) +
  geom_sf(aes(fill = bivar_cat), color = NA) +
  scale_fill_manual(values = bivar_palette, name = "Change combo") +
  labs(
    title = "Map of changes in potential yields and crop area",
    caption = "Color encodes joint sign of yield and area change"
  ) +
  theme_minimal()
ggsave(here(dir$output, "RPG_GAEZ_flax_map.png"), plot = map_flax, width = 10, height = 8)
