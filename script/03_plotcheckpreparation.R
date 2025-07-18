# Compare GAEZ raster yield and communes-level aggregated yields
# To check if aggregation procedure goes fine

data_files <- list.files(here(dir$dataprepared), pattern = "\\.rds$", recursive=TRUE, full.names=TRUE)


gaez_df <- readRDS(data_files[7])
# Convert tidy df as a raster
gaez_sf <- gaez_df %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(as.numeric(crs_gaez))) %>% 
  st_transform(st_crs(communes_sf)) # Transform to the same CRS as communes


gaez_sf_filt <- gaez_sf %>% 
  filter(model == "HadGEM2-ES", theme_id == 4, rcp == "rcp8p5", crop == "Wheat", variable == "ylHr") %>% 
  filter(value > 0)

commune_yield_filt <- all_summaries_filtered %>% 
  filter(model == "HadGEM2-ES", theme_id == 4, rcp == "rcp8p5", crop == "Wheat", variable == "ylHr") %>% 
  filter(value > 0) %>% 
  st_set_geometry("geometry")

ggplot(data = commune_yield_filt) +
  geom_sf(aes(fill = value,
              color = value), 
          lwd = 0.1, alpha = 0.35) +
  geom_sf_text(aes(label = round(value, 1)),
               size = 0.5) +
  geom_sf(data = gaez_sf_filt,
          aes(color = value,
              fill = value), 
          size = 0.2,
          alpha = 0.45)+
  geom_sf_text(data = gaez_sf_filt,
          aes(label = round(value, 1)),
          size = 0.5)

ggsave(filename = here(dir$output, "gaez_yield_wheat_rcp8p5.pdf"),
       width = 10, height = 8)
