library(sf)
library(tidyverse)
library(here)
library(rnaturalearth)
library(ggspatial)
library(raster)

proj_crs <- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=20 +lat_2=-23 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

countries <- ne_countries(continent = "Africa",
                          returnclass = "sf") %>%
  st_transform(proj_crs)

rail <- here("a1-files",
             "sample-files",
             "rail") %>%
  st_read() %>% 
  st_transform(proj_crs) %>%
  st_filter(countries)


airports <- here("a1-files",
                 "sample-files",
                 "airports") %>%
  st_read() %>% 
  st_transform(proj_crs) %>%
  st_filter(countries)


africa <- here("a3-files",
               "large-image-files",
               "africa_modified.tif") %>%
  stack()

ggplot() +
  layer_spatial(africa) +
  geom_sf(data = rail, aes(color = "Rail line"),
          size = .75,
          lty = "dashed") + 
  geom_sf(data = airports, aes(color = "Airport")) +
  scale_color_manual(values = c("Rail line" = "orange",
                                "Airport" = "blue"),
                     guide = guide_legend(override.aes = 
                                            list(linetype = c("dashed", 
                                                              "blank"),
                                                 shape = c(NA, 16))),
                     name = "Current Infrastructure (2022)") +
  theme_void() +
  theme(legend.position = c(0.27, 0.23))

here("a3-files",
     "Examples",
     "africa-3.png") %>%
  ggsave(height = 8, width = 7, units = "in", dpi = 300)