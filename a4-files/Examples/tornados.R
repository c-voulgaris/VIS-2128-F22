##

# https://gis.sarpy.gov/datasets/3e5e276b46914943a49abaf3d2887007/about

library(tidyverse)
library(sf)
library(tidycensus)
library(osmextract)
library(ggthemes)
library(here)
library(ggspatial)

NE_state_plane <- "+proj=lcc +lat_1=40 +lat_2=43 +lat_0=39.83333333333334 +lon_0=-100 +x_0=500000.0000000002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

blocks <- get_decennial(year = 2020,
                        geography = "block", 
                        variables = c(Population = "P2_001N"), 
                        output = "wide", 
                        state = "NE",
                        county = "Sarpy",
                        geometry = TRUE) %>%
  st_transform(NE_state_plane)

county <- st_union(block_groups)

streets <- oe_get("Nebraska") %>%
  st_transform(NE_state_plane) %>%
  st_filter(county) %>%
  filter(highway == "motorway" |
           highway == "primary" |
           highway == "secondary" |
           highway == "tertiary" |
           highway == "trunk")

Sys.time()
pop_pts <- st_sample(block_groups, size = ceiling(block_groups$Population/100)) %>%
  st_as_sf()
Sys.time()

Sys.time()
block_pts <- st_sample(blocks, size = ceiling(blocks$Population/50)) %>%
  st_as_sf()
Sys.time()

tornado_sirens <- here("a4-files",
                       "Examples",
                       "Outdoor_Warning_Sirens.geojson") %>%
  st_read() %>%
  st_transform(NE_state_plane)

siren_buffer <- st_buffer(tornado_sirens, dist = 5280) %>%
  st_union

nearest_siren <- st_nearest_feature(pop_pts,
                                    tornado_sirens)

pop_pts <- pop_pts %>%
  mutate(distance_to_siren = 
           as.numeric(st_distance(pop_pts, 
                       tornado_sirens[nearest_siren,],
                       by_element = TRUE)))

dist_palette <- RColorBrewer::brewer.pal(7, "PuRd")

ggplot() +
  geom_sf(data = county,
          size = 1.1,
          fill = NA,
          aes(lty = "County line")) +
  geom_sf(data = streets,
          color = "gray") +
  geom_sf(data = siren_buffer,
          aes(fill = "Area within one mile of a siren"),
          color = NA,
          alpha = 0.5) +
  geom_sf(data = pop_pts,
          size = 0.5,
          aes(color = distance_to_siren)) +
  scale_color_gradientn(colors = dist_palette,
                        name = "Distance from nearest siren (miles)\n(1 dot = 100 residents)",
                        breaks = seq(5280, 5 * 5280, by = 5280),
                        labels = c("1",
                                   "2",
                                   "3",
                                   "4",
                                   "5")) +
  scale_fill_manual(values = "darkslategray", name = "") +
  scale_linetype(name = "") +
  annotation_scale(location = "bl", unit_category = "imperial") +
  annotation_north_arrow(style = north_arrow_minimal, location = "tl") +
  guides(lty = guide_legend(order = 1),
         col = guide_colorbar(order = 3),
         fill = guide_legend(order = 2)) +
  theme_map() +
  theme(plot.background =  element_rect(fill = "white",
                                        color = "white"),
        legend.background = element_blank(),
        legend.position = "bottom")

here("a4-files",
     "Examples",
     "sirens.png") %>%
  ggsave(height = 8.5, width = 10, dpi = 300, units = "in")