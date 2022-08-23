library(sf)
library(tidyverse)
library(here)
library(rnaturalearth)
library(MetBrewer)
library(ggspatial)

degas_cat <- met.brewer(name = "Degas", n = 7, type = "discrete")
proj_crs <- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=20 +lat_2=-23 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

countries <- ne_countries(continent = "Africa",
                          returnclass = "sf") %>%
  st_transform(proj_crs)

xmin <- st_bbox(countries)$xmin
xmax <- st_bbox(countries)$xmax

ymin <- st_bbox(countries)$ymin
ymax <- st_bbox(countries)$ymax


world <- ne_countries(returnclass = "sf") 

airports <- here("a1-files",
                 "sample-files",
                 "airports") %>%
  st_read() %>%
  st_filter(countries)

rail <- here("a1-files",
             "sample-files",
             "rail") %>%
  st_read() %>%
  st_filter(countries)

ggplot(world) +
  geom_sf(color = NA,
          fill = "lightgray",
          show.legend = FALSE) +
  geom_sf(data = countries,
          color = NA,
          aes(fill=as.character(mapcolor7)),
          show.legend = FALSE) +
  geom_sf(data = rail, aes(color = "Rail line"),
          shape = 16) + 
  geom_sf(data = airports, aes(color = "Airport")) +
  scale_fill_manual(values = degas_cat) +
  scale_color_manual(values = c("Rail line" = "darkgray",
                                "Airport" = "yellow"),
                     guide = guide_legend(override.aes = 
                                            list(linetype = c("solid", 
                                                              "blank"),
                                                 shape = c(NA, 16))),
                     name = "Legend") +
  theme(panel.grid.major = element_line(color = "lightblue", 
                                        linetype = "dashed", 
                                        size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.key = element_rect(fill = NA),
        legend.position = c(0.1,0.08)) +
  coord_sf(crs = proj_crs,
           xlim = c(xmin, xmax),
           ylim = c(ymin, ymax))

here("a2-files",
     "Examples",
     "africa-2.png") %>%
  ggsave(height = 8, width = 7, units = "in", dpi = 300)