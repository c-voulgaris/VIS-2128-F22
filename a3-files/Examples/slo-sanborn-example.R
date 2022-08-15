## Assignment 2

library(tidyverse)
library(sf)
library(here)
library(raster)

## Load images

sanborn2 <- here("a2-files",
                 "large-image-files",
                 "slo-sanborn-2_modified.tif") %>%
  stack()

sanborn3 <- here("a2-files",
                 "large-image-files",
                 "sanborn3_modified.tif") %>%
  stack()

sanborn4 <- here("a2-files",
                 "large-image-files",
                 "slo-sanborn-4_modified.tif") %>%
  stack()

sanborn5 <- here("a2-files",
                 "large-image-files",
                 "slo-sanborn-5_modified.tif") %>%
  stack()

sanborn6 <- here("a2-files",
                 "large-image-files",
                 "slo-sanborn-6_modified.tif") %>%
  stack()


xmin <- min(st_bbox(sanborn2)$xmin,
            st_bbox(sanborn3)$xmin,
            st_bbox(sanborn4)$xmin,
            st_bbox(sanborn5)$xmin,
            st_bbox(sanborn6)$xmin)

xmax <- max(st_bbox(sanborn2)$xmax,
            st_bbox(sanborn3)$xmax,
            st_bbox(sanborn4)$xmax,
            st_bbox(sanborn5)$xmax,
            st_bbox(sanborn6)$xmax)

ymin <- min(st_bbox(sanborn2)$ymin,
            st_bbox(sanborn3)$ymin,
            st_bbox(sanborn4)$ymin,
            st_bbox(sanborn5)$ymin,
            st_bbox(sanborn6)$ymin)

ymax <- max(st_bbox(sanborn2)$ymax,
            st_bbox(sanborn3)$ymax,
            st_bbox(sanborn4)$ymax,
            st_bbox(sanborn5)$ymax,
            st_bbox(sanborn6)$ymax)

CA_st_plane <- "+proj=lcc +lat_1=34.03333333333333 +lat_2=35.46666666666667 +lat_0=33.5 +lon_0=-118 +x_0=2000000 +y_0=500000 +ellps=GRS80 +units=m +no_defs"

bldgs <- st_read(here("a2-files",
                          "large-image-files",
                          "Building_Footprints.geojson")) 

bldgs_filter <- bldgs %>%
  st_crop(xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax)

ggplot() +
  layer_spatial(data = sanborn2)  +
  layer_spatial(data = sanborn3)  +
  layer_spatial(data = sanborn4)  +
  layer_spatial(data = sanborn5)  +
  layer_spatial(data = sanborn6)  +
  geom_sf(data = bldgs_filter)
