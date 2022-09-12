## Assignment 2

library(tidyverse)
library(sf)
library(here)
library(raster)
library(ggspatial)
library(rnaturalearth)

## Load image

africa <- here("a3-files",
                 "large-image-files",
                 "africa_modified.tif") %>%
  stack()

ggplot() +
  layer_spatial(africa)
