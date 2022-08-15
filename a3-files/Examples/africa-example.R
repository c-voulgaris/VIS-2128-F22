## Assignment 2

library(tidyverse)
library(sf)
library(here)
library(raster)
library(ggspatial)
library(rnaturalearth)

## Load images

africa <- here("a2-files",
                 "large-image-files",
                 "africa_modified.tif") %>%
  stack()

africa_df <- as.data.frame(image, xy = TRUE) %>%
  rename(Red = africa_modified.1,
         Green = africa_modified.2,
         Blue = africa_modified.3) %>%
  filter(!is.na(Red) | !is.na(Green) | !is.na(Blue)) %>%
  replace_na(list(Red = 0,
                  Green = 0,
                  Blue = 0))

ggplot() +
  layer_spatial(africa)