#####
library(tidyverse)
library(sf)
library(tidycensus)
library(here)
library(stars)
library(ggspatial)
library(ggthemes)
library(gridExtra)
library(grid)

# Define CA-state-plane
CA_st_plane <- "+proj=lcc +lat_1=34.03333333333333 +lat_2=35.46666666666667 +lat_0=33.5 +lon_0=-118 +x_0=2000000 +y_0=500000.0000000002 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

# Load layers
co_trails <- here("a1-files",
                  "County_Trails.geojson") %>%
  st_read() %>%
  dplyr::select(TrailName, Length, LengthUnits)

city_trails <- here("a2-files",
                    "Examples",
                    "Trail.geojson") %>%
  st_read() %>%
  rename(TrailName = Trail_Name,
         Length = SLength) %>%
  mutate(LengthUnits = "MILES") %>%
  dplyr::select(TrailName, Length, LengthUnits)

trails <- rbind(city_trails, co_trails) %>%
  st_transform(CA_st_plane)

rta_stops <- here("a1-files",
                  "bus-stops.csv") %>%
  st_read(options=c("X_POSSIBLE_NAMES=stop_lon",
                    "Y_POSSIBLE_NAMES=stop_lat")) %>%
  mutate(`Transit Provider` = "San Luis Obispo RTA") 

slo_stops <- here("a2-files",
                  "Examples",
                  "slo-stops.csv") %>%
  st_read(options=c("X_POSSIBLE_NAMES=stop_lon",
                    "Y_POSSIBLE_NAMES=stop_lat")) %>%
  mutate(`Transit Provider` = "SLO Transit") %>%
  dplyr::select(colnames(rta_stops)) 

all_stops <- rbind(rta_stops, slo_stops) %>%
  st_set_crs("WGS84") %>%
  mutate(id = 
           as.character(seq(1,
                            length(rta_stops$stop_id) + 
                              length(slo_stops$stop_id),
                            by=1))) %>%
  st_transform(CA_st_plane)

tracts <- get_decennial(year = 2020,
                        geography = "tract", 
                        variables = c(Population = "P2_001N"), 
                        output = "wide", 
                        state = "CA",
                        county = "San Luis Obispo",
                        geometry = TRUE) %>%
  st_transform(CA_st_plane)

county <- st_union(tracts)

# Buffer around a point or line:
#   Area within a half mile (2640 feet) of a trail
trail_buffer <- trails %>%
  st_buffer(dist = 2640) %>%
  st_union()

buffer <- ggplot(county) +
  geom_sf(color = "gray",
          fill = NA) +
  geom_sf(data = trail_buffer, 
          aes(color = "Area within a half mile of a trail"),
          size = 0.5) +
  scale_color_manual(values = "black",
                     name = "") +
  theme_map()
  
# Spatial sampling
people_pts <- tracts %>%
  filter(!st_is_empty(.)) %>%
  st_sample(size = floor(tracts$Population/100)) 

sample <- ggplot(tracts) +
  geom_sf(color = "Census tract boundaries",
          fill = NA) +
  geom_sf(data = people_pts, 
          aes(color = "1 point = 100 residents"),
          size = 0.5) +
  scale_color_manual(values = "black",
                     name = "") +
  theme_map()

# Centroids
tract_centers <- tracts %>%
  filter(!st_is_empty(.)) %>%
  st_centroid() 

centroid <- ggplot(tracts) +
  geom_sf(color = "black",
          fill = NA) +
  geom_sf(data = tract_centers, 
          aes(color = "Tract centroid"),
          size = 7,
          shape = "*") +
  scale_color_manual(values = "black",
                     name = "") +
  theme_map() +
  theme(legend.background = element_rect(fill = NA))

# Make a grid

grid <- tracts %>%
  st_make_grid(cellsize = 1320,
               what = "centers") %>%
  st_as_sf() %>%
  st_filter(tracts)

# Raster distances
nearest <- st_nearest_feature(trails, all_stops)

trails <- trails %>%
  mutate(dist_to_transit = as.numeric(st_distance(., all_stops[nearest,], 
                                       by_element = TRUE)))

trail_dist <- ggplot(trails) +
  geom_sf(data = trails,
          aes(color = dist_to_transit)) +
  geom_sf(data = trail_buffer,
          fill = NA) +
  geom_sf(data = all_stops, size = 0.1, color = "pink") +
  theme_map()

trail_dist

## arrange plots

title <- textGrob("Distance to trails",
                  x = 0.04, 
                  y = 0.75,
                        just = "left",
                        gp = gpar(cex = 1.5))

grid.arrange(title, 
             dist_raster, 
             centroid, 
             buffer, 
             sample, 
             ncol = 2,
             widths = c(3, 1),
             layout_matrix = rbind(c(1,2),
                                   c(4,2),
                                   c(4,3),
                                   c(4,3),
                                   c(4,5),
                                   c(4,5)))
