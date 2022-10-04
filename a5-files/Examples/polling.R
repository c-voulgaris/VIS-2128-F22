#####

options(java.parameters = '-Xmx2G')

library(tidyverse)
library(sf)
library(r5r)
library(ggspatial)
library(ggthemes)
library(interp)
library(isoband)
library(tigris)
library(here)
library(RColorBrewer)

MA_st_plane <- "+proj=lcc +lat_1=42.68333333333333 +lat_2=41.71666666666667 +lat_0=41 +lon_0=-71.5 +x_0=200000.0001016002 +y_0=750000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +to_meter=0.3048006096012192 +no_defs"

somerville <- places(state = "MA") %>%
  filter(NAME == "Somerville") %>%
  st_transform(MA_st_plane) %>%
  st_buffer(dist = 2640)

transit <- here("a5-files",
                "Examples",
                "MBTA-GTFS",
                "stops.txt") %>%
  read_csv() %>%
  filter(!is.na(stop_lat)) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"),
           crs = "WGS84") %>%
  st_transform(MA_st_plane) %>%
  st_filter(somerville) %>%
  st_transform("WGS84") %>%
  mutate(id = as.character(stop_id))

polls <- here("a5-files",
              "Examples",
              "somerville-polling",
              "PollingPlaces.shp") %>%
  st_read() %>%
  st_transform("WGS84") %>%
  mutate(id = as.character(OBJECTID))

grid_points <- somerville %>%
  st_make_grid(cellsize = 250,
               what = "corners") %>%
  st_as_sf() %>%
  st_transform("WGS84")

grid_points <- grid_points %>%
  mutate(id = as.character(seq(1, length(grid_points$x), by = 1)))

my_core <- here("a5-files",
     "Examples",
     "networks") %>%
  setup_r5(verbose=FALSE)

walk_from_grid <- travel_time_matrix(my_core,
                                     origins = grid_points,
                                     destinations = polls,
                                     mode = "WALK",
                                     verbose = FALSE) %>%
  pivot_wider(names_from = toId,
              values_from = travel_time) %>%
  rowwise() %>%
  mutate(min_time = min(c_across(-fromId),na.rm = TRUE)) %>%
  select(fromId, min_time) %>%
  rename(id = fromId)

grid_points <- grid_points %>%
  left_join(walk_from_grid)

walk_to_polls <- travel_time_matrix(my_core,
                                    origins = transit,
                                    destinations = polls,
                                    mode = "WALK",
                                    verbose = FALSE) %>%
  pivot_wider(names_from = fromId,
              values_from = travel_time) %>%
  rowwise() %>%
  mutate(min_time = min(c_across(-toId),na.rm = TRUE)) %>%
  select(toId, min_time) %>%
  rename(id = toId)

polls <- polls %>%
  left_join(walk_to_polls)

stop_r5()

grid_points <- grid_points %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

interp <- interp(grid_points$lon, 
                 grid_points$lat, 
                 grid_points$min_time,
                 nx = 100,
                 ny = 100)

isochrones <- isobands(x = interp$x,
                       y = interp$y,
                       z = t(interp$z),
                       levels_low = 0,
                       levels_high = 5) %>%
  iso_to_sfg() 

iso_poly <- st_sf(level = 1:length(isochrones),
                  geometry = st_sfc(isochrones)) %>%
  st_set_crs("WGS84")

iso_poly_st_plane <- iso_poly %>%
  st_transform(MA_st_plane)

close_transit <- transit %>%
  st_transform(MA_st_plane) %>%
  st_filter(iso_poly_st_plane) %>%
  st_transform("WGS84")


ggplot(iso_poly) +
  annotation_map_tile(type = "cartolight", zoomin = 1) +
  geom_sf(data = iso_poly,
          color = NA,
          alpha = 0.7,
          aes(fill = "5-minute walkshed for polling place")) +
  geom_sf(data = close_transit,
          color = "orange",
          size = 1.5,
          aes(shape = "Transit stop within 5 minute walk from polling place")) +
  geom_sf(data = polls,
          aes(color = min_time),
          size = 3) +
  scale_color_gradientn(colors = 
                          brewer.pal(name = "PuBuGn", 
                                     n = 5),
                        name = "Polling location,\nWalking time to\nnearest transit\nstop (minutes)") +
  scale_shape(name = "") +
  scale_fill_manual(values = "darkgray",
                    name = "") +
  coord_sf(xlim = c(-71.13262, -71.065)) +
  theme_map() +
  theme(legend.position = c(0.02, 0.05),
        legend.background = element_blank(),
        legend.key = element_blank())

here("a5-files",
     "Examples",
     "polling.png") %>%
  ggsave(width = 11,
         heigh = 8.5,
         units = "in",
         dpi = 350)
  