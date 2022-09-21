#####
options(java.parameters = '-Xmx2G')

library(tidyverse)
library(sf)
library(tidycensus)
library(here)
library(r5r)
library(stars)
library(ggspatial)
library(interp)
library(isoband)

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

trails <- rbind(city_trails, co_trails) 

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
                            by=1)))

tracts <- get_decennial(year = 2020,
                        geography = "tract", 
                        variables = c(Population = "P2_001N"), 
                        output = "wide", 
                        state = "CA",
                        county = "San Luis Obispo",
                        geometry = TRUE) %>%
  st_transform(CA_st_plane)

# Buffer around a point or line:
#   Area within a half mile (2640 feet) of a trail
trail_buffer <- trails %>%
  st_transform(CA_st_plane) %>%
  st_buffer(dist = 2640) %>%
  st_union()
  
# Spatial sampling
people_pts <- tracts %>%
  filter(!st_is_empty(.)) %>%
  st_sample(size = ceiling(tracts$Population/100)) 

# Centroids
tract_centers <- tracts %>%
  filter(!st_is_empty(.)) %>%
  st_centroid() 

# Make a grid

grid <- tracts %>%
  st_make_grid(cellsize = 1320,
               what = "centers") %>%
  st_as_sf() %>%
  st_filter(tracts)

# Raster distances

transit_dist <- st_distance(grid, 
                            st_transform(all_stops, CA_st_plane)) %>%
  as.data.frame() %>%
  mutate_all(as.numeric) %>%
  mutate(min_dist = apply(. , 1, min)) %>%
  dplyr::select(min_dist)
  
distance_raster <- cbind(grid, transit_dist) %>%
  st_rasterize() 

distance_raster <- distance_raster[tracts]

ggplot() + 
  geom_stars(data = distance_raster, sf=TRUE) + 
  scale_fill_viridis_c()

# Non-motorized isochrone

r5r_core <- here("a4-files",
     "networks") %>%
  setup_r5() 

wgs_grid <- grid %>%
  st_transform("WGS84") %>%
  mutate(id = as.character(seq(1:length(grid$x))))

min_walk_times <- travel_time_matrix(r5r_core,
                                 origins = all_stops,
                                 destinations = wgs_grid,
                                 mode = "WALK",
                                 max_trip_duration = 480) %>%
  group_by(toId) %>%
  summarise(min_walk = min(travel_time)) %>%
  mutate(id = toId)

walk_time_grid <- wgs_grid %>%
  right_join(min_walk_times) %>%
  dplyr::select(min_walk) %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  arrange(lon, lat)

# Interpolation is necessary because of the projection
interp <- interp(walk_time_grid$lon, 
                 walk_time_grid$lat, 
                 walk_time_grid$min_walk) 

# the t() is very important.
isochrones <- isobands(x = interp$x,
                       y = interp$y,
                       z = t(interp$z),
                       levels_low = c(0, 60),
                       levels_high = c(60, 120)) %>%
  iso_to_sfg() 

iso_poly <- st_sf(level = 1:length(isochrones),
                  geometry = st_sfc(isochrones)) %>%
  st_set_crs("WGS84")

ggplot(walk_time_grid) + 
  geom_sf(aes(color = min_walk), size = 0.01, alpha = 0.2) + 
  geom_sf(data = tracts, fill = NA) + 
  geom_sf(data = iso_poly, color = "yellow", aes(fill =level)) 


# Motorized isochrone

