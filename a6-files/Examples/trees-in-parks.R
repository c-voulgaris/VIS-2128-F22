library(tidyverse)
library(sf)
library(tidycensus)
library(here)
library(ggspatial)
library(ggthemes)
library(gridExtra)
library(grid)
library(leaflet)
library(tigris)

# Define CA-state-plane
CA_st_plane <- "+proj=lcc +lat_1=34.03333333333333 +lat_2=35.46666666666667 +lat_0=33.5 +lon_0=-118 +x_0=2000000 +y_0=500000.0000000002 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"


city <- places(state = "CA") %>%
  filter(NAME == "San Luis Obispo") %>%
  st_transform(CA_st_plane)

bbox <- st_bbox(city)


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
  st_transform(CA_st_plane) %>%
  st_filter(city)

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
  st_transform(CA_st_plane) %>%
  st_filter(city)

streets <- osmextract::oe_get("San Luis Obispo") %>%
  st_transform(CA_st_plane) %>%
  st_filter(city) %>%
  filter(highway == "primary" |
           highway == "secondary" |
           highway == "motorway" |
           highway == "tertiary" |
           highway == "residential")
  
closest_stop <- st_nearest_feature(trails,
                                   all_stops)

trails <- trails %>%
  mutate(dist_to_transit = st_distance(trails, all_stops[closest_stop,], by_element = TRUE)) %>%
  mutate(dist_to_transit = as.numeric(dist_to_transit))

stop_buffer <- all_stops %>%
  st_buffer(dist = 1320) %>%
  st_union

trail_colors <- RColorBrewer::brewer.pal(7, "YlOrBr")

ggplot() +
  geom_sf(data = streets,
          color = "gray") +
  geom_sf(data = stop_buffer,
          alpha = 0.2,
          color = NA,
          aes(fill = "Area within 1/4 mile of transit")) +
  geom_sf(data = all_stops,
          aes(shape = "Transit stop")) +
  geom_sf(data = trails,
          size = 1.2,
          aes(color = dist_to_transit)) +
  scale_color_gradientn(colors = rev(trail_colors),
                        breaks = seq(0, 5280, by = 1320),
                        labels = c("At transit stop",
                                   "1/4 mile from transit",
                                   "1/2 mile from transit",
                                   "3/4 mile from transit",
                                   "1 mile from transit"),
                        limits = c(0, 6000),
                        name = "Trails") +
  scale_shape(name = "") +
  scale_fill_manual(name = "", values = "gray") +
  annotation_north_arrow(style = north_arrow_minimal,
                         location = "tr") +
  theme_map() +
  theme(legend.background = element_rect(fill = NA),
        legend.margin = margin(0,0,0,0),
        plot.background = element_rect(fill = "white",
                                       color = NULL)) +
  guides(shape = guide_legend(order = 3),
         col = guide_legend(order = 1),
         fill = guide_legend(order = 2))

ggsave(filename = here("a4-files", "Examples", "slo-trails.png"), 
       width = 8.5, height = 8, units = "in", dpi = 300)

wgs_trails <- trails %>%
  st_transform("WGS84") %>%
  mutate(popup_txt = paste0("<b>",
                            TrailName,
                            "</b><br>",
                            formatC(dist_to_transit/5280, digits = 1, format = "f"),
                            " miles from a transit stop")) %>%
  mutate(miles = dist_to_transit / 5280)

wgs_buffer <- stop_buffer %>%
  st_transform("WGS84")

dist_palette = colorNumeric(palette = "YlOrBr", 
                              domain = wgs_trails$miles)



map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(provider = providers$Stamen.TonerLite) %>%
  addPolygons(data = wgs_buffer,
              fillColor = "black",
              fillOpacity = 0.6,
              stroke = FALSE) %>%
  addPolylines(data = wgs_trails,
               popup = ~popup_txt,
               color = ~dist_palette(miles),
               opacity = 1,
               highlightOptions = highlightOptions(color = "green")) %>%
  addLegend(colors = "black",
            opacity = 0.6,
            labels = "Area within 1/4 mile of transit",
            position = "bottomright") %>%
  addLegend(pal = dist_palette, 
            values = wgs_trails$miles,
            title = "Miles to nearest transit stop",
            opacity = 1,
            position = "bottomright") %>%
  addLegend(title = "Trails and Transit",
            colors = rep("white", 6),
            labels = c("How accessible is San Luis Obispo’s",
                       "trail system by public transit? This map",
                       "shows that while much of the city’s urban",
                       "trails are with a quarter mile of a transit",
                       "stop, a few remain accessible primarily by",
                       "private car"),
            position = "topleft") %>%
  addLegend(title = "Sources and acknowledgments",
            colors = rep("white", 8),
            labels = c("Transit stop locations from SLO Transit's",
                       "and SLO RTA's GTFS feeds. Trail locations",
                       "from City of San Luis Obispo GIS Hub and ",
                       "SLO County Open Data.",
                       "",
                       "My classmate, Rachel Meltzer, showed me",
                       "how to hack a legend control to annotate a",
                       "Leaflet map."),
            position = "bottomleft") 

map

htmlwidgets::saveWidget(map, file = here("a4-files",
                                         "Examples",
                                         "slo-trails.html"))