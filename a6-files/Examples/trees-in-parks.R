library(tidyverse)
library(sf)
library(tidycensus)
library(here)
library(leaflet)

# Define State Plane
MA_st_plane <- "+proj=lcc +lat_1=42.68333333333333 +lat_2=41.71666666666667 +lat_0=41 +lon_0=-71.5 +x_0=200000.0001016002 +y_0=750000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +to_meter=0.3048006096012192 +no_defs" 

# Load Layers
trees <- st_read("https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::trees.geojson") %>%
  st_transform(MA_st_plane)

parks <- st_read("https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::open-space.geojson") %>%
  st_transform(MA_st_plane) %>%
  mutate(trees_in_park = lengths(st_intersects(., trees))) %>%
  mutate(trees_per_acre = trees_in_park / ACRES) %>%
  filter(ACRES > 0 & trees_in_park > 0) %>%
  st_transform("WGS84") %>%
  mutate(popup_text = 
           paste0("<b>",
                  SITE_NAME,
                  "</b><br>",
                  formatC(ACRES, format = "f", digits = 2),
                  " Acres<br>",
                  trees_in_park,
                  " trees<br>",
                  formatC(trees_per_acre, format = "f", digits = 2),
                  " trees per acre"))

my_ramp = colorRamp(c("pink", 
                      "brown", 
                      "orange", 
                      "lightgreen", 
                      "darkgreen"), 
                    interpolate = "linear")

density_palette = colorNumeric(palette = my_ramp, 
                              domain = log(parks$trees_per_acre))

map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(provider = providers$Stamen.TonerLite) %>%
  addPolygons(data = parks, fillColor = ~density_palette(log(trees_per_acre)),
                   stroke = FALSE,
             fillOpacity = 0.6,
             highlightOptions = highlightOptions(fillOpacity = 0.9),
             popup = ~popup_text)  %>%
  addLegend(data = parks,
            pal = density_palette, 
            values = ~log(trees_per_acre),
            labFormat = labelFormat(transform = function(x) exp(x),
                                    digits = 1),
            title = "Number of trees per acre",
            position = "bottomleft") %>%
  addLegend(title = "Sources and acknowledgments",
            colors = rep("white", 3),
            labels = c("Park boundaries and tree locations from Analyze Boston (data.boston.gov).",
                       "",
                       "My classmate, Marin Bradshaw, showed me how to define a leaflet color ramp."),
            position = "bottomright") %>%
  addLegend(title = "How Green is Boston's Open Space?",
            colors = rep("white", 6),
            labels = c("Boston is home to more than 6,700 acres of",
                       "open space, but there is a wide variation in ",
                       "the density of trees within these spaces.",
                       "This map shows a greater density of public ",
                       "trees per acre in the central parts of the ",
                       "city than on the periphery."),
            position = "topleft") 
map

htmlwidgets::saveWidget(map, file = here("a6-files",
                                         "Examples",
                                         "park-map.html"))