rm(list=ls())

## load libraries
library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)
library(sf)
library(leaflet)

## define data directory
datadir <- 'C:/Users/dhardy/Dropbox/r_data/gce-99'

## import GCE boundary & plot on leaflet map
gce <- st_read(file.path(datadir, 'shapefiles/GCE_LTER_Boundary.shp')) %>%
  st_transform(4326)

# all_vars <- load_variables(2016, 'acs5', cache = TRUE)

var = c(white = "B03002_003E", black = "B03002_004E",
        native_american = "B03002_005E", asian = "B03002_006E",
        hawaiian = "B03002_007E", other = "B03002_008E",
        multiracial = "B03002_009E", latinx = "B03002_012E", total = "B03002_001E",
        medhhinc = "B19049_001E", agghhinc = "B19025_001E", hu = "B25001_001E")

## import census data for area
bg <- get_acs(geography = 'block group',
              variables = var,
              state = 'GA',
              county = c('McIntosh', 'Glynn', 'Liberty'),
              output = 'wide',
              geometry = TRUE,
              keep_geo_vars = TRUE) %>%
  st_transform(4326)

leaflet() %>%
  addTiles(group = 'Open Street Map') %>%
  setView(lng = -81, lat = 31.5, zoom = 9) %>%
  addPolylines(data = gce,
              color = 'green') %>%
  addPolygons(data = bg)
  