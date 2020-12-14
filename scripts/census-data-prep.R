rm(list=ls())

## load libraries
library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)
library(sf)
library(leaflet)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/gce-99'

## set variables
YR <- 2019
ST <- 'GA'
CO <- c('McIntosh', 'Glynn', 'Liberty')
VAR = c(white = "B03002_003E", black = "B03002_004E",
        native_american = "B03002_005E", asian = "B03002_006E",
        hawaiian = "B03002_007E", other = "B03002_008E",
        multiracial = "B03002_009E", latinx = "B03002_012E", total = "B03002_001E",
        medhhinc = "B19049_001E", agghhinc = "B19025_001E", hu = "B25001_001E")

## import site boundary
site <- st_read(file.path(datadir, 'shapefiles/GCE_LTER_Boundary.shp')) %>%
  mutate(sqkm_site = as.numeric(st_area(geometry) / 1e6)) %>%
  st_transform(4326)

## import census data for area
df <- get_acs(geography = 'block group',
              variables = VAR,
              state = ST,
              year = YR, 
              county = CO,
              output = 'wide',
              geometry = TRUE,
              keep_geo_vars = TRUE) %>%
  st_transform(4326) %>%
  mutate(sqkm_bg = as.numeric(st_area(geometry)) / 1e6, mnhhinc = agghhinc/hu,
         propPOC = 1 - (white/total)) %>%
  dplyr::select(GEOID, ALAND, AWATER, sqkm_bg, total, white, black, native_american, asian, hawaiian,
                other, multiracial, latinx, propPOC, medhhinc, agghhinc, hu, mnhhinc)

## define intersection between buffer zones and block groups
int <- as.tibble(st_intersection(site, df))

## proportional area adjustment/allocation method
percBGinSITE <- int %>%
  mutate(sqkm_bginsite = as.numeric(st_area(geometry) / 1e6)) %>%
  mutate(perc_bginsite = (sqkm_bginsite/sqkm_bg))

site_df <- percBGinSITE %>%
  mutate(tot_pop = total * perc_bginsite,
         white = white * perc_bginsite, 
         black = black * perc_bginsite,
         other = (native_american+asian+hawaiian+other+multiracial) * perc_bginsite,
         #other = multiracial * perc_bginsite,
         latinx = latinx * perc_bginsite,
         hu = hu * perc_bginsite,
         ALAND = ALAND * perc_bginsite) %>%
  mutate(agghhinc = hu * mnhhinc) %>%
  summarise(tot_pop = sum(tot_pop), white = sum(white), black = sum(black), 
            other = sum(other), latinx = sum(latinx), 
            hu = sum(hu, na.rm = TRUE), agghhinc = sum(agghhinc, na.rm = TRUE),
            sqkm_site = mean(sqkm_site), ALAND = sum(ALAND)) %>%
  mutate(pwhite = round(white/tot_pop, 2), pblack = round(black/tot_pop, 2), pother = round(other/tot_pop, 2), 
         platinx = round(latinx/tot_pop, 2), popden = round(tot_pop/ALAND, 2), propPOC = round(1 - pwhite, 2),
         mnhhinc = round(agghhinc/hu, 0), pland = round((ALAND * 0.000001)/sqkm_site, 2)) %>%
  dplyr::select(tot_pop, popden, sqkm_site, pland, pwhite, pblack, pother, platinx, propPOC, hu, mnhhinc) %>%
  merge(site) %>%
  st_as_sf()

leaflet() %>%
  addTiles(group = 'Open Street Map') %>%
  setView(lng = -81, lat = 31.5, zoom = 9) %>%
  addPolylines(data = site,
              color = 'green') %>%
  addPolygons(data = site_df,
              popup = paste(
                            "2014-2018 ACS Data", "<br>",
                            "People of Color (%):", 100*site_df$propPOC, "<br>",
                            "Black (%):", 100*site_df$pblack, "<br>",
                            "Other race (%):", 100*site_df$pother, "<br>",
                            "Latinx (%):", 100*site_df$platinx, "<br>",
                            "White (%):", 100*site_df$pwhite, "<br>",
                            "Housing Units:", round(site_df$hu,0), "<br>",
                            "Estimated Mean HH Income (US$):", site_df$mnhhinc))
  