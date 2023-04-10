rm(list=ls())

## load libraries
library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)
library(sf)
library(leaflet)
library(ggmap)


## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/gce-99'

## set variables
YR <- c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
vars <- load_variables(2021, 'acs5', cache = FALSE)
ST <- 'GA'
CO <- c('McIntosh', 'Glynn', 'Liberty', 'Long', 'Wayne')
GCE <- NULL

## variables set for ACS data
VAR = c(white = "B03002_003E", black = "B03002_004E",
        native_american = "B03002_005E", asian = "B03002_006E",
        hawaiian = "B03002_007E", other = "B03002_008E",
        multiracial = "B03002_009E", latinx = "B03002_012E", total = "B03002_001E",
        medhhinc = "B19049_001E", agghhinc = "B19025_001E", hu = "B25001_001E")

## variable set for ACS data with MOE
VAR2 = c(white = "B03002_003", black = "B03002_004",
        native_american = "B03002_005", asian = "B03002_006",
        hawaiian = "B03002_007", other = "B03002_008",
        multiracial = "B03002_009", latinx = "B03002_012", total = "B03002_001",
        medhhinc = "B19049_001", agghhinc = "B19025_001", hu = "B25001_001")

# ## variables set for ACS data 2012-2005; note 5-yr ACS data don't start until 2009
# VAR = c(white = "B03002_003", black = "B03002_004",
#         nativ_american = "B03002_005", asian = "B03002_006",
#         hawaiian = "B03002_007", other = "B03002_008",
#         multiracial = "B03002_009", latinx = "B03002_012", total = "B03002_001",
#         mdhhinc = "B19049_001", agghhinc = "B19025_001", hu = "B25001_001")

## import site boundary
gce <- st_read(file.path(datadir, 'shapefiles/GCE_LTER_Boundary.shp')) %>%
  mutate(sqkm_site = as.numeric(st_area(geometry) / 1e6)) %>%
  st_transform(4326)

## import watershed boundary
wbd <- st_read(file.path(datadir, 'shapefiles/nhd_georgia/Shape/WBDHU10.shp')) %>%
  mutate(sqkm_site = as.numeric(st_area(geometry) / 1e6)) %>%
  st_transform(4326) %>%
  filter(huc10 %in% c('0306020407', '0306020408', '0307010605'))
  
wbd2 <- st_as_sf(st_union(wbd)) %>%
  rename(geometry = x) %>%
  mutate(sqkm_site = as.numeric(st_area(geometry) / 1e6))

st_write(wbd, paste0(datadir, '/SAPHU10.shp'), 'ESRI Shapefile')

## define function following stackoverflow post
# https://stackoverflow.com/questions/18887382/how-to-calculate-the-median-on-grouped-dataset
## but revised per variables from 
# https://www.mathsisfun.com/data/frequency-grouped-mean-median-mode.html
## B modified to account for when median group is the 0-9999 bin
GMedian <- function(frequencies, intervals, sep = NULL, trim = NULL) {
  # If "sep" is specified, the function will try to create the 
  #   required "intervals" matrix. "trim" removes any unwanted 
  #   characters before attempting to convert the ranges to numeric.
  if (!is.null(sep)) {
    if (is.null(trim)) pattern <- ""
    else if (trim == "cut") pattern <- "\\[|\\]|\\(|\\)"
    else pattern <- trim
    intervals <- sapply(strsplit(gsub(pattern, "", intervals), sep), as.numeric)
  }
  
  cf <- cumsum(frequencies)
  Midrow <- findInterval(max(cf)/2, cf) + 1
  L <- intervals[1, Midrow]      # lower class boundary of the group containing the median 
  w <- diff(intervals[, Midrow]) # width of median class
  G <- frequencies[Midrow]       # frequency of median class
  B <- ifelse(Midrow > 1, cf[Midrow - 1], as.vector(0))  # cumulative frequency of the groups before median group
  n_2 <- max(cf)/2               # total observations divided by 2
  
  unname(L + (n_2 - B)/G * w)
}

site <- wbd2

## working on adding decennial data, though need to figure out how to automate correcting for boundaries changes
# get_decennial()


####################################################################
## for loop to assess demographics of site over assigned time period 
####################################################################
for (i in 1:length(YR)) {
  OUT <- get_acs(geography = 'block group',
                 variables = VAR,
                 state = ST,
                 year = YR[[i]],
                 # year = 2021,
                 county = CO,
                 output = 'wide',
                 geometry = TRUE,
                 keep_geo_vars = TRUE,
                 cache_table = TRUE) %>%
    st_transform(4326) %>%
    mutate(sqkm_bg = as.numeric(st_area(geometry)) / 1e6, mnhhinc = agghhinc/hu,
           propPOC = 1 - (white/total)) %>%
    # mutate(sqkm_bg = as.numeric(st_area(geometry)) / 1e6, mnhhincM = agghhincM/huM,  mnhhincE = agghhincE/huE,
    #        propPOCE = 1 - (whiteE/totalE),  propPOCM = 1 - (whiteM/totalM)) %>%
    # dplyr::select(GEOID, ALAND, AWATER, sqkm_bg, totalE, totalM, whiteE, whiteM, blackE, blackM, native_americanE, native_americanM, 
    #               asianE, asianM, hawaiianE, hawaiianM, otherE, otherM, multiracialE, multiracialM, latinxE, latinxM, 
    #               propPOCE, propPOCM, medhhincE, medhhincM, agghhincE, agghhincM, huE, huM, mnhhincE, mnhhincM)
    dplyr::select(GEOID, ALAND, AWATER, sqkm_bg, total, white, black, native_american, asian, hawaiian,
                  other, multiracial, latinx, propPOC, medhhinc, agghhinc, hu, mnhhinc)

## define intersection between buffer zones and block groups
int <- as.tibble(st_intersection(site, OUT))

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
         mnhhinc = round(agghhinc/hu, 0), pland = round((ALAND * 0.000001)/sqkm_site, 2),
         yr = YR[[i]]) %>%
  dplyr::select(yr, tot_pop, popden, sqkm_site, pland, pwhite, pblack, pother, platinx, propPOC, hu, mnhhinc) %>%
  merge(site) %>%
  st_as_sf()

#########################################################
## estimate median household incomes within buffer zones
#########################################################
## this section of code calculates an estimated median from grouped (aka binned) household income data 
## within an area that overlaps several block groups

## good explanation for how this works mathematically and upon which the function below is based
## https://www.mathsisfun.com/data/frequency-grouped-mean-median-mode.html

## download hh income distribution tables for block groups & label bins
gm <- get_acs(geography = "block group",
                 table = 'B19001',
                 state = ST,
                 county = CO,
                 year = YR[[i]]) %>%
    select(-NAME, -moe) %>%
    rename(households = estimate) %>%
    filter(variable != 'B19001_001') %>%
    mutate(bin_min = ifelse(variable == 'B19001_002', 0, 
                            ifelse(variable == 'B19001_003', 10000, 
                                   ifelse(variable == 'B19001_004', 15000,
                                          ifelse(variable == 'B19001_005', 20000,
                                                 ifelse(variable == 'B19001_006', 25000,
                                                        ifelse(variable == 'B19001_007', 30000,
                                                               ifelse(variable == 'B19001_008', 35000,
                                                                      ifelse(variable == 'B19001_009', 40000,
                                                                             ifelse(variable == 'B19001_010', 45000,
                                                                                    ifelse(variable == 'B19001_011', 50000,
                                                                                           ifelse(variable == 'B19001_012', 60000,
                                                                                                  ifelse(variable == 'B19001_013', 75000,
                                                                                                         ifelse(variable == 'B19001_014', 100000,
                                                                                                                ifelse(variable == 'B19001_015', 125000,
                                                                                                                       ifelse(variable == 'B19001_016', 150000,
                                                                                                                              ifelse(variable == 'B19001_017', 200000, variable)))))))))))))))),
           bin_max = ifelse(variable == 'B19001_002', 9999, 
                            ifelse(variable == 'B19001_003', 14999, 
                                   ifelse(variable == 'B19001_004', 19999,
                                          ifelse(variable == 'B19001_005', 24999,
                                                 ifelse(variable == 'B19001_006', 29999,
                                                        ifelse(variable == 'B19001_007', 34999,
                                                               ifelse(variable == 'B19001_008', 39999,
                                                                      ifelse(variable == 'B19001_009', 44999,
                                                                             ifelse(variable == 'B19001_010', 49999,
                                                                                    ifelse(variable == 'B19001_011', 59999,
                                                                                           ifelse(variable == 'B19001_012', 74999,
                                                                                                  ifelse(variable == 'B19001_013', 99999,
                                                                                                         ifelse(variable == 'B19001_014', 124999,
                                                                                                                ifelse(variable == 'B19001_015', 149999,
                                                                                                                       ifelse(variable == 'B19001_016', 199999,
                                                                                                                              ifelse(variable == 'B19001_017', NA, variable))))))))))))))))) %>%
    mutate(interval = paste(bin_min, bin_max, sep = "-"))

## estimate hh median income
gm2 <- gm %>%
  left_join(percBGinSITE, by = "GEOID") %>%
  filter(perc_bginsite != 'NA') %>%
  mutate(eHH = households * perc_bginsite) %>%
  group_by(variable) %>%
  summarise(interval = interval[[1]], eHH = sum(eHH), households = sum(households)) %>%
  summarise(gmedian = GMedian(eHH, interval, sep = "-", trim = 'cut'))

## attache gmedian estimates for hh income
site_df <- merge(site_df, gm2)

GCE <- rbind(GCE, site_df)

}


#########################################
## plot data
#########################################
library(lubridate)

GCE2 <- st_set_geometry(GCE, NULL) %>%
  mutate(year = ymd(yr, truncated = 2L))

png(paste0(datadir, '/WBD_tot_pop.png'), width = 7, height =5, units = 'in', res = 150)
pop <- ggplot(GCE2, aes(year, tot_pop)) + 
  geom_line() +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(title = 'Population', x = 'Year', y = 'Number') + 
  theme_bw(base_size = 15)
pop
dev.off()

png(paste0(datadir, '/WBD_propPOC.png'), width = 7, height =5, units = 'in', res = 150)
poc <- ggplot(GCE2, aes(year, propPOC)) + 
  geom_line() +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(title = 'People of Color', x = 'Year', y = 'Proportion') + 
  theme_bw(base_size = 15)
poc
dev.off()

png(paste0(datadir, '/WBD_gmedian.png'), width = 7, height =5, units = 'in', res = 150)
gmedian <- ggplot(GCE2, aes(year, gmedian)) + 
  geom_line() +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(title = 'Median Household Income', x = 'Year', y = 'Income ($)') + 
  theme_bw(base_size = 15)
gmedian
dev.off()

png(paste0(datadir, '/WBD_hu.png'), width = 7, height =5, units = 'in', res = 150)
hu <- ggplot(GCE2, aes(year, hu)) + 
  geom_line() +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(title = 'Households', x = 'Year', y = 'Number') + 
  theme_bw(base_size = 15)
hu
dev.off()

library(gridExtra)
## export as pngs and tiffs
png(file.path(datadir, "figures/gce-demographic.png"), width = 13.33, height = 6, units = 'in', res = 150)
grid.arrange(pop, poc, gmedian, hu, ncol = 2)
dev.off()


#########################################
## map data
#########################################

leaflet() %>%
  addTiles(group = 'Open Street Map') %>%
  setView(lng = -81, lat = 31.5, zoom = 9) %>%
  addPolygons(data = OUT,
              color = 'blue',
              fill = FALSE,
              opacity = 1,
              weight = 1) %>%
  addPolygons(data = wbd,
              color = 'red',
              weight = 1) %>%
  addPolygons(data = gce,
              color = 'green',
              fill = FALSE,
              opacity = 1,
              weight = 5)
  # addPolygons(data = site_df,
  #             popup = paste("ACS Data:", YR, "<br>",
  #                           "People of Color (%):", 100*site_df$propPOC, "<br>",
  #                           "Black (%):", 100*site_df$pblack, "<br>",
  #                           "Other race (%):", 100*site_df$pother, "<br>",
  #                           "Latinx (%):", 100*site_df$platinx, "<br>",
  #                           "White (%):", 100*site_df$pwhite, "<br>",
  #                           "Housing Units:", round(site_df$hu,0), "<br>",
  #                           "Estimated Mean HH Income (US$):", site_df$mnhhinc, "<br>",
  #                           "Estimated Median HH Income (US$):", round(site_df$gmedian, 0)))

#############
## static map
#############
library(tmap)
library(tmaptools)
library(osmdata)
library(rJava)
library(OpenStreetMap)
## help: https://ajsmit.github.io/Intro_R_Official/mapping-google.html

bb <- getbb('Sapelo Island, GA')

# define the spatial extent to OpenStreetMap
lat1 <- bb[2,1]
lat2 <- bb[2,2]
lon1 <- bb[1,1]
lon2 <- bb[1,2]

# other 'type' options are "osm", "maptoolkit-topo", "bing", "stamen-toner",
# "stamen-watercolor", "esri", "esri-topo", "nps", "apple-iphoto", "skobbler";
# play around with 'zoom' to see what happens; 10 seems just right to me
sap_map <- openmap(c(lat2, lon1), c(lat1, lon2), zoom = 11,
                  type = "osm", mergeTiles = TRUE)

# reproject onto WGS84
# sap_map2 <- openproj(sap_map)

# OpenStreetMap::autoplot.OpenStreetMap(sap_map2) + 
  # # geom_sf() + 
  # xlab("Longitude (°W)") + ylab("Latitude (°N)")
  
# tm_shape(site) + 
#   tm_polygons(col = 'red', alpha = 0.5) + 
#   tm_shape(gce) + 
#   tm_borders(col = 'green') +
#   tm_basemap() + 
#   tm_shape(map) + 
#   tm_raster()



# map <- get_map("Sapelo Island, Georgia", zoom = 9, source = "google")
# ggmap(map) + 
#   # coord_sf(crs = st_crs(4326)) + # force the ggplot2 map to be in 3857
#   # geom_sf(data = gce, aes(fill = area), inherit.aes = FALSE) + 
#   geom_sf(data = wbd2, aes(fill = sqkm_site), inherit.aes= FALSE)
