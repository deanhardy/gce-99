rm(list=ls())

## load libraries
library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)
library(sf)
library(leaflet)

## define data directory
datadir <- 'C:/Users/dhardy/Dropbox/r_data/gce-99'

## set variables
YR <- 2017
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
                 year = YR) %>%
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

gm2 <- gm %>%
  left_join(percBGinSITE, by = "GEOID") %>%
  filter(perc_bginsite != 'NA') %>%
  mutate(eHH = households * perc_bginsite) %>%
  group_by(variable) %>%
  summarise(interval = interval[[1]], eHH = sum(eHH), households = sum(households)) %>%
  summarise(gmedian = GMedian(eHH, interval, sep = "-", trim = 'cut'))

## attache gmedian estimates for hh income
site_df <- merge(site_df, gm2)



#########################################
## map data
#########################################

leaflet() %>%
  addTiles(group = 'Open Street Map') %>%
  setView(lng = -81, lat = 31.5, zoom = 9) %>%
  addPolylines(data = site,
              color = 'green') %>%
  addPolygons(data = site_df,
              popup = paste("People of Color (%):", 100*site_df$propPOC, "<br>",
                            "Black (%):", 100*site_df$pblack, "<br>",
                            "Other race (%):", 100*site_df$pother, "<br>",
                            "Latinx (%):", 100*site_df$platinx, "<br>",
                            "White (%):", 100*site_df$pwhite, "<br>",
                            "Housing Units:", round(site_df$hu,0), "<br>",
                            "Estimated Mean HH Income (US$):", site_df$mnhhinc, "<br>",
                            "Estimated Median HH Income (US$):", round(site_df$gmedian, 0)))
  
              