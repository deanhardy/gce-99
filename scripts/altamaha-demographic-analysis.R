rm(list=ls())

## load libraries
library(tidyverse)
library(tidycensus)
library(ipumsr)
options(tigris_use_cache = TRUE)
library(sf)

## set variables
# YR <- c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
YR = c(2020)
# vars <- load_variables(2010, 'sf1', cache = FALSE)
ST <- 'GA'
CO <- c('McIntosh', 'Glynn', 'Liberty', 'Long', 'Wayne')
ALT <- NULL
OUT <- NULL

for (i in 1:length(YR)) {
OUT <- get_decennial(
  geography = "block group",
  state = 'GA',
  county = CO,
  output = 'wide',
  geometry = TRUE,
  keep_geo_vars = TRUE,
  cache_table = TRUE,
  variables = "P001001",
  year = YR[[i]])

ALT <- rbind(ALT, OUT)
}

