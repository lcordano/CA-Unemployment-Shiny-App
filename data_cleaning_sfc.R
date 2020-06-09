library(sf)
library(tidyverse)
library(stringr)
library("RSocrata")

# access API and prep data
unemployment <- read.socrata(
  "https://data.edd.ca.gov/resource/e6gw-gvii.json?area_type=County",
  app_token = "akoVDHlvIapmiBIkJ8peAwNUp",
  email     = "lcordano@ucdavis.edu",
  password  = "LC72lc09!"
) %>% filter(seasonally_adjusted_y_n == "N") %>% 
  select(area_name, year, month, unemployment, unemployment_rate)

boundaries <- st_read(
  "CA_Counties/CA_Counties_TIGER2016.shp") %>% 
  mutate(area_name = as.character(NAMELSAD)) %>% select(area_name, geometry)

county_data <- st_transform(st_as_sf(full_join(unemployment, boundaries, by="area_name") %>% 
  select(area_name, year, month, unemployment_rate, geometry)), 4326)
rm("unemployment")
rm("boundaries")

# drop 3rd dimension in order to coerce data into .shp file
newsf <- st_zm(county_data, drop=T, what='ZM')
st_write(newsf, 'shp_data/county_data.shp')
