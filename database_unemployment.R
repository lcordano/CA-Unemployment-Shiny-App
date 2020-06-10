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
  mutate(year = as.numeric(year)) %>% filter(year > 1999) %>%
  select(area_name, year, month, unemployment, unemployment_rate)


# save data in SQLite Database
# bug: geometry data changes to character binary vector and I cannot
# coerce it back to Multipolygon sfc object
library(DBI)
library(RSQLite)

unemployment_db <- dbConnect(RSQLite::SQLite(), dbname = "CA_unemployment.sqlite")

dbWriteTable(unemployment_db, "unemployment", unemployment)
dbListTables(unemployment_db)

#sf <- st_set_geometry(df, sfc) # set geometry, return sf
dbDisconnect(unemployment_db)