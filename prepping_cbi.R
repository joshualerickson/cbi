library(rgee)
ee_Initialize()
library(sf)
library(raster)
library(tidyverse)

#prepping fire data for cbi in Earth Engine

fire_11_19 <- read_sf('cbi_test/fire_2011_present.shp')

fire_15_all <- fire_11_19 %>% filter(FIREYEAR >= 2015) %>% st_transform(4326) %>%
  select(Fire_Year = "FIREYEAR",Fire_Name = "FIRENAME") %>%
  mutate(Start_Day = 182, End_Day = 273,
         Fire_ID = row_number())

st_write(fire_15_all, dsn = 'cbi_test', layer = 'fire_15_all',
         driver = 'ESRI Shapefile')

