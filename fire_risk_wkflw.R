library(sf)
library(tidyverse)
library(arcgisbinding)
arc.check_product()
library(exactextractr)

#bring in fire risk raster
fire_risk <- raster::raster('T:/FS/NFS/R01/Program/2500WatershedAirMgmt/GIS/WorkSpace/jerickson/fire_risk/fire_risk_abs')

#read in HUC used in analysis

huc12_wcc <- read_sf('T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Data/BaseDatasets.gdb', layer = 'WCATT_WCC_HU12_R1_Clip')


# extract the sum
huc12_wcc$fire_risk_sum <- exact_extract(fire_risk, huc12_wcc, 'sum')

#write for backup
write_sf(huc12_wcc, 'huc12_wcc_fire_risk.gpkg', layer = 'fire_risk')

#bring in final_wcc_reassessment
wcc_add_fire_risk <- read_sf('T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Data/OutputDatasets_Review.gdb', layer = 'Final_WCC_Reassessment')


ggplot() + geom_sf(data = huc12_wcc, aes(fill = log10(fire_risk_sum)))
huc12_wcc %>% st_drop_geometry() %>%
  ggplot() + geom_histogram(aes(fire_risk_sum)) + scale_x_log10(labels  = scales::comma)
