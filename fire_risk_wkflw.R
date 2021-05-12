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
fire_risk_sf <- read_sf( 'huc12_wcc_fire_risk.gpkg', layer = 'fire_risk') %>% select(-SHAPE_Length, -SHAPE_Area)

#write for exploring in ArcGIS
arc.write(path = 'T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Data/OutputDatasets_Review.gdb/WCC_HUC12_r1_FireRisk', data = fire_risk_sf,
          validate = TRUE)

#bring in final_wcc_reassessment
wcc_add_fire_risk <- read_sf('T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Data/OutputDatasets_Review.gdb', layer = 'Final_WCC_Reassessment')




st_layers('T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Data/OutputDatasets_Review.gdb/')
ggplot() + geom_sf(data = huc12_wcc, aes(fill = log10(fire_risk_sum)))
huc12_wcc %>% st_drop_geometry() %>%
  ggplot() + geom_histogram(aes(fire_risk_sum)) + scale_x_log10(labels  = scales::comma)

############This is for the updated version of fire risk!!!!!!!!!!!!! ########################
#bring in fire risk raster
fire_risk <- raster::raster('T:/FS/NFS/R01/Program/2500WatershedAirMgmt/GIS/WorkSpace/jerickson/fire_risk/erABS_x_bp')

#read in HUC used in analysis

huc12_wcc <- read_sf('T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Data/BaseDatasets.gdb', layer = 'WCATT_WCC_HU12_R1_Clip')


# extract the sum
huc12_wcc$fire_risk_sum <- exact_extract(fire_risk, huc12_wcc, 'sum')

#write for backup
write_sf(huc12_wcc, 'huc12_wcc_fire_risk.gpkg', layer = 'fire_risk_Er_x_Bp')
fire_risk_sf <- read_sf( 'huc12_wcc_fire_risk.gpkg', layer = 'fire_risk_Er_x_Bp') %>% select(-SHAPE_Length, -SHAPE_Area)
mapview::mapview(fire_risk_sf, zcol = 'fire_risk_sum')

#write for exploring in ArcGIS
arc.write(path = 'T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Data/OutputDatasets_Review.gdb/WCC_HUC12_r1_FireRisk', data = fire_risk_sf,
          validate = TRUE)

#bring in final_wcc_reassessment
final_wcc_reassessment <- read_sf('T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Data/OutputDatasets_Review.gdb', layer = 'Final_WCC_Reassessment')

#bring in the final product Andy developed

final_fire_risk_sf <- read_sf('T:/FS/NFS/R01/Program/2500WatershedAirMgmt/GIS/WorkSpace/jefta/WCC_reassessment/Fire_risk1.shp')

glimpse(final_fire_risk_sf)

glimpse(final_wcc_reassessment)

final_fire_risk <- left_join(final_wcc_reassessment %>% select(1:5),
          final_fire_risk_sf %>% select(WldFireRegRisk_Class = 'risk_class', HUC_12) %>% st_drop_geometry())

glimpse(final_fire_risk)

arc.write(path = 'T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Data/OutputDatasets_Review.gdb/WCC_HUC12_r1_FireRisk_Rating', data = final_fire_risk,
          validate = TRUE)
