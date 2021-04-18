library(rgee)
ee_Initialize()
library(sf)
library(raster)
library(tidyverse)
library(arcgisbinding)
arc.check_product()

#prepping fire data for cbi in Earth Engine

fire_11_19 <- read_sf('cbi_test/fire_2011_present.shp')

fire_15_all <- fire_11_19 %>% filter(FIREYEAR >= 2015) %>% st_transform(4326) %>%
  select(Fire_Year = "FIREYEAR",Fire_Name = "FIRENAME") %>%
  mutate(Start_Day = 182, End_Day = 273,
         Fire_ID = row_number())

st_write(fire_15_all, dsn = 'cbi_test', layer = 'fire_15_all',
         driver = 'ESRI Shapefile')

# Once downloaded, then reclassify

cbi_bc_final <- raster('cbi_test/cbi_bc_R1.tif')
cbi_bc_rc <- reclassify(cbi_bc_final, c(0,0.1,0,0.1,1.24,1,1.24,2.24,2,2.24,3,3))

writeRaster(cbi_bc_rc, 'cbi_test/cbi_bc_rc.tif')

cbi_bc_high <- reclassify(cbi_bc_rc, c(0,2,0, 2, 3, 1))
plot(cbi_bc_high)
cbi_bc_mod <- reclassify(cbi_bc_rc, c(0,1,0,1,2,1, 2, 3, 0))
plot(cbi_bc_mod)
# Now we can extract the sum per huc12

huc12_wcc <- read_sf('T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Data/BaseDatasets.gdb', layer = 'WCATT_WCC_HU12_R1_Clip')

library(exactextractr)
wcc_cbi <- huc12_wcc
wcc_cbi$sum <- exact_extract(cbi_bc_rc, huc12_wcc, fun = 'sum')
wcc_cbi$sum_high <- exact_extract(cbi_bc_high, huc12_wcc, fun = 'sum')
wcc_cbi$sum_mod <- exact_extract(cbi_bc_mod, huc12_wcc, fun = 'sum')

wcc_cbi <- wcc_cbi %>% mutate(high_acres = (sum_high*900)/4046.8564224,
                              mod_acres = (sum_mod*900)/4046.8564224,
                              acres = as.numeric(units::set_units(st_area(.), 'acre')),
                              per_high = high_acres/acres,
                              per_mod = mod_acres/acres,
                              per_all = (high_acres+mod_acres)/acres)


glimpse(wcc_cbi)

# convert to dataframe

wcc_cbi_df <- wcc_cbi %>% st_drop_geometry()

path <- 'T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Data/Josh'
arc.write(paste0(path, '/josh_wcc.gdb/cbi_bc_final'), data = wcc_cbi)

# here we'll start exploring the data.
#read in (if needed)


wcc_cbi <- arc.open(paste0(path, '/josh_wcc.gdb/cbi_bc_final'))

wcc_cbi <- arc.data2sf(arc.select(wcc_cbi))
# look at a distributions
lines <- data.frame(vlines = c(134, 1579, 6040,22549,262046),
                    labels = c("80%", "85%", "90%", "95%", "100%"))

#log-scaled
wcc_cbi %>% st_drop_geometry() %>%
  ggplot(aes(sum)) + geom_density(lwd = 2) +
  theme_bw() +
  geom_vline(data = lines, aes(xintercept = vlines), linetype = 2)+
  geom_text(data = lines, aes(x = vlines, y = 0, label = labels), nudge_x = -0.2) +
  scale_x_log10(labels = scales::comma, limits = c(0.1,1e9), expand = c(0,0)) +
  labs(title = "Distribution of Summed CBI per Huc12",
       subtitle = 'dotted represent percentiles \nnote x is log-scaled!',
       x = 'Summed CBI (log-scaled)')

#normal

wcc_cbi %>% st_drop_geometry() %>%
  ggplot(aes(sum)) + geom_density(lwd = 1) +
  theme_bw() + scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribution of Summed CBI per Huc12",
       x = 'Summed CBI')

#better looking using cairo
trace(grDevices::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
  }), print = FALSE)

  wcc_cbi %>% filter(sum >= 6040) %>% mapview::mapview()

#quantiles of the sum
quantile(wcc_cbi$sum, probs = seq(0,1,.05), na.rm = TRUE)

#view quantile breaks

wcc_cbi %>% st_drop_geometry() %>%  mutate(breaks = ifelse(sum < 134, '<80%',ifelse(
  sum >= 134 & sum < 1579, '80-85%', ifelse(
    sum >= 1579 & sum < 6040, '85-90%', ifelse(
      sum >= 6040 & sum < 22549, '90-95%', ifelse(
        sum >= 22549, '>95%', "NA"))))),
  breaks = fct_relevel(breaks, c('<80%', '80-85%', '85-90%', '90-95%', '>95%' ))) %>% group_by(breaks) %>%
  summarise(break_count = n()) %>%
  ggplot(aes(breaks, break_count)) +
  geom_col() +
  labs(y = 'count', x = 'percentiles', title = 'Count per break in percentiles of summed CBI per HUC 12') +
  geom_point() + ggrepel::geom_label_repel(aes(label = break_count))



# finally, bring in the quality controlled results for the ratings
andys_cbi <- read_sf('T:/FS/NFS/R01/Program/2500WatershedAirMgmt/GIS/WorkSpace/jefta/WCC_reassessment/CBI_bc_copy1.shp')

#now we can write them to the final gdb after fixing some ambiguous attribute names.
andys_cbi <- andys_cbi %>% rename(sum_high = 'sum_hgh', high_acres = 'hgh_crs',
                                  mod_acres = 'mod_crs', fraction_high = 'per_hgh',
                                  fraction_mod = 'per_mod', cbi_fraction = 'per_all',
                                  class_fire_regime_wildfire = 'Rate_1') %>%
  select(HUC_12, HU_12_N, acres, sum_high, sum_mod, mod_acres, high_acres, fraction_high, fraction_mod, cbi_fraction, class_fire_regime_wildfire)


arc.write("T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Data/OutputDatasets_Review.gdb/fire_regime_wildfire", data = andys_cbi, overwrite = TRUE, validate = TRUE)
quantile(andys_cbi$cbi_fraction, probs = seq(0,1,.05), na.rm = TRUE)
