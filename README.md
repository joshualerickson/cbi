# cbi

This repo has steps to get the Composite Burn Index (CBI) from [Parks et al. (2019)](https://www.mdpi.com/2072-4292/11/14/1735/pdf) and extract it to sub-watersheds (HUC12). The `prepping_cbi.R` file includes details and code on processing the CBI results from `cbi_WCC.txt`. The `cbi_WCC.txt` is a text file with JavaScript code to use in Google Earth Engine (GEE); this code was taken from the Parks paper. The `test_cbi_mtbs.Rmd` file deep-dives into the differences between the Monitoring Trends on Burn Severity product with CBI.
