

setwd("/Users/christianbaehr/Dropbox/charisma_project/data/")

pacman::p_load(haven, sf, xtable)

sf_use_s2(F)

counties <- st_read("original/GIS/US_AtlasHCB_Counties_Gen0001/US_HistCounties_Gen0001_Shapefile/US_HistCounties_Gen0001.shp",
                    stringsAsFactors=F)

## 1952

counties_temp <- counties[which(counties$START_N < 19520401 & counties$END_N > 19520401), ]
counties_temp$county_area <- st_area(counties_temp$geometry)

districts <- st_read("original/GIS/condistrict_boundaries/1952/districts083.shp", stringsAsFactors=F)
districts$geometry <- st_transform(districts$geometry, "EPSG:4326")
districts$district_area <- st_area(districts$geometry)

cd_cty_units <- st_intersection(counties_temp, districts)
cd_cty_units$unit_area <- st_area(cd_cty_units$geometry)
cd_cty_units$county_prop <- as.numeric(cd_cty_units$unit_area / cd_cty_units$county_area)

#hist(cd_cty_units$county_prop)
prop_wholecounty_1952 <- sum(cd_cty_units$county_prop>0.9) / sum(cd_cty_units$county_prop>0.1)


## 1962

counties_temp <- counties[which(counties$START_N < 19620401 & counties$END_N > 19620401), ]
counties_temp$county_area <- st_area(counties_temp$geometry)

districts <- st_read("original/GIS/condistrict_boundaries/1962/districts088.shp", stringsAsFactors=F)
districts$geometry <- st_transform(districts$geometry, "EPSG:4326")
districts$district_area <- st_area(districts$geometry)

cd_cty_units <- st_intersection(counties_temp, districts)
cd_cty_units$unit_area <- st_area(cd_cty_units$geometry)
cd_cty_units$county_prop <- as.numeric(cd_cty_units$unit_area / cd_cty_units$county_area)

#hist(cd_cty_units$county_prop)
prop_wholecounty_1962 <- sum(cd_cty_units$county_prop>0.9) / sum(cd_cty_units$county_prop>0.1)


## 1972

counties_temp <- counties[which(counties$START_N < 19720401 & counties$END_N > 19720401), ]
counties_temp$county_area <- st_area(counties_temp$geometry)

districts <- st_read("original/GIS/condistrict_boundaries/1972/districts093.shp", stringsAsFactors=F)
districts$geometry <- st_transform(districts$geometry, "EPSG:4326")
districts$district_area <- st_area(districts$geometry)

cd_cty_units <- st_intersection(counties_temp, districts)
cd_cty_units$unit_area <- st_area(cd_cty_units$geometry)
cd_cty_units$county_prop <- as.numeric(cd_cty_units$unit_area / cd_cty_units$county_area)

#hist(cd_cty_units$county_prop)
prop_wholecounty_1972 <- sum(cd_cty_units$county_prop>0.9) / sum(cd_cty_units$county_prop>0.1)


## 1982

counties_temp <- counties[which(counties$START_N < 19820401 & counties$END_N > 19820401), ]
counties_temp$county_area <- st_area(counties_temp$geometry)

districts <- st_read("original/GIS/condistrict_boundaries/1982/districts098.shp", stringsAsFactors=F)
districts$geometry <- st_transform(districts$geometry, "EPSG:4326")
districts$district_area <- st_area(districts$geometry)

cd_cty_units <- st_intersection(counties_temp, districts)
cd_cty_units$unit_area <- st_area(cd_cty_units$geometry)
cd_cty_units$county_prop <- as.numeric(cd_cty_units$unit_area / cd_cty_units$county_area)

#hist(cd_cty_units$county_prop)
prop_wholecounty_1982 <- sum(cd_cty_units$county_prop>0.9) / sum(cd_cty_units$county_prop>0.1)

##

out <- data.frame(year=seq(1952, 1982, 10), 
                  complete_county_prop = c(prop_wholecounty_1952, prop_wholecounty_1962, prop_wholecounty_1972, prop_wholecounty_1982))

out.table <- xtable(out)
print(out.table, "../results/summary_statistics/complete_counties_in_districts.tex", type = "latex")

