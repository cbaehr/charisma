
library(sf)

sf_use_s2(F)

allyears <- seq(1952, 1982, 10)

cong <- list()
for(i in unique(allyears)) {
  temp <- st_read(paste0("/Users/christianbaehr/Downloads/cd_boundaries/", i), stringsAsFactors = F)
  temp <- temp[which(temp$DISTRICT != 0), ]
  cong[[as.character(i)]] <- st_transform(temp, crs=4326)
  
}

county <- st_read("/Users/christianbaehr/Downloads/US_AtlasHCB_Counties_Gen0001/US_HistCounties_Gen0001_Shapefile/US_HistCounties_Gen0001.shp", stringsAsFactors=F)
county$county_area <- as.numeric(st_area(county$geometry)) / 1000000

county_nogeom <- data.frame(county)
county_nogeom <- county_nogeom[, names(county_nogeom) != "geometry"]

##########

for(i in 1:length(allyears)) {
  if(i==1) {datnew2 <- list()}
  
  year <- allyears[i]
  congyear <- cong[[as.character(year)]]
  yearlong <- as.numeric(paste0(year, "0101"))
  #cqyear <- cq[ which(as.character(year) == cq$raceYear), ]
  
  countyyear <- county[which(county$START_N<=yearlong & county$END_N>=yearlong), ]
  countyyear_nogeom <- county_nogeom[which(county_nogeom$START_N<=yearlong & county_nogeom$END_N>=yearlong), ]
  
  #countyyear <- countyyear[which(countyyear$STATE_TERR=="Arizona"),]
  #countyyear_nogeom <- countyyear_nogeom[which(countyyear_nogeom$STATE_TERR=="Arizona"), ]
  
  int <- st_intersection(congyear, countyyear)
  int <- int[which(int$STATENAME==int$STATE_TERR), ]
  int$unit_area <- as.numeric(st_area(int$geometry)) / 1000000
  int$county_prop <- int$unit_area / int$county_area # proportion of the county that is in this district
  write_sf(int, "/Users/christianbaehr/Downloads/processed.geojson")
  
  keep <- int$county_prop > 0.001 # only keep those cases with >0.1% of the county in the district
  int <- int[which(keep), ]
  #row <- countyyear_nogeom[rep(j, nrow(int)), ]
  
  
  
  
  
  #temp <- list()
  for(j in 1:nrow(countyyear)) {
    
    int <- st_intersection(congyear, countyyear[j,])
    int <- int[which(int$STATENAME==int$STATE_TERR), ]
    int$unit_area <- as.numeric(st_area(int$geometry)) / 1000000
    int$county_prop <- int$unit_area / int$county_area # proportion of the county that is in this district
    
    #keep <- int$county_prop > 0.05 # only keep those cases with >5% of the county in the district
    keep <- int$county_prop > 0.001 # only keep those cases with >0.1% of the county in the district
    int <- int[which(keep), ]
    row <- countyyear_nogeom[rep(j, nrow(int)), ]
    
    row$cd <- int$DISTRICT
    row$county_prop <- int$county_prop
    #datnew2[[paste(i, j)]] <- row
    temp[[paste(i, j)]] <- row
  }
  
  #out <- i
  
}

##########

for(i in 1:length(allyears)) {
  if(i==1) {datnew2 <- list()}
  
  year <- allyears[i]
  congyear <- cong[[as.character(year)]]
  yearlong <- as.numeric(paste0(year, "0101"))
  #cqyear <- cq[ which(as.character(year) == cq$raceYear), ]
  
  countyyear <- county[which(county$START_N<=yearlong & county$END_N>=yearlong), ]
  countyyear_nogeom <- county_nogeom[which(county_nogeom$START_N<=yearlong & county_nogeom$END_N>=yearlong), ]
  
  countyyear <- countyyear[which(countyyear$STATE_TERR=="Arizona"),]
  countyyear_nogeom <- countyyear_nogeom[which(countyyear_nogeom$STATE_TERR=="Arizona"), ]
  
  temp <- list()
  for(j in 1:nrow(countyyear)) {
    
    int <- st_intersection(congyear, countyyear[j,])
    int <- int[which(int$STATENAME==int$STATE_TERR), ]
    int$unit_area <- as.numeric(st_area(int$geometry)) / 1000000
    int$county_prop <- int$unit_area / int$county_area # proportion of the county that is in this district
    
    #keep <- int$county_prop > 0.05 # only keep those cases with >5% of the county in the district
    keep <- int$county_prop > 0.001 # only keep those cases with >0.1% of the county in the district
    int <- int[which(keep), ]
    row <- countyyear_nogeom[rep(j, nrow(int)), ]
    
    row$cd <- int$DISTRICT
    row$county_prop <- int$county_prop
    #datnew2[[paste(i, j)]] <- row
    temp[[paste(i, j)]] <- row
  }
  
  #out <- i
  
}

for(i in 1:length(allyears)) {
  year <- allyears[i]
  for(j in 1:nrow(countyyear)) {
    if(!is.null(datnew2[[paste(i, j)]])) {
      if(nrow(datnew2[[paste(i, j)]]) > 0) {
        datnew2[[paste(i, j)]]$year <- year
      } else {
        datnew2[[paste(i, j)]]$year <- NULL
      }
    }
  }
}

datout2 <- do.call(rbind, datnew2)
write.csv(datout2, "/Users/christianbaehr/Desktop/condistrict_to_county_1952_82_fullreplace.csv", row.names=F)

###

test2 <- datout2
test2 <- test2[test2$year %in% c(1952, 1962, 1972, 1982), ]
testearly <- aggregate(test2$cd, by=list(test2$year, test2$STATE_TERR), FUN=function(x) length(x))

View(datout2[datout2$replaceid==-2, ])
table(datout2$replaceid)

latedat <- read.csv("/Users/christianbaehr/Dropbox/charisma_project/data/condistrict_to_county_mapping_withcountynames.csv", stringsAsFactors = F)
testlate <- aggregate(latedat$con_district, by=list(latedat$year, latedat$state), FUN=function(x) length(x))

fulltest <- rbind(testearly, testlate)
fulltest$Group.2 <- gsub("_", " ", fulltest$Group.2)
fulltest <- reshape(fulltest, direction="wide", idvar="Group.2", timevar="Group.1")
rownames(fulltest) <- 1:nrow(fulltest)
names(fulltest) <- c("state", seq(1952, 2012, 10))

###

cq <- do.call(rbind, list(read.csv("/Users/christianbaehr/Dropbox/charisma_project/data/congressdata/congressdata_1944-1960.csv", stringsAsFactors = F, skip=2),
                          read.csv("/Users/christianbaehr/Dropbox/charisma_project/data/congressdata/congressdata_1962-1980.csv", stringsAsFactors = F, skip=2),
                          read.csv("/Users/christianbaehr/Dropbox/charisma_project/data/congressdata/congressdata_1982-2000.csv", stringsAsFactors = F, skip=2)))
#cq$Area <- as.numeric(gsub("District ", "", cq$Area))

sort(unique(cq$Area))
cq <- cq[which(!cq$Area %in% c("", "Area", "At Large")), ]

cq1982 <- cq[which(cq$raceYear=="1982"), ]
cq1992 <- cq[which(cq$raceYear=="1992"), ]

n1982 <- aggregate(cq1982$Area, by=list(cq1982$State), FUN=function(x) length(x))
n1992 <- aggregate(cq1992$Area, by=list(cq1992$State), FUN=function(x) length(x))

ndist <- merge(n1982, n1992, by="Group.1")
ndist <- data.frame(state=ndist$Group.1, distadd1992 = (ndist$x.y - ndist$x.x))

fulltest <- merge(fulltest, ndist)

###

print(xtable::xtable(fulltest, type = "latex"))







