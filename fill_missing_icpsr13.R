
setwd("/Users/christianbaehr/Dropbox/charisma_project/data")

library(sf)
library(haven)

sf_use_s2(F)

dat <- data.frame(read_dta("../codes-from-Rocio-APSR-county-analysis/counties/output/icpsr13_final.dta"))
# order by year - will reduce the number of times we have to load senate data in to the loop
dat <- dat[order(dat$year), ]

dat <- dat[which(dat$year %in% seq(1952, 1982, 10)), ] # just need one observation per decade

dat <- dat[which(dat$statenm != "Alaska"), ] # dont have actual county names for alaska

dat <- dat[which(dat$countynm!="OAHU"), ]
dat$countynm[which(dat$countynm=="ORANGE/MOSQUITO")] <- "ORANGE"

# read in CQ data for all years 
cq <- do.call(rbind, list(read.csv("congressdata/congressdata_1944-1960.csv", stringsAsFactors = F, skip=2),
                          read.csv("congressdata/congressdata_1962-1980.csv", stringsAsFactors = F, skip=2),
                          read.csv("congressdata/congressdata_1982-2000.csv", stringsAsFactors = F, skip=2)))
cq$Area <- as.numeric(gsub("District ", "", cq$Area))
cq$Area[which(cq$State=="Alaska")] <- 1 # Alaska has only one CD

# for the 999 cases, can use the GIS files to identify the congressional district each county lies in. Do a spatial intersection and then keep whichever
# observation the vast majority of the county lies in

# at large elections coded as 98 or 99, dont worry about those for now. This is about congressional districts

# for the 902s, select the two cases with the most proportion of the district within them. Checked Camden NJ and Hartford CT 1986 and the GIS files identify the districts

# create a list for congressional district shapefiles by year. We will call the relevant year shapefile in to the loop
# when necessary
cong <- list()
for(i in unique(dat$year)) {
  temp <- st_read(paste0("cd_boundaries/", i), stringsAsFactors = F)
  temp <- temp[which(temp$DISTRICT != 0), ] # drop districts with district number 0
  cong[[as.character(i)]] <- st_transform(temp, crs=4326) # transform shapefile CRS to 4326
  
}

# load county shapefile
county <- st_read("US_AtlasHCB_Counties_Gen0001/US_HistCounties_Gen0001_Shapefile/US_HistCounties_Gen0001.shp", stringsAsFactors=F)
# create area variable in squared km
county$county_area <- as.numeric(st_area(county$geometry)) / 1000000

dat$countynm[which(dat$countynm=="BREVARD/ST LUCIE" & dat$year<1986)] <- "BREVARD" # need to change ICPSR for this one. Dependent on the year

county$NAME[which(county$NAME=="Baltimore City (IC)")] <- "BALTIMORE CITY"
county$NAME[which(county$NAME=="DEKALB")] <- "DE KALB"
county$NAME[which(county$NAME=="DESOTO")] <- "DE SOTO"
county$NAME[which(county$NAME=="Lynchburg (IC)")] <- "LYNCHBURG"
county$NAME[which(county$NAME=="Virginia Beach (IC)")] <- "VIRGINIA BEACH"
county$NAME[which(county$NAME=="MANASSAS PARK (IC)")] <- "MANASSAS"
county$NAME[which(county$NAME=="DOÑA ANA")] <- "DONA ANA"

# drop periods from county names to match up with census naming
county$NAME <- gsub("\\.", "", county$NAME)
county$NAME <- toupper(county$NAME) # all county names to uppercase

# running variable
out <- 1

for( i in 1:nrow(dat) ) {
  
  if(i>=out) { # skip previously processed counties
    
    if(i==1){ # if first iteration, load in the congressional district shapefile for that year
      year <- dat$year[1]
      datnew <- list()
      congyear <- cong[[as.character(year)]]
      yearlong <- as.numeric(paste0(year, "0101"))
      cqyear <- cq[ which(as.character(year) == cq$raceYear), ]
    } else if(year!=dat$year[i]) { # if starting on a new year, replace the local congress district shapefile with new year
      year <- dat$year[i]
      congyear <- cong[[as.character(year)]]
      yearlong <- as.numeric(paste0(year, "0101"))
      cqyear <- cq[ which(as.character(year) == cq$raceYear), ]
    } 
    
    if(dat$cd[i] %in% c(902, 903, 904, 905, 906, 907, 914, 918, 999) ) { # if congressional district is either "missing" (999) or county is in multiple CDs (9xx)
      
      ctymatch <- county[which(county$STATE_TERR==dat$statenm[i] & county$NAME==dat$countynm[i] & (county$START_N<=yearlong & county$END_N>=yearlong ) ), ] # find the corresponding county in county shapefile, based on state name county name and year
      int <- st_intersection(congyear, ctymatch) # intersect the matching county feature with the congressional district shapefile to identify which CDs the county is in
      int <- int[which(int$STATENAME==int$STATE_TERR), ] # only keep matches that are from the same state as the CD
      int$unit_area <- as.numeric(st_area(int$geometry)) / 1000000 # area in square kilometers of the county -CD unit
      int$county_prop <- int$unit_area / int$county_area # proportion of the county that is in this district
      
      if(dat$cd[i] == 999 ) { # CD is missing according to Census
        
        #keep <- int$county_prop > 0.05 # only keep those cases with >5% of the county in the district
        keep <- int$county_prop > 0.001 # only keep those cases with >0.1% of the county in the district
        int <- int[which(keep), ]
        row <- dat[rep(i, nrow(int)), ]
        row$replaceid <- 3 # replacing a previously missing CD id
        
      } else if(dat$cd[i] %in% c(902, 903, 904, 905, 906, 907, 914, 918)) { # cases where counties intersect more than one district
        
        n <- dat$cd[i] - 900
        keep <- int$unit_area >= sort(int$unit_area, decreasing =T)[n]
        #HANDLING EXCEPTIONS WHERE NUMBER OF LEGIT INTERSECTIONS IS ACTUALLY GREATER THAN CD 9xx NUMBER IMPLIES
        if(dat$statenm[i]=="Florida" & dat$countynm[i]=="BROWARD" & dat$year[i] %in% c(1986, 1990)) {
          keep <- c(F,T,T,T)
          n <- 3 #LOOKS LIKE BROWARD COUNTY ACTUALLY INTERSECTED 3 DISTRICTS IN 1986
        } else if (dat$statenm[i]=="Oklahoma" & dat$countynm[i]=="OKLAHOMA" & dat$year[i]%in% c(1986, 1988)) {
          keep <- rep(T, 3)
          n <- 3
        } else if (dat$statenm[i]=="Florida" & dat$countynm[i]=="DADE" & dat$year[i]==1988) {
          keep <- rep(T, 4)
          n <- 4
        } else if (dat$statenm[i]=="Pennsylvania" & dat$countynm[i]=="ALLEGHENY" & dat$year[i]==1990) {
          keep <- c(F,T,T,T,F,T)
          n <- 4
        } else if (dat$statenm[i]=="Pennsylvania" & dat$countynm[i]=="PHILADELPHIA" & dat$year[i]==1990) {
          keep <- c(F,T,T,T,T,T)
          n <- 5
        }
        
        #if(any(int$county_prop[!keep] > 0.05)) {stop("MAY BE SKIPPING A RELEVANT COUNTY")}
        if(any(int$county_prop[!keep] > 0.001)) {stop("MAY BE SKIPPING A RELEVANT COUNTY")}
        int <- int[which(keep), ]
        row <- dat[rep(i, n), ]
        row$replaceid <- 2 # filling in CDs for counties that cross multiple
        
      }
      
      row$cd <- int$DISTRICT
      row$county_prop <- int$county_prop

    } else if (!dat$cd[i] %in% cqyear$Area[cqyear$State==dat$statenm[i]] & !(dat$cd[i] %in% c(98, 99)) ) { # cases where the district number according to Census does not exist in the CQ data (esp. Los Angeles with district 67)
      
      if (!dat$countynm[i] %in% county$NAME) {stop("MAY NEED TO ADJUST COUNTY NAME")}
      ctymatch <- county[which(county$STATE_TERR==dat$statenm[i] & county$NAME==dat$countynm[i] & (county$START_N<=yearlong & county$END_N>=yearlong ) ), ]
      
      int <- st_intersection(congyear, ctymatch)
      int <- int[which(int$STATENAME==int$STATE_TERR), ]
      int$unit_area <- as.numeric(st_area(int$geometry)) / 1000000
      int$county_prop <- int$unit_area / int$county_area # proportion of the county that is in this district

      keep <- int$county_prop > 0.001 # only keep those cases with >0.1% of the county in the district
      int <- int[which(keep), ]
      
      if(nrow(int)>0) { # in some states all of the elections are essentially at-large, and the district shapefiles from those states are omitted so there is no intersection
        row <- dat[rep(i, nrow(int)), ] # if multiple CDs overlap the county, create new row for each
        row$replaceid <- 1 # replacing CD for counties whose CD number is NOT IN THE CQ DATA FOR THAT YEAR. Treat these like missing cases
        row$cd <- int$DISTRICT
        row$county_prop <- int$county_prop
      } else {
        row <- dat[i, ]
        row$replaceid <- -2 # at large elections - state shapefile missing
        row$county_prop <- NA
      }
      
    } else if (dat$cd[i] %in% c(98, 99)) { # 98 or 99 implies at large according to Rocio notes
      row <- dat[i, ]
      row$replaceid <- -1 # these counties are for at large elections. We will probably omit these from the data
      row$county_prop <- NA
      
    } else {
      row <- dat[i, ]
      row$replaceid <- 0
      row$county_prop <- 1 # not split amongst more than 1 district, so these counties must be entirely contained
    }
    
    datnew[[i]] <- row
    
    out <- i
    
  }
}

datout <- do.call(rbind, datnew)
datout <- datout[, c("year", "statenm", "state", "countynm", "county", "cd", "replaceid", "county_prop")]

write.csv(datout, "condistrict_to_county_mapping_withcountynames_1952-82.csv", row.names=F)

# -2 implies CDs for which we have a missing state shapefile (I assume these are mostly at large)
# -1 implies at large CDs, as coded by the census
# 0 implies correct and entirely contained CDs, untouched by my program
# 1 implies CDs which were not in the CQ data, so I replace these
# 2 implies counties in multiple CDs, I replace the 9xx code with actual CDs
# 3 implies replacing a previously missing CD code

