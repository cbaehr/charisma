
## Downloading census redistricting files from Census website. Must do separately
## for 1992, 2002, and 2012 decades. Then processing the data and joining it into
## a single dataset that creates a map for EACH decade from congressional district
## to county. 

library(PL94171)

## DO 1992

setwd("/Users/christianbaehr/Dropbox/charisma_project/data/original/census_redistricting/census_redistricting_2000")

# ## file roots for 1992 data
# root2000a <- "https://www2.census.gov/census_2000/datasets/redistricting_file--pl_94-171/%s/%s00001.upl.zip"
# root2000b <- "https://www2.census.gov/census_2000/datasets/redistricting_file--pl_94-171/%s/%s00002.upl.zip"
# root2000geo <- "https://www2.census.gov/census_2000/datasets/redistricting_file--pl_94-171/%s/%sgeo.upl.zip"
# 
# ## skeleton filenames
# patha <- "%s/%s00001_2000_a.upl.zip"
# pathb <- "%s/%s00001_2000_b.upl.zip"
# pathgeo <- "%s/%s00001_2000_geo.upl.zip"
# 
# ## build a dataframe for all states
# state <- data.frame(year=2000, long=state.name, short=tolower(state.abb))
# state$long <- gsub(" ", "_", state$long)
# 
# ## download the 1992 files by state
# for(i in 1:nrow(state)) {
#   
#   long <- state$long[i]
#   short <- state$short[i]
#   
#   if (!long %in% list.files()) {dir.create(long)} # if state-specific directory doesnt exist, create it
#   
#   download.file(url=sprintf(root2000a, long, short), destfile=sprintf(patha, long, short))
#   download.file(url=sprintf(root2000b, long, short), destfile=sprintf(pathb, long, short))
#   download.file(url=sprintf(root2000geo, long, short), destfile=sprintf(pathgeo, long, short))
#   
#   for(j in setdiff(list.files(long), "unzipped")) {
#     unzip(zipfile=paste(long, j, sep="/"), exdir = paste(long, "unzipped", sep="/"))
#   }
#   
# }

#####

## DO 2002

setwd("../census_redistricting_2010")

# root <- "https://www2.census.gov/census_2010/01-Redistricting_File--PL_94-171/%s/%s2010.pl.zip"
# 
# path <- "%s/%s2010.pl.zip"
# 
# state <- data.frame(year=2000, long=state.name, short=tolower(state.abb))
# state$long <- gsub(" ", "_", state$long)
# 
# for(i in 1:nrow(state)) {
#   long <- state$long[i]
#   short <- state$short[i]
#   
#   if (!long %in% list.files()) {dir.create(long)}
#   
#   download.file(url=sprintf(root, long, short), destfile=sprintf(path, long, short))
#   
#   for(j in setdiff(list.files(long), "unzipped")) {
#     unzip(zipfile=paste(long, j, sep="/"), exdir = paste(long, "unzipped", sep="/"))
#   }
#   
# }

#####

## DO 2012

setwd("../census_redistricting_2020")

# root <- "https://www2.census.gov/programs-surveys/decennial/2020/data/01-Redistricting_File--PL_94-171/%s/%s2020.pl.zip"
# 
# path <- "%s/%s2020.pl.zip"
# 
# state <- data.frame(year=2000, long=state.name, short=tolower(state.abb))
# state$long <- gsub(" ", "_", state$long)
# 
# for(i in 1:nrow(state)) {
#   long <- state$long[i]
#   short <- state$short[i]
#   
#   if (!long %in% list.files()) {dir.create(long)}
#   
#   download.file(url=sprintf(root, long, short), destfile=sprintf(path, long, short))
#   
#   for(j in setdiff(list.files(long), "unzipped")) {
#     unzip(zipfile=paste(long, j, sep="/"), exdir = paste(long, "unzipped", sep="/"))
#   }
#   
# }

#####

## Now upzip the 1992 data and process

setwd("../census_redistricting_2000/")

state <- data.frame(year=2000, long=state.name, short=tolower(state.abb))
state$long <- gsub(" ", "_", state$long)

holder <- list()

for(i in 1:nrow(state)) {
  temp <- pl_read(paste(state$long[i], "unzipped", sep="/"))
  tempjoin <- temp$geo
  #tempjoin <- do.call(cbind, temp)

  tempjoin <- tempjoin[which(tempjoin$SUMLEV=="750"), ]
  county <- aggregate(as.numeric(tempjoin$POP100), by=list(tempjoin$CD106, tempjoin$COUNTY), FUN=sum)
  
  county$state <- state$long[i]
  county$year <- 1992
  #tempjoin <- tempjoin[, grep("geo.", names(tempjoin), value=T)]
  holder[[state$long[i]]] <- county
  
}

dat2000 <- do.call(rbind, holder)

#####

## Unzip the 2002 data and process

setwd("../census_redistricting_2010/")

state <- data.frame(year=2000, long=state.name, short=tolower(state.abb))
state$long <- gsub(" ", "_", state$long)

holder <- list()

for(i in 1:nrow(state)) {
  temp <- pl_read(paste(state$long[i], "unzipped", sep="/"))
  tempjoin <- temp$geo
  #tempjoin <- do.call(cbind, temp)
  
  tempjoin <- tempjoin[which(tempjoin$SUMLEV=="750"), ]
  county <- aggregate(as.numeric(tempjoin$POP100), by=list(tempjoin$CD, tempjoin$COUNTY), FUN=sum)
  
  county$state <- state$long[i]
  county$year <- 2002
  #tempjoin <- tempjoin[, grep("geo.", names(tempjoin), value=T)]
  holder[[state$long[i]]] <- county
  
}

dat2010 <- do.call(rbind, holder)

#####

## Unzip the 2012 data and process

setwd("../census_redistricting_2020/")

state <- data.frame(year=2000, long=state.name, short=tolower(state.abb))
state$long <- gsub(" ", "_", state$long)

holder <- list()

for(i in 1:nrow(state)) {
  temp <- pl_read(paste(state$long[i], "unzipped", sep="/"))
  tempjoin <- temp$geo
  #tempjoin <- do.call(cbind, temp)
  
  tempjoin <- tempjoin[which(tempjoin$SUMLEV=="750"), ]
  county <- aggregate(as.numeric(tempjoin$POP100), by=list(tempjoin$CD116, tempjoin$COUNTY), FUN=sum)
  
  county$state <- state$long[i]
  county$year <- 2012
  #tempjoin <- tempjoin[, grep("geo.", names(tempjoin), value=T)]
  holder[[state$long[i]]] <- county
  
}

dat2020 <- do.call(rbind, holder)

#####

## join the data from all years
allyears <- do.call(rbind, list(dat2000, dat2010, dat2020))

names(allyears) <- c("con_district", "county", "unit_pop", "state", "year")

## aggregate population data from district to county level (for each year)
countypop <- aggregate(allyears$unit_pop, by=list(allyears$year, allyears$state, allyears$county), FUN=sum)

names(countypop) <- c("year", "state", "county", "countypop")

## join main data with county-level population variable
allyears <- merge(allyears, countypop)

## create county "weights" 
allyears$unit_weight <- allyears$unit_pop / allyears$countypop

#####

## join processed census data with the cleaned FIPS data and write to file

setwd("../../../working/")

## load in FIPS codes
fips <- read.csv("fips_codes_clean.csv", stringsAsFactors = F)
fips$fips <- as.character(fips$fips)
fips$fipschar <- ifelse(nchar(fips$fips)==4, paste0("0", fips$fips), fips$fips) # add leading zero if necessary

fips$statefips <- substr(fips$fipschar, 1, 2) # first two digits

state <- fips[, c("state", "statefips")]
state <- state[!duplicated(state$state), ]

allyears$county <- as.character(allyears$county)
## add leading zeroes if necessary to census data
allyears$countychar <- ifelse(nchar(allyears$county)==1, paste0("00", allyears$county), 
                              ifelse(nchar(allyears$county)==2, paste0("0", allyears$county), allyears$county))

# omitting alaska
allyears$stateabb <- state.abb[match(gsub("_", " ", allyears$state), state.name)]
allyears$statefips <- state$statefips[match(allyears$stateabb, state$state)]
allyears <- allyears[!is.na(allyears$statefips), ]

allyears$fips <- paste0(allyears$statefips, allyears$countychar) # create full fips code

sum(allyears$fips %in% fips$fipschar)
allyears$fips[which(!allyears$fips %in% fips$fipschar)]
allyears$state[which(!allyears$fips %in% fips$fipschar)]

# dropping 55 observations from the county dataset which do not have a match 
countyplusname <- merge(allyears, fips[, c("name", "fipschar")], by.x="fips", by.y="fipschar")

test <- aggregate(countyplusname$con_district, by=list(countyplusname$year, countyplusname$state), FUN=function(x) length(unique(x)))
# LOOKS GOOD!

write.csv(countyplusname, "condistrict_to_county_mapping_withcountynames_1992-2012.csv", row.names=F)

###

## validation check for census data

# countyplusname$testvar <- paste(countyplusname$state, countyplusname$county)
# 
# test1992 <- table(countyplusname$testvar[countyplusname$year==1992])
# mean(test1992 > 1)
# 
# test2002 <- table(countyplusname$testvar[countyplusname$year==2002])
# mean(test2002 > 1)
# 
# test2012 <- table(countyplusname$testvar[countyplusname$year==2012])
# mean(test2012 > 1)
# 
# out <- data.frame(congress = c("1992-2002", "2002-2012", "2012-2020"),
#                   pct_counties_in_multiple_districts = c(mean(test1992 > 1), mean(test2002 > 1), mean(test2012 > 1)))
# 
# write.csv(out, "/Users/christianbaehr/Desktop/countystats.csv", row.names = F)
# xtable::xtable(out)
