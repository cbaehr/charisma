
library(PL94171)

setwd("/Users/christianbaehr/Downloads/census_redistricting/census_redistricting_2000")

root2000a <- "https://www2.census.gov/census_2000/datasets/redistricting_file--pl_94-171/%s/%s00001.upl.zip"
root2000b <- "https://www2.census.gov/census_2000/datasets/redistricting_file--pl_94-171/%s/%s00002.upl.zip"
root2000geo <- "https://www2.census.gov/census_2000/datasets/redistricting_file--pl_94-171/%s/%sgeo.upl.zip"

patha <- "%s/%s00001_2000_a.upl.zip"
pathb <- "%s/%s00001_2000_b.upl.zip"
pathgeo <- "%s/%s00001_2000_geo.upl.zip"

state <- data.frame(year=2000, long=state.name, short=tolower(state.abb))
state$long <- gsub(" ", "_", state$long)

for(i in 1:nrow(state)) {
  
  long <- state$long[i]
  short <- state$short[i]
  
  if (!long %in% list.files()) {dir.create(long)}
  
  download.file(url=sprintf(root2000a, long, short), destfile=sprintf(patha, long, short))
  download.file(url=sprintf(root2000b, long, short), destfile=sprintf(pathb, long, short))
  download.file(url=sprintf(root2000geo, long, short), destfile=sprintf(pathgeo, long, short))
  
  for(j in setdiff(list.files(long), "unzipped")) {
    unzip(zipfile=paste(long, j, sep="/"), exdir = paste(long, "unzipped", sep="/"))
  }
  
}


#####

setwd("../census_redistricting_2010")

root <- "https://www2.census.gov/census_2010/01-Redistricting_File--PL_94-171/%s/%s2010.pl.zip"

path <- "%s/%s2010.pl.zip"

state <- data.frame(year=2000, long=state.name, short=tolower(state.abb))
state$long <- gsub(" ", "_", state$long)

#for(i in 1:nrow(state)) {
for(i in which(state$long=="Georgia"):nrow(state)) {
  long <- state$long[i]
  short <- state$short[i]
  
  if (!long %in% list.files()) {dir.create(long)}
  
  download.file(url=sprintf(root, long, short), destfile=sprintf(path, long, short))
  #download.file(url=sprintf(root2000b, long, short), destfile=sprintf(pathb, long, short))
  #download.file(url=sprintf(root2000geo, long, short), destfile=sprintf(pathgeo, long, short))
  
  for(j in setdiff(list.files(long), "unzipped")) {
    unzip(zipfile=paste(long, j, sep="/"), exdir = paste(long, "unzipped", sep="/"))
  }
  
}

#####

setwd("../census_redistricting_2020")

root <- "https://www2.census.gov/programs-surveys/decennial/2020/data/01-Redistricting_File--PL_94-171/%s/%s2020.pl.zip"

path <- "%s/%s2020.pl.zip"

state <- data.frame(year=2000, long=state.name, short=tolower(state.abb))
state$long <- gsub(" ", "_", state$long)

for(i in 1:nrow(state)) {
#for(i in which(state$long=="Georgia"):nrow(state)) {
  long <- state$long[i]
  short <- state$short[i]
  
  if (!long %in% list.files()) {dir.create(long)}
  
  download.file(url=sprintf(root, long, short), destfile=sprintf(path, long, short))
  #download.file(url=sprintf(root2000b, long, short), destfile=sprintf(pathb, long, short))
  #download.file(url=sprintf(root2000geo, long, short), destfile=sprintf(pathgeo, long, short))
  
  for(j in setdiff(list.files(long), "unzipped")) {
    unzip(zipfile=paste(long, j, sep="/"), exdir = paste(long, "unzipped", sep="/"))
  }
  
}

#####

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

allyears <- do.call(rbind, list(dat2000, dat2010, dat2020))

names(allyears) <- c("con_district", "county", "unit_pop", "state", "year")

countypop <- aggregate(allyears$unit_pop, by=list(allyears$year, allyears$state, allyears$county), FUN=sum)

names(countypop) <- c("year", "state", "county", "countypop")

allyears <- merge(allyears, countypop)

allyears$unit_weight <- allyears$unit_pop / allyears$countypop

#####

setwd("/Users/christianbaehr/Dropbox/charisma_project/data/census_redistricting")

fips <- read.csv("fips_codes_clean.csv", stringsAsFactors = F)
fips$fips <- as.character(fips$fips)
fips$fipschar <- ifelse(nchar(fips$fips)==4, paste0("0", fips$fips), fips$fips)

fips$statefips <- substr(fips$fipschar, 1, 2)

state <- fips[, c("state", "statefips")]
state <- state[!duplicated(state$state), ]

allyears$county <- as.character(allyears$county)
allyears$countychar <- ifelse(nchar(allyears$county)==1, paste0("00", allyears$county), ifelse(nchar(allyears$county)==2, paste0("0", allyears$county), allyears$county))

# omitting alaska
allyears$stateabb <- state.abb[match(gsub("_", " ", allyears$state), state.name)]
allyears$statefips <- state$statefips[match(allyears$stateabb, state$state)]
allyears <- allyears[!is.na(allyears$statefips), ]

allyears$fips <- paste0(allyears$statefips, allyears$countychar)

sum(allyears$fips %in% fips$fipschar)
allyears$fips[which(!allyears$fips %in% fips$fipschar)]
allyears$state[which(!allyears$fips %in% fips$fipschar)]

# dropping 55 observations from the county dataset which do not have a match 
countyplusname <- merge(allyears, fips[, c("name", "fipschar")], by.x="fips", by.y="fipschar")

test <- aggregate(countyplusname$con_district, by=list(countyplusname$year, countyplusname$state), FUN=function(x) length(unique(x)))
# LOOKS GOOD!

write.csv(countyplusname, "../condistrict_to_county_mapping_withcountynames.csv", row.names=F)



