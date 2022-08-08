
# set main directory with "data2000", "data2010" and "data2020" folders in it
setwd("/Users/christianbaehr/Desktop/census_redistricting/")

###

# 1992-2000 districts

# point to the 1992-2000 folder
setwd("data2000")

# create utility dataset with long and short state names
state <- data.frame(long=state.name, short=state.abb)

# create empty list to store processed data for each state
holder <- list()

for(i in 1:nrow(state)) {
  temp <- sapply(dir(state$short[i]), function(x) read.csv(paste0(state$short[i], "/", x)))
  b <- do.call(cbind, temp)
  
  b <- b[which(b$geo.csv.SUMLEV=="750"), ]
  
  county <- aggregate(b[ , c("geo.csv.POP100")], by=list(b$geo.csv.CD106, b$geo.csv.COUNTY), FUN=sum)
  
  county$state <- state$short[i]
  
  total <- aggregate(b[, c("geo.csv.POP100")], by=list(b$geo.csv.COUNTY), FUN=sum)

  county$county_pop <- total$x[match(county$Group.2, total$Group.1)]
  county$weight <- county$x / county$county_pop
  
  county <- county[, c("state", "Group.1", "Group.2", "x", "weight", "county_pop")]
  names(county) <- c("state", "con_district", "county", "unit_pop", "unit_weight", "county_pop")
  
  holder[[state$short[i]]] <- county
  
}

full2000 <- do.call(rbind, holder)

full2000$year <- 1992

print("done 2000")

##########

setwd("../data2010")

holder <- list()

for(i in 1:nrow(state)) {

  temp <- sapply(dir(state$short[i]), function(x) read.csv(paste0(state$short[i], "/", x)))
  b <- do.call(cbind, temp)
  
  b <- b[which(b$geo.csv.SUMLEV=="750"), ]
  
  county <- aggregate(b[ , c("geo.csv.POP100")], by=list(b$geo.csv.CD, b$geo.csv.COUNTY), FUN=sum)
  
  county$state <- state$short[i]
  
  total <- aggregate(b[, c("geo.csv.POP100")], by=list(b$geo.csv.COUNTY), FUN=sum)
  
  county$county_pop <- total$x[match(county$Group.2, total$Group.1)]
  county$weight <- county$x / county$county_pop
  
  county <- county[, c("state", "Group.1", "Group.2", "x", "weight", "county_pop")]
  names(county) <- c("state", "con_district", "county", "unit_pop", "unit_weight", "county_pop")
  
  holder[[state$short[i]]] <- county
  
  print(state$short[i])
  
}

full2010 <- do.call(rbind, holder)

full2010$year <- 2002

print("done 2010")


##########


setwd("../data2020")

#rm(list = setdiff(ls(), c("full2000"))

holder <- list()

for(i in 1:nrow(state)) {
  #for(i in 1:which(state$short=="MO")) {
  
  # read in the 2020 redistricting files for state i
  temp <- sapply(dir(state$short[i]), function(x) read.csv(paste0(state$short[i], "/", x)))
  # bind files data together
  b <- do.call(cbind, temp)
  
  b <- b[which(b$geo.csv.SUMLEV=="750"), ] #only keep rows in summary level 750, block level
  # only the CD level and 750 level contain CD values
  # 750 is the State-County-Voting District/Remainder-County Subdivision-Place/Remainder- Census Tract-Block Group-Block
  
  # sum the population of all census blocks within each congressional district-county unit
  county <- aggregate(b[ , c("geo.csv.POP100")], by=list(b$geo.csv.CD116, b$geo.csv.COUNTY), FUN=sum)
  
  county$state <- state$short[i]
  
  # compute total population at the COUNTY level. Using these values to construct portion weights for counties
  # falling in multiple districts
  total <- aggregate(b[, c("geo.csv.POP100")], by=list(b$geo.csv.COUNTY), FUN=sum)
  
  # create county-level population variable in the CD-county dataset
  county$county_pop <- total$x[match(county$Group.2, total$Group.1)]
  # construct weights. Population of the CD-county unit divided by total county population
  county$weight <- county$x / county$county_pop
  
  # only keep specific variables
  county <- county[, c("state", "Group.1", "Group.2", "x", "weight", "county_pop")]
  # renaming variables
  names(county) <- c("state", "con_district", "county", "unit_pop", "unit_weight", "county_pop")
  
  # store result in the list
  holder[[state$short[i]]] <- county
  
}

print("done 2020")

full2020 <- do.call(rbind, holder)

full2020$year <- 2012

##########

# bind 1990s, 2000s, and 2010s data togather 
complete_counties <- do.call(rbind, list(full2000, full2010, full2020))

# write processed data to csv file
write.csv(complete_counties, "../condistrict_to_county_mapping.csv", row.names=F)


