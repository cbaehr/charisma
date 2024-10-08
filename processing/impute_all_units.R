

## Dealing with missing county observations from the 1952-82 district-county mapping.
## I use the GIS shapefiles for congressional districts and counties over time to
## fill in these gaps in the county observations. I don't replace any cases that
## are non-missing. 

setwd("/Users/christianbaehr/Dropbox/charisma_project/data/")

library(sf)
library(haven)

sf_use_s2(F)

# for the 999 cases, can use the GIS files to identify the congressional district each county lies in. Do a spatial intersection and then keep whichever
# observation the vast majority of the county lies in

# at large elections coded as 98 or 99, dont worry about those for now. This is about congressional districts

# for the 902s, select the two cases with the most proportion of the district within them. Checked Camden NJ and Hartford CT 1986 and the GIS files identify the districts

# create a list for congressional district shapefiles by year. We will call the relevant year shapefile in to the loop
# when necessary

yrs <- seq(1952, 2012, 10)

cong <- list()
for(i in yrs) {
  temp <- st_read(paste0("original/GIS/condistrict_boundaries/", i), stringsAsFactors = F)
  temp <- temp[which(temp$DISTRICT != 0), ] # drop districts with district number 0
  cong[[as.character(i)]] <- st_transform(temp, crs=4326) # transform shapefile CRS to 4326
  
}

# load county shapefile
county <- st_read("original/GIS/US_AtlasHCB_Counties_Gen0001/US_HistCounties_Gen0001_Shapefile/US_HistCounties_Gen0001.shp", stringsAsFactors=F)
# create area variable in squared km
county$county_area <- as.numeric(st_area(county$geometry)) / 1000000

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

################################################################################

# this is an important parameter. It defines the threshold under which a unit will be dropped.
# if the ratio of unit area/district area and unit/county area is less than this, the unit
# will be dropped because we assume it is a result of spatial data error (spilling over).
sensitivity <- 0.001 

for(i in yrs) {
  con <- cong[[as.character(i)]]
  con <- con[, c("STATENAME", "ID", "DISTRICT", "STARTCONG", "ENDCONG")]
  con$cd_area <- st_area(con$geometry)
  yearlong <- as.numeric(paste0(i, "0101"))
  if(yearlong<20001231) {
    cty <- county[which(county$START_N<=yearlong & county$END_N>=yearlong ), ]
  } else {
    cty <- county[which(county$START_N<=yearlong & county$END_N== 20001231), ]
  }
  
  cty <- cty[, c("ID_NUM", "NAME", "STATE_TERR", "FIPS", "START_N", "END_N")]
  cty$cty_area <- st_area(cty$geometry)

  int <- st_intersection(con, cty)
  int$unit_area <- st_area(int$geometry)
  int$cd_prop <- as.numeric(int$unit_area / int$cd_area)
  int$cty_prop <- as.numeric(int$unit_area / int$cty_area)
  drop <- (int$cd_prop < sensitivity) & (int$cty_prop < sensitivity)
  int <- int[which(int$STATENAME == int$STATE_TERR), ] # drop if county and cd state disagree
  test <- int[drop, ]
  int <- int[!drop, ]
  
  int$year <- i
  if(i==yrs[1]) {
    out <- int
  } else {
    out <- rbind(out, int)
  }
}

pre <- aggregate(out$NAME, by=list(out$year, out$STATENAME, out$DISTRICT), FUN=length) |>
  setNames(c("year", "state", "district", "n_units_pre"))



out.df <- st_drop_geometry(out)
setwd("/Users/christianbaehr/Dropbox/charisma_project/data/")
write.csv(out.df, "working/condistrict_to_county_mapping_withcountynames_1952-2012_fips_added_FULLYGEO.csv", row.names = F)

#################################################################################

rm(list = setdiff(ls(), c("out.df", "pre")))

out.df <- read.csv("working/condistrict_to_county_mapping_withcountynames_1952-2012_fips_added_FULLYGEO.csv",
                   stringsAsFactors = F)


library(readxl)
library(stringi)

###

#county <- read.csv("working/condistrict_to_county_mapping_withcountynames_1952-2012_fips_added.csv", stringsAsFactors = F)

out.df$state <- ifelse(nchar(out.df$FIPS)==4, substr(out.df$FIPS, 1, 1), substr(out.df$FIPS, 1, 2))

names(out.df)
#names(county)
#county$cd <- as.numeric(county$cd)
#county$fips <- as.numeric(county$fips)
out.df <- out.df[, c("NAME", "STATENAME", "DISTRICT", "year", "state", "FIPS", "cty_prop")]
#names(out.df) <- paste0(c("countynm", "statenm", "cd", "year", "state", "fips", "unit_weight"), "_GEO")
names(out.df) <- c("countynm", "statenm", "cd", "year", "state", "fips", "unit_weight")
#out.df$cd_GEO <- as.numeric(out.df$cd_GEO)
county <- out.df
#county$mergevar <- paste(county$cd, county$statenm, county$year, county$cd)
#out.df$mergevar <- paste(out.df$cd_GEO, out.df$statenm_GEO, out.df$year_GEO, out.df$cd_GEO)
#test <- merge(county, out.df, by.x = c("statenm", "year", "cd", "fips"), by.y = c("statenm_GEO", "year_GEO", "cd_GEO", "fips_GEO"))
#test <- merge(county, out.df, by="mergevar")
county$decade <- county$year-2

#pop <- read.csv("working/population_data_countylevel.csv", stringsAsFactors = F)
#county <- county[which(county$decade %in% pop$decade), ]
#county <- merge(county, pop, by.x = c("fips", "decade"), by.y = c("fips", "decade")) # lose ~3000 obs

# dont understand why there are a few CDs coded as "ZZ" in the ICPSR 13 data for 1992-2012. But
# I checked and in each instance, the county coded with CD=="ZZ" had entries for other CDs as 
# well. And the "unit weight" for the other entries always summed up to 1. So these "ZZ" 
# entries are completely redundant
#county <- county[which(county$cd!="ZZ"), ]

county$cd <- as.numeric(county$cd)

# county <- county[, c("countynm", "statenm", "cd", "year", "fips", "unit_pop",
#                      "countypop", "unit_weight")]

county <- county[which(county$year >= 1972), ]

# missing unit weights for 820 cases. All of these cases were coded in congressional district (52, 96, 98, 99)
# these either represent at large elections or district numbers that do not actually exist
# but North Dakota only has one congressional district, so we can assume all weights are 1

###

# only one CD in ND after 1972
# county$cd[which(county$year>=1972 & county$statenm=="North Dakota")] <- 1
# county$unit_weight[which(county$year>=1972 & county$statenm=="North Dakota")] <- 1
# 
# # only one CD in DE
# county$cd[which(county$statenm=="Delaware")] <- 1
# county$unit_weight[which(county$statenm=="Delaware")] <- 1
# 
# # only one CD in WY
# county$cd[which(county$statenm=="Wyoming")] <- 1
# county$unit_weight[which(county$statenm=="Wyoming")] <- 1
# 
# # only one CD in VT
# county$cd[which(county$statenm=="Vermont")] <- 1
# county$unit_weight[which(county$statenm=="Vermont")] <- 1
# 
# # only one CD in SD after 1982
# county$cd[which(county$year>=1982 & county$statenm=="South Dakota")] <- 1
# county$unit_weight[which(county$year>=1982 & county$statenm=="South Dakota")] <- 1
# 
# # only one CD in MT after 1982
# county$cd[which(county$year>=1982 & county$statenm=="Montana")] <- 1
# county$unit_weight[which(county$year>=1982 & county$statenm=="Montana")] <- 1
# 
# # only one CD in NV prior 1982
# county$cd[which(county$year<1982 & county$statenm=="Nevada")] <- 1
# county$unit_weight[which(county$year<1982 & county$statenm=="Nevada")] <- 1

# drop the remaining at-large cases with no unit weight. These won't be factored into the final data anyway.
# There are only 10 cases and the only ones that involve urban areas are three counties near Norfolk VA.
# The others represent random counties across the country
county <- county[!is.na(county$unit_weight), ]

###

# merging on state and county names. Change to lowercase and remove punctuation
county$statenm <- tolower(county$statenm)
county$countynm <- tolower(county$countynm)
county$countynm <- gsub("\\.", "", county$countynm)

county$decade <- county$year

# tweaking county names to match with names from the congress voting data
# county$countynm[which(county$statenm=="alabama" & county$countynm=="calhoun/benton")] <- "calhoun"
# county$countynm[which(county$statenm=="alabama" & county$countynm=="chilton/baker")] <- "chilton"
# county$countynm[which(county$statenm=="alabama" & county$countynm=="de kalb")] <- "dekalb"
# county$countynm[which(county$statenm=="alabama" & county$countynm=="morgan/cotaco")] <- "morgan"
# county$countynm[which(county$statenm=="florida" & county$countynm=="bradford/new rive")] <- "bradford"
# county$countynm[which(county$statenm=="florida" & county$countynm=="dade")] <- "miami-dade"
# county$countynm[which(county$statenm=="florida" & county$countynm=="de soto")] <- "desoto"
# county$countynm[which(county$statenm=="florida" & county$countynm=="hernando/benton")] <- "hernando"
# county$countynm[which(county$statenm=="georgia" & county$countynm=="bartow/cass")] <- "bartow"
# county$countynm[which(county$statenm=="georgia" & county$countynm=="de kalb")] <- "dekalb"
# county$countynm[which(county$statenm=="illinois" & county$countynm=="de kalb")] <- "dekalb"
# county$countynm[which(county$statenm=="indiana" & county$countynm=="de kalb")] <- "dekalb"
# county$countynm[which(county$statenm=="iowa" & county$countynm=="lyon/buncombe")] <- "lyon"
# county$countynm[which(county$statenm=="iowa" & county$countynm=="o brien")] <- "o'brien"
# county$countynm[which(county$statenm=="iowa" & county$countynm=="obrien")] <- "o'brien"
# county$countynm[which(county$statenm=="kansas" & county$countynm=="neosho/dorn")] <- "neosho"
# county$countynm[which(county$statenm=="kentucky" & county$countynm=="mcclean")] <- "mclean"
# county$countynm[which(county$statenm=="louisiana" & county$countynm=="st john the bapti")] <- "st john the baptist"
# county$countynm[which(county$statenm=="louisiana" & county$countynm=="vermillion")] <- "vermilion"
# county$countynm[which(county$statenm=="michigan" & county$countynm=="mackinac/michilim")] <- "mackinac"
# county$countynm[which(county$statenm=="missouri" & county$countynm=="de kalb")] <- "dekalb"
# county$countynm[which(county$statenm=="nevada" & county$countynm=="carson")] <- "carson city"
# county$countynm[which(county$statenm=="tennessee" & county$countynm=="de kalb")] <- "dekalb"
# county$countynm[which(county$statenm=="texas" & county$countynm=="cass/davis")] <- "cass"
# county$countynm[which(county$statenm=="texas" & county$countynm=="stephens/buchanan")] <- "stephens"
# county$countynm[which(county$statenm=="virginia" & county$countynm=="alexandria city")] <- "alexandria"
# county$countynm[which(county$statenm=="virginia" & county$countynm=="arlington/alexand")] <- "arlington"
# county$countynm[which(county$statenm=="virginia" & county$countynm=="norfolk city")] <- "norfolk"
# county$countynm[which(county$statenm=="washington" & county$countynm=="grays harbor/cheh")] <- "grays harbor"
# county$countynm[which(county$statenm=="wisconsin" & county$countynm=="bayfield/la point")] <- "bayfield"
# 
# county$fips[which(county$countynm=="miami-dade")] <- 12086
# county$fips[which(county$countynm=="dekalb" & county$statenm=="georgia")] <- 13089
# county$fips[which(county$countynm=="st clair" & county$statenm=="illinois")] <- 17163
# county$fips[which(county$countynm=="mackinac" & county$statenm=="michigan")] <- 26097
# county$fips[which(county$countynm=="st louis city" & county$decade<1992)] <- 29510

################################################################################

congress <- lapply(paste0("original/elections/cq_congress_district/", list.files("original/elections/cq_congress_district")), function(x) read.csv(x, stringsAsFactors = F, skip=2, fileEncoding = "cp1252"))
congress <- do.call(rbind, congress)

congress <- congress[which(congress$Office=="House"), ]

congress$statenm <- tolower(congress$State)

congress$cd <- gsub("District ", "", congress$Area)

congress$raceYear <- as.numeric(congress$raceYear)

# for some reason the CQ data only includes one observation for michigan in 2010. So I omit the election entirely
congress <- congress[which(!(congress$statenm=="michigan" & congress$raceYear==2010)), ]

# change district # for single district states to 1
congress$cd[which(congress$raceYear>=1972 & congress$statenm=="north dakota")] <- 1
congress$cd[which(congress$statenm=="delaware")] <- 1
congress$cd[which(congress$statenm=="wyoming")] <- 1
congress$cd[which(congress$statenm=="vermont")] <- 1
congress$cd[which(congress$raceYear>=1982 & congress$statenm=="south dakota")] <- 1
congress$cd[which(congress$raceYear>=1992 & congress$statenm=="montana")] <- 1
congress$cd[which(congress$raceYear<1982 & congress$statenm=="nevada")] <- 1

congress$countynm <- tolower(congress$Area)
congress$countynm <- gsub("\\.", "", congress$countynm)
congress$countynm <- trimws(congress$countynm)

dfloor <- function(x) {
  return(((x-2) %/% 10) *10 + 2)
}
congress$decade <- dfloor(congress$raceYear)

# one weird special election in which the candidate died and his wife ran in the special election to replace
# him, but they still coded the special election with his name
congress <- congress[which(!(congress$RepCandidate=="Letlow, Luke J." & congress$DemStatus=="N/A")),]

# dropping these duplicated observations which include primary elections AND runoffs (primary elections indicated when there is a 
#non-missing value for OtherVotes or ThirdVotes)
drop <- (duplicated(congress[, c("statenm", "cd", "raceYear")])|duplicated(congress[, c("statenm", "cd", "raceYear")], fromLast=T)) & 
  (congress$OtherVotes!="N/A" & congress$ThirdVotes!="N/A")
congress <- congress[!drop, ]

congress <- congress[, c("statenm", "cd", "decade", "raceYear", "RepVotes", "DemVotes", "ThirdVotes", "OtherVotes",
                         "PluralityVotes", "RepVotesMajorPercent", "DemVotesMajorPercent", "ThirdVotesTotalPercent",
                         "RepCandidate", "RepStatus", "DemCandidate", "DemStatus", "ThirdCandidate", "ThirdStatus")]

names(congress) <- c("statenm", "cd", "decade",
                     paste0("con_", c("raceYear", "RepVotes", "DemVotes", "ThirdVotes", "OtherVotes",
                                      "PluralityVotes", "RepVotesMajorPercent", "DemVotesMajorPercent", "ThirdVotesTotalPercent",
                                      "RepCandidate", "RepStatus", "DemCandidate", "DemStatus", "ThirdCandidate", "ThirdStatus")))

congress$con_RepUnopposed <- congress$con_RepVotes=="Unopposed"
congress$con_DemUnopposed <- congress$con_DemVotes=="Unopposed"

congress <- congress[congress$con_raceYear>=1972, ]

# district 13 in California and district 3 in kentucky have no districts. Otherwise all have matches.
countyplus <- merge(county, congress, by = c("statenm", "cd", "decade"))

################################################################################

load("original/elections/Algara_Amlani_presidential_county_returns_1868_2020.Rdata")
pres <- pres_elections_release


countyplus$fips <- ifelse(nchar(countyplus$fips)==5, countyplus$fips, paste0("0", countyplus$fips))
sum(countyplus$fips %in% pres_elections_release$fips)

###

# files <- paste0("original/elections/cq_president_county/", grep("president", list.files("original/elections/cq_president_county"), value=T))
# pres <- lapply(files, function(x) read.csv(x, stringsAsFactors = F, skip=2, fileEncoding = "cp1252"))
# pres <- do.call(rbind, pres)
# 
# pres <- pres[which(pres$Office=="President"),]
# 
# pres$statenm <- tolower(pres$State)
# pres$countynm <- tolower(pres$Area)
# pres$countynm <- gsub("\\.", "", pres$countynm)
# pres$countynm <- trimws(pres$countynm)
# 
# pres$RaceDate <- as.numeric(pres$RaceDate)
# pres$RaceDate <- as.numeric(substr(pres$RaceDate, 1, 4))
# 
# ###
# 
# pres <- pres[which(!(pres$statenm=="alaska" & pres$RaceDate==2020 & pres$countynm=="election district 1" & pres$TotalVotes==0)), ]
# 
# drop <- (pres$statenm=="virginia" & pres$countynm=="norfolk" & pres$RaceDate==1960 & pres$TotalVotes=="8,936")
# keep <- which(pres$statenm=="virginia" & pres$countynm=="norfolk" & pres$RaceDate==1960 & pres$TotalVotes!="8,936")
# 
# for(i in c("TotalVotes", "RepVotes", "DemVotes", "OtherVotes", "PluralityVotes")) {
#   pres[keep, i] <- gsub(",", "", pres[keep, i])
#   pres[which(drop), i] <- gsub(",", "", pres[which(drop), i])
#   pres[keep, i] <- pres[which(drop), i]
# }
# 
# pres <- pres[which(!drop), ]
# pres$TotalVotes[keep] <- gsub(",", "", pres$TotalVotes[keep])
# 
# ###
# 
# pres <- pres[which(pres$RaceDate>=1972), ]
# 
# pres$countynm[which(pres$statenm=="virginia" & pres$countynm=="norfolk city" & pres$RaceDate>=2016)] <- "norfolk"
# 
# # no match for Hawaii Kalawao or Maui or Nevada Ormsby
# # armstrong county in South Dakota is defunct, has no match
# # la paz county in Arizona was created in 1983, not present in county data until 1992. Missing matches for 1984, 1988.
# # milton county georgia does not appear to exist anymore
# # no match for assumption parish Louisiana 1982
# # presidential voting data considers Kansas City to be its own county after 2000. However Kansas city is actually a part of several counties
# # there are also several counties in Virginia that have no matches in a few years. No record of Manassas/Manassas Park prior to 1982

pres$OtherVotes <- pres$raw_county_vote_totals - (pres$republican_raw_votes + pres$democratic_raw_votes)
pres$PluralityVotes <- NA
pres$RepVotesTotalPercent <- pres$republican_raw_votes / pres$raw_county_vote_totals
pres$DemVotesTotalPercent <- pres$democratic_raw_votes / pres$raw_county_vote_totals
pres$OtherVotesTotalPercent <- pres$OtherVotes / pres$raw_county_vote_totals
pres$RepVotesMajorPercent <- pres$republican_raw_votes / (pres$republican_raw_votes + pres$democratic_raw_votes)
pres$DemVotesMajorPercent <- pres$democratic_raw_votes / (pres$republican_raw_votes + pres$democratic_raw_votes)
pres$PluralityParty <- ifelse(pres$democratic_raw_votes > pres$republican_raw_votes, "Democrat", "Republican")
pres$DemStatus <- NA
pres$RepStatus <- NA
pres <- pres[, c("state", "county_name", "fips", "election_year",
                 "raw_county_vote_totals", "republican_raw_votes", "democratic_raw_votes", "OtherVotes", "PluralityVotes", "RepVotesTotalPercent",
                 "DemVotesTotalPercent", "OtherVotesTotalPercent", "RepVotesMajorPercent", "DemVotesMajorPercent",
                 "PluralityParty", "dem_nominee", "RepStatus", "rep_nominee", "DemStatus")]

names(pres) <- c("statenm", "countynm", "fips",
                 paste0("pres_", c("RaceDate",
                                   "TotalVotes", "RepVotes", "DemVotes", "OtherVotes", "PluralityVotes", "RepVotesTotalPercent",
                                   "DemVotesTotalPercent", "OtherVotesTotalPercent", "RepVotesMajorPercent", "DemVotesMajorPercent",
                                   "PluralityParty", "RepCandidate", "RepStatus", "DemCandidate", "DemStatus")))


pres <- pres[which(pres$pres_RaceDate>=1972), ]
pres <- pres[, !names(pres) %in% c("statenm", "countynm")]

#test <- countyplus[which(countyplus$con_raceYear %in% seq(1972, 2020, 4)), ]
#matcha <- paste(test$fips, test$con_raceYear)
#matchb <- paste(pres$fips, pres$pres_RaceDate)

#sum(matcha %in% matchb)
#sum(matchb %in% matcha)

#pres$statenm <- tolower(state.name[match(pres$statenm, state.abb)])

# countyplus <- merge(countyplus, pres, by.x = c("con_raceYear", "fips", "statenm", "countynm"), by.y = c("pres_RaceDate", "fips", "statenm", "countynm"),
#                     all.x = T)
countyplus <- merge(countyplus, pres, by.x = c("con_raceYear", "fips"), by.y = c("pres_RaceDate", "fips"),
                    all.x = T)

#View(data.frame(matcha[!matcha %in% matchb]))
#View(data.frame(matchb[!matchb %in% matcha]))

# one county gets duplicated after this merge. Omit the duplicated copy.
# countyplus <- countyplus[which(!(countyplus$statenm=="virginia" & 
#                                    countyplus$countynm=="norfolk" & 
#                                    countyplus$con_raceYear==2020 & 
#                                    countyplus$pres_TotalVotes=="0")), ]


################################################################################

senate <- lapply(paste0("original/elections/cq_senate_county/", list.files("original/elections/cq_senate_county")), function(x) read.csv(x, stringsAsFactors = F, skip=2, fileEncoding = "cp1252"))
senate <- do.call(rbind, senate)
senate <- senate[which(senate$Office=="Senate"), ]

senate$statenm <- tolower(senate$State)
senate$countynm <- tolower(senate$Area)
senate$countynm <- gsub("\\.", "", senate$countynm)
senate$countynm <- trimws(senate$countynm)

senate$RaceDate <- as.numeric(senate$RaceDate)
senate$RaceDate <- as.numeric(substr(senate$RaceDate, 1, 4))

senate$RepUnopposed <- senate$RepVotes=="Unopposed"
senate$DemUnopposed <- senate$DemVotes=="Unopposed"

drop <- (duplicated(senate[, c("statenm", "countynm", "RaceDate")]) | duplicated(senate[, c("statenm", "countynm", "RaceDate")], fromLast = T)) & 
  (senate$RepVotes=="N/A" & senate$DemVotes=="N/A") & (senate$statenm %in% c("georgia", "alaska"))
senate <- senate[which(!drop), ]

###

# there are several counties (~40) in the senate data that have no match in the countyplus
# dataset for a given year of 

senate <- senate[which(!(senate$statenm=="georgia" & senate$RaceDate==2020)),]
senate$RaceDate[which(senate$statenm=="georgia" & senate$RaceDate==2021)] <- 2020

senate$countynm[which(senate$statenm=="virginia" & senate$countynm=="norfolk city" & senate$RaceDate>=2012)] <- "norfolk"

# couple observations in virginia repeated for 2016 and 2020 election
senate <- senate[which(!(senate$RepVotes=="N/A" & senate$statenm=="virginia" & senate$countynm=="norfolk" & 
                           (senate$RepCandidate %in% c("Stewart, Corey A.", "Gade, Daniel MacArthur") ))),  ]

###

senate <- senate[which(senate$RaceDate>=1972), ]

senate$RepVotes <- as.numeric(gsub(",", "", senate$RepVotes))
senate$DemVotes <- as.numeric(gsub(",", "", senate$DemVotes))
senate$OtherVotes <- as.numeric(gsub(",", "", senate$OtherVotes))
senate$ThirdVotes <- as.numeric(gsub(",", "", senate$ThirdVotes))

idx <- which(duplicated(senate[, c("statenm", "countynm", "RaceDate")])|duplicated(senate[, c("statenm", "countynm", "RaceDate")], fromLast = T))
for(i in 1:nrow(senate)) {
  # correcting a weird quirk from Louisiana 2014 where Dem and Rep returns for the same election
  # are on different rows
  if(i==1) {drop <- c()}
  
  if(i %in% idx) {
    
    if(is.na(senate[i, "RepVotes"])) {
      idx2 <- which(senate$statenm==senate$statenm[i] & senate$countynm==senate$countynm[i] & senate$RaceDate==senate$RaceDate[i])
      idx2 <- idx2[idx2!=i]
      senate[i, c("RepVotes", "RepCandidate", "RepStatus")] <- senate[idx2, c("RepVotes", "RepCandidate", "RepStatus")]
      senate$ThirdVotesTotalPercent[i] <- senate$ThirdVotes[i]/sum(senate[i, c("RepVotes", "DemVotes", "ThirdVotes", "OtherVotes")])
      senate$RepVotesMajorPercent[i] <- senate$RepVotes[i]/sum(senate[i, c("RepVotes", "DemVotes")])
      senate$DemVotesMajorPercent[i] <- senate$DemVotes[i]/sum(senate[i, c("RepVotes", "DemVotes")])
      drop <- c(drop, F)
      
    } else if(is.na(senate[i, "DemVotes"])) {
      drop <- c(drop, T)
    }
    
  } else {
    drop <- c(drop, F)
  }
  
}

senate <- senate[!drop, ]

###

seats <- function(x) {
  years <- sort(unique(x))
  seat1 <- seq(years[1], years[length(years)] , 6)
  seat2 <- seq(years[2], years[length(years)] , 6)
  return(list(seat1=seat1, seat2=seat2))
}

seatyears <- tapply(senate$RaceDate, INDEX=list(senate$statenm), FUN=seats)

# also omitting a Georgia special election from 2020
for(i in 1:length(seatyears)) {
  if(i==1) {seatyears1 <- list(); seatyears2 <- list()}
  seatyears1[[i]] <- data.frame(state=names(seatyears)[i], year=seatyears[[i]]$seat1)
  seatyears2[[i]] <- data.frame(state=names(seatyears)[i], year=seatyears[[i]]$seat2)
}
seatyears1 <- do.call(rbind, seatyears1)
seatyears2 <- do.call(rbind, seatyears2)

senate_seat1 <- merge(seatyears1, senate, by.x=c("state", "year"), by.y=c("statenm", "RaceDate"))
senate_seat2 <- merge(seatyears2, senate, by.x=c("state", "year"), by.y=c("statenm", "RaceDate"))
# 265 missing cases, which accounts for the Illinois special election in 1970, Georgia special
# election in 2021, and Hawaii special election in 2014

senate_seat1 <- senate_seat1[, c("state", "countynm", "year", "RepVotes", "DemVotes", "ThirdVotes", "OtherVotes", 
                                 "PluralityVotes", "RepVotesMajorPercent", "DemVotesMajorPercent", "ThirdVotesTotalPercent",
                                 "RepCandidate", "RepStatus", "DemCandidate", "DemStatus", "PluralityParty", "ThirdParty", 
                                 "ThirdCandidate", "ThirdStatus", "RepUnopposed", "DemUnopposed")]

names(senate_seat1) <- c("statenm", "countynm",
                         paste0("sen1_", c("RaceDate", "RepVotes", "DemVotes", "ThirdVotes", "OtherVotes", "PluralityVotes", 
                                           "RepVotesMajorPercent", "DemVotesMajorPercent", "ThirdVotesTotalPercent",
                                           "RepCandidate", "RepStatus", "DemCandidate", "DemStatus", "PluralityParty", 
                                           "ThirdParty", "ThirdCandidate", "ThirdStatus", "RepUnopposed", "DemUnopposed")))

senate_seat2 <- senate_seat2[, c("state", "countynm", "year", "RepVotes", "DemVotes", "ThirdVotes", "OtherVotes", 
                                 "PluralityVotes", "RepVotesMajorPercent", "DemVotesMajorPercent", "ThirdVotesTotalPercent",
                                 "RepCandidate", "RepStatus", "DemCandidate", "DemStatus", "PluralityParty", "ThirdParty", 
                                 "ThirdCandidate", "ThirdStatus", "RepUnopposed", "DemUnopposed")]

names(senate_seat2) <- c("statenm", "countynm",
                         paste0("sen2_", c("RaceDate", "RepVotes", "DemVotes", "ThirdVotes", "OtherVotes", "PluralityVotes", 
                                           "RepVotesMajorPercent", "DemVotesMajorPercent", "ThirdVotesTotalPercent",
                                           "RepCandidate", "RepStatus", "DemCandidate", "DemStatus", "PluralityParty", 
                                           "ThirdParty", "ThirdCandidate", "ThirdStatus", "RepUnopposed", "DemUnopposed")))

countyplus <- merge(countyplus, senate_seat1, by.x=c("statenm", "countynm", "con_raceYear"),
                    by.y=c("statenm", "countynm", "sen1_RaceDate"), all.x=T) # need to pull forward the senate results from prior year
countyplus <- merge(countyplus, senate_seat2, by.x=c("statenm", "countynm", "con_raceYear"),
                    by.y=c("statenm", "countynm", "sen2_RaceDate"), all.x=T) # need to pull forward the senate results from prior year

# missing a few cases still from the same counties that were missing in presidential merge

###

governor <- lapply(paste0("original/elections/cq_governor_county//", list.files("original/elections/cq_governor_county/")), function(x) read.csv(x, stringsAsFactors = F, skip=2, fileEncoding = "cp1252"))
governor <- do.call(rbind, governor)

# some cases were erroneously coded as Texas elections when they were actually in Nevada
governor <- governor[which(!(governor$RepCandidate=="List, Robert F." & governor$DemCandidate=="Bryan, Richard H." & governor$State=="Texas")), ]

governor <- governor[which(governor$Office=="Governor"), ]

governor$statenm <- tolower(governor$State)
governor$countynm <- tolower(governor$Area)
governor$countynm <- gsub("\\.", "", governor$countynm)
governor$countynm <- trimws(governor$countynm)

governor$RaceDate <- as.numeric(governor$RaceDate)
governor$RaceDate <- as.numeric(substr(governor$RaceDate, 1, 4))

# this removes observations for primary elections preceding runoffs in Louisiana 
governor <- governor[!(governor$State=="Louisiana" & governor$RepCandidate=="N/A" & governor$DemCandidate=="N/A"), ]

governor <- governor[which(governor$RaceDate>=1972), ]

governor$countynm[which(governor$statenm=="virginia" & governor$countynm=="norfolk city" & governor$RaceDate>=2010)] <- "norfolk"

# if the gubernatorial election occurred on a non-congressional race year (e.g. an odd year), 
# I move the governor race forward one year, so it corresponds to the congressional race in the
# year FOLLOWING the governor race
governor$RaceDate <- ifelse(governor$RaceDate %%2 ==0, governor$RaceDate, governor$RaceDate+1)

# there are a bunch of erroneous elections from Nevada 1982 election where the candidates
# correspond to counties from Texas. These are omitted.

# because we are omitting Michigan 2010, all the gubernatorial rows corresponding to that election year
# for michigan are omitted

governor <- governor[, c("statenm", "countynm", "RaceDate", "RepVotes",
                         "DemVotes", "ThirdVotes", "OtherVotes", "PluralityVotes", "RepVotesMajorPercent", "DemVotesMajorPercent", "ThirdVotesTotalPercent", 
                         "RepCandidate", "RepStatus", "DemCandidate", "DemStatus", "PluralityParty", "ThirdParty", "ThirdCandidate", "ThirdStatus")]

names(governor) <- c("statenm", "countynm",
                     paste0("gov_", c("RaceDate", "RepVotes",
                                      "DemVotes", "ThirdVotes", "OtherVotes", "PluralityVotes", "RepVotesMajorPercent", "DemVotesMajorPercent", "ThirdVotesTotalPercent", 
                                      "RepCandidate", "RepStatus", "DemCandidate", "DemStatus", "PluralityParty", "ThirdParty", "ThirdCandidate", "ThirdStatus")))

countyplus <- merge(countyplus, governor, by.x=c("statenm", "countynm", "con_raceYear"), by.y=c("statenm", "countynm", "gov_RaceDate"), all.x=T)

countyplus$fips <- ifelse(nchar(countyplus$fips)==4, paste0("0", countyplus$fips), countyplus$fips)

###

census <- read.csv("working/population_data_countylevel.csv", stringsAsFactors = F)
census$decade <- census$decade + 2
census$fips <- as.character(census$fips)
census$fips[nchar(census$fips)==4] <- paste0("0", census$fips[nchar(census$fips)==4])

census$fips[which(census$fips==12025)] <- 12086 # miami dade FIPS code changed
census$fips[which(census$fips==12027 & census$decade<1992)] <- 12053 # desoto florida FIPS changed from 12053 to 12027
census$fips[which(census$fips==29193 & census$decade<1982)] <- 29186 # desoto florida FIPS changed from 12053 to 12027

census <- census[which(substr(census$fips,1,2)!="72"), ] # omitting puerto rico becuase its not in the voting data

countyplus <- merge(countyplus, census, by.x = c("fips", "decade"), by.y = c("fips", "decade"), all.x=T)
# either no idaho or no hawaii (11)
# no honolulu (fips code 15003, 15005)
# no assumption LA
# fips 29193 no match
# drop yellowstone national park county which was dissolved
# 18 total non matches

###

for(i in 1:nrow(countyplus)) {
  
  year <- countyplus$con_raceYear[i]
  county <- countyplus$countynm[i]
  cd <- countyplus$cd[i]
  state <- countyplus$statenm[i]
  
  cty_ind <- countyplus$statenm==state & countyplus$countynm==county
  
  if(is.na(countyplus$gov_RepVotesMajorPercent[i])) { # check if the major percent vote is missing - implies a governor midterm year
    # pull all observations for county i in state j in years before current, and for which percent vote is nonmissing
    cands <- countyplus[which(cty_ind & countyplus$con_raceYear<year & !is.na(countyplus$gov_RepVotesMajorPercent)), ]
    fit <- cands[which.max(cands$con_raceYear),] # identify the observation for the most recent year from the candidates. This is the lagged election we want
    # replace all missing governor variables with the data from the most previous election
    countyplus[i, grep("gov_", names(countyplus))] <- fit[1, grep("gov_", names(countyplus))] 
  }
  if(is.na(countyplus$pres_RepVotesTotalPercent[i])) { # do now for president
    cands <- countyplus[which(cty_ind & countyplus$con_raceYear<year & !is.na(countyplus$pres_RepVotesTotalPercent)), ]
    fit <- cands[which.max(cands$con_raceYear),]
    countyplus[i, grep("pres_", names(countyplus))] <- fit[1, grep("pres_", names(countyplus))]
  }
  if(is.na(countyplus$sen1_RepVotesMajorPercent[i])) { # do now for senator 1
    cands <- countyplus[which(cty_ind & countyplus$con_raceYear<year & !is.na(countyplus$sen1_RepVotesMajorPercent)), ]
    fit <- cands[which.max(cands$con_raceYear),]
    countyplus[i, grep("sen1_", names(countyplus))] <- fit[1, grep("sen1_", names(countyplus))]
  }
  if(is.na(countyplus$sen2_RepVotesMajorPercent[i])) { # do now for senator 2
    cands <- countyplus[which(cty_ind & countyplus$con_raceYear<year & !is.na(countyplus$sen2_RepVotesMajorPercent)), ]
    fit <- cands[which.max(cands$con_raceYear),]
    countyplus[i, grep("sen2_", names(countyplus))] <- fit[1, grep("sen2_", names(countyplus))]
  }
}

###

names(countyplus) <- tolower(names(countyplus))

countyplus <- countyplus[, !names(countyplus) %in% c("x", "year")]

# seats are also unopposed if both the candidate and status variables are NA for the other party
countyplus$con_demunopposed[which(countyplus$con_repcandidate=="N/A" & countyplus$con_repstatus=="N/A")] <- T
countyplus$con_repunopposed[which(countyplus$con_demcandidate=="N/A" & countyplus$con_demstatus=="N/A")] <- T

###

sum(is.na(countyplus$unit_pop))

vars <- c("pres_totalvotes", "pres_repvotes", "pres_demvotes", "pres_othervotes",
          "sen1_repvotes", "sen1_demvotes", "sen1_thirdvotes", "sen1_othervotes",
          "sen2_repvotes", "sen2_demvotes", "sen2_thirdvotes", "sen2_othervotes",
          "gov_repvotes", "gov_demvotes", "gov_thirdvotes", "gov_othervotes",
          "pop_total", "pop_male", "pop_over65", "pop_white", "pop_black", "pop_spanishorigin")

# if("pop_total.y" %in% names(countyplus)) {
#   countyplus <- countyplus[, !names(countyplus) %in% grep("\\.y", names(countyplus), value=T)]
#   nm <- names(countyplus)[grepl("\\.x", names(countyplus))]
#   nm <- gsub("\\.x", "", nm)
#   names(countyplus)[grepl("\\.x", names(countyplus))] <- nm
# }

for(i in vars) {
  
  countyplus[which(countyplus[,i]=="N/A"), i] <- 0
  countyplus[,i] <- gsub(",", "", countyplus[,i])
  
  countyplus[,i] <- as.numeric(countyplus[,i])
  
  countyplus[,i] <- countyplus[,i] * countyplus$unit_weight
  #countyplus[,i] <- countyplus[,i] * countyplus$cty_prop
}

adminvars <- c("statenm", "cd", "con_raceyear", "con_repcandidate", "con_demcandidate", "con_thirdcandidate", "con_repstatus", "con_demstatus", "con_thirdstatus",
               "con_repunopposed", "con_demunopposed", "con_repvotes", "con_demvotes", "con_thirdvotes", "con_othervotes", "con_pluralityvotes"
               #"pres_repcandidate", "pres_demcandidate", "pres_repstatus", "pres_demstatus",
               #"sen1_repcandidate", "sen1_demcandidate", "sen1_repstatus", "sen1_demstatus",
               #"sen2_repcandidate", "sen2_demcandidate", "sen2_repstatus", "sen2_demstatus",
               #"gov_repcandidate", "gov_demcandidate", "gov_repstatus", "gov_demstatus"
)

adminvarlist <- as.list(countyplus[, adminvars])

###

post <- aggregate(countyplus$pres_repvotes, by=list(countyplus$decade, countyplus$statenm, countyplus$cd), FUN=function(x) sum(!is.na(x))) |>
  setNames(c("year", "state", "district", "n_units_post"))

pre$state <- tolower(pre$state)

test <- merge(pre, post) |>
  setNames(c("decade", "statenm", "cd", "n_units_pre", "n_units_post"))
test$n_units_pre <- test$n_units_pre*5

test$units_missing <- test$n_units_pre - test$n_units_post
test$any_missing <- test$units_missing>0
test$pct_missing <- test$units_missing / test$n_units_pre

countyplus <- merge(countyplus, test)

###
vars <- c(vars, "any_missing")
cddata <- aggregate(countyplus[, vars], 
                    by=adminvarlist, 
                    FUN=function(x) sum(x, na.rm=T))

votevars <- c("con_repvotes", "con_demvotes", "con_thirdvotes", "con_othervotes", "con_pluralityvotes") 
for(i in votevars) {
  
  cddata[which(cddata[,i]=="N/A"), i] <- 0
  cddata[,i] <- gsub(",", "", cddata[,i])
  
  cddata[,i] <- as.numeric(cddata[,i])
}

###

# test <- cddata2[which( (cddata2$con_othervotes > cddata2$con_thirdvotes) & cddata2$con_thirdvotes!=0 )[1:49], 
#                 c("con_raceyear", "statenm", "cd", "con_repvotes", "con_demvotes", "con_thirdvotes", "con_othervotes", "con_thirdcandidate")]
# View(test[order(test$statenm, test$cd, test$con_raceyear),]) # rest of this data is all good

ind <- which(cddata$statenm=="wisconsin" & cddata$cd==4 & cddata$con_raceyear==2004)
cddata$con_thirdvotes[ind] <- 3733
cddata$con_thirdvotes[ind] <- 1861+897+341

ind <- which(cddata$statenm=="louisiana" & cddata$cd==5 & cddata$con_raceyear==2020)
cddata$con_repvotes[ind] <- 49182
cddata$con_demvotes[ind] <- 0
cddata$con_thirdvotes[ind] <- 30124
cddata$con_othervotes[ind] <- 0

###

## incorporate Dahyun's handcoded democratic incumbency measure

incumb <- read.csv("original/handcoded/congress_demterm_hand_corrected_bin_DONOTEDIT.csv", stringsAsFactors = F)
incumb$dem_incumb_dum <- ( incumb$con_dem_inc_count >0 ) * 1

incumb <- incumb[, c("cd", "statenm", "con_raceyear", "dem_incumb_dum")]

cddata <- merge(cddata, incumb, by.x=c("statenm", "cd", "con_raceyear"), by.y=c("statenm", "cd", "con_raceyear"), all.x=T)

###

cddata$con_totalvotes <- cddata$con_repvotes + cddata$con_demvotes + cddata$con_thirdvotes + cddata$con_othervotes

cddata$con_repshare <- cddata$con_repvotes / cddata$con_totalvotes
cddata$con_repshare[which(cddata$con_repunopposed)] <- 1
cddata$con_repshare[which(cddata$con_demunopposed)] <- 0

cddata$con_demshare <- cddata$con_demvotes / cddata$con_totalvotes
cddata$con_demshare[which(cddata$con_demunopposed)] <- 1
cddata$con_demshare[which(cddata$con_repunopposed)] <- 0

cddata$pres_totalvotes <- cddata$pres_repvotes + cddata$pres_demvotes + cddata$pres_othervotes

cddata$pres_repshare <- cddata$pres_repvotes / cddata$pres_totalvotes
cddata$pres_demshare <- cddata$pres_demvotes / cddata$pres_totalvotes

cddata$sen1_totalvotes <- cddata$sen1_demvotes + cddata$sen1_repvotes + cddata$sen1_thirdvotes + cddata$sen1_othervotes

cddata$sen1_repshare <- cddata$sen1_repvotes / cddata$sen1_totalvotes
cddata$sen1_demshare <- cddata$sen1_demvotes / cddata$sen1_totalvotes

cddata$sen2_totalvotes <- cddata$sen2_demvotes + cddata$sen2_repvotes + cddata$sen2_thirdvotes + cddata$sen2_othervotes

cddata$sen2_repshare <- cddata$sen2_repvotes / cddata$sen2_totalvotes
cddata$sen2_demshare <- cddata$sen2_demvotes / cddata$sen2_totalvotes

cddata$gov_totalvotes <- cddata$gov_repvotes + cddata$gov_demvotes + cddata$gov_thirdvotes + cddata$gov_othervotes

cddata$gov_repshare <- cddata$gov_repvotes / cddata$gov_totalvotes
cddata$gov_demshare <- cddata$gov_demvotes / cddata$gov_totalvotes

popvars <- c("pop_male", "pop_over65", "pop_white", "pop_black", "pop_spanishorigin")
for(i in popvars ) {
  cddata[, paste0(i, "_pct")] <- cddata[, i] / cddata$pop_total
}

cddata[, c("con_repcandidate", "con_demcandidate", "con_thirdcandidate")] <- apply(cddata[, c("con_repcandidate", "con_demcandidate", "con_thirdcandidate")], 
                                                                                   2, 
                                                                                   FUN=function(x) stri_trans_general(x, "Latin-ASCII")) # remove accents from candidate names

# dropping 19 cases in which incumbent and opponent coded as incumbent
cddata <- cddata[which(!(cddata$con_repstatus=="Incumbent" & cddata$con_demstatus=="Incumbent")), ]

write.csv(cddata, "working/condistrict_panel_full_FULLGEO.csv", row.names=F)







