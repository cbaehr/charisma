
setwd("/Users/christianbaehr/Dropbox/charisma_project/data/")

library(readxl)

###

county <- read.csv("condistrict_to_county_mapping_withcountynames_1952-2012_fips_added_nomissingness.csv", stringsAsFactors = F)

temp <- read.csv("condistrict_to_county_mapping_withcountynames_1952-2012_fips_added.csv", stringsAsFactors = F)

county$unit_weight <- temp$unit_weight # missing geographic weights for 820 cases
county$unit_pop <- temp$unit_pop # missing unit populations for 1952-82. Will use the geographic weights 
county$county_pop <- temp$countypop

# missing unit weights for 820 cases. All of these cases were coded in congressional district (52, 96, 98, 99)
# these either represent at large elections or district numbers that do not actually exist
# but North Dakota only has one congressional district, so we can assume all weights are 1

county$unit_weight[which(county$cd==52 & county$statenm=="North Dakota")] <- 1

county$statenm <- tolower(county$statenm)
county$countynm <- tolower(county$countynm)
county$countynm <- gsub("\\.", "", county$countynm)

county$decade <- county$year

county$countynm[which(county$statenm=="alabama" & county$countynm=="calhoun/benton")] <- "calhoun"
county$countynm[which(county$statenm=="alabama" & county$countynm=="chilton/baker")] <- "chilton"
county$countynm[which(county$statenm=="alabama" & county$countynm=="de kalb")] <- "dekalb"
county$countynm[which(county$statenm=="alabama" & county$countynm=="morgan/cotaco")] <- "morgan"
county$countynm[which(county$statenm=="florida" & county$countynm=="bradford/new rive")] <- "bradford"
county$countynm[which(county$statenm=="florida" & county$countynm=="dade")] <- "miami-dade"
county$countynm[which(county$statenm=="florida" & county$countynm=="de soto")] <- "desoto"
county$countynm[which(county$statenm=="florida" & county$countynm=="hernando/benton")] <- "hernando"
county$countynm[which(county$statenm=="georgia" & county$countynm=="bartow/cass")] <- "bartow"
county$countynm[which(county$statenm=="georgia" & county$countynm=="de kalb")] <- "dekalb"
county$countynm[which(county$statenm=="illinois" & county$countynm=="de kalb")] <- "dekalb"
county$countynm[which(county$statenm=="indiana" & county$countynm=="de kalb")] <- "dekalb"
county$countynm[which(county$statenm=="iowa" & county$countynm=="lyon/buncombe")] <- "lyon"
county$countynm[which(county$statenm=="iowa" & county$countynm=="o brien")] <- "o'brien"
county$countynm[which(county$statenm=="iowa" & county$countynm=="obrien")] <- "o'brien"
county$countynm[which(county$statenm=="kansas" & county$countynm=="neosho/dorn")] <- "neosho"
county$countynm[which(county$statenm=="kentucky" & county$countynm=="mcclean")] <- "mclean"
county$countynm[which(county$statenm=="louisiana" & county$countynm=="st john the bapti")] <- "st john the baptist"
county$countynm[which(county$statenm=="louisiana" & county$countynm=="vermillion")] <- "vermilion"
county$countynm[which(county$statenm=="michigan" & county$countynm=="mackinac/michilim")] <- "mackinac"
county$countynm[which(county$statenm=="missouri" & county$countynm=="de kalb")] <- "dekalb"
county$countynm[which(county$statenm=="nevada" & county$countynm=="carson")] <- "carson city"
county$countynm[which(county$statenm=="tennessee" & county$countynm=="de kalb")] <- "dekalb"
county$countynm[which(county$statenm=="texas" & county$countynm=="cass/davis")] <- "cass"
county$countynm[which(county$statenm=="texas" & county$countynm=="stephens/buchanan")] <- "stephens"
county$countynm[which(county$statenm=="virginia" & county$countynm=="alexandria city")] <- "alexandria"
county$countynm[which(county$statenm=="virginia" & county$countynm=="arlington/alexand")] <- "arlington"
county$countynm[which(county$statenm=="washington" & county$countynm=="grays harbor/cheh")] <- "grays harbor"
county$countynm[which(county$statenm=="wisconsin" & county$countynm=="bayfield/la point")] <- "bayfield"


###

congress <- lapply(paste0("congressdata/", list.files("congressdata")), function(x) read.csv(x, stringsAsFactors = F, skip=2))
congress <- do.call(rbind, congress)

congress <- congress[which(congress$Office=="House"), ]

congress$statenm <- tolower(congress$State)

congress$cd <- gsub("District ", "", congress$Area)

congress$countynm <- tolower(congress$Area)
congress$countynm <- gsub("\\.", "", congress$countynm)
congress$countynm <- trimws(congress$countynm)

congress$raceYear <- as.numeric(congress$raceYear)

dfloor <- function(x) {
  return(((x-2) %/% 10) *10 + 2)
}

congress$decade <- dfloor(congress$raceYear)

congress <- congress[which(!(congress$RepCandidate=="Letlow, Luke J." & congress$DemStatus=="N/A")),]

drop <- (duplicated(congress[, c("statenm", "cd", "raceYear")])|duplicated(congress[, c("statenm", "cd", "raceYear")], fromLast=T)) & (congress$OtherVotes!="N/A" & congress$ThirdVotes!="N/A")
congress <- congress[!drop, ] # dropping these duplicated observations which include primary elections AND runoffs (primary elections indicated when there is a non-missing value for OtherVotes or ThirdVotes)

congress <- congress[, c("statenm", "cd", "decade", "raceYear", "RepVotes", "DemVotes", "ThirdVotes", "OtherVotes",
                         "PluralityVotes", "RepVotesMajorPercent", "DemVotesMajorPercent", "ThirdVotesTotalPercent",
                         "RepCandidate", "RepStatus", "DemCandidate", "DemStatus", "ThirdCandidate", "ThirdStatus")]

names(congress) <- c("statenm", "cd", "decade",
                     paste0("con_", c("raceYear", "RepVotes", "DemVotes", "ThirdVotes", "OtherVotes",
                                      "PluralityVotes", "RepVotesMajorPercent", "DemVotesMajorPercent", "ThirdVotesTotalPercent",
                                      "RepCandidate", "RepStatus", "DemCandidate", "DemStatus", "ThirdCandidate", "ThirdStatus")))

countyplus <- merge(county, congress, by = c("statenm", "cd", "decade"), all.y=T)

#sum(duplicated(congress[, c("statenm", "cd", "con_raceYear")]))
#View(congress[which(duplicated(congress[, c("statenm", "cd", "con_raceYear")]) | duplicated(congress[, c("statenm", "cd", "con_raceYear")], fromLast = T)), ])

###

files <- paste0("elections/", grep("president", list.files("elections"), value=T))
pres <- lapply(files, function(x) read.csv(x, stringsAsFactors = F, skip=2))
pres <- do.call(rbind, pres)

pres <- pres[which(pres$Office=="President"),]
#View(pres[which(tolower(pres$Area) == tolower(pres$State)), ])

pres$statenm <- tolower(pres$State)
pres$countynm <- tolower(pres$Area)
pres$countynm <- gsub("\\.", "", pres$countynm)
pres$countynm <- trimws(pres$countynm)

pres$RaceDate <- as.numeric(pres$RaceDate)
pres$RaceDate <- as.numeric(substr(pres$RaceDate, 1, 4))

#View(pres[duplicated(pres[, c("statenm", "countynm", "RaceDate")])|duplicated(pres[, c("statenm", "countynm", "RaceDate")], fromLast = T),])

pres <- pres[which(!(pres$statenm=="alaska" & pres$RaceDate==2020 & pres$countynm=="election district 1" & pres$TotalVotes==0)), ]

drop <- (pres$statenm=="virginia" & pres$countynm=="norfolk" & pres$RaceDate==1960 & pres$TotalVotes=="8,936")
keep <- which(pres$statenm=="virginia" & pres$countynm=="norfolk" & pres$RaceDate==1960 & pres$TotalVotes!="8,936")

for(i in c("TotalVotes", "RepVotes", "DemVotes", "OtherVotes", "PluralityVotes")) {
  pres[keep, i] <- gsub(",", "", pres[keep, i])
  pres[which(drop), i] <- gsub(",", "", pres[which(drop), i])
  pres[keep, i] <- pres[which(drop), i]
}

#pres$DemVotesTotalPercent[keep] <- as.numeric(pres$DemVotes[keep])/as.numeric(pres$TotalVotes[keep])
#pres$RepVotesTotalPercent[keep] <- as.numeric(pres$RepVotes[keep])/as.numeric(pres$TotalVotes[keep])
#pres$OtherVotesTotalPercent[keep] <- as.numeric(pres$OtherVotes[keep]) / as.numeric(pres$TotalVotes[keep])
#pres$DemVotesMajorPercent[keep] <- as.numeric(pres$DemVotes[keep])/(as.numeric(pres$DemVotes[keep])+as.numeric(pres$RepVotes[keep]))
#pres$RepVotesMajorPercent[keep] <- as.numeric(pres$RepVotes[keep])/(as.numeric(pres$DemVotes[keep])+as.numeric(pres$RepVotes[keep]))

pres <- pres[which(!drop), ]


pres$TotalVotes[keep] <- gsub(",", "", pres$TotalVotes[keep])


#pres <- pres[which(pres$decade!=1942), ]

# no match for Hawaii Kalawao or Maui or Nevada Ormsby
# armstrong county in South Dakota is defunct, has no match

pres <- pres[, c("statenm", "countynm", "RaceDate",
                 "TotalVotes", "RepVotes", "DemVotes", "OtherVotes", "PluralityVotes", "RepVotesTotalPercent",
                 "DemVotesTotalPercent", "OtherVotesTotalPercent", "RepVotesMajorPercent", "DemVotesMajorPercent",
                 "PluralityParty", "RepCandidate", "RepStatus", "DemCandidate", "DemStatus")]

names(pres) <- c("statenm", "countynm",
                 paste0("pres_", c("RaceDate",
                                   "TotalVotes", "RepVotes", "DemVotes", "OtherVotes", "PluralityVotes", "RepVotesTotalPercent",
                                   "DemVotesTotalPercent", "OtherVotesTotalPercent", "RepVotesMajorPercent", "DemVotesMajorPercent",
                                   "PluralityParty", "RepCandidate", "RepStatus", "DemCandidate", "DemStatus")))

countyplus <- merge(countyplus, pres, by.x=c("statenm", "countynm", "con_raceYear"), by.y=c("statenm", "countynm", "pres_RaceDate"), all.x=T)

###

senate <- lapply(paste0("cq_senate_county/", list.files("cq_senate_county")), function(x) read.csv(x, stringsAsFactors = F, skip=2))
senate <- do.call(rbind, senate)
senate <- senate[which(senate$Office=="Senate"), ]

senate$statenm <- tolower(senate$State)
senate$countynm <- tolower(senate$Area)
senate$countynm <- gsub("\\.", "", senate$countynm)
senate$countynm <- trimws(senate$countynm)

senate$RaceDate <- as.numeric(senate$RaceDate)
senate$RaceDate <- as.numeric(substr(senate$RaceDate, 1, 4))

drop <- (duplicated(senate[, c("statenm", "countynm", "RaceDate")])|duplicated(senate[, c("statenm", "countynm", "RaceDate")], fromLast = T)) & (senate$RepVotes=="N/A" & senate$DemVotes=="N/A") & (senate$statenm %in% c("georgia", "alaska"))
senate <- senate[which(!drop), ]

#View(senate[duplicated(senate[, c("statenm", "countynm", "RaceDate")])|duplicated(senate[, c("statenm", "countynm", "RaceDate")], fromLast = T), ])

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

seatyears$illinois$seat2 <- seq(1966, 2022, 6) # this will correct for (ignore) a special election in 1970
# also ignoring a Georgia special election from 2021
for(i in 1:length(seatyears)) {
  if(i==1) {seatyears1 <- list(); seatyears2 <- list()}
  seatyears1[[i]] <- data.frame(state=names(seatyears)[i], year=seatyears[[i]]$seat1)
  seatyears2[[i]] <- data.frame(state=names(seatyears)[i], year=seatyears[[i]]$seat2)
}
seatyears1 <- do.call(rbind, seatyears1)
seatyears2 <- do.call(rbind, seatyears2)

#seatyears1$merge <- paste(seatyears1$state, seatyears1$year)
#seatyears2$merge <- paste(seatyears2$state, seatyears2$year)

#senate$merge <- paste(senate$statenm, senate$sen_RaceDate)

#senate_seat1 <- merge(seatyears1, senate, by="merge")
#senate_seat2 <- merge(seatyears2, senate, by="merge")

#test <- rbind(seatyears1, seatyears2)

#View(senate[!senate$merge %in% test$merge, ])

senate_seat1 <- merge(seatyears1, senate, by.x=c("state", "year"), by.y=c("statenm", "RaceDate"))
senate_seat2 <- merge(seatyears2, senate, by.x=c("state", "year"), by.y=c("statenm", "RaceDate"))
# 265 missing cases, which accounts for the Illinois special election in 1970, Georgia special
# election in 2021, and Hawaii special election in 2014

senate_seat1 <- senate_seat1[, c("state", "countynm", "year", "RepVotes", "DemVotes", "ThirdVotes", "OtherVotes", 
                                 "PluralityVotes", "RepVotesMajorPercent", "DemVotesMajorPercent", "ThirdVotesTotalPercent",
                                 "RepCandidate", "RepStatus", "DemCandidate", "DemStatus", "PluralityParty", "ThirdParty", 
                                 "ThirdCandidate", "ThirdStatus")]

names(senate_seat1) <- c("statenm", "countynm",
                         paste0("sen1_", c("RaceDate", "RepVotes", "DemVotes", "ThirdVotes", "OtherVotes", "PluralityVotes", 
                                           "RepVotesMajorPercent", "DemVotesMajorPercent", "ThirdVotesTotalPercent",
                                           "RepCandidate", "RepStatus", "DemCandidate", "DemStatus", "PluralityParty", 
                                           "ThirdParty", "ThirdCandidate", "ThirdStatus")))

senate_seat2 <- senate_seat2[, c("state", "countynm", "year", "RepVotes", "DemVotes", "ThirdVotes", "OtherVotes", 
                                 "PluralityVotes", "RepVotesMajorPercent", "DemVotesMajorPercent", "ThirdVotesTotalPercent",
                                 "RepCandidate", "RepStatus", "DemCandidate", "DemStatus", "PluralityParty", "ThirdParty", 
                                 "ThirdCandidate", "ThirdStatus")]

names(senate_seat2) <- c("statenm", "countynm",
                         paste0("sen2_", c("RaceDate", "RepVotes", "DemVotes", "ThirdVotes", "OtherVotes", "PluralityVotes", 
                                           "RepVotesMajorPercent", "DemVotesMajorPercent", "ThirdVotesTotalPercent",
                                           "RepCandidate", "RepStatus", "DemCandidate", "DemStatus", "PluralityParty", 
                                           "ThirdParty", "ThirdCandidate", "ThirdStatus")))

countyplus <- merge(countyplus, senate_seat1, by.x=c("statenm", "countynm", "con_raceYear"),
                    by.y=c("statenm", "countynm", "sen1_RaceDate"), all.x=T) # need to pull forward the senate results from prior year
countyplus <- merge(countyplus, senate_seat2, by.x=c("statenm", "countynm", "con_raceYear"),
                    by.y=c("statenm", "countynm", "sen2_RaceDate"), all.x=T) # need to pull forward the senate results from prior year


# 459 observations still duplicated across state, county, year, cd. CHECK THESE CASES
#View(data.frame(senate[!senate$countynm %in% countyplus$countynm, c("statenm", "countynm")]))
#View(data.frame(countyplus[!countyplus$countynm %in% senate$countynm, c("statenm", "countynm")]))

###

governor <- lapply(paste0("governor_county//", list.files("governor_county/")), function(x) read.csv(x, stringsAsFactors = F, skip=2))
governor <- do.call(rbind, governor)

# some cases were erroneously coded as Texas elections when they were actually in Nevada
governor$State[which(governor$RepCandidate=="List, Robert F." & governor$DemCandidate=="Bryan, Richard H.")] <- "Nevada"

#View(governor[which(governor$RepCandidate=="List, Robert F." & governor$DemCandidate=="Bryan, Richard H."),])

governor <- governor[which(governor$Office=="Governor"), ]

governor$statenm <- tolower(governor$State)
governor$countynm <- tolower(governor$Area)
governor$countynm <- gsub("\\.", "", governor$countynm)
governor$countynm <- trimws(governor$countynm)

governor$RaceDate <- as.numeric(governor$RaceDate)
governor$RaceDate <- as.numeric(substr(governor$RaceDate, 1, 4))

# this removes observations for primary elections preceding runoffs in Louisiana 
governor <- governor[!(governor$State=="Louisiana" & governor$RepCandidate=="N/A" & governor$DemCandidate=="N/A"), ]

#View(governor[duplicated(governor[,c("statenm", "countynm", "RaceDate")]) | duplicated(governor[,c("statenm", "countynm", "RaceDate")], fromLast=T), ])
#governor <- governor[which(governor$RepVotes!="N/A" & governor$DemVotes!="N/A"), ]

governor <- governor[, c("statenm", "countynm", "RaceDate", "RepVotes",
                         "DemVotes", "ThirdVotes", "OtherVotes", "PluralityVotes", "RepVotesMajorPercent", "DemVotesMajorPercent", "ThirdVotesTotalPercent", 
                         "RepCandidate", "RepStatus", "DemCandidate", "DemStatus", "PluralityParty", "ThirdParty", "ThirdCandidate", "ThirdStatus")]

names(governor) <- c("statenm", "countynm",
                     paste0("gov_", c("RaceDate", "RepVotes",
                                      "DemVotes", "ThirdVotes", "OtherVotes", "PluralityVotes", "RepVotesMajorPercent", "DemVotesMajorPercent", "ThirdVotesTotalPercent", 
                                      "RepCandidate", "RepStatus", "DemCandidate", "DemStatus", "PluralityParty", "ThirdParty", "ThirdCandidate", "ThirdStatus")))

countyplus <- merge(countyplus, governor, by.x=c("statenm", "countynm", "con_raceYear"), by.y=c("statenm", "countynm", "gov_RaceDate"), all.x=T) # need to pull forward the governor results from prior year

###

unique(countyplus$fips[nchar(countyplus$fips)==4])
countyplus$fips <- ifelse(nchar(countyplus$fips)==4, paste0("0", countyplus$fips), countyplus$fips)

for(i in 1980:1989) {
  if(i == 1980) {temp <- list()}
  pop1980 <- read_xls("/Users/christianbaehr/Downloads/pe-02.xls", skip = 5, sheet = as.character(i))
  pop1980 <- pop1980[-1, ]
  
  pop1980$pop <- apply(pop1980[, !names(pop1980) %in% c("Year of Estimate", "FIPS State and County Codes", "Race/Sex Indicator")],
                       1, FUN=function(x) sum(x))
  
  pop1980$sex <- ifelse(grepl("female", pop1980$`Race/Sex Indicator`), "female", "male")
  pop1980$race <- ifelse(grepl("Black", pop1980$`Race/Sex Indicator`), "black",
                         ifelse(grepl("White", pop1980$`Race/Sex Indicator`), "white", "other"))
  
  pop1980 <- pop1980[,c("Year of Estimate", "FIPS State and County Codes", "race", "sex", "pop")]
  names(pop1980) <- c("year", "fips", "race", "sex", "pop")
  pop1980 <- data.frame(pop1980)
  
  pop1980 <- reshape(pop1980, direction = "wide", idvar=c("year", "fips", "race"), timevar = "sex")
  pop1980 <- reshape(pop1980, direction = "wide", idvar=c("year", "fips"), timevar = "race")
  temp[[as.character(i)]] <- pop1980
  
}
pop1980 <- do.call(rbind, temp)

for(i in 1970:1979) {
  if(i == 1970) {temp <- list()}
  pop1970 <- read_xls(sprintf("/Users/christianbaehr/Downloads/pop1970/co-asr-%s.xls",as.character(i)), skip=4)
  pop1970 <- pop1970[-1, ]
  
  pop1970$pop <- apply(pop1970[, !names(pop1970) %in% c("Year of Estimate", "FIPS State and County Codes", "Race/Sex Indicator")],
                       1, FUN=function(x) sum(x))
  
  pop1970$sex <- ifelse(grepl("female", pop1970$`Race/Sex Indicator`), "female", "male")
  pop1970$race <- ifelse(grepl("Black", pop1970$`Race/Sex Indicator`), "black",
                         ifelse(grepl("White", pop1970$`Race/Sex Indicator`), "white", "other"))
  
  pop1970 <- pop1970[,c("Year of Estimate", "FIPS State and County Codes", "race", "sex", "pop")]
  names(pop1970) <- c("year", "fips", "race", "sex", "pop")
  pop1970 <- data.frame(pop1970)
  
  pop1970 <- reshape(pop1970, direction = "wide", idvar=c("year", "fips", "race"), timevar = "sex")
  pop1970 <- reshape(pop1970, direction = "wide", idvar=c("year", "fips"), timevar = "race")
  temp[[as.character(i)]] <- pop1970
}
pop1970 <- do.call(rbind, temp)

pop_1970_89 <- do.call(rbind, list(pop1970, pop1980))
# pull the 1989 population data forward one year to 1990 so we have a measure. Redistricting files with pop start in 1992
pop_1970_89$year[which(pop_1970_89$year==1989)] <- 1990

countyplus <- merge(countyplus, pop_1970_89, by.x=c("fips", "con_raceYear"), by.y=c("fips", "year"), all.x=T)
countyplus$county_pop[is.na(countyplus$county_pop)] <- apply(countyplus[is.na(countyplus$county_pop), grep("pop\\.", names(countyplus))], 1, sum)

county$mergeid <- paste(tolower(county$statenm), tolower(county$countynm), county$decade)
pres$mergeid <- paste(tolower(pres$statenm), tolower(pres$countynm), pres$decade)
#View(data.frame(unique(county$mergeid[!(county$mergeid %in% pres$mergeid)])))
#View(data.frame(unique(pres$mergeid[!(pres$mergeid %in% county$mergeid)])))
#sum(duplicated(county$mergeid))
#View(county[duplicated(county$mergeid) | duplicated(county$mergeid, fromLast = T), ])

###


which(is.na(countyplus$gov_RepVotesMajorPercent))[50]
# reference alabama 2004 - no governor election
for(i in 1:nrow(countyplus)) {
  #i=82
  
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

for(i in c("con_DemVotes", "con_RepVotes", "con_ThirdVotes", "con_OtherVotes", "pres_RepVotes", "pres_DemVotes", "pres_OtherVotes",
           "sen1_DemVotes", "sen1_RepVotes", "sen1_ThirdVotes", "sen1_OtherVotes", "sen2_DemVotes", "sen2_RepVotes",
           "sen2_ThirdVotes", "sen2_OtherVotes", "gov_RepVotes", "gov_DemVotes", "gov_ThirdVotes", "gov_OtherVotes")) {
  countyplus[which(countyplus[,i]=="N/A"), i] <- 0
  countyplus[,i] <- gsub(",", "", countyplus[,i])
}

###

names(countyplus) <- tolower(names(countyplus))
names(countyplus) <- gsub("\\.","_",names(countyplus))

countyplus <- countyplus[, !names(countyplus) %in% c("x", "decade", "year")]

countyplus$con_unopposed <- (countyplus$con_demcandidate=="N/A" & countyplus$con_demstatus=="N/A") | (countyplus$con_repcandidate=="N/A" & countyplus$con_repstatus=="N/A")

###

#View(countyplus[is.na(countyplus$unit_weight),]) 
# only missing weights cases are in at Large districts or before 1950

View(countyplus[is.na(countyplus$county_pop), ])
table(countyplus$con_raceyear[is.na(countyplus$county_pop)]) # why are there 3248 missing countypop obs. in 1990?
table(countyplus$con_raceyear)

#countyplus$con_demvotes <- gsub(",", "", countyplus$con_demvotes)
#countyplus$con_repvotes <- gsub(",", "", countyplus$con_repvotes)

#countyplus$con_demvotesmajorpercent <- as.numeric(countyplus$con_demvotes)/ (as.numeric(countyplus$con_demvotes)+as.numeric(countyplus$con_repvotes))

#a <- tapply(countyplus$con_demvotesmajorpercent, INDEX=list(countyplus$statenm, countyplus$decade), FUN=mean, na.rm=T)

sum(is.na(countyplus$unit_weight))
View(countyplus[is.na(countyplus$unit_weight), ]) # bunch of at large elections. Drop these
# check why we are dropping counties such as Louisville kentucky (CD 3 1988)

#countyplus$con_demvotes <- gsub(",", "", countyplus$con_demvotes)
#countyplus$con_demvotes <- as.numeric(countyplus$con_demvotes)

###

sum(is.na(countyplus$unit_pop))

for(i in grep("pop_", names(countyplus), value=T)) {
  countyplus[, paste0("unit_", i)] <- countyplus[,i] * countyplus$unit_weight
}

vars <- c("pres_repvotes", "pres_demvotes", 
          "sen1_repvotes", "sen1_demvotes", "sen2_repvotes", "sen2_demvotes",
          "gov_repvotes", "gov_demvotes")
for(i in vars) {
  countyplus[,i] <- gsub(",", "", countyplus[,i])
  countyplus[,i] <- as.numeric(countyplus[,i])
  countyplus[,paste0("unit_", i)] <- countyplus[,i] * countyplus$unit_weight
}

countyplus$unit_con_repvotes <- countyplus$con_repvotes # dont need to rescale for congressional elections
countyplus$unit_con_demvotes <- countyplus$con_demvotes

cd_level <- aggregate(countyplus[, setdiff(grep("unit_",names(countyplus),value=T), c("unit_weight", "unit_pop"))],
                      by=list(countyplus$statenm, countyplus$cd, countyplus$con_raceyear),
                      FUN=sum, na.rm=T)
# dont have 435 districts for 2010, check on this
cd_level <- cd_level[which(cd_level$Group.3>1968), ]
names(cd_level)[1:3] <- c("state", "cd", "year")
names(cd_level) <- gsub("unit_", "", names(cd_level))

cd_level$con_repshare <- cd_level$con_repvotes / (cd_level$con_demvotes+cd_level$con_repvotes)
cd_level$con_demshare <- cd_level$con_demvotes / (cd_level$con_demvotes+cd_level$con_repvotes)

cd_level$pres_demshare <- cd_level$pres_demvotes / (cd_level$pres_demvotes+cd_level$pres_repvotes)
cd_level$pres_repshare <- cd_level$pres_repvotes / (cd_level$pres_demvotes+cd_level$pres_repvotes)

cd_level$gov_repshare <- cd_level$gov_repvotes / (cd_level$gov_demvotes+cd_level$gov_repvotes)
cd_level$gov_demshare <- cd_level$gov_demvotes / (cd_level$gov_demvotes+cd_level$gov_repvotes)

cd_level$sen1_repshare <- cd_level$sen1_repvotes / (cd_level$sen1_demvotes+cd_level$sen1_repvotes)
cd_level$sen1_demshare <- cd_level$sen1_demvotes / (cd_level$sen1_demvotes+cd_level$sen1_repvotes)

cd_level$sen2_demshare <- cd_level$sen2_demvotes / (cd_level$sen2_demvotes+cd_level$sen2_repvotes)
cd_level$sen2_repshare <- cd_level$sen2_repvotes / (cd_level$sen2_demvotes+cd_level$sen2_repvotes)

cd_level <- cd_level[,!grepl("votes", names(cd_level))]

library(modelsummary)
f_summary <- All(cd_level) ~ N+Mean+SD+Median+Min+P25+P75+Max
datasummary(formula=f_summary, data=cd_level, output="/Users/christianbaehr/Desktop/sum_stats.tex")


load("WDI_vars.RData")
sumdat <- imf_panel[, !names(imf_panel) %in% keep_vars]
sumdat <- sumdat[, sort(names(sumdat))]

f_summary <- All(sumdat) ~ N+Mean+SD+Median+Min+P25+P75+Max
datasummary(formula=f_summary, data=sumdat, output="../results/sum_stats.tex")



county_store <- countyplus

for(i in vars) {
  countyplus[,i] <- gsub(",", "", countyplus[,i])
  countyplus[,i] <- as.numeric(countyplus[,i])
  countyplus[,i] <- countyplus[,i] * countyplus$unit_weight
}

test <- aggregate(countyplus[,vars],
                  by=list(countyplus$con_raceyear, countyplus$statenm, countyplus$cd),
                  FUN=mean, na.rm=T)
test <- test[test$Group.1>1970, ]

test$con_repvoteshare <- test$con_repvotes / (test$con_demvotes+test$con_repvotes)

test$decade <- dfloor(test$Group.1)
test2 <- aggregate(test[,c(vars, "con_repvoteshare")],
                   by=list(test$Group.2, test$decade), 
                   FUN=mean,  na.rm=T)

#test2 <- test2[test2$Group.3>1962, ]
#test2 <- test2[test2$Group.2!="At Large", ]

conrep <- test2[, c("Group.1", "Group.2", "con_repvoteshare")]
conrep <- reshape(conrep, direction="wide", idvar=c("Group.1"), timevar="Group.2")
library(xtable)
print(xtable(conrep, type = "latex"), file = "/Users/christianbaehr/Desktop/repvoteshare.tex")

###

test <- aggregate(countyplus[,vars],
                  by=list(countyplus$con_raceyear, countyplus$statenm, countyplus$cd),
                  FUN=sum, na.rm=T)
test <- test[test$Group.1>1970, ]
test$decade <- dfloor(test$Group.1)

test2 <- aggregate(test[,c(vars, "unit_pop")],
                   by=list(test$Group.2, test$decade), 
                   FUN=sum,  na.rm=T)

conrep <- test2[, c("Group.1", "Group.2", "unit_pop")]
conrep <- reshape(conrep, direction="wide", idvar=c("Group.1"), timevar="Group.2")
library(xtable)
print(xtable(conrep, type = "latex"), file = "/Users/christianbaehr/Desktop/population.tex")







