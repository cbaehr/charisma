setwd("/Users/christianbaehr/Dropbox/charisma_project/data/census_redistricting/")

countyplusname <- read.csv("../condistrict_to_county_mapping_withcountynames.csv", stringsAsFactors = F)

ndists <- aggregate(countyplusname$con_district, by=list(countyplusname$year, countyplusname$state, countyplusname$fips), FUN=function(x) length(unique(x)))
names(ndists) <- c("year", "state", "fips", "multidist")

countyplusname <- merge(countyplusname, ndists) #a value of 2 implies county crosses multiple districts


###

#n_districts <- aggregate(countyplusname$con_district, by=list(countyplusname$state, countyplusname$year), FUN=function(x) length(unique(x)))

#Update: good now. 

# Iowa a problem
# Arkansas potentially a problem
# Massachusetts potentially a problem
# Maryland potentially a problem
# Missouri potentially a problem
# North carolina a problem
# North Dakota a problem
# New Jersey a problem
# Virginia a problem
# Wisconsin a problem

###

congress <- do.call(rbind, list(read.csv("../congressdata/congressdata_1962-1980.csv", stringsAsFactors = F, skip=2),
                                read.csv("../congressdata/congressdata_1982-2000.csv", stringsAsFactors = F, skip=2),
                                read.csv("../congressdata/congressdata_2002-2020.csv", stringsAsFactors = F, skip=2)))
#congress <- read.csv("../congressdata/congressdata_1962-1980.csv", stringsAsFactors = F, skip=2)
#congress$RepVotes <- as.numeric(gsub(",", "", congress$RepVotes))
#congress$DemVotes <- as.numeric(gsub(",", "", congress$DemVotes))
#congress$OtherVotes <- as.numeric(gsub(",", "", congress$OtherVotes))
#congress$ThirdVotes <- as.numeric(gsub(",", "", congress$ThirdVotes))


congress <- congress[which(congress$Office=="House"), ] # for 1966 1964 and 1962 they only show returns for candidates who received >5% of the total vote

names(congress) <- tolower(names(congress))

congress$con_district <- gsub("District ", "", congress$area)
congress$con_district <- ifelse(nchar(congress$con_district)==1, paste0("0", congress$con_district), congress$con_district)

congress$raceyear <- as.numeric(congress$raceyear)
congress <- congress[which(congress$raceyear >= 1992), ]
congress$decade <- ifelse(congress$raceyear %in% c(1992:2001), 1992, ifelse(congress$raceyear %in% c(2002:2011), 2002, 2012))

names(countyplusname)[names(countyplusname)=="year"] <- "decade"

countyplusname$state <- gsub("_", " ",countyplusname$state) # Alaska still not matching because omitted from countyplusname

congress <- congress[, !names(congress) %in% c("office", "racenotes", "area")]

countypluscongress <- merge(countyplusname, congress) # no AK, WY, or VT

###

pres <- read.csv("/Users/christianbaehr/Downloads/presidentdata_1984-2020.csv", stringsAsFactors = F, skip=2)

pres <- pres[which(pres$Office=="President"), ]

names(pres) <- paste0(names(pres), "_pres")

names(pres)[names(pres)=="State_pres"] <- "state" # countypluscongress missing a bunch of states
names(pres)[names(pres)=="Area_pres"] <- "name"

pres$RaceDate_pres <- as.numeric(substr(pres$RaceDate_pres, 1, 4))

pres <- pres[which(pres$RaceDate_pres >= 1992), ]

countypluscongress$RaceDate_pres <- ifelse(countypluscongress$raceyear %in% seq(1992, 2020, 4), countypluscongress$raceyear, countypluscongress$raceyear -2)

#pres$decade <- ifelse(pres$RaceDate_pres %in% c(1992:2001), 1992, ifelse(pres$RaceDate_pres %in% c(2002:2011), 2002, 2012))

countypluscongress <- merge(countypluscongress, pres)

###

gov <- do.call(rbind, list(read.csv("/Users/christianbaehr/Downloads/governordata_1981-1990.csv", stringsAsFactors = F, skip=2),
                           read.csv("/Users/christianbaehr/Downloads/governordata_1991-2000.csv", stringsAsFactors = F, skip=2),
                           read.csv("/Users/christianbaehr/Downloads/governordata_2001-2010.csv", stringsAsFactors = F, skip=2),
                           read.csv("/Users/christianbaehr/Downloads/governordata_2011-2020.csv", stringsAsFactors = F, skip=2)))

gov <- gov[which(gov$Office=="Governor"), ]
names(gov) <- paste0(names(gov), "_gov")
gov <- gov[which(gov$RepVotes_gov!="N/A" | gov$DemVotes_gov!="N/A"), ]

names(gov)[names(gov)=="State_gov"] <- "state"
names(gov)[names(gov)=="Area_gov"] <- "name"

gov$RaceDate_gov <- as.numeric(substr(gov$RaceDate_gov, 1, 4))

#gov$RaceDate_gov[!(gov$RaceDate_gov %in% seq(1990, 2020, 2))]

countypluscongress$RaceDate_gov <- NA

years <- tapply(gov$RaceDate_gov, INDEX=list(gov$state), FUN=function(x) list(unique(x)))

# assigning the most recent governors election year to each observation
for(i in 1:nrow(countypluscongress)) {
  raceyears <- years[which(names(years)== countypluscongress$state[i])][[1]]
  if(!countypluscongress$raceyear[i] %in% raceyears) {
    raceyears <- raceyears[raceyears < countypluscongress$raceyear[i]]
    countypluscongress$RaceDate_gov[i] <- max(raceyears)
  } else{
    countypluscongress$RaceDate_gov[i] <- countypluscongress$raceyear[i]
  }
}

countypluscongress <- merge(countypluscongress, gov)

###

sen <- do.call(rbind, list(read.csv("../cq_senate_county/senate_1968-1982.csv", stringsAsFactors = F, skip=2),
                           read.csv("../cq_senate_county/senate_1984-2002.csv", stringsAsFactors = F, skip=2),
                           read.csv("../cq_senate_county/senate_2004-2021.csv", stringsAsFactors = F, skip=2)))

sen <- sen[which(sen$Office=="Senate"), ]

names(sen) <- paste0(names(sen), "_sen")
sen <- sen[which(sen$RepVotes_sen!="N/A" | sen$DemVotes_sen!="N/A"), ]

names(sen)[names(sen)=="State_sen"] <- "state"
names(sen)[names(sen)=="Area_sen"] <- "name"

sen$RaceDate_sen <- as.numeric(substr(sen$RaceDate_sen, 1, 4))

countypluscongress$RaceDate_sen <- NA

years <- tapply(sen$RaceDate_sen, INDEX=list(sen$state), FUN=function(x) list(unique(x)))

# assigning the most recent senate election year to each observation
for(i in 1:nrow(countypluscongress)) {
  raceyears <- years[which(names(years)== countypluscongress$state[i])][[1]]
  if(!countypluscongress$raceyear[i] %in% raceyears) {
    raceyears <- raceyears[raceyears < countypluscongress$raceyear[i]]
    countypluscongress$RaceDate_sen[i] <- max(raceyears)
  } else{
    countypluscongress$RaceDate_sen[i] <- countypluscongress$raceyear[i]
  }
}

sum(duplicated(sen[, c("state", "name", "RaceDate_sen")]))

#View(sen[duplicated(sen[, c("state", "name", "RaceDate_sen")]) | duplicated(sen[, c("state", "name", "RaceDate_sen")], fromLast = T), ])

for(i in 1:nrow(countypluscongress)) {
  
  if(sen$RaceDate_sen[i]==2014 & ( sen$DemCandidate_sen[i]=="Landrieu, Mary L.") ) {
    # this gives the row of the second observation corresponding to this election-county observation
    j <- which(sen$RaceDate_sen==2014 & sen$name==sen$name[i] & sen$RepCandidate_sen=="Cassidy, Bill")
    
    sen$RepCandidate_sen[i] <- sen$RepCandidate_sen[j]
    sen$RepVotes_sen[i] <- sen$RepVotes_sen[j]
    sen$RepStatus_sen[i] <- sen$RepStatus_sen[j]
    
    sen$OtherVotes_sen[i] <- 0 # setting "Other candidate" votes to zero because the runoff attributes these votes to other major party
    
  }
  
}

#View(sen[sen$RaceDate_sen==2014 & (sen$RepCandidate_sen=="Cassidy, Bill" | sen$DemCandidate_sen=="Landrieu, Mary L."), ])
sen <- sen[!(sen$RaceDate_sen==2014 & sen$RepCandidate_sen=="Cassidy, Bill" & sen$DemCandidate_sen=="N/A"), ]

countypluscongress <- merge(countypluscongress, sen)




###

library(haven)
rocio <- read_dta("../../codes-from-Rocio-APSR-county-analysis/counties/output/counties_final.dta")
rocio <- data.frame(rocio)

a <- aggregate(rocio$statenm, by=list(rocio$statenm), FUN=function(x) length(x))
a <- a$x[match(state.name, a$Group.1)]

b <- aggregate(countypluscongress$state[countypluscongress$raceyear==1992], by=list(countypluscongress$state[countypluscongress$raceyear==1992]),
               FUN=length)
b <- b$x[match(state.name, b$Group.1)]

c <- (rocio$y72_cd - rocio$y74_cd) != 0
c <- aggregate(c, by=list(rocio$statenm), FUN=mean)
c <- c$x[match(state.name, c$Group.1)]

cpc_nooverlap <- countypluscongress[which(countypluscongress$unit_weight==1), ]

out <- data.frame(state=state.name, n_counties_1972=a, pct_changed_cd_1972_1974=c, n_counties_1992=b)
out <- data.frame(state=state.name, n_counties_1972=a, n_counties_1992=b)

write.csv(out, "/Users/christianbaehr/Desktop/number_of_counties_comparison.csv", row.names=F)

countyout <- data.frame()
print(xtable(out, type = "latex"), file = "/Users/christianbaehr/Desktop/comparison.tex")


###

oldcounties <- read.csv("/Users/christianbaehr/Desktop/counties_final.csv", stringsAsFactors = F)

###

n_districts_72 <- aggregate(oldcounties$y72_cd, by=list(oldcounties$state), FUN=function(x) length(unique(x)))



###

View(countyplusname[which(countyplusname$name=="AUTAUGA"), ])

View(oldcounties[which(oldcounties$statenm=="Iowa" & oldcounties$countynm=="TAMA"), ])
View(countyplusname[which(countyplusname$state=="IA" & countyplusname$name=="TAMA"), ])

oldcounties$y88_cd[which(oldcounties$statenm=="Iowa" & oldcounties$countynm=="TAMA")]
oldcounties$y84_cd[which(oldcounties$statenm=="Iowa" & oldcounties$countynm=="TAMA")]
oldcounties$y80_cd[which(oldcounties$statenm=="Iowa" & oldcounties$countynm=="TAMA")]
oldcounties$y76_cd[which(oldcounties$statenm=="Iowa" & oldcounties$countynm=="TAMA")]
oldcounties$y72_cd[which(oldcounties$statenm=="Iowa" & oldcounties$countynm=="TAMA")]

# no agreement among Iowa counties. There are also not 8 congressional districts in Iowa
# but Tama in 2002 was apparently in district 8. The census redistricting data has 
# 8 districts for Iowa in 2010 data


# AUTAUGA COUNTY in district 2 in 1972 and district 3 in 1992
# but Rocios data changes it to district 3 by 1988






