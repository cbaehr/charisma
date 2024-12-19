
# Author: Christian Baehr
# Date: 8/9/2024
# Description: Fix errors in FIPS codes. These FIPS codes will be used to merge 
#              the county map with county population data later on.

#setwd("/Users/christianbaehr/Dropbox/charisma_project/data/original/census_redistricting/")
setwd("/Users/christianbaehr/Dropbox/Charisma_Data/")

fips <- read.csv("fips_codes.csv", stringsAsFactors = F)
fips <- fips[!is.na(fips$state), ]
fips$name <- toupper(fips$name)

# drop PARISH from county names
fips$name <- trimws(gsub("PARISH", "", fips$name))

# ID which counties have both an associated city and county
short <- paste(fips$state, trimws(gsub("COUNTY|CITY", "", fips$name)))
dups <- (duplicated(short) | duplicated(short, fromLast=T))

# for cities with no corresponding county, drop CITY from the name
fips$name[!dups] <- gsub("CITY", "", fips$name[!dups])
# drop COUNTY from the name
fips$name <- gsub("COUNTY", "", fips$name)

# remove apostrophes from county names
fips$name <- trimws(gsub("'", "", fips$name))
#senate$Area <- trimws(gsub("'", "", senate$Area))

#fips <- fips[order(fips$state, fips$name), ]
#senate <- senate[order(senate$state2, senate$Area), ]
# need to correct Alaska district names in CQ senate data

#senate <- senate[which(senate$Area!="Votes Not Reported by County"), ]

# NEED TO FIX THE ALASKA COUNTY NAMES IN THE SENATE DATA
fips <- fips[which(fips$state!="AK"),]
#senate <- senate[which(senate$state2!="AK"), ]

###

fips$name[which(fips$state=="IL" & fips$name=="DUPAGE")] <- "DU PAGE"
fips$name[which(fips$state=="IL" & fips$name=="LASALLE")] <- "LA SALLE"
fips$name[which(fips$state=="IN" & fips$name=="LAPORTE")] <- "LA PORTE"
fips$name[which(fips$state=="MS" & fips$name=="DESOTO")] <- "DE SOTO"
fips$name[which(fips$state=="ND" & fips$name=="LAMOURE")] <- "LA MOURE"
fips$name[which(fips$fips==35013)] <- "DONA ANA" # weird expression characters
fips$name[which(fips$state=="TX" & fips$name=="DEWITT")] <- "DE WITT"
fips$name[which(fips$state=="VA" & fips$name=="CHARLES")] <- "CHARLES CITY"
fips$name[which(fips$state=="VA" & fips$name=="JAMES")] <- "JAMES CITY"
fips$name[which(fips$state=="VA" & fips$name=="NORFOLK CITY")] <- "NORFOLK"
fips$name[which(fips$state=="NV" & fips$name=="CARSON")] <- "CARSON CITY"

write.csv(fips, "../../working/fips_codes_clean.csv", row.names=F)

