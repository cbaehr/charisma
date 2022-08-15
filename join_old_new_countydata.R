setwd("/Users/christianbaehr/Dropbox/charisma_project/data/census_redistricting/")

countyplusname <- read.csv("../condistrict_to_county_mapping_withcountynames.csv", stringsAsFactors = F)

###

n_districts <- aggregate(countyplusname$con_district, by=list(countyplusname$state, countyplusname$year), FUN=function(x) length(unique(x)))

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






