
oldcounties <- read.csv("/Users/christianbaehr/Desktop/counties_final.csv", stringsAsFactors = F)

View(oldcounties[1:100, ])

oldcounties$testvar <- paste(oldcounties$state, oldcounties$county)
test1972 <- table(oldcounties$testvar)
mean(test1972 > 1)
# no duplicated counties in the 1972-1990 data. They definitely only keep the counties which do not overlap 
# with multiple congressional districts. I checked the Rocio replication data and shapefiles


aggregate(oldcounties$countynm, by=list(oldcounties$state, oldcounties))




countyplusname$testvar <- paste(countyplusname$state, countyplusname$county)

test1992 <- table(countyplusname$testvar[countyplusname$year==1992])
mean(test1992 > 1)