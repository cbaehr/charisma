
setwd("/Users/christianbaehr/Dropbox/charisma_project/data/")

lines <- paste(readLines("input/us.1969_2020.19ages.txt"), collapse="\n")

census <- read.fwf(textConnection(lines), widths=c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8))

names(census) <- c("year", "state", "state_fips", "county_fips", "registry", "race", "origin",
                   "sex", "age", "population")

census_mean <- aggregate(census$population, 
                         by=list(census$year, census$state_fips, census$county_fips, census$sex, census$race),
                         FUN=sum)
names(census_mean) <- c("year", "state_fips", "county_fips", "sex", "race", "population")

census_mean$sex <- ifelse(census_mean$sex==1, "male", "female")

census_mean$race <- ifelse(census_mean$race==1, "white", 
                           ifelse(census_mean$race==2, "black", "other"))

census_mean_sex <- reshape(census_mean, direction="wide", 
                           idvar=c("year", "state_fips", "county_fips", "race"), timevar=c("sex"))
census_mean_sex_race <- reshape(census_mean_sex, direction="wide", 
                           idvar=c("year", "state_fips", "county_fips"), timevar=c("race"))

fips <- lapply(census_mean_sex_race$county_fips, FUN = function(x) c(rep("0", 3-nchar(x)), x))

fipsnew <- sapply(fips, function(x) paste(x, collapse=""))

census_mean_sex_race$county_fips <- fipsnew

census_mean_sex_race$state_fips <- as.character(census_mean_sex_race$state_fips)
census_mean_sex_race$state_fips <- ifelse(nchar(census_mean_sex_race$state_fips)==2, 
                                          census_mean_sex_race$state_fips, 
                                          paste0("0", census_mean_sex_race$state_fips))

census_mean_sex_race$fips <- paste0(census_mean_sex_race$state_fips, census_mean_sex_race$county_fips)

census_mean_sex_race <- census_mean_sex_race[, !names(census_mean_sex_race) %in% c("state_fips", "county_fips")]

write.csv(census_mean_sex_race, "census_county.csv", row.names = F)








