
# 1970 breaks total population up into hispanic-non-hispanic
# 1980 gives the portion of the population that is NOT of hispanic origin
# 1990 breaks total population up into hispanic/nonhispanic
# 2000 breaks total population up into hispanic/nonhispanic
# 2010 breaks up total population into hispanic/nonhispanic

setwd("/Users/christianbaehr/Dropbox/charisma_project/data/")

pop1970 <- read.csv("original/socialexplorer/census1970/R13208694_SL4601.csv", stringsAsFactors = F)

pop1970 <- pop1970[, c("Geo_FIPS", 
                       "SE_T001_001", "SE_T004_002",
                       "SE_T007_011", "SE_T007_012",
                       "SE_T012_002", "SE_T012_003", 
                       "SE_T013_002",
                       "SE_T029_001", "SE_T029_004", "SE_T029_005",
                       "SE_T056_001", "SE_T056_002")]

names(pop1970) <- c("fips", 
                    "pop_total", "pop_male",
                    "pop_65to74", "pop_over75",
                    "pop_white", "pop_black",
                    "pop_spanishorigin",
                    "pop_over25", "pop_over25_hsgrad", "pop_over25_collgrad",
                    "pop_over16", "pop_over16_employed")

pop1970$decade <- 1970

pop1970$pop_over65 <- pop1970$pop_65to74 + pop1970$pop_over75

# pop1970$pctmale <- pop1970$pop_male / pop1970$pop_total
# pop1970$pctover65 <- (pop1970$pop_65to74 + pop1970$pop_over75) / pop1970$pop_total
# 
# pop1970$pctwhite <- pop1970$pop_white/pop1970$pop_total #seems high even for 1970
# pop1970$pctblack <- pop1970$pop_black/pop1970$pop_total
# pop1970$pcthispanic <- pop1970$pop_spanishorigin / pop1970$pop_total
# pop1970$pcthsgrad <- pop1970$pop_over25_hsgrad / pop1970$pop_over25 # seems low, 63% on avg. But if we add HS and college grads for this measure, the max is >1
# pop1970$pctcollgrad <- pop1970$pop_over25_collgrad / pop1970$pop_over25 
# pop1970$pctunemploy <- 1- (pop1970$pop_over16_employed / pop1970$pop_over16)

###

pop1980 <- read.csv("original/socialexplorer/census1980/R13208700_SL11.csv", stringsAsFactors = F)

pop1980 <- pop1980[, c("Geo_FIPS",
                       "SE_T001_001", "SE_T003_002",
                       "SE_T006_011", "SE_T006_012", "SE_T006_013",
                       "SE_T012_002", "SE_T012_003",
                       "SE_T013_007",
                       "SE_T180_001", "SE_T180_003", "SE_T180_004",
                       "SE_T040_001", "SE_T040_003")]

names(pop1980) <- c("fips", 
                    "pop_total", "pop_male",
                    "pop_65to74", "pop_75to84", "pop_over85",
                    "pop_white", "pop_black",
                    "pop_spanishorigin",
                    "pop_over25", "pop_over25_hsgrad", "pop_over25_collgrad",
                    "pop_over16", "pop_over16_employed")

pop1980$decade <- 1980

pop1980$pop_over65 <- pop1980$pop_65to74 + pop1980$pop_75to84 + pop1980$pop_over85

# pop1980$pctmale <- pop1980$pop_male / pop1980$pop_total
# pop1980$pctover65 <- (pop1980$pop_65to74 + pop1980$pop_75to84 + pop1980$pop_over85) / pop1980$pop_total
# 
# pop1980$pctwhite <- pop1980$pop_white/pop1980$pop_total
# pop1980$pctblack <- pop1980$pop_black/pop1980$pop_total
# pop1980$pcthispanic <- pop1980$pop_spanishorigin / pop1980$pop_total
# pop1980$pcthsgrad <- pop1980$pop_over25_hsgrad / pop1980$pop_over25
# pop1980$pctcollgrad <- pop1980$pop_over25_collgrad / pop1980$pop_over25 
# pop1980$pctunemploy <- 1- (pop1980$pop_over16_employed / pop1980$pop_over16)

###

pop1990 <- read.csv("original/socialexplorer/census1990/R13208704_SL050.csv", stringsAsFactors = F)

pop1990 <- pop1990[, c("Geo_FIPS",
                       "SE_T001_001", "SE_T005_002",
                       "SE_T008_011", "SE_T008_012", "SE_T008_013",
                       "SE_T012_002", "SE_T012_003",
                       "SE_T013_008",
                       "SE_T117_001", "SE_T117_003", "SE_T117_005", "SE_T117_006",
                       "SE_T029_001", "SE_T029_002")]

names(pop1990) <- c("fips", 
                    "pop_total", "pop_male",
                    "pop_65to74", "pop_75to84", "pop_over85",
                    "pop_white", "pop_black",
                    "pop_spanishorigin",
                    "pop_over25", "pop_over25_hsgrad", "pop_over25_collgrad", "pop_over25_gradschool",
                    "pop_over16", "pop_over16_employed")


pop1990$decade <- 1990

pop1990$pop_over65 <- pop1990$pop_65to74 + pop1990$pop_75to84 + pop1990$pop_over85

# pop1990$pctmale <- pop1990$pop_male / pop1990$pop_total
# pop1990$pctover65 <- (pop1990$pop_65to74 + pop1990$pop_75to84 + pop1990$pop_over85) / pop1990$pop_total
# 
# pop1990$pctwhite <- pop1990$pop_white/pop1990$pop_total #seems high even for 1970
# pop1990$pctblack <- pop1990$pop_black/pop1990$pop_total
# pop1990$pcthispanic <- pop1990$pop_spanishorigin / pop1990$pop_total
# pop1990$pcthsgrad <- pop1990$pop_over25_hsgrad / pop1990$pop_over25 # seems low, 63% on avg
# pop1990$pctcollgrad <- (pop1990$pop_over25_collgrad +pop1990$pop_over25_gradschool) / pop1990$pop_over25 
# pop1990$pctunemploy <- 1- (pop1990$pop_over16_employed / pop1990$pop_over16)
###

pop2000 <- read.csv("original/socialexplorer/census2000/R13208705_SL050.csv", stringsAsFactors = F)

pop2000 <- pop2000[, c("Geo_FIPS",
                       "SE_T001_001", "SE_T005_002",
                       "SE_T008_011", "SE_T008_012", "SE_T008_013",
                       "SE_T014_002", "SE_T014_003",
                       "SE_T015_010",
                       "SE_T043_001", "SE_T043_003", "SE_T043_005", "SE_T043_006", "SE_T043_007", "SE_T043_008",
                       "SE_T073_001", "SE_T073_002")] # pop switched to "white alone" in this year

names(pop2000) <- c("fips", 
                    "pop_total", "pop_male",
                    "pop_65to74", "pop_75to84", "pop_over85",
                    "pop_white", "pop_black",
                    "pop_spanishorigin",
                    "pop_over25", "pop_over25_hsgrad", "pop_over25_collgrad", "pop_over25_master", "pop_over25_doctor", "pop_over25_professional",
                    "pop_over16", "pop_over16_employed")

pop2000$decade <- 2000

pop2000$pop_over65 <- pop2000$pop_65to74 + pop2000$pop_75to84 + pop2000$pop_over85

#pop2000$pctmale <- pop2000$pop_male / pop2000$pop_total
#pop2000$pctover65 <- (pop2000$pop_65to74 + pop2000$pop_75to84 + pop2000$pop_over85) / pop2000$pop_total

#pop2000$pctwhite <- pop2000$pop_white/pop2000$pop_total #seems high even for 1970
#pop2000$pctblack <- pop2000$pop_black/pop2000$pop_total
#pop2000$pcthispanic <- pop2000$pop_spanishorigin / pop2000$pop_total
#pop2000$pcthsgrad <- pop2000$pop_over25_hsgrad / pop2000$pop_over25 # seems low, 63% on avg
#pop2000$pctcollgrad <- (pop2000$pop_over25_collgrad +pop2000$pop_over25_master + pop2000$pop_over25_professional + pop2000$pop_over25_doctor) / pop2000$pop_over25 
#pop2000$pctunemploy <- 1- (pop2000$pop_over16_employed / pop2000$pop_over16)

###

pop2010 <- read.csv("original/socialexplorer/census2010/R13208706_SL050.csv", stringsAsFactors = F)

pop2010 <- pop2010[, c("Geo_FIPS",
                       "SE_T001_001", "SE_T003_002",
                       "SE_T008_011", "SE_T008_012", "SE_T008_013",
                       "SE_T054_002", "SE_T054_003",
                       "SE_T055_010")]

names(pop2010) <- c("fips", 
                    "pop_total", "pop_male",
                    "pop_65to74", "pop_75to84", "pop_over85",
                    "pop_white", "pop_black",
                    "pop_spanishorigin")


pop2010$decade <- 2010

pop2010$pop_over65 <- pop2010$pop_65to74 + pop2010$pop_75to84 + pop2010$pop_over85

#pop2010$pctmale <- pop2010$pop_male / pop2010$pop_total
#pop2010$pctover65 <- (pop2010$pop_65to74 + pop2010$pop_75to84 + pop2010$pop_over85) / pop2010$pop_total

#pop2010$pctwhite <- pop2010$pop_white/pop2010$pop_total #seems high even for 1970
#pop2010$pctblack <- pop2010$pop_black/pop2010$pop_total
#pop2010$pcthispanic <- pop2010$pop_spanishorigin / pop2010$pop_total
#pop2000$pcthsgrad <- pop2000$pop_over25_hsgrad / pop2000$pop_over25 # seems low, 63% on avg
#pop2000$pctcollgrad <- (pop2000$pop_over25_collgrad +pop2000$pop_over25_master + pop2000$pop_over25_professional + pop2000$pop_over25_doctor) / pop2000$pop_over25 
#pop2000$pctunemploy <- 1- (pop2000$pop_over16_employed / pop2000$pop_over16)

#pop2010$fips <- as.character(pop2010$fips)
#pop2010$fips[nchar(pop2010$fips)==4] <- paste0("0", pop2010$fips[nchar(pop2010$fips)==4])


###

common <- c("fips", "decade", "pop_total", "pop_male", "pop_over65", "pop_white", "pop_black", "pop_spanishorigin")

poplist <- lapply(list(pop1970, pop1980, pop1990, pop2000, pop2010), FUN=function(x) x[,common])

pop <- do.call(rbind, poplist)

write.csv(pop, "working/population_countylevel.csv", row.names = F)



