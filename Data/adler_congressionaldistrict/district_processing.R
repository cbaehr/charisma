
setwd("/Users/christianbaehr/Downloads/adler_congressionaldistrict/")

fin105 <- read.csv("fin105.csv", stringsAsFactors = F)
fin104 <- read.csv("fin105.csv", stringsAsFactors = F)

#all.equal(names(fin104), names(fin105))
demo_combined <- rbind(fin104, fin105)

returns <- read.csv("/Users/christianbaehr/Documents/GitHub/charisma/Data/U.S. Senate Precinct-Level Returns 2020/1976-2020-house.csv",
                    stringsAsFactors = F)

demo_combined$STATE_MERGE <- demo_combined$FIPSTATE
returns$STATE_MERGE <- returns$state_fips

demo_combined$CD_MERGE <- demo_combined$CD
returns$STATE_MERGE <- returns$state_fips


merge(returns, demo_combined)

