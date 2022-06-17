
setwd("/Users/christianbaehr/Documents/GitHub/charisma/Data/adler_congressionaldistrict/")

read_in <- function(x) {
  y <- read.csv(sprintf("fin%i.csv", x), stringsAsFactors=F)
  y$YEAR_MERGE <- (x*2) + 1786 # using the current congressional term data for the PRIOR election (i.e. 1985-86 term data goes into 1984 election)
  return(y)
}

#fin <- lapply(c(101:104), function(x) read.csv(sprintf("fin%i.csv", x), stringsAsFactors=F))
fin <- lapply(c(101:104), FUN=read_in)
#fin104 <- read.csv("fin104.csv", stringsAsFactors = F)
#fin105 <- read.csv("fin105.csv", stringsAsFactors = F)

vars <- lapply(fin, names)
common_vars <- Reduce(intersect, vars)

fin_trim <- lapply(fin, function(x) x[, common_vars])

fin_merge <- do.call(rbind, fin_trim)

#all.equal(names(fin104), names(fin105))

returns <- read.csv("/Users/christianbaehr/Documents/GitHub/charisma/Data/U.S. Senate Precinct-Level Returns 2020/1976-2020-house.csv",
                    stringsAsFactors = F)

fin_merge$STATE_MERGE <- fin_merge$FIPSTATE
returns$STATE_MERGE <- returns$state_fips

fin_merge$CD_MERGE <- fin_merge$CD
returns$CD_MERGE <- returns$district

returns$YEAR_MERGE <- returns$year


full_dat <- merge(returns, fin_merge)




