#install.packages(c('tibble', 'dplyr', 'readr'))

setwd("/Users/christianbaehr/Dropbox/charisma_project/data")

rocio1 <- read.csv("condistrict_to_county_mapping_withcountynames_1952-82.csv")
rocio2 <- read.csv("condistrict_to_county_mapping_withcountynames_1992-2012.csv")

##column name
colnames(rocio2)[3] <- "statenm"
colnames(rocio2)[12] <- "countynm"


##cleaning rocio2's statenames
new_str <- gsub('_',' ',rocio2$statenm)
rocio2$statenm <- new_str



#extracting unique values and merging data
unique <- unique(rocio2[c("fips","countynm", "statenm")])
result <- merge(rocio1, unique, by=c("countynm", "statenm"), all.x = TRUE)

write.csv(result,"condistrict_to_county_mapping_withcountynames_1952-82_fips_added.csv")
summary(is.na(result$fips)) ## 272 rows with missing fips



####combining result and rocio2
rocio2 <- rocio2[, c("countynm","statenm", "year" , "statefips", "county" , "con_district", "fips", "unit_pop", "countypop", "unit_weight")]
colnames(rocio2)[4] <- "state" 
colnames(rocio2)[6] <- "cd"

names(result)[names(result)=="county_prop"] <- "unit_weight"
result$unit_pop <- NA
result$countypop <- NA

result <- subset(result, select=c( "countynm", "statenm",  "year", "state", "county", "cd","fips", "unit_pop", "countypop", "unit_weight"))

merged <- rbind(result, rocio2) ## 24401 obs in total

###

# want to utilize the fips codes that Dahyun manually added to this dataset
fips <- read.csv("condistrict_to_county_mapping_withcountynames_1952-2012_fips_added_nomissingness_DONOTEDIT.csv", stringsAsFactors = F)

temp <- merge(merged, fips[,c("countynm", "statenm", "year", "cd", "fips")], by=c("countynm", "statenm", "cd", "year"), all.x=T)

temp$fips.x[is.na(temp$fips.x)] <- temp$fips.y[is.na(temp$fips.x)]

temp <- temp[, names(temp)!="fips.y"]
names(temp)[names(temp)=="fips.x"] <- "fips"

merged <- temp

###

write.csv(merged, "condistrict_to_county_mapping_withcountynames_1952-2012_fips_added.csv")

summary(merged)


##summary statistics
remove.packages("rlang")
install.packages("rlang")
library(rlang)
library(dplyr)
tapply(merged$statenm,summary) 

summary1<- merged %>%
  group_by(merged$statenm, merged$year) %>%
  summarise(n = n())

library(reshape2)
summary2<- dcast(summary1,summary1$`merged$statenm`~summary1$`merged$year`)


library(xtable)
xtable(as.data.frame(summary2))

 
missingfips <- subset(merged, is.na(result$fips)== TRUE)

summary1<- missingfips %>%
  group_by(missingfips$statenm, missingfips$year) %>%
  summarise(n = n())

summary2<- dcast(summary1, summary1$`missingfips$statenm`~ summary1$`missingfips$year`)


##########

dat <- read.csv("/Users/christianbaehr/Dropbox/charisma_project/data/condistrict_to_county_mapping_withcountynames_1952-2012_fips_added.csv", stringsAsFactors = F)

dat$cd <- as.numeric(dat$cd)

out <- aggregate(dat$countynm, by=list(dat$year, dat$statenm, dat$cd), FUN=function(x) length(unique(x)))

stateout <- aggregate(out$x, by=list(out$Group.1, out$Group.2), FUN=function(x) sum(!x>1)*100/length(x))
stateout$x <- paste0(round(stateout$x))

fulltest <- reshape(stateout, direction="wide", idvar=c("Group.2"), timevar="Group.1")
rownames(fulltest) <- seq_len(nrow(fulltest))

names(fulltest) <- c("state", seq(1952, 2012, 10))
print(xtable::xtable(fulltest, type = "latex"))

###

dat$dupcounty <- duplicated(dat[, c("countynm", "statenm", "year")])

out <- aggregate(dat$dupcounty, by=list(dat$year, dat$statenm, dat$cd), FUN=function(x) sum(x) > 0)

stateout <- aggregate(out$x, by=list(out$Group.1, out$Group.2), FUN=function(x) sum(x)*100/length(x))
stateout$x <- paste0(round(stateout$x))


fulltest <- reshape(stateout, direction="wide", idvar=c("Group.2"), timevar="Group.1")
rownames(fulltest) <- seq_len(nrow(fulltest))

names(fulltest) <- c("state", seq(1952, 2012, 10))

print(xtable::xtable(fulltest, type = "latex"))





