install.packages(c('tibble', 'dplyr', 'readr'))

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
rocio2 <- rocio2[, c("countynm","statenm", "year" , "statefips", "county" , "con_district","fips" )]
colnames(rocio2)[4] <- "state" 
colnames(rocio2)[6] <- "cd"

result <- subset(result, select=c( "countynm", "statenm",  "year", "state", "county", "cd","fips"))

merged <- rbind(result, rocio2) ## 24401 obs in total
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
