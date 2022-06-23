
setwd("/Users/christianbaehr/Dropbox/charisma_project/data")

congress <- read.csv("district_panel.csv")

congress$voteshare_D_House <- congress$votes_D_House / congress$totalvote_House
congress$voteshare_R_House <- congress$votes_R_House / congress$totalvote_House

congress$voteshare_D_Pres <- congress$votes_D_Pres / congress$totalvote_Pres
congress$voteshare_R_Pres <- congress$votes_R_Pres / congress$totalvote_Pres

congress$voteshare_D_Gov <- congress$votes_D_Gov / congress$totalvote_Gov
congress$voteshare_R_Gov <- congress$votes_R_Gov / congress$totalvote_Gov

congress <- congress[order(congress$state, congress$district, congress$year), ]

min_years <- tapply(congress$year, INDEX=list(paste(congress$state, congress$district)), FUN=min, na.rm=T)
if(any(min_years!=min(congress$year))) {stop("LAGGED VARIABLE WILL BE WRONG. PANEL START YEARS NOT IDENTICAL")}

congress$voteshare_D_House_l1 <- c(NA, congress$voteshare_D_House[1:(nrow(congress)-1)])
congress$voteshare_D_House_l1[congress$year==min(congress$year)] <- NA

congress$voteshare_R_House_l1 <- c(NA, congress$voteshare_R_House[1:(nrow(congress)-1)])
congress$voteshare_R_House_l1[congress$year==min(congress$year)] <- NA



View(congress[, c("year", "state", "district", "voteshare_D_House", "voteshare_D_House_l1")])








