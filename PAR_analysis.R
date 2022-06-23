
set.seed(123)

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

#View(congress[, c("year", "state", "district", "voteshare_D_House", "voteshare_D_House_l1")])

##########

unique(congress$status_D_House)

openseats <- congress[which(congress$status_D_House=="Challenger" & congress$status_R_House=="Challenger"), ]

# linear cubic spline for incumbency advantage

library(glmnet)

### DEM ###

demo <- openseats[, c("voteshare_D_House", "voteshare_D_Pres", "voteshare_D_Gov",
                      "gini", "mean_income", "pct_unemp", "pct_hsgrads", "pct_BAdegree", "pct_over60",
                      "totalpop", "pct_black", "pct_hispanic", "pct_white",
                      "voteshare_D_House_l1")]

demo <- na.omit(demo)
rownames(demo) <- c(1:nrow(demo))

DT <- sample(rownames(demo), replace=F, size=0.6*nrow(demo))
X_D_test <- as.matrix(demo[as.numeric(DT), -1])
X_D_val <- as.matrix(demo[!rownames(demo) %in% DT, -1])

y_D_test <- demo$voteshare_D_House[as.numeric(DT)]
y_D_val <- demo$voteshare_D_House[!rownames(demo) %in% DT]

# ridge
grid <-  10^seq(10, -2, length = 100)
mod <- glmnet(X_D_test, y_D_test, alpha=0, lambda=grid)









