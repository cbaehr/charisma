
setwd("/Users/christianbaehr/Dropbox/charisma_project/data/")

library(rdrobust)
library(modelsummary)
library(xtable)

tidy.rdrobust <- function(model, ...) {
  ret <- data.frame(
    term = row.names(model$coef),
    estimate = model$coef[1,],
    std.error = model$se[, 1],
    p.value = model$pv[, 1],
    conf.low=model$ci[,1],
    conf.high=model$ci[,2]
  )
  row.names(ret) <- NULL
  ret
}
glance.rdrobust <- function(model, ...) {
  ret <- data.frame(
    Kernel = model$kernel,
    Bandwidth = model$bwselect,
    n = as.character(model$N[2])
  )
  ret
}

###

elections <- read.csv("working/cd_panel_full.csv", stringsAsFactors = F)

elections$decade <- sapply(elections$con_raceyear, FUN=function(x) (((x-2) %/% 10) *10 + 2))

elections$highestshare_nondem <- apply(elections[, c("con_repshare", "con_thirdshare")], 1, FUN=function(x) max(x, na.rm=T))

elections$demmv <- elections$con_demshare - elections$highestshare_nondem # difference between Dem share and next most competitive candidate

elections$incumb_running <- ifelse(elections$dem_inc_count_cumulative>0 | elections$con_repstatus=="Incumbent", 1, 0) # should I be using consecutive?

elections$con_demshare_tplus1 <- elections$incumb_running_tplus1 <- elections$demmv_tplus1 <-  NA
for(i in 1:nrow(elections)) { # creating forward-lagged dummies of Dem vote share and "incumbent running" indicator
  
  ind <- which(elections$statenm==elections$statenm[i] & 
                 elections$cd==elections$cd[i] & 
                 elections$con_raceyear == (elections$con_raceyear[i]+2) ) # find the t+1 observation for unit i
  
  if(length(ind)>0) { # if such an observation exists
    elections$con_demshare_tplus1[i] <- elections$con_demshare[ind]
    elections$incumb_running_tplus1[i] <- elections$incumb_running[ind]
    elections$demmv_tplus1[i] <- elections$demmv[ind]
  }
}

#set.seed(123)
#test <- elections[sample(rownames(elections), 10), ]
#View(test)

###

# vote share outcome, by decade

rd1 <- rdrobust(y = elections$incumb_running_tplus1, x=elections$demmv, bwselect = "msetwo") # first model - outcome is incumbent running in t+1
leftintercept <- rd1$beta_p_l[1]
rightintercept <- rd1$beta_p_r[1]

rd2 <- rdrobust(y=elections$con_demshare_tplus1, x=elections$demmv, bwselect = "msetwo") # second model - outcome is Dem vote share in t+1
rd2$coef_adj <- rd2$coef / (leftintercept + rightintercept)
rd2$ci_adj <- rd2$ci / (leftintercept + rightintercept)

out <- list(`Full Sample` = rd2)
stage1 <- matrix(ncol=7)
stats <- c(round(leftintercept+rightintercept,3), round(rd2$coef_adj[1],3), paste0("[", round(rd2$ci_adj[1,1],3), ", ", round(rd2$ci_adj[1,2],3), "]"), 
           round(rd2$beta_p_l[1],3), rd2$N_h[1], rd2$N_h[2], round(rd2$bws[1,1],3))
stage1 <- rbind(stage1, stats)

for(j in 1:length(unique(elections$decade))) {
  i <- sort(unique(elections$decade))[j]
  electemp <- elections[which(elections$decade==i),]
  
  rd1 <- rdrobust(y = electemp$incumb_running_tplus1, x=electemp$demmv, bwselect = "msetwo") # first model - outcome is incumbent running in t+1
  leftintercept <- rd1$beta_p_l[1]
  rightintercept <- rd1$beta_p_r[1]
  
  rd2 <- rdrobust(y=electemp$con_demshare_tplus1, x=electemp$demmv, bwselect = "msetwo") # second model - outcome is Dem vote share in t+1
  rd2$coef_adj <- rd2$coef / (leftintercept + rightintercept)
  rd2$ci_adj <- rd2$ci / (leftintercept + rightintercept)
  
  stats <- c(round(leftintercept+rightintercept,3), round(rd2$coef_adj[1],3), paste0("[", round(rd2$ci_adj[1,1],3), ", ", round(rd2$ci_adj[1,2],3), "]"), 
             round(rd2$beta_p_l[1],3), round(rd2$N_h[1],3), round(rd2$N_h[2],3), round(rd2$bws[1,1],3))
  stage1 <- rbind(stage1, stats)
  
  out[[as.character(i)]] <- rd2 # store the result
  
  if(j==length(unique(elections$decade))) {
    modelsummary(out, output = sprintf("../results/RDestimates_decade_voteshare_%s.tex", Sys.Date()), coef_omit= "Intercept|factor|Conventional|Bias",
                 stars=F, gof_omit = "Std|BIC|AIC|Log|Adj|R2|F|n", statistic = "conf.int", coef_rename = c("Robust"="Conventional"),
                 add_rows = data.frame(cbind(c("Incumb. Factor Estimate", "Conventional (adj.)", " ",  "Intercept left of CO", "Eff. N left of cutoff", "Eff. N right of cutoff", "Bandwidth"),
                                             matrix(stage1[-1,], nrow = ncol(stage1), byrow = T))))
  }
}

###

# Probability of Dem. victory outcome, by decade

elections$demwin_tplus1 <- (elections$demmv_tplus1>0)

rd1 <- rdrobust(y = elections$incumb_running_tplus1, x=elections$demmv, bwselect = "msetwo") # first model - outcome is incumbent running in t+1
leftintercept <- rd1$beta_p_l[1]
rightintercept <- rd1$beta_p_r[1]

rd2 <- rdrobust(y=elections$demwin_tplus1, x=elections$demmv, bwselect = "msetwo") # second model - outcome is Dem vote share in t+1
rd2$coef_adj <- rd2$coef / (leftintercept + rightintercept)
rd2$ci_adj <- rd2$ci / (leftintercept + rightintercept)

out <- list(`Full Sample` = rd2)
stage1 <- matrix(ncol=7)
stats <- c(round(leftintercept+rightintercept,3), round(rd2$coef_adj[1],3), paste0("[", round(rd2$ci_adj[1,1],3), ", ", round(rd2$ci_adj[1,2],3), "]"), 
           round(rd2$beta_p_l[1],3), rd2$N_h[1], rd2$N_h[2], round(rd2$bws[1,1],3))
stage1 <- rbind(stage1, stats)

for(j in 1:length(unique(elections$decade))) {
  i <- sort(unique(elections$decade))[j]
  electemp <- elections[which(elections$decade==i),]
  
  rd1 <- rdrobust(y = electemp$incumb_running_tplus1, x=electemp$demmv, bwselect = "msetwo") # first model - outcome is incumbent running in t+1
  leftintercept <- rd1$beta_p_l[1]
  rightintercept <- rd1$beta_p_r[1]
  
  rd2 <- rdrobust(y=electemp$demwin_tplus1, x=electemp$demmv, bwselect = "msetwo") # second model - outcome is Dem vote share in t+1
  rd2$coef_adj <- rd2$coef / (leftintercept + rightintercept)
  rd2$ci_adj <- rd2$ci / (leftintercept + rightintercept)
  
  stats <- c(round(leftintercept+rightintercept,3), round(rd2$coef_adj[1],3), paste0("[", round(rd2$ci_adj[1,1],3), ", ", round(rd2$ci_adj[1,2],3), "]"), 
             round(rd2$beta_p_l[1],3), rd2$N_h[1], rd2$N_h[2], round(rd2$bws[1,1],3))
  stage1 <- rbind(stage1, stats)
  
  out[[as.character(i)]] <- rd2 # store the result
  
  # if(j==length(unique(elections$decade))) {
  #   modelsummary(out, output = sprintf("../results/RDestimates_decade_windummy_%s.tex", Sys.Date()), coef_omit= "Intercept|factor|Conventional|Bias", 
  #                stars=F, gof_omit = "Std|BIC|AIC|Log|Adj|R2|F|n", statistic = "conf.int", coef_rename = c("Robust"="Conventional"),
  #                add_rows = data.frame(cbind(c("Incumb. Factor Estimate", "Conventional (adj.)", " ",  "Intercept left of CO", "Eff. N left of cutoff", "Eff. N right of cutoff", "Bandwidth"),
  #                                            matrix(stage1[-1,], nrow = ncol(stage1), byrow = T))))
  # }
}

###

# vote share outcome, by cumulative length of incumbency

rd1 <- rdrobust(y = elections$incumb_running_tplus1, x=elections$demmv, bwselect = "msetwo") # first model - outcome is incumbent running in t+1
leftintercept <- rd1$beta_p_l[1]
rightintercept <- rd1$beta_p_r[1]

rd2 <- rdrobust(y=elections$con_demshare_tplus1, x=elections$demmv, bwselect = "msetwo") # second model - outcome is Dem vote share in t+1
rd2$coef_adj <- rd2$coef / (leftintercept + rightintercept)
rd2$ci_adj <- rd2$ci / (leftintercept + rightintercept)

out <- list(`Full Sample` = rd2)
stage1 <- matrix(ncol=7)
stats <- c(round(leftintercept+rightintercept,3), round(rd2$coef_adj[1],3), paste0("[", round(rd2$ci_adj[1,1],3), ", ", round(rd2$ci_adj[1,2],3), "]"), 
           round(rd2$beta_p_l[1],3), rd2$N_h[1], rd2$N_h[2], round(rd2$bws[1,1],3))
stage1 <- rbind(stage1, stats)

elections$dem_inc_bin <- ifelse(elections$dem_inc_bin==0, 0, 1)
bins <- unique(na.omit(elections$dem_inc_bin))

for(j in 1:length(bins)) {
  i <- sort(bins)[j]
  electemp <- elections[which(elections$dem_inc_bin==i),]
  
  rd1 <- rdrobust(y = electemp$incumb_running_tplus1, x=electemp$demmv, bwselect = "msetwo") # first model - outcome is incumbent running in t+1
  leftintercept <- rd1$beta_p_l[1]
  rightintercept <- rd1$beta_p_r[1]
  
  rd2 <- rdrobust(y=electemp$con_demshare_tplus1, x=electemp$demmv, bwselect = "msetwo") # second model - outcome is Dem vote share in t+1
  rd2$coef_adj <- rd2$coef / (leftintercept + rightintercept)
  rd2$ci_adj <- rd2$ci / (leftintercept + rightintercept)
  
  stats <- c(round(leftintercept+rightintercept,3), round(rd2$coef_adj[1],3), paste0("[", round(rd2$ci_adj[1,1],3), ", ", round(rd2$ci_adj[1,2],3), "]"), 
             round(rd2$beta_p_l[1],3), rd2$N_h[1], rd2$N_h[2], round(rd2$bws[1,1],3))
  stage1 <- rbind(stage1, stats)
  
  #out[[c("Open", "1", "2-3", "4+")[j]]] <- rd2 # store the result
  out[[c("Open", "1+")[j]]] <- rd2 # store the result
  # if(j==length(bins)) {
  #   modelsummary(out, output = sprintf("../results/RDestimates_incumblgth_voteshare_%s.tex", Sys.Date()), coef_omit= "Intercept|factor|Conventional|Bias", 
  #                stars=F, gof_omit = "Std|BIC|AIC|Log|Adj|R2|F|n", statistic = "conf.int", coef_rename = c("Robust"="Conventional"),
  #                add_rows = data.frame(cbind(c("Incumb. Factor Estimate", "Conventional (adj.)", " ",  "Intercept left of CO", "Eff. N left of cutoff", "Eff. N right of cutoff", "Bandwidth"),
  #                                            matrix(stage1[-1,], nrow = ncol(stage1), byrow = T))))
  # }
}

###

# prob of victory outcome, by cumulative length of incumbency

rd1 <- rdrobust(y = elections$incumb_running_tplus1, x=elections$demmv, bwselect = "msetwo") # first model - outcome is incumbent running in t+1
leftintercept <- rd1$beta_p_l[1]
rightintercept <- rd1$beta_p_r[1]

rd2 <- rdrobust(y=elections$demwin_tplus1, x=elections$demmv, bwselect = "msetwo") # second model - outcome is Dem vote share in t+1
rd2$coef_adj <- rd2$coef / (leftintercept + rightintercept)
rd2$ci_adj <- rd2$ci / (leftintercept + rightintercept)

out <- list(`Full Sample` = rd2)
stage1 <- matrix(ncol=7)
stats <- c(round(leftintercept+rightintercept,3), round(rd2$coef_adj[1],3), paste0("[", round(rd2$ci_adj[1,1],3), ", ", round(rd2$ci_adj[1,2],3), "]"), 
           round(rd2$beta_p_l[1],3), rd2$N_h[1], rd2$N_h[2], round(rd2$bws[1,1],3))
stage1 <- rbind(stage1, stats)

bins <- unique(na.omit(elections$dem_inc_bin))
for(j in 1:length(bins)) {
  i <- sort(bins)[j]
  electemp <- elections[which(elections$dem_inc_bin==i),]
  
  rd1 <- rdrobust(y = electemp$incumb_running_tplus1, x=electemp$demmv, bwselect = "msetwo") # first model - outcome is incumbent running in t+1
  leftintercept <- rd1$beta_p_l[1]
  rightintercept <- rd1$beta_p_r[1]
  
  rd2 <- rdrobust(y=electemp$demwin_tplus1, x=electemp$demmv, bwselect = "msetwo") # second model - outcome is Dem vote share in t+1
  rd2$coef_adj <- rd2$coef / (leftintercept + rightintercept)
  rd2$ci_adj <- rd2$ci / (leftintercept + rightintercept)
  
  stats <- c(round(leftintercept+rightintercept,3), round(rd2$coef_adj[1],3), paste0("[", round(rd2$ci_adj[1,1],3), ", ", round(rd2$ci_adj[1,2],3), "]"), 
             round(rd2$beta_p_l[1],3), rd2$N_h[1], rd2$N_h[2], round(rd2$bws[1,1],3))
  stage1 <- rbind(stage1, stats)
  
  #out[[c("Open", "1", "2-3", "4+")[j]]] <- rd2 # store the result
  out[[c("Open", "1+")[j]]] <- rd2 # store the result
  
  # if(j==length(bins)) {
  #   modelsummary(out, output = sprintf("../results/RDestimates_incumblgth_windummy_%s.tex", Sys.Date()), coef_omit= "Intercept|factor|Conventional|Bias", 
  #                stars=F, gof_omit = "Std|BIC|AIC|Log|Adj|R2|F|n", statistic = "conf.int", coef_rename = c("Robust"="Conventional"),
  #                add_rows = data.frame(cbind(c("Incumb. Factor Estimate", "Conventional (adj.)", " ",  "Intercept left of CO", "Eff. N left of cutoff", "Eff. N right of cutoff", "Bandwidth"),
  #                                            matrix(stage1[-1,], nrow = ncol(stage1), byrow = T))))
  # }
}


###########################################################################

elections$highestshare_nonrep <- apply(elections[, c("con_demshare", "con_thirdshare")], 1, FUN=function(x) max(x, na.rm=T))
elections$repmv <- elections$con_repshare - elections$highestshare_nonrep

elections$incumb_voteshare <- NA
elections$incumb_voteshare[which(elections$dem_inc_count_consecutive>0)] <- elections$demmv[which(elections$dem_inc_count_consecutive>0)]
elections$incumb_voteshare[which(elections$con_repstatus=="Incumbent")] <- elections$repmv[which(elections$con_repstatus=="Incumbent")]

elections$incumb_win <- (elections$incumb_voteshare>0) * elections$incumb_running

reelection_rates <- tapply(elections$incumb_win[which(elections$incumb_running==1)], 
                           INDEX = elections$con_raceyear[which(elections$incumb_running==1)], 
                           FUN=function(x) mean(x, na.rm=T))
reelection_rates <- data.frame(year=names(reelection_rates), rate = round(reelection_rates, digits = 2))

elections$demwin <- elections$demmv>0

demwin <- tapply(elections$demwin, INDEX=elections$con_raceyear, FUN=function(x) sum(x, na.rm=T))
demwin <- data.frame(year=names(demwin), seats = demwin)
demseats <- read.csv(file = "original/summary/demhouseseats.csv")
demseats$Congress[demseats$Years=="1991-1992"] <- 102
demseats$Seats[demseats$Congress==116 & demseats$Region=="Plains"] <- 21
demseats$demseats <- demseats$Seats * (demseats$Percent/100)
demseats <- aggregate(demseats$demseats, by=list(demseats$Congress), FUN=function(x)as.character(round(sum(x))))
demseats$year <- 1786+2*demseats$Group.1

demwin <- merge(demwin, demseats, by="year")
demwin <- merge(demwin, reelection_rates, by="year")
demwin <- demwin[, c("year", "seats", "x", "rate")]
names(demwin) <- c("Year", "Dem. Seats (our data)", "Dem. Seats (valid)", "Incumb. Reelec Pct.")
print.xtable(xtable(demwin, type="latex"), file="../results/demstats.tex", include.rownames = F)

elections$openseat <- (elections$incumb_running==0)
os <- tapply(elections$openseat, INDEX=elections$con_raceyear, FUN = function(x) sum(x, na.rm=T))
os <- data.frame(year=names(os), seats = os)
openseats <- data.frame(matrix(c(2000, 35,
                                 2002, 45,
                                 2004, 34,
                                 2006, 32, 
                                 2008, 37,
                                 2010, 41,
                                 2012, 73,
                                 2014, 46,
                                 2016, 47,
                                 2018, 63,
                                 2020, 48), ncol = 2, byrow=T))
os <- merge(os, openseats, by.x = "year", by.y = "X1")
os$X2 <- as.character(round(os$X2))
names(os) <- c("year", "open seats (our data)", "open seats (wiki)")

print.xtable(xtable(os, type="latex"), file="../results/openseats.tex", include.rownames = F)













