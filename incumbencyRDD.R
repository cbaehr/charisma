
setwd("/Users/christianbaehr/Dropbox/charisma_project/data/")

library(rdrobust)
library(modelsummary)

tidy.rdrobust <- function(model, ...) {
  ret <- data.frame(
    term = row.names(model$coef),
    estimate = model$coef[, 1],
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

elections$majorshare_nondem <- apply(elections[, c("con_repshare", "con_thirdshare")], 1, FUN=function(x) max(x, na.rm=T))

elections$demmv <- elections$con_demshare - elections$majorshare_nondem

elections$dem_incumb_bin <- ifelse(elections$con_dem_inc_count==0, 1,
                                   ifelse(elections$con_dem_inc_count==1, 2,
                                          ifelse(elections$con_dem_inc_count >=2 & elections$con_dem_inc_count <=3, 3, 4)))

elections$incumb_running <- ifelse(elections$con_dem_inc_count>0 | elections$con_repstatus=="Incumbent", 1, 0)

elections$con_demshare_tplus1 <- elections$incumb_running_tplus1 <-  NA
for(i in 1:nrow(elections)) { # creating forward-lagged dummies of Dem vote share and "incumbent running" indicator
  
  ind <- which(elections$statenm==elections$statenm[i] & 
                 elections$cd==elections$cd[i] & 
                 elections$con_raceyear == (elections$con_raceyear[i]+2) ) # find the t+1 observation for unit i
  
  if(length(ind)>0) { # if such an observation exists
    elections$con_demshare_tplus1[i] <- elections$con_demshare[ind]
    elections$incumb_running_tplus1[i] <- elections$incumb_running[ind]
  }
}

###

rd1 <- rdrobust(y = elections$incumb_running_tplus1, x=elections$demmv) # first model - outcome is incumbent running in t+1

rd2 <- rdrobust(y=elections$con_demshare_tplus1, x=elections$demmv) # second model - outcome is Dem vote share in t+1

rd2$Estimate <- rd2$Estimate / (rd1$beta_p_l + rd1$beta_p_r)
rd2$ci <- rd2$ci / (rd1$beta_p_l + rd1$beta_p_r)

out <- list()
stage1 <- c()
for(i in sort(unique(elections$decade))) {
  
  electemp <- elections[which(elections$decade==i),]
  
  rd1 <- rdrobust(y = elections$incumb_running_tplus1, x=elections$demmv) # first model - outcome is incumbent running in t+1
  leftintercept <- rd1$beta_p_l
  rightintercept <- rd1$beta_p_r
  stage1 <- c(stage1, rd1$Estimate[2])
  
  rd2 <- rdrobust(y=elections$con_demshare_tplus1, x=elections$demmv) # second model - outcome is Dem vote share in t+1
  
  rd2$Estimate <- rd2$Estimate / (leftintercept + rightintercept)
  rd2$ci <- rdd2$ci / (leftintercept + rightintercept)
  
  out[[as.character(i)]] <- rd2 # store the result
  
  modelsummary(out,
               output = sprintf("../results/RDestimates_decade_%s.tex", i),
               coef_omit= "Intercept|factor|Conventional|Bias", stars=F,
               gof_omit = "Std|BIC|AIC|Log|Adj|R2|F",
               statistic = "conf.int",
               add_rows = data.frame(matrix(c("Incumb. RD Estimate", stage1), ncol = 1+length(stage1), byrow=T)))
}


  


