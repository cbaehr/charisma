
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


elections <- read.csv("working/cd_panel_full.csv", stringsAsFactors = F)

elections$seat <- paste(elections$statenm, elections$cd)
elections <- elections[order(elections$seat, elections$con_raceyear), ]

elections$con_demshare_twopty <- elections$con_demvotes / (elections$con_demvotes + elections$con_repvotes)
elections$con_repshare_twopty <- elections$con_repvotes / (elections$con_demvotes + elections$con_repvotes)

for(i in 1:nrow(elections)) { 
  if(i==1) {
    seat <- elections$seat[i]
    seatdat <- elections[which(elections$seat==seat), ]
    elections$con_demshare_twopty_tplus1 <- elections$con_repshare_twopty_tplus1 <- NA
    elections$con_demshare_tplus1 <- elections$con_repshare_tplus1 <- NA
    elections$con_demvotes_tplus1 <- elections$con_repvotes_tplus1 <- elections$con_thirdvotes_tplus1 <- NA
    elections$con_demcandidate_tplus1 <- elections$con_repcandidate_tplus1 <- NA
    
  } else if(elections$seat[i]!=seat) {
    seat <- elections$seat[i]
    seatdat <- elections[which(elections$seat==seat), ]
  }
  tplus1 <- elections$con_raceyear[i] + 2 # the election year of t+1
  ind <- which(seatdat$con_raceyear==tplus1)
  if(length(ind)>0) {
    #computing the one period forward lagged value of vote share
    elections$con_demshare_tplus1[i] <- seatdat$con_demshare[ind]
    elections$con_repshare_tplus1[i] <- seatdat$con_repshare[ind]
    elections$con_demshare_twopty_tplus1[i] <- seatdat$con_demshare_twopty[ind]
    elections$con_repshare_twopty_tplus1[i] <- seatdat$con_repshare_twopty[ind]
    elections$con_demvotes_tplus1[i] <- seatdat$con_demvotes[ind]
    elections$con_repvotes_tplus1[i] <- seatdat$con_repvotes[ind]
    elections$con_thirdvotes_tplus1[i] <- seatdat$con_thirdvotes[ind]
    elections$con_demcandidate_tplus1[i] <- seatdat$con_demcandidate[ind]
    elections$con_repcandidate_tplus1[i] <- seatdat$con_repcandidate[ind]
  }
}

openseats <- elections[which(elections$con_repstatus=="Challenger" & elections$con_demstatus=="Challenger"), ] 

south <- c("alabama", "arkansas", "florida", "georgia", "kentucky", "louisiana", "maryland", "mississippi", "north carolina", "oklahoma", "south carolina", "tennessee", "texas", "virginia", "west virginia")
openseats <- openseats[!(openseats$statenm %in% south), ] # excluding southern states

# drop cases when redistricting occurs between open seat election and freshman election
openseats <- openseats[!(openseats$con_raceyear %% 10==0), ]

demincumbent <- (openseats$con_demshare_twopty > openseats$con_repshare_twopty) & openseats$con_demcandidate==openseats$con_demcandidate_tplus1
repincumbent <- (openseats$con_demshare_twopty < openseats$con_repshare_twopty) & openseats$con_repcandidate==openseats$con_repcandidate_tplus1

openseats$I <- ifelse(demincumbent, 1,
                      ifelse(repincumbent, -1, 0))
openseats <- openseats[which(openseats$I!=0), ] # only use obs where incumbent is running in t+1

bernie <- (openseats$con_demvotes_tplus1 < openseats$con_thirdvotes_tplus1) & (openseats$con_repvotes_tplus1 < openseats$con_thirdvotes_tplus1) # NOT USING TWO PARTY VOTE SHARE BC ROCIO QJPS DID NOT
openseats <- openseats[!bernie, ]

openseats$demmv <- (openseats$con_demshare - openseats$con_repshare) # running variable is margin of victory in percentage points of total vote share
# NOT USING TWO PARTY VOTE SHARE BC ROCIO QJPS DID NOT

openseats$con_demshare_tplus1 <- openseats$con_demshare_tplus1*100 # multiply covars by 100 to match Rocios variables
openseats$con_demshare <- openseats$con_demshare*100
openseats$pres_demshare <- openseats$pres_demshare*100

# do we want to estimate only for a single decade? Since congressional districts change from one decade
# to the next

# should we omit races where incumbent was unopposed in t+1?
openseats <- openseats[which(!(openseats$con_demshare_tplus1 %in% c(0, 100))), ]

### COMPARE ESTIMATES TO QJPS 2015 ###

rocio <- read.csv("/Users/christianbaehr/Downloads/100/EriksonTitiunik-QJPS-data.csv", stringsAsFactors = F)
rocio <- rocio[rocio$use==1, ]

pdf("../results/RDDvars_hist.pdf", width=11, height=8)
par(mfrow=c(2, 2))
hist(rocio$demmvlag, breaks = 8, main = "QJPS 2015", xlab=" ", yaxt="n", ylab="")
hist(openseats$demmv, breaks=8, main="Our sample", xlab=" ", yaxt="n", ylab="")
mtext("Democratic margin of victory in period t", side=1, outer=T, line=-26)
hist(rocio$demvotesh, breaks = 8, main = " ", xlab=" ", yaxt="n", ylab="")
hist(openseats$con_demshare_tplus1, breaks=8, main=" ", xlab=" ", yaxt="n", ylab="")
mtext("Democratic vote share t+1", side=1, outer=T, line=-2)
dev.off()


###

lm1 <- lm(con_demshare_tplus1 ~ I + con_demshare, data = openseats)
lm2 <- lm(con_demshare_tplus1 ~ I + con_demshare + factor(con_raceyear), data = openseats)
lm3 <- lm(con_demshare_tplus1 ~ I + con_demshare + pres_demshare + factor(con_raceyear), data = openseats)

models <- list(lm1=lm1, lm2=lm2, lm3=lm3)

rdd <- rdrobust(y=openseats$con_demshare_tplus1, x=openseats$demmv, c=0, bwselect = "mserd")
rdd$coef <- rdd$coef/2 # divide effect by two
rdd$ci <- rdd$ci/2
models[["rdall"]] <- rdd

for(i in sort(unique(openseats$decade))) {
  
  rdd <- rdrobust(y=openseats$con_demshare_tplus1[openseats$decade==i],
                  x=openseats$demmv[openseats$decade==i], c=0, bwselect = "mserd")
  rdd$coef <- rdd$coef/2 # divide effect by two
  rdd$ci <- rdd$ci/2
  
  models[[paste0("rd", i)]] <- rdd
  
}

modelsummary(models, 
             output = "../results/RDDestimates.tex", 
             coef_omit= "Intercept|factor|Conventional|Bias", stars=F,
             gof_omit = "Std|BIC|AIC|Log|Adj|R2|F",
             statistic = "conf.int",
             add_rows = data.frame(matrix(c(
               "Year FE", " ", "X", "X", " ", " ", " ", " ", " ", " "
             ), ncol = 10, byrow=T)))

###

dfloor <- function(x) {return(((x-2) %/% 10) *10 + 2)}

rocio$decade <- dfloor(as.numeric(rocio$year))

models <- list()

rdd <- rdrobust(y=rocio$demvotesh, x=rocio$demmvlag, c=0, bwselect = "mserd") 
rdd$coef <- rdd$coef/2 # divide effect by two
rdd$ci <- rdd$ci/2

models[["rdall"]] <- rdd # rdd using using 196 obs. Checked with STATA replication

for(i in sort(unique(rocio$decade))) {
  
  rdd <- rdrobust(y=rocio$demvotesh[rocio$decade==i],
                  x=rocio$demmvlag[rocio$decade==i], c=0, bwselect = "mserd")
  rdd$coef <- rdd$coef/2 # divide effect by two
  rdd$ci <- rdd$ci/2
  
  models[[paste0("rd", i)]] <- rdd
  
}

modelsummary(models, 
             output = "../results/RDDestimates_QJPSreplication.tex", 
             coef_omit= "Intercept|factor|Conventional|Bias", stars=F,
             gof_omit = "Std|BIC|AIC|Log|Adj|R2|F",
             statistic = "conf.int")

# openseats$cd <- ifelse(nchar(openseats$cd)==2, openseats$cd, paste0("0", openseats$cd))
# openseats$state <- match(openseats$statenm, tolower(state.name))
# openseats$stcd <- paste0(openseats$state, openseats$cd)
# 
# openseats$rocioyear <- openseats$con_raceyear + 2
# openseats$stcdyr <- paste0(openseats$stcd, openseats$rocioyear)
# rocio$stcdyr <- paste0(rocio$stcd, rocio$year)
# 
# test <- merge(openseats[, c("statenm", "cd", "con_raceyear", "stcdyr", "con_demshare_cutoff", "con_demshare_tplus1")],
#               rocio[, c("stcdyr", "demvotesh", "demmvlag")],
#               by="stcdyr")
# View(test)
# 
# mod1_r <- rdrobust(y=rocio$demvotesh[rocio$use==1], x = rocio$demmvlag[rocio$use==1], c=0)
# summary(mod1_r)
# 
# hist(rocio$demmvlag[rocio$use==1])
# hist(openseats$con_demshare_twopty[openseats$con_raceyear<2010])
# hist(openseats$con_demshare_twopty[openseats$stcdyr %in% rocio$stcdyr] - 0.5)
# 
# summary(openseats$con_demshare_twopty_tplus1[openseats$stcdyr %in% rocio$stcdyr])
# summary(rocio$demvotesh[rocio$use==1 & rocio$stcdyr %in% openseats$stcdyr])
# 
# mod1_r <- rdrobust(y=rocio$demvotesh[rocio$use==1 & rocio$stcdyr %in% openseats$stcdyr],
#                    x = rocio$demmvlag[rocio$use==1 & rocio$stcdyr %in% openseats$stcdyr],
#                    c=0)
# summary(mod1_r)
# 
# mod1 <- rdrobust(y=openseats$con_demshare_tplus1[openseats$stcdyr %in% rocio$stcdyr],
#                  x=openseats$con_demshare_cutoff[openseats$stcdyr %in% rocio$stcdyr])
# summary(mod1)






