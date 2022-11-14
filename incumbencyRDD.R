
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

#elections <- read.csv("working/cd_panel_full.csv", stringsAsFactors = F)
elections <- read.csv("working/cd_panel_full_incumbency.csv", stringsAsFactors = F)

###

elec1972 <- read.csv("/Users/christianbaehr/Downloads/congress_1972_withincumblength.csv", stringsAsFactors = F)

elec1972$statenm <- tolower(elec1972$State)
elec1972$cd <- gsub("District ", "", elec1972$Area)

elections <- merge(elections, elec1972, by = c("statenm", "cd"), all.x=T)

elections$start72 <- (elections$con_raceyear - (2*elections$Incumbency) == 1972)

View(elections[, c("statenm", "cd", "con_raceyear", "Incumbency", "incumblength")])

###

elections$seat <- paste(elections$statenm, elections$cd)
elections <- elections[order(elections$seat, elections$con_raceyear), ]

###

elections$con_thirdshare <- elections$con_thirdvotes/ 
  apply(elections[,c("con_repvotes", "con_demvotes", "con_thirdvotes", "con_othervotes")], 1, sum)

#con_demshare_twopty <- elections$con_demvotes / (elections$con_demvotes + elections$con_repvotes)
elections$con_demshare_twopty <- ifelse(elections$con_demunopposed, 1,
                                        ifelse(elections$con_repunopposed, 0, con_demshare_twopty))

#con_repshare_twopty <- elections$con_repvotes / (elections$con_demvotes + elections$con_repvotes)
elections$con_repshare_twopty <- ifelse(elections$con_repunopposed, 1,
                                        ifelse(elections$con_demunopposed, 0, con_repshare_twopty))

###

sum(elections$con_pluralityvotes != abs(elections$con_demvotes - elections$con_repvotes), na.rm=T)

View(elections[which(elections$con_pluralityvotes != abs(elections$con_demvotes - elections$con_repvotes)), c("statenm", "cd", "con_raceyear", "con_demvotes", "con_repvotes", "con_thirdvotes", "con_othervotes", "con_pluralityvotes")])

View(elections[is.na(elections$con_demvotes), ])

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


elections <- read.csv("working/congress_demterm_hand_corrected_bin.csv", stringsAsFactors = F)
elections$demmv <- (elections$con_demshare - elections$con_repshare) # running variable is margin of victory in percentage points of total vote share

dfloor <- function(x) {
  return(((x-2) %/% 10) *10 + 2)
}
elections$decade <- dfloor(elections$con_raceyear)

rdd <- rdrobust(y=openseats$con_demshare_tplus1, x=openseats$demmv, c=0, bwselect = "mserd")


rdd <- rdrobust(y=elections$con_incumbency, x=elections$demmv, c=0, bwselect = "mserd")


for(i in unique(elections$decade)) {
  if(i==unique(elections$decade)[1]) {
    out <- matrix(data=NA, nrow = length(unique(elections$dem_inc_bin)), ncol = length(unique(elections$decade)))
    rownames(out) <- unique(elections$dem_inc_bin)
    colnames(out) <- unique(elections$decade)
  }
  
  for(j in unique(elections$dem_inc_bin)) {
    
    temp <- elections[which(elections$decade==i & elections$dem_inc_bin==j), ]
    
    rdd <- rdrobust(y=temp$con_incumbency, x=temp$demmv, c=0, bwselect = "mserd")
    
    out[as.character(j), as.character(i)] <- rdd$Estimate[[1]]
    
  }
}








