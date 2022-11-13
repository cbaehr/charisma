
setwd("/Users/christianbaehr/Dropbox/charisma_project/data/")

library(rjson)
library(stringi)

files <- list.files("original/BioguideProfiles_1970-2020/")

grab <- function(x) {
  y <- list(cnum = x$congressAffiliation$congress$congressNumber,
            state = x$congressAffiliation$represents$regionCode,
            party = x$congressAffiliation$partyAffiliation[[1]]$party$name,
            jobtype = x$job$name)
  y[sapply(y, is.null)] <- NA
  return(y)
}

for(i in 1:length(files)) {
  
  cand <- fromJSON(file=paste0("original/BioguideProfiles_1970-2020/", files[i]))

  cdetails <- lapply(cand$jobPositions, grab)
    
  cdetails <- do.call(rbind.data.frame, cdetails)
  cdetails <- cdetails[which(cdetails$jobtype=="Representative"), ]
  
  if(nrow(cdetails)>0) {
    cdetails$year <- 1786 + 2*cdetails$cnum
    cdetails$id <- cand$usCongressBioId
    cdetails$last <- cand$familyName
    cdetails$first <- cand$givenName
    if(i==1) {incumbdf <- cdetails} else {incumbdf <- rbind(incumbdf, cdetails)}
  }
  
}


incumbdf$last <- tolower(incumbdf$last)
incumbdf$first <- tolower(incumbdf$first)

incumbdf$last <- stri_trans_general(str = incumbdf$last, id = "Latin-ASCII")
incumbdf$first <- stri_trans_general(str = incumbdf$first, id = "Latin-ASCII")

incumbdf <- aggregate(incumbdf$year, by = as.list(incumbdf[,c("state", "party", "first", "last")]), FUN=function(x) list(x))

post1970 <- lapply(incumbdf$x, FUN=function(x) if(length(x)>0) {max(x)>=1970} else{F})
incumbdf <- incumbdf[unlist(post1970), ]

names(incumbdf) <- c("state", "party", "first", "last",  "years")

save(incumbdf, file = "working/incumbentdf.Rdata")




