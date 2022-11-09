
setwd("/Users/christianbaehr/Dropbox/charisma_project/data/")

library(rjson)

files <- list.files("original/BioguideProfiles_1970-2020/")

for(i in 1:length(files)) {
  
  if(i ==1) {cong <- list()}
  
  cand <- fromJSON(file=paste0("original/BioguideProfiles_1970-2020/", files[i]))
  
  cnums <- lapply(cand$jobPositions, FUN=function(x) {x$congressAffiliation$congress$congressNumber})
  cnums <- unlist(cnums)
  
  yrs <- 1786 + 2*cnums
  
  cong[[i]] <- list(id=cand$usCongressBioId, lastname=cand$familyName, 
                    firstname=cand$givenName, congressyrs=c(yrs))
  
}

congdf <- do.call(rbind, cong)
congdf <- data.frame(congdf)

names(congdf) <- c("id", "last", "first", "elecyrs")

congdf$last <- tolower(congdf$last)
congdf$first <- tolower(congdf$last)

save(congdf, file = "working/incumbentdf.Rdata")
