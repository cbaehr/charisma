
setwd("/Users/christianbaehr/Dropbox/charisma_project/data")

congress <- read.csv("U.S. housePrecinct-Level Returns 2020/1976-2020-house.csv", stringsAsFactors = F)
congress$runoff[is.na(congress$runoff)] <- F

congress <- congress[which(congress$stage=="GEN" & congress$runoff==F & congress$special==F & congress$party %in% c("DEMOCRAT", "REPUBLICAN") & congress$fusion_ticket==F), ]

congress$elec_id <- paste0(congress$state_po, ".", congress$district, "_", congress$year)

# keeping only the candidate from each party with the greatest vote share in a given election
congress <- congress[order(congress$elec_id, congress$party, congress$candidatevotes, decreasing=T), ]
congress <- congress[which(!duplicated(congress[,c("elec_id", "party")])), ]

congress <- congress[, c("elec_id", "year", "state_po", "district", "candidate", "party", "candidatevotes", "totalvotes")]

congress$party <- ifelse(congress$party == "DEMOCRAT", "D", "R")

congress_wide <- reshape(congress, direction="wide", timevar="party", idvar="elec_id", v.names=c("candidate", "candidatevotes"))


##########


cong <- do.call(rbind, list(read.csv("congressdata/congressdata_1962-1980.csv", stringsAsFactors=F, skip=2), 
                            read.csv("congressdata/congressdata_1982-2000.csv", stringsAsFactors=F, skip=2), 
                            read.csv("congressdata/congressdata_2002-2020.csv", stringsAsFactors=F, skip=2)))

cong <- cong[which(cong$State!="" & cong$State!="State" & cong$Area!="At Large"), ]
cong$state_po <- state.abb[match(cong$State, state.name)]
cong$district <- trimws(sapply(cong$Area, function(x) substr(x, nchar(x)-1, nchar(x))))

cong$elec_id <- paste0(cong$state_po, ".", cong$district, "_", cong$raceYear)

cong <- cong[, c("elec_id", "RepStatus", "DemStatus")]

congress_wide <- merge(congress_wide, cong, by="elec_id")


##########


demo <- read.csv("district_ses/allCongressDataPublishV2.csv", stringsAsFactors = F)
demo <- demo[!duplicated(demo), ]

demo$year <- 1786 + (demo$congNum*2)

demo$elec_id <- paste(demo$stateDist, demo$year, sep="_")

demo <- demo[, c("elec_id", "numberTerms", "fracServed", "meanIncome", "gini", "totalPopRaceFile", "prcntUnemp", "prcntBA", "prcntHS", 
                 "prcntBlackNotHisp", "prcntWhite", "prcntHisp", "prcntOld", "medianAge", "lastName")]

dat <- merge(congress_wide, demo, by="elec_id")

dat$candidate.R <- gsub(" SR| JR| IV| III| II", "", dat$candidate.R)
dat$candidate.D <- gsub(" SR| JR| IV| III| II", "", dat$candidate.D)

dat$candlast.R <- sub(".*\\s", "", trimws(dat$candidate.R))
dat$candlast.D <- sub(".*\\s", "", trimws(dat$candidate.D))


##########


dat$candlast.R[dat$lastName=="Bachus" & dat$candlast.R=="BAUCHUS"] <- "BACHUS"
dat$candlast.R[dat$lastName=="LaMalfa" & dat$candlast.R=="MALFA"] <- "LAMALFA"
dat$candlast.R[dat$lastName=="Bono Mack" & dat$candlast.R=="MACK"] <- "BONO MACK"
dat$candlast.R[dat$lastName=="Dannemeyer" & dat$candlast.R=='"BILL"DANNEMEYER'] <- "DANNEMEYER"
dat$candlast.R[dat$lastName=="Issa" & dat$candlast.R=="ESSA"] <- "ISSA"
dat$candlast.R[dat$lastName=="Tipton" & dat$candlast.R=="TIPTIN"] <- "TIPTON"
dat$candlast.R[dat$lastName=="Bono Mack" & dat$candlast.R=="BONO"] <- "BONO MACK"
dat$candlast.R[dat$lastName=="Chenoweth-Hage" & dat$candlast.R=="CHENOWETH"] <- "CHENOWETH-HAGE"
dat$candlast.R[dat$lastName=="Fawell" & dat$candlast.R=="FAREWELL"] <- "FAWELL"
dat$candlast.R[dat$lastName=="Ryun" & dat$candlast.R=="RYAN"] <- "RYUN"
dat$candlast.R[dat$lastName=="Jenkins" & dat$candlast.R=="JENKINKS"] <- "JENKINS"
dat$candlast.R[dat$lastName=="Vander Jagt" & dat$candlast.R=="JAGT"] <- "VANDER JAGT"
dat$candlast.R[dat$lastName=="Quie" & dat$candlast.R=="QUIC"] <- "QUIE"
dat$candlast.R[dat$lastName=="Nunnelee" & dat$candlast.R=="NUNELEE"] <- "NUNNELEE"
dat$candlast.R[dat$lastName=="Hinson" & dat$candlast.R=="Henson"] <- "HINSON"
dat$candlast.R[dat$lastName=="Marlenee" & dat$candlast.R=="MARLENCE"] <- "MARLENEE"
dat$candlast.R[dat$lastName=="Marlenee" & dat$candlast.R=="MARIENEE"] <- "MARLENEE"
dat$candlast.R[dat$lastName=="Gillmor" & dat$candlast.R=="GILMOR"] <- "GILLMOR"
dat$candlast.R[dat$lastName=="McEwen" & dat$candlast.R=="MCEWAN"] <- "MCEWEN"
dat$candlast.R[dat$lastName=="Cremeans" & dat$candlast.R=="CREMANS"] <- "CREMEANS"
dat$candlast.R[dat$lastName=="Barton" & dat$candlast.R=="BARON"] <- "BARTON"
dat$candlast.R[dat$lastName=="Greene Waldholtz" & dat$candlast.R=="WALDHOTZ"] <- "GREENE WALDHOLTZ"
dat$candlast.R[dat$lastName=="Herrera Beutler" & dat$candlast.R=="HERRERA"] <- "HERRERA BEUTLER"
dat$candlast.R[dat$lastName=="Herrera Beutler" & dat$candlast.R=="BEUTLER"] <- "HERRERA BEUTLER"
dat$candlast.R[dat$lastName=="McMorris Rodgers" & dat$candlast.R=="MCMORRIS"] <- "MCMORRIS RODGERS"
dat$candlast.R[dat$lastName=="McMorris Rodgers" & dat$candlast.R=="RODGERS"] <- "MCMORRIS RODGERS"
dat$candlast.R[dat$lastName=="Bucshon" & dat$candlast.R=="BUCHSON"] <- "BUCSHON"

dat$candlast.D[dat$lastName=="Van Deerlin" & dat$candlast.D=="DEERLIN"] <- "VAN DEERLIN"
dat$candlast.D[dat$lastName=="Roybal-Allard" & dat$candlast.D=="ROYABL-ALLARD"] <- "ROYBAL-ALLARD"
dat$candlast.D[dat$lastName=="Wasserman Schultz" & dat$candlast.D=="SCHULTZ"] <- "WASSERMAN SCHULTZ"
dat$candlast.D[dat$lastName=="Evans" & dat$candlast.D=="EVAMS"] <- "EVANS"
dat$candlast.D[dat$lastName=="Lipinski" & dat$candlast.D=="LIPINKSI"] <- "LIPINSKI"
dat$candlast.D[dat$lastName=="Schakowsky" & dat$candlast.D=="SCAKOWSKY"] <- "SCHAKOWSKY"
dat$candlast.D[dat$lastName=="Long Thompson" & dat$candlast.D=="LONG"] <- "LONG THOMPSON"
dat$candlast.D[dat$lastName=="O'Neill" & dat$candlast.D=="O'NEIL"] <- "O'NEILL"
dat$candlast.D[dat$lastName=="Van Hollen" & dat$candlast.D=="HOLLEN"] <- "VAN HOLLEN"
dat$candlast.D[dat$lastName=="Gephardt" & dat$candlast.D=="GEPHARD"] <- "GEPHARDT"
dat$candlast.D[dat$lastName=="Volkmer" & dat$candlast.D=="WOLKMER"] <- "VOLKMER"
dat$candlast.D[dat$lastName=="Montgomery" & dat$candlast.D=="MONTGOMER"] <- "MONTGOMERY"
dat$candlast.D[dat$lastName=="Whitley" & dat$candlast.D=="WHITELY"] <- "WHITLEY"
dat$candlast.D[dat$lastName=="LeFante" & dat$candlast.D=="FANTE"] <- "LEFANTE"
dat$candlast.D[dat$lastName=="Pascrell" & dat$candlast.D=="BASCRELL"] <- "PASCRELL"
dat$candlast.D[dat$lastName=="Lujan Grisham" & dat$candlast.D=="GRISHAM"] <- "LUJAN GRISHAM"
dat$candlast.D[dat$lastName=="St. Germain" & dat$candlast.D=="GERMAIN"] <- "ST. GERMAIN"
dat$candlast.D[dat$lastName=="Clyburn" & dat$candlast.D=="CLYBRUN"] <- "CLYBURN"
dat$candlast.D[dat$lastName=="Herseth Sandlin" & dat$candlast.D=="HERSETH"] <- "HERSETH SANDLIN"
dat$candlast.D[dat$lastName=="Herseth Sandlin" & dat$candlast.D=="SANDLIN"] <- "HERSETH SANDLIN"
dat$candlast.D[dat$lastName=="de la Garza" & dat$candlast.D=="GARZA"] <- "DE LA GARZA"
dat$candlast.D[dat$lastName=="Jackson Lee" & dat$candlast.D=="JACKSON-LEE"] <- "JACKSON LEE"
dat$candlast.D[dat$lastName=="Jackson Lee" & dat$candlast.D=="LEE"] <- "JACKSON LEE"
dat$candlast.D[dat$lastName=="Doggett" & dat$candlast.D=="DOGETT"] <- "DOGGETT"
dat$candlast.D[dat$lastName=="Shepherd" & dat$candlast.D=="SHEPGERD"] <- "SHEPHERD"
dat$candlast.D[dat$lastName=="Oakar" & dat$candlast.D=="OAKER"] <- "OAKAR"

dat$lastName <- gsub("í", "i", dat$lastName)
dat$lastName <- gsub("á", "a", dat$lastName)
dat$lastName <- gsub("é", "e", dat$lastName)

dat <- dat[which(toupper(dat$lastName)==dat$candlast.D | toupper(dat$lastName)==dat$candlast.R), ]


##########


president <- read.csv("presidential/1976-2020-president.csv", stringsAsFactors=F)

president <- president[which(president$party_simplified %in% c("DEMOCRAT", "REPUBLICAN") & !president$candidate %in% c("", "OTHER") & !president$writein), ]

president$candidate[president$candidate=="MITT, ROMNEY"] <- "ROMNEY, MITT"

president$party_simplified <- ifelse(president$party_simplified=="DEMOCRAT", "D", "R")

president$elec_id_state <- paste0(president$state_po, ".", president$year)

names(president)[names(president)=="candidatevotes"] <- "candidatevotes_pres"
names(president)[names(president)=="candidate"] <- "candidate_pres"
names(president)[names(president)=="totalvotes"] <- "totalvotes_pres"

president_wide <- reshape(president, direction="wide", timevar="party_simplified", idvar="elec_id_state", v.names=c("candidate_pres", "candidatevotes_pres"))

temp <- president_wide
temp$year <- temp$year+2
temp$elec_id_state <- paste0(temp$state_po, ".", temp$year)
president_wide <- rbind(president_wide, temp)

president_wide <- president_wide[, c("elec_id_state", "candidate_pres.D", "candidate_pres.R", "totalvotes_pres", "candidatevotes_pres.D", "candidatevotes_pres.R")]

dat$elec_id_state <- paste0(dat$state_po, ".", dat$year)

dat_all <- merge(dat, president_wide, by="elec_id_state")


##########


clean_gov_files <- function(x) {
  y <- read.csv(sprintf("gubernatorial/gubernatorial_%is.csv", x), stringsAsFactors=F, skip=2)
  y <- y[y[,"Office"]=="Governor", ]
  return(y)
}
# clean each of the gubernatorial decade files
state_files <- lapply(seq(1960,2010,10), function(x) clean_gov_files(x))

state_data <- do.call(rbind, state_files)

state_data[which(state_data$RaceTypeName=="General"), ]

state_data$state_po <- state.abb[match(state_data$Area, state.name)]
state_data$raceYear <- substr(state_data$raceYear, 1, 4)
state_data$elec_id_state <- paste0(state_data$state_po, ".", state_data$raceYear)

state_data$RepVotes <- as.numeric(gsub(",", "", state_data$RepVotes))
state_data$DemVotes <- as.numeric(gsub(",", "", state_data$DemVotes))

state_data$totalvotes <- (state_data$RepVotes * (as.numeric(state_data$RepVotesMajorPercent)/100) + state_data$DemVotes * (as.numeric(state_data$DemVotesMajorPercent)/100)) / (as.numeric(state_data$RepVotesMajorPercent)/100+as.numeric(state_data$RepVotesMajorPercent)/100)

temp1 <- state_data
temp1$raceYear <- as.numeric(temp1$raceYear)+1
temp1$elec_id_state <- paste0(temp1$state_po, ".", temp1$raceYear)

temp2 <- state_data
temp2$raceYear <- as.numeric(temp2$raceYear)+2
temp2$elec_id_state <- paste0(temp2$state_po, ".", temp2$raceYear)

temp3 <- state_data
temp3$raceYear <- as.numeric(temp3$raceYear)+3
temp3$elec_id_state <- paste0(temp3$state_po, ".", temp3$raceYear)

temp <- do.call(rbind, list(state_data, temp1, temp2, temp3))

state_data <- temp[!duplicated(temp$elec_id_state), ]

state_data <- state_data[, c("elec_id_state", "RepCandidate", "DemCandidate", "RepStatus", "DemStatus", "RepVotes", "DemVotes", "totalvotes")]

names(state_data) <- c("elec_id_state", "cand_R_Gov", "cand_D_Gov", "status_R_Gov", "status_D_Gov",
                       "votes_R_Gov", "votes_D_Gov", "totalvote_Gov")

dat_all <- merge(dat_all, state_data, by="elec_id_state")

##########


dat_trim <- dat_all[, c("year", "state_po", "district", "totalvotes", "candidate.R", "RepStatus", "candidatevotes.R",
                        "candidate.D", "DemStatus", "candidatevotes.D", "totalvotes_pres", "candidate_pres.R", 
                        "candidatevotes_pres.R", "candidate_pres.D", "candidatevotes_pres.D", "cand_R_Gov", "cand_D_Gov", "status_R_Gov", "status_D_Gov",
                        "votes_R_Gov", "votes_D_Gov", "totalvote_Gov", "gini",
                        "meanIncome", "prcntUnemp", "prcntHS", "prcntBA", "prcntOld", "medianAge", "totalPopRaceFile",
                        "prcntBlackNotHisp", "prcntHisp", "prcntWhite")]


names(dat_trim) <- c("year", "state", "district", "totalvote_House", "cand_R_House", "status_R_House", "votes_R_House",
                     "cand_D_House", "status_D_House", "votes_D_House", "totalvote_Pres", "cand_R_Pres", 
                     "votes_R_Pres", "cand_D_Pres", "votes_D_Pres", "cand_R_Gov", "cand_D_Gov", "status_R_Gov", "status_D_Gov",
                     "votes_R_Gov", "votes_D_Gov", "totalvote_Gov", "gini",
                     "mean_income", "pct_unemp", "pct_hsgrads", "pct_BAdegree", "pct_over60", "median_age", "totalpop",
                     "pct_black", "pct_hispanic", "pct_white")

dat_trim$sd_code <- paste0(dat_trim$state, "_", dat_trim$district)


for(year in sort(unique(dat_trim$year))) {
  for(code in sort(unique(dat_trim$sd_code))) {
    id <- which(dat_trim$year==year & dat_trim$sd_code==code)
    if(length(id) == 0) {
      state <- substr(code, 1, 2)
      dist <- substr(code, nchar(code)-1, nchar(code))
      dist <- gsub("_", "", dist)
      new <- c(year, state, dist, rep(NA, ncol(dat_trim)-4), code)
      dat_trim <- rbind(dat_trim, new)
    }
  }
}

dat_trim <- dat_trim[which(!duplicated(dat_trim[, !names(dat_trim)%in%c("status_D_House", "status_R_House")])), ]

write.csv(dat_trim, "district_panel.csv", row.names=F)









