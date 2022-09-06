

congress <- read.csv("/Users/christianbaehr/Documents/GitHub/charisma/Data/U.S. housePrecinct-Level Returns 2020/1976-2020-house.csv", stringsAsFactors = F)
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


demo <- read.csv("/Users/christianbaehr/Downloads/dataverse_files-3/allCongressDataPublishV2.csv", stringsAsFactors = F)
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
#View(dat[which(toupper(dat$lastName)!=dat$candlast.D & toupper(dat$lastName)!=dat$candlast.R), ])


##########


sum(dat$numberTerms==1)




















