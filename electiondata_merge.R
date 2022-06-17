
setwd("/Users/christianbaehr/Documents/GitHub/charisma")

congress <- read.csv("Data/U.S. housePrecinct-Level Returns 2020/1976-2020-house.csv", stringsAsFactors = F)
congress <- congress[congress$stage=="GEN" ,]
congress <- congress[congress$party %in% c("DEMOCRAT", "REPUBLICAN"), ]
congress <- congress[!congress$writein, ]
congress <- congress[!congress$special, ]
congress <- congress[!congress$unofficial, ]
congress <- congress[!congress$runoff, ]

# drop cases where total votes are 1, 0, or -1
congress <- congress[!(congress$totalvotes%in% c(1, -1)), ]
#View(congress[congress$totalvotes<30000,]) # 1978 Chisholm outlier seems correct despite low vote total

# same candidate running for both parties
#View(congress[congress$fusion_ticket, ])
bothparties <- (duplicated(congress[,c("year","state","candidate")]) | duplicated(congress[,c("year","state","candidate")], fromLast=T))
congress <- congress[!bothparties, ]

# create unique election id variable
congress$election_id <- paste(congress$state_po, congress$year, congress$district, sep="_")

n_candidates <- table(congress$election_id)
# create temporary number of candidates variable to drop elections with more than two candidates running on D or R tickets
# in the general
congress$n_candidates <- n_candidates[match(congress$election_id, names(n_candidates))]
#View(congress[congress$n_candidates>2,])
congress <- congress[congress$n_candidates <= 2, ]

# drop cases if both candidates from same party
sameparty <- (duplicated(congress[,c("election_id", "party")]) | duplicated(congress[,c("election_id", "party")], fromLast=T))
congress <- congress[!sameparty, ]

# candidate vote share variable
congress$voteshare <- congress$candidatevotes / congress$totalvotes

# drop unnecessary variables
dropcols <- names(congress) %in% c("stage","candidatevotes","totalvotes","mode","version", "writein", "special", 
                                   "unofficial", "runoff", "fusion_ticket", "candidatevotes", "totalvotes", "n_candidates")
congress <- congress[!dropcols]

congress$party <- ifelse(congress$party=="DEMOCRAT", "D.congress", "R.congress")

# reshape data so demo and repub voteshare are in same row
congress_wide <- reshape(congress, direction="wide", timevar="party", idvar="election_id", v.names=c("candidate", "voteshare"))
#sum(duplicated(congress_wide[,c("district", "state", "year")]))


##########

clean_gov_files <- function(x) {
  y <- read.csv(sprintf("Data/gubernatorial/gubernatorial_%is.csv", x), stringsAsFactors=F, skip=2)
  y <- y[y[,"Office"]=="Governor", ]
  
  return(y)
}
# clean each of the gubernatorial decade files
state_files <- lapply(seq(1960, 2010, 10), function(x) clean_gov_files(x))

state_data <- do.call(rbind, state_files)

state_data$state_po <- state.abb[match(state_data$Area, state.name)]
state_data$raceYear <- substr(state_data$raceYear, 1, 4)
state_data$election_id <- paste(state_data$state_po, state_data$raceYear, sep="_")

state_data$RepVotesMajorPercent <- as.numeric(state_data$RepVotesMajorPercent) / 100
state_data$DemVotesMajorPercent <- as.numeric(state_data$DemVotesMajorPercent) / 100

state_data <- state_data[,c("election_id", "RepCandidate", "DemCandidate", "RepVotesMajorPercent", "DemVotesMajorPercent")]

names(state_data) <- c("election_id", "candidate.R.governor", "candidate.D.governor", "voteshare.R.governor", "voteshare.D.governor")

congress_wide$election_id_slim <- substr(congress_wide$election_id, 1, 7)

full_dat <- merge(congress_wide, state_data, by.x="election_id_slim", by.y="election_id")


##########


presidential <- read.csv("Data/presidential/1976-2020-president.csv", stringsAsFactors=F)
presidential <- presidential[-c(3543, 3544),]
#View(presidential[presidential$state=="MARYLAND",])

presidential <- presidential[presidential$party_detailed %in% c("DEMOCRAT", "REPUBLICAN"),]

presidential$party <- ifelse(presidential$party_detailed=="DEMOCRAT", "D", "R")
presidential$election_id <- paste(presidential$state_po, presidential$year, sep="_")
presidential$voteshare <- presidential$candidatevotes / presidential$totalvotes

presidential <- presidential[!(presidential$candidate %in% c("", "OTHER")), ]

presidential_wide <- reshape(presidential, direction="wide", timevar="party", idvar="election_id", v.names=c("candidate", "voteshare"))

presidential_wide <- presidential_wide[,c("election_id", "candidate.D", "candidate.R", "voteshare.D", "voteshare.R")]

names(presidential_wide) <- c("election_id", "candidate.D.president", "candidate.R.president", "voteshare.D.president", "voteshare.R.president")

full_dat <- merge(full_dat, presidential_wide, by.x="election_id_slim", by.y="election_id")


write.csv(full_dat, "election_data_districtlevel.csv", row.names=F)










