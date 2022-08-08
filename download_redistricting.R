
setwd("/Users/christianbaehr/Desktop")
#setwd("/scratch/network/cb8007/charisma_project/census_redistricting")


library(PL94171)

# each state
#for(i in state.abb) {
for(i in state.abb[which(state.abb=="FL"):length(state.abb)]) {
  # each decade of data
  #for(k in c(2000, 2010, 2020)) {
  for(k in c(2010)) {
    
    mydir <- sprintf(paste0(getwd(), "/data%i/%s"), k, i)
    
    # read in the data files together for state i decade k
    temp <- pl_read(pl_url(i, k))
    
    if(dir.exists(mydir)) {
      do.call(file.remove, list(list.files(mydir, full.names = TRUE)))
    } else if (!dir.exists(mydir)) {
      dir.create(mydir)
    }
    
    #if(!dir.exists(sprintf("data%i/%s", k, i))) {dir.create(sprintf("data%i/%s", k, i))}
    
    # write each of the files individually as a csv
    for(j in names(temp)) {
      
      out <- data.frame(temp[[j]])
      write.csv(out, sprintf(paste0(mydir, "/%s.csv"), j), row.names=F)
    }
    
  }
  print(i)
}

