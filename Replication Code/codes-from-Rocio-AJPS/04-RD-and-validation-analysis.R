library(rdrobust)
library(Hmisc)
r = function(x) round(x, digits=3)

#######################
#
# RD analysis
#
########################
library(rdrobust)
library(readstata13)
library(ggplot2)
data = read.dta13("./output/candlevel_covars_2000-2012-FINAL.dta")
names(data)

# We use the age and occupation variables declared the first time the candidate appears in our dataset. 
# remember: the incumbent party is the party that won election t-1, and is therefore the incumbent party at the moment of election t.
# This party may win or lose election t, and may win or lose election t+1 (and may or may not run in either election, but it for sure ran at t-1)
# ageF_inc_for1: age of the incumbent party's candidate at t+1, as reported the first time the candidate appears in our time period
# out2_ocpF_inc_for1: outsider indicator of the incumbent party's candidate at t+1, based on the first occupation reported by the candidate in our time period
# out3_ocpF_inc_for1: non-business outsider indicator of the incumbent party's candidate at t+1, based on the first occupation reported by the candidate in our time period
# outyg2F_inc_for1: young outsider indicator of the incumbent party's candidate at t+1, based on the first occupation and the first age reported by the candidate in our time period
# outyg3F_inc_for1: non-business young outsider indicator of the incumbent party's candidate at t+1, based on the first occupation and the first age reported by the candidate in our time period

###############################

# RD tables 

###############################

# ----------------------------- #
# Now put results into a table to print to latex
# ----------------------------- #
outnm = c("Young", "Young and Outsider", "Young-notF", "Young-notF and Outsider") 
outvar = cbind(data$youngF_inc_for1, data$outyg1F_inc_for1, data$young_inc_for1, data$outyg1_inc_for1)

samplenm = c("All", "Inc", "Opn")
samplevar = cbind(data$dlameduck_runs>=0, data$dlameduck_runs==1, data$dlameduck_runs==0)

colnms = c("Outcome","Sample", "Effect", "Robust p-value",  "Robust 95 CI", "Bandwidth (h)", "Bandwidth (b)", "N right", "N left")
res = matrix(data=NA, nrow=length(outnm)*3, ncol=length(colnms), dimnames = list(NULL, colnms))

# default rdrobust
for(k in 1:length(outnm)) {
  for(j in 1:length(samplenm)) {
    cat("Analyzing outcome ", outnm[k], " sample ", samplenm[j], "\n")
    i = (k - 1) * 3 + 1 + (j-1)
    
    indx = samplevar[,j]
    y = outvar[indx,k]
    x = data$mv_incparty[indx]
    
    rr = rdrobust(y=y, x=x)
    
    res[i,"Outcome"]        = outnm[k]
    res[i, "Sample"]        = samplenm[j]
    res[i,"Effect"]         = r(rr$Estimate[1])
    res[i,"Robust p-value"] = r(rr$pv[3])
    res[i,"Robust 95 CI"]   = paste("[", r(rr$ci["Robust",1]), ",",  r(rr$ci["Robust",2]), "]", sep="")
    res[i,"Bandwidth (h)"]  = r(rr$bws["h",1])
    res[i,"Bandwidth (b)"]  = r(rr$bws["b",1])
    res[i,"N right"]        = rr$N_h[1]
    res[i,"N left"]         = rr$N_h[2]
    
  }
}

res.hb = res

# rdrobust with rho=1
res = matrix(data=NA, nrow=length(outnm)*3, ncol=length(colnms), dimnames = list(NULL, colnms))

for(k in 1:length(outnm)) {
  for(j in 1:length(samplenm)) {
    cat("Analyzing outcome ", outnm[k], " sample ", samplenm[j], "\n")
    i = (k - 1) * 3 + 1 + (j-1)
    
    indx = samplevar[,j]
    y = outvar[indx,k]
    x = data$mv_incparty[indx]
    
    rr = rdrobust(y=y, x=x, rho=1)
    
    res[i,"Outcome"]        = outnm[k]
    res[i, "Sample"]        = samplenm[j]
    res[i,"Effect"]         = r(rr$Estimate[1])
    res[i,"Robust p-value"] = r(rr$pv[3])
    res[i,"Robust 95 CI"]   = paste("[", r(rr$ci["Robust",1]), ",",  r(rr$ci["Robust",2]), "]", sep="")
    res[i,"Bandwidth (h)"]  = r(rr$bws["h",1])
    res[i,"Bandwidth (b)"]  = r(rr$bws["b",1])
    res[i,"N right"]        = rr$N_h[1]
    res[i,"N left"]         = rr$N_h[2]
    
  }
}
res.rho1 = res

# Default rdrobust specs
print(res.hb)
# Using h=b (rho=1)
print(res.rho1)


# Make table pretty and print
# Young and Young outsider for Open seat sample only
tab = rbind(res.hb[c(3,6),c(-2)])
colnames(tab)[2] = "$\\tau^\\mathtt{RD}$"
colnames(tab)[3] = "$\\mathtt{p}$-$\\mathtt{value}$"
colnames(tab)[4] = "$\\mathtt{95\\%\\;CI}$"
colnames(tab)[5] = "$\\mathtt{h}$"
colnames(tab)[6] = "$\\mathtt{b}$"
colnames(tab)[7] = "$\\mathtt{N}_{+}$"
colnames(tab)[8] = "$\\mathtt{N}_{-}$"
# Add confidence interval for rho=1
tab = cbind(tab,res.rho1[c(3,6),"Robust 95 CI"])
colnames(tab)[9] = "$\\mathtt{95\\%\\;CI\\; (h=b)}$"
table=latex(tab, file= "./output/tables/tabRD-hb.txt", title="", table.env=FALSE, center="none")

h.y  = res.hb[3,6]
b.y  = res.hb[3,7]
t.y  = res.hb[3,3]
ci.y=  res.hb[3,5]


h.yo  = res.hb[6,6]
b.yo  = res.hb[6,7]
t.yo  = res.hb[6,3]
ci.yo=  res.hb[6,5]


h.ynF  = res.hb[9,6]
b.ynF  = res.hb[9,7]
t.ynF  = res.hb[9,3]
ci.ynF =  res.hb[9,5]

#################

# RD plots

##################

##----------------------------#
# youngF_inc_for1: Young indicator (35 or less)
##----------------------------#
outnm = "young"
y=data$youngF_inc_for1
x = data$mv_incparty
indx = (data$dlameduck_runs==0)

# entire support 
# all 
pdf(file = paste("./output/tables/RDplot-", outnm, "-all.pdf", sep=""))
rdplot(y, x)
dev.off()

xpos = -25
ypos = 0.5
# only munis where lame-duck does *not* run 
g = rdplot(y[indx], x[indx], subset = (abs(x[indx]) <= 50), p=2,
       x.label="Incumbent party's margin of victory at t", y.label = "Incumbent party's candidate at t+1 young indicator (original report)", title="")
annotation = data.frame( 
  x = c(xpos, xpos, xpos, xpos),
  y = c(ypos, ypos-0.03, ypos-0.06, ypos-0.09),
  label = c(paste("Local linear RD effect = ",t.y,sep=""), paste("Robust 95% CI = ",ci.y,sep=""), paste("Main bandwidth = ", h.y, sep=""),
            paste("Bias bandwidth = ", b.y, sep=""))
)
pdf(file = paste("./output/tables/RDplot-", outnm, "-Nld.pdf", sep=""))
#g$rdplot + geom_text(data=annotation, aes(x=x, y=y, label=label), color="red", size=4) + theme(axis.text=element_text(size=10),axis.title = element_text(size=15))
g$rdplot +  theme(axis.text=element_text(size=10),axis.title = element_text(size=15))
dev.off()

# only munis where lame-duck does run
pdf(file = paste("./output/tables/RDplot-", outnm, "-Yld.pdf", sep=""))
rdplot(y[!indx], x[!indx], binselect = "esmv", x.lim=c(-xlim, xlim), p=4, y.lim = c(0,1),
       x.label="Incumbent party's margin of victory at t", y.label = "Incumbent party's candidate at t+1 young indicator (original report)")
dev.off()

# only munis where lame-duck does *not* run, WITHIN h
pdf(file = paste("./output/tables/RDplot-", outnm, "-Nld-RDeffect.pdf", sep=""))
rdplot(y[indx], x[indx], binselect = "esmv", x.lim=c(-xlim, xlim), h=as.numeric(h.y), p=1,
       x.label="Incumbent party's margin of victory at t", y.label = "Incumbent party's candidate at t+1 young", title="")
dev.off()

##----------------------------#
# outyg1F_inc_for1: Young outsider indicator (==0 is reserved for age>=35 and elected officials and government employees excluding military and police)
##----------------------------#
outnm = "out1young"
y=data$outyg1F_inc_for1
x = data$mv_incparty
indx = (data$dlameduck_runs==0)

# all 
pdf(file = paste("./output/tables/RDplot-", outnm, "-all.pdf", sep=""))
rdplot(y, x)
dev.off()

# only munis where lame-duck does *not* run 
g = rdplot(y[indx], x[indx], subset = (abs(x[indx]) <= 50), p=2,
           x.label="Incumbent party's margin of victory at t", y.label = "Incumbent party's candidate at t+1 young and outsider", title="")
annotation = data.frame( 
  x = c(xpos, xpos, xpos, xpos),
  y = c(ypos, ypos-0.03, ypos-0.06, ypos-0.09),
  label = c(paste("Local linear RD effect = ",t.yo,sep=""), paste("Robust 95% CI = ",ci.yo,sep=""), paste("Main bandwidth = ", h.yo, sep=""),
            paste("Bias bandwidth = ", b.yo, sep=""))
)
pdf(file = paste("./output/tables/RDplot-", outnm, "-Nld.pdf", sep=""))
#g$rdplot + geom_text(data=annotation, aes(x=x, y=y, label=label), color="red", size=4) + theme(axis.text=element_text(size=10), axis.title = element_text(size=15))
g$rdplot + theme(axis.text=element_text(size=10), axis.title = element_text(size=15))
dev.off()

# only munis where lame-duck does run
pdf(file = paste("./output/tables/RDplot-", outnm, "-Yld.pdf", sep=""))
rdplot(y[!indx], x[!indx], binselect = "esmv", x.lim=c(-xlim, xlim), p=3, y.lim = c(0,1))
dev.off()

# only munis where lame-duck does *not* run, WITHIN h
pdf(file = paste("./output/tables/RDplot-", outnm, "-Nld-RDeffect.pdf", sep=""))
rdplot(y[indx], x[indx], binselect = "esmv", x.lim=c(-xlim, xlim), h=as.numeric(h.yo), p=1)
dev.off()

##----------------------------#
# young_inc_for1: Young indicator (35 or less contemporaneous, not in the first run)
##----------------------------#
outnm = "young-notF"
y=data$young_inc_for1
x = data$mv_incparty
indx = (data$dlameduck_runs==0)

# entire support 
# all 
pdf(file = paste("./output/tables/RDplot-", outnm, "-all.pdf", sep=""))
rdplot(y, x)
dev.off()

xpos = -25
ypos = 0.5
# only munis where lame-duck does *not* run 
g = rdplot(y[indx], x[indx], binselect = "esmv", x.lim=c(-xlim, xlim), p=4, y.lim = c(0,1),
           x.label="Incumbent party's margin of victory at t", y.label = "Incumbent party's candidate at t+1 young indicator (original report)", title="")
annotation = data.frame( 
  x = c(xpos, xpos, xpos, xpos),
  y = c(ypos, ypos-0.03, ypos-0.06, ypos-0.09),
  label = c(paste("Local linear RD effect = ",t.ynF,sep=""), paste("Robust 95% CI = ",ci.ynF,sep=""), paste("Main bandwidth = ", h.ynF, sep=""),
            paste("Bias bandwidth = ", b.ynF, sep=""))
)
pdf(file = paste("./output/tables/RDplot-", outnm, "-Nld.pdf", sep=""))
g$rdplot + geom_text(data=annotation, aes(x=x, y=y, label=label), color="red", size=4) + theme(axis.text=element_text(size=10), 
                                                                                               axis.title = element_text(size=15))
dev.off()

# only munis where lame-duck does run
pdf(file = paste("./output/tables/RDplot-", outnm, "-Yld.pdf", sep=""))
rdplot(y[!indx], x[!indx], binselect = "esmv", x.lim=c(-xlim, xlim), p=4, y.lim = c(0,1),
       x.label="Incumbent party's margin of victory at t", y.label = "Incumbent party's candidate at t+1 young indicator (original report)")
dev.off()

# only munis where lame-duck does *not* run, WITHIN h
pdf(file = paste("./output/tables/RDplot-", outnm, "-Nld-RDeffect.pdf", sep=""))
rdplot(y[indx], x[indx], binselect = "esmv", x.lim=c(-xlim, xlim), h=as.numeric(h.y), p=1,
       x.label="Incumbent party's margin of victory at t", y.label = "Incumbent party's candidate at t+1 young", title="")
dev.off()

##########################################################
#
# Validation analysis
#
##########################################################

data = read.dta13("./output/candlevel_covars_2000-2012-long-PRED.dta")

# keep only parties that had more than 500 observations in the prediction analysis
partypredsObs =  c( "PP", "PDT" ,"PT" ,"PTB" ,"PMDB" ,  "PR", "PPS", "DEM", "PSB", "PSDB","PSD")

dim(data)
ii = data$party %in% partypredsObs 
sum(ii)

data = data[ii,]

outnm = c("Young Indicator", "Young and Outsider Indicator", "Young-notF Indicator", "Young-notF and Outsider Indicator")
outvar = cbind(data$cand_youngF, data$cand_outyg1F, data$cand_young, data$cand_outyg1)

colnms = c("Outcome", "Mean (I==1)", "Mean (I==0)",  "p-value", "N (I==1)", "N (I==0)")
res = matrix(data=NA, nrow=length(outnm), ncol=length(colnms), dimnames = list(NULL, colnms))

for(i in 1:length(outnm)) {
    cat("Analyzing outcome ", outnm[i], "\n")
    y = outvar[,i]
    x = data$err_mvparty
    
    rr = t.test(x[y==1],  x[y==0])
    
    res[i,"Outcome"]        = outnm[i]
    res[i,"Mean (I==1)"]    = r(mean(x[y==1], na.rm=T))
    res[i,"Mean (I==0)"]    = r(mean(x[y==0], na.rm=T))
    res[i,"p-value"]        = r(rr$p.value)
    res[i,"N (I==1)"]       = sum(!is.na(x[y==1]))
    res[i,"N (I==0)"]       = sum(!is.na(x[y==0]))
}
print(res)
hist(data$err_mvparty)

# Make table pretty and print
# Young and Young outsider for Open seat sample only
tab = res[1:2,]
colnames(tab)[2] = "$\\mathtt{Mean}_1$"
colnames(tab)[3] = "$\\mathtt{Mean}_0$"
colnames(tab)[4] = "$\\mathtt{p}$-$\\mathtt{value}$"
colnames(tab)[5] = "$\\mathtt{N}_1$"
colnames(tab)[6] = "$\\mathtt{N}_0$"
tab[1,1] = "Margin of victory: Young vs. not"
tab[2,1] = "Margin of victory: Young and Outsider vs. not"
table=latex(tab, file="./output/tables/tabValidation-500above.txt", title="", table.env=FALSE, center="none")

## Without dropping any parties 
data = read.dta13("./output/candlevel_covars_2000-2012-long-PRED.dta")
dim(data)

outnm = c("Young Indicator", "Young and Outsider Indicator", "Young-notF Indicator", "Young-notF and Outsider Indicator")
outvar = cbind(data$cand_youngF, data$cand_outyg1F, data$cand_young, data$cand_outyg1)

colnms = c("Outcome", "Mean (I==1)", "Mean (I==0)",  "p-value", "N (I==1)", "N (I==0)")
res = matrix(data=NA, nrow=length(outnm), ncol=length(colnms), dimnames = list(NULL, colnms))

for(i in 1:length(outnm)) {
  cat("Analyzing outcome ", outnm[i], "\n")
  y = outvar[,i]
  x = data$err_mvparty
  
  rr = t.test(x[y==1],  x[y==0])
  
  res[i,"Outcome"]        = outnm[i]
  res[i,"Mean (I==1)"]    = r(mean(x[y==1], na.rm=T))
  res[i,"Mean (I==0)"]    = r(mean(x[y==0], na.rm=T))
  res[i,"p-value"]        = r(rr$p.value)
  res[i,"N (I==1)"]       = sum(!is.na(x[y==1]))
  res[i,"N (I==0)"]       = sum(!is.na(x[y==0]))
}
print(res)
hist(data$err_mvparty)

# Make table pretty and print
# Young and Young outsider for Open seat sample only
tab = res[1:2,]
colnames(tab)[2] = "$\\mathtt{Mean}_1$"
colnames(tab)[3] = "$\\mathtt{Mean}_0$"
colnames(tab)[4] = "$\\mathtt{p}$-$\\mathtt{value}$"
colnames(tab)[5] = "$\\mathtt{N}_1$"
colnames(tab)[6] = "$\\mathtt{N}_0$"
tab[1,1] = "Margin of victory: young vs. not"
tab[2,1] = "Margin of victory: young \\& outsider vs. not"
table=latex(tab, file="./output/tables/tabValidation-ALL.txt", title="", table.env=FALSE, center="none")

