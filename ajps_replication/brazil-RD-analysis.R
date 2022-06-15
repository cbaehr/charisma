##################################################################
# Partial replication files for "Parties as Disciplinarians: Charisma and Commitment Problems in Programmatic Campaigning" 
#by James R. Hollyer, Marko Klasnja and Rocio Titiunik, forthcoming at AJPS in 2021.
##################################################################
#install.packages('rdrobust')
#install.packages('Hmisc')
#install.packages('readstata13')

library(Hmisc)
r = function(x) round(x, digits=3)
#######################
#
# RD analysis
#
########################
library(rdrobust)
library(readstata13)
data = read.dta13("./brazil-RD-data.dta")
names(data)

###############################

# Table 2

###############################

# ----------------------------- #
# Now put results into a table to print to latex
# ----------------------------- #
outnm = c("Young", "Young and Outsider") 
outvar = cbind(data$youngF_inc_for1, data$outyg1F_inc_for1)

samplenm = c("Opn")
samplevar = (data$dlameduck_runs==0)

colnms = c("Outcome","Sample", "Effect", "Robust p-value",  "Robust 95 CI", "Bandwidth (h)", "Bandwidth (b)", "N right", "N left")
res = matrix(data=NA, nrow=length(outnm), ncol=length(colnms), dimnames = list(NULL, colnms))

# default rdrobust
for(k in 1:length(outnm)) {
    cat("Analyzing outcome ", outnm[k], " sample ", samplenm, "\n")
    i = k
    
    indx = samplevar
    y = outvar[indx,k]
    x = data$mv_incparty[indx]
    
    rr = rdrobust(y=y, x=x)
    
    res[i,"Outcome"]        = outnm[k]
    res[i, "Sample"]        = samplenm
    res[i,"Effect"]         = r(rr$Estimate[1])
    res[i,"Robust p-value"] = r(rr$pv[3])
    res[i,"Robust 95 CI"]   = paste("[", r(rr$ci["Robust",1]), ",",  r(rr$ci["Robust",2]), "]", sep="")
    res[i,"Bandwidth (h)"]  = r(rr$bws["h",1])
    res[i,"Bandwidth (b)"]  = r(rr$bws["b",1])
    res[i,"N right"]        = rr$N_h[1]
    res[i,"N left"]         = rr$N_h[2]
}

res.hb = res

# rdrobust with rho=1
res = matrix(data=NA, nrow=length(outnm), ncol=length(colnms), dimnames = list(NULL, colnms))

for(k in 1:length(outnm)) {
    cat("Analyzing outcome ", outnm[k], " sample ", samplenm, "\n")
    i = k
    
    indx = samplevar
    y = outvar[indx,k]
    x = data$mv_incparty[indx]
    
    rr = rdrobust(y=y, x=x, rho=1)
    
    res[i,"Outcome"]        = outnm[k]
    res[i, "Sample"]        = samplenm
    res[i,"Effect"]         = r(rr$Estimate[1])
    res[i,"Robust p-value"] = r(rr$pv[3])
    res[i,"Robust 95 CI"]   = paste("[", r(rr$ci["Robust",1]), ",",  r(rr$ci["Robust",2]), "]", sep="")
    res[i,"Bandwidth (h)"]  = r(rr$bws["h",1])
    res[i,"Bandwidth (b)"]  = r(rr$bws["b",1])
    res[i,"N right"]        = rr$N_h[1]
    res[i,"N left"]         = rr$N_h[2]
    
}

res.rho1 = res

# Default rdrobust specs
print(res.hb)
# Using h=b (rho=1)
print(res.rho1)


# Make table pretty and print
# Young and Young outsider for Open seat sample only
tab = rbind(res.hb[,c(-2)])
colnames(tab)[2] = "$\\tau^\\mathtt{RD}$"
colnames(tab)[3] = "$\\mathtt{p}$-$\\mathtt{value}$"
colnames(tab)[4] = "$\\mathtt{95\\%\\;CI}$"
colnames(tab)[5] = "$\\mathtt{h}$"
colnames(tab)[6] = "$\\mathtt{b}$"
colnames(tab)[7] = "$\\mathtt{N}_{+}$"
colnames(tab)[8] = "$\\mathtt{N}_{-}$"
# Add confidence interval for rho=1
tab = cbind(tab,res.rho1[,"Robust 95 CI"])
colnames(tab)[9] = "$\\mathtt{95\\%\\;CI\\; (h=b)}$"
print(tab)

h.y  = res.hb[1,6]
b.y  = res.hb[1,7]
t.y  = res.hb[1,3]
ci.y=  res.hb[1,5]


h.yo  = res.hb[2,6]
b.yo  = res.hb[2,7]
t.yo  = res.hb[2,3]
ci.yo=  res.hb[2,5]


#################

# Figure 6

##################

##----------------------------#
# youngF_inc_for1: Young indicator (35 or less)
##----------------------------#
outnm = "young"
y=data$youngF_inc_for1
x = data$mv_incparty
indx = (data$dlameduck_runs==0)

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
g$rdplot +  theme(axis.text=element_text(size=10),axis.title = element_text(size=15))

##----------------------------#
# outyg1F_inc_for1: Young outsider indicator (==0 is reserved for age>=35 and elected officials and government employees excluding military and police)
##----------------------------#
outnm = "out1young"
y=data$outyg1F_inc_for1
x = data$mv_incparty
indx = (data$dlameduck_runs==0)

# only munis where lame-duck does *not* run 
g = rdplot(y[indx], x[indx], subset = (abs(x[indx]) <= 50), p=2,
           x.label="Incumbent party's margin of victory at t", y.label = "Incumbent party's candidate at t+1 young and outsider", title="")
annotation = data.frame( 
  x = c(xpos, xpos, xpos, xpos),
  y = c(ypos, ypos-0.03, ypos-0.06, ypos-0.09),
  label = c(paste("Local linear RD effect = ",t.yo,sep=""), paste("Robust 95% CI = ",ci.yo,sep=""), paste("Main bandwidth = ", h.yo, sep=""),
            paste("Bias bandwidth = ", b.yo, sep=""))
)
g$rdplot + theme(axis.text=element_text(size=10), axis.title = element_text(size=15))

##########################################################
#
# Table E3
#
##########################################################
data = read.dta13("./brazil-validation-data.dta")
dim(data)

outnm = c("Young Indicator", "Young and Outsider Indicator")
outvar = cbind(data$cand_youngF, data$cand_outyg1F)

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
print(tab)

