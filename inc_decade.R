library(glmnet)
library(modelsummary)
#setwd("/Users/christianbaehr/Dropbox/charisma_project/data")
##Vote Sharevariable : con_demshare, con_repshare, pres_demshare, pres_repshare, gov_repshare, gov_demshare

############################################################
###################Creating 1-laog##########################
############################################################


congress <- read.csv("cd_panel_full_LOCincumb.csv")
congress <- congress[order(congress$con_demcandidate,congress$con_raceyear, congress$statenm,congress$cd, congress$con_incumbency), ]

#min_years <- tapply(congress$year, INDEX=list(paste(congress$state, congress$district)), FUN=min, na.rm=T)
#if(any(min_years!=min(congress$year))) {stop("LAGGED VARIABLE WILL BE WRONG. PANEL START YEARS NOT IDENTICAL")}

congress$voteshare_D_House_l1 <- c(NA, congress$con_demshare[1:(nrow(congress)-1)])
congress$voteshare_D_House_l1[congress$year==min(congress$year)] <- NA
congress$voteshare_R_House_l1 <- c(NA, congress$con_repshare[1:(nrow(congress)-1)])
congress$voteshare_R_House_l1[congress$year==min(congress$year)] <- NA

View(congress[, c("con_raceyear", "statenm", "cd", "con_demshare", "voteshare_D_House_l1")])


############################################################
#########################open-seat##########################
############################################################

unique(congress$con_demstatus)  ## 5088 Challenger, 5202 Incumbents, 520 NAs
unique(congress$con_repstatus)  ##  5590  Challenber, 4293 Incumbents, 927 NAs
congress$incumb <- ifelse(congress$con_demstatus == "Incumbent",1, 
                          ifelse(congress$con_repstatus == "Incumbent", -1,0 ))

#open <- congress[which(congress$con_repstatus=="Challenger" & congress$con_demstatus=="Challenger"), ] ##1237 obs

model1 <- lm(congress$con_demshare~ congress$voteshare_D_House_l1+ factor(congress$incumb))
summary(model1)

model2 <- lm(congress$con_demshare~ congress$voteshare_D_House_l1 + factor(congress$incumb):factor(congress$decade))
summary(model2)

model3 <- lm(congress$con_demshare~ congress$voteshare_D_House_l1 + factor(congress$incumb)*factor(congress$decade))
summary(model3)




############################################################
##################Incumbency##########################
############################################################
library(data.table)

#, congress$con_demstatusm, congress$con_repstatus
congress <- read.csv("cd_panel_full_LOCincumb.csv")
congress <-congress [!(congress$con_demcandidate=="N/A"),]
congress <- congress[order(congress $con_demcandidate, congress$con_raceyear, congress$statenm,congress$cd, congress$con_incumbency), ]


#setDT(congress)[, Seq := rleid(con_raceyear), by=con_demcandidate]
#congress$Seq <- congress$Seq-1
#congress_seq<-subset(congress, select=c("con_demcandidate","con_raceyear", "statenm", "cd","Seq","con_incumbency","con_demstatus"))
#congress <-congress[order(congress$con_demcandidate, congress$con_raceyear, congress$statenm, congress$cd), ]

## "congress_demterm_hand_corrected.csv" is hand-corrected version
## Corrections includes 1) candidates in 1972 elections 2) candidates whose first election is missing 3) candidates who lost in 

corrected <- read.csv("congress_demterm_hand_corrected.csv")

##con_dem_inc_count = term counts (dem candidates)
##dem_inc_bin - 4 bins
congress$con_dem_inc_count <- corrected$dem_termcount

congress$dem_inc_bin <- ifelse(congress$con_dem_inc_count ==0,0, 
                       ifelse(congress$con_dem_inc_count ==1, 1,
                              ifelse(congress$con_dem_inc_count ==2 |congress$con_dem_inc_count ==3, 2,
                                     ifelse(congress$con_dem_inc_count>3, 4, "NA"))))

#write.csv(congress, "congress_demterm_hand_corrected_bin.csv")

congressaa<- subset(congress, select=c("con_demcandidate","con_raceyear", "statenm", "cd","bin","con_dem_inc_count","con_demstatus"))



par(3,1)

K <- congress$term_count
K.cut <- cut(K, c(0,1, 2, 4, 6, 8, 10, Inf))
xax <- barplot(table(K.cut), xaxt='n')
axis(1, at=xax, labels=c('1', '2', '3~4', '5~6', '7~8', '9~10', '> 10'))
box(bty='L')



K.cut <- cut(K, c(0,1, 2, 4, 7, 10, Inf))
xax <- barplot(table(K.cut), xaxt='n')
axis(1, at=xax, labels=c('1', '2', '3~4', '5~6', '7~9', '> 10'))
box(bty='L')


K.cut <- cut(K, c(0,1, 2, 4, 7, Inf))
xax <- barplot(table(K.cut), xaxt='n')
axis(1, at=xax, labels=c('1', '2', '3~4', '5~6', '> 7'))
box(bty='L')


















# linear cubic spline for incumbency advantage
### DEM ###
D <- open[, c("con_demshare", "pres_demshare", "sen1_demshare",
              "sen2_demshare", "gov_demshare", "pop_male_pct", "pop_over65_pct", "pop_white_pct", "pop_black_pct",
              "pop_spanishorigin_pct", "decade",
              "voteshare_D_House_l1")]


D_names <- names(D)[-1]

D_s <- data.frame(apply(D[ , D_names], 2, function(x) x^2))
D_c <- data.frame(apply(D[ , D_names], 2, function(x) x^3))
names(D_s) <- paste0(D_names, "_sq")
names(D_c) <- paste0(D_names, "_cu")
D <- do.call(cbind, list(D, D_s, D_c))

interactions <- combn(names(D)[-1], 2, FUN=paste, collapse='*')

for(i in interactions) {
  nms <- strsplit(i, "\\*")[[1]]
  D[, i] <- D[, nms[1]] * D[, nms[2]]
}




R <- open[, c("voteshare_R_House", "voteshare_R_Pres", "voteshare_R_Gov",
              "gini", "mean_income", "pct_unemp", "pct_hsgrads", "pct_BAdegree", "pct_over60",
              "totalpop", "pct_black", "pct_hispanic", "pct_white",
              "voteshare_R_House_l1", yrdums)]
R_names <- names(R)[-1]

R_s <- data.frame(apply(R[ , R_names], 2, function(x) x^2))
R_c <- data.frame(apply(R[ , R_names], 2, function(x) x^3))
names(R_s) <- paste0(R_names, "_sq")
names(R_c) <- paste0(R_names, "_cu")
R <- do.call(cbind, list(R, R_s, R_c))

interactions <- combn(names(R)[-1], 2, FUN=paste, collapse='*')

for(i in interactions) {
  nms <- strsplit(i, "\\*")[[1]]
  R[, i] <- R[, nms[1]] * R[, nms[2]]
}

D <- na.omit(D)
R <- na.omit(R)

X_D <- as.matrix(D[, -1])
X_R <- as.matrix(R[, -1])

y_D <- D[,1]
y_R <- R[,1]

prep <- function(x, y) {
  dum <- sample(c(T,F), size=nrow(x), prob=c(.6,.4), replace=T)
  return(list(test.x=x[dum, ], test.y=y[dum], valid.x=x[!dum, ], valid.y=y[!dum]))
}
D_part <- prep(X_D, y_D)
R_part <- prep(X_R, y_R)

#rm(list = setdiff(ls(), "D_part"))


lambda <- 10^seq(2, -5, -0.25)









set.seed(1011)
# ridge
Ridge <- glmnet(x=D_part$test.x, y=D_part$test.y, family="gaussian", alpha=0, lambda=lambda, standardize=T)
plot(Ridge)

View(Ridge)

matbeta <- as.matrix(Ridge$beta)
b <- matbeta[, 1]

SQdev <- apply(matbeta, 2, function(B) { (( D_part$valid.x %*% B ) - D_part$valid.y) ^ 2 })
mu <- apply(SQdev, 2, function(x) mean(x))

# use sum instead of mean?
se <- apply(SQdev, 2, function(x) {sqrt( sum((x - mean(x)) ^ 2) / (length(x)-1))})
# cvsd=with(cvstuff, sqrt(apply(scale(cvraw, cvm, FALSE)^2, 2, weighted.mean,
#                               w = weights, na.rm = TRUE)/(N - 1)))

mse <- data.frame(lambda=log(Ridge$lambda), MSE=mu, LSE=mu-se, USE=mu+se)

plot(mse$lambda, mse$MSE, ylim=c(min(mse$LSE), max(mse$USE)), pch=20, col="red")
apply(mse, 1, 
      function(a) {
        lines(x=rep(a[1], 2), y=c(a[3], a[4]), col="gray")
        lines(x=c(a[1]-0.1, a[1]+0.1), y=rep(a[4], 2), col="gray")
        lines(x=c(a[1]-0.1, a[1]+0.1), y=rep(a[3], 2), col="gray")
      })
par(new=T)
plot(mse$lambda, mse$MSE, ylim=c(min(mse$LSE), max(mse$USE)), pch=20, col="red")





# Ridge <- cv.glmnet(x=X_D, y=y_D, lambda=lambda, type.measure="mse", nfolds=5, 
#                    gamma=1, relax=F, alpha=0, standardize=T)
#plot(Ridge)
#coef(Ridge)

#lasso
Lasso <- cv.glmnet(x=X_D, y=y_D, lambda=lambda, type.measure="mse", nfolds=5, 
                   gamma=1, relax=F, alpha=1, standardize=T)
#plot(Lasso)


pdf("../results/PARmse_Dem.pdf", width=11, height=6)
par(mfrow=c(1,2))
plot(Ridge)
mtext("Ridge",side=3,line=0,outer=F,cex=1.3, padj=-3.2)
plot(Lasso, ylab="")
mtext("Lasso",side=3,line=0,outer=F,cex=1.3, padj=-3.2)
dev.off()

Lasso_relax <- cv.glmnet(x=X_D, y=y_D, lambda=lambda, type.measure="mse", nfolds=5, relax=T, alpha=1)
#plot(Lasso_relax)

lambda <- exp(-6.5)

mod <- glmnet(x=X_D, y=y_D, lambda=lambda, alpha=0)

pred <- mod$a0 +  X_D %*% mod$beta

openD <- open[rownames(D), ]
openD$PAR_D <- as.vector(pred)
openD$PARerror <- openD$voteshare_D_House - openD$PAR_D
openD <- openD[order(openD$PARerror, decreasing=T), ]


#View(openD[, c("year", "state", "district", "cand_D_House", "voteshare_D_House", "PAR_D", "PARerror")])
write.csv(openD[, c("year", "state", "district", "cand_D_House", "voteshare_D_House", "PAR_D", "PARerror")],
          "../results/PARdata_Dem.csv",
          row.names=F)

write.csv(data.frame(var=rownames(mod$beta), beta=as.vector(round(mod$beta, digits=4))),
          "../results/PARbetas_Dem.csv",
          row.names=F)

#install.packages("magick")
#webshot::install_phantomjs()
library(modelsummary)
#library(sandwich)
#library(lmtest)

#cols <- !grepl("Y", names(openD))
#f_summary <- All(openD[,cols]) ~ N+Mean+SD+Median+Min+P25+P75+Max
f_summary <- All(D) ~ N+Mean+SD+Median+Min+P25+P75+Max
datasummary(formula=f_summary, data=D, output="../results/sum_stats_Dem.tex")

###

D_nolag <- open[, c("voteshare_D_House", "voteshare_D_Pres", "voteshare_D_Gov",
                    "gini", "mean_income", "pct_unemp", "pct_hsgrads", "pct_BAdegree", "pct_over60",
                    "totalpop", "pct_black", "pct_hispanic", "pct_white",
                    yrdums)]

D <- na.omit(D_nolag)
X_D <- as.matrix(D[, -1])
y_D <- D[,1]

lambda <- 10^seq(2, -5, -0.25)

set.seed(1011)

# ridge
Ridge <- cv.glmnet(x=X_D, y=y_D, lambda=lambda, type.measure="mse", nfolds=5, 
                   gamma=1, relax=F, alpha=0, standardize=T)
#plot(Ridge)
#coef(Ridge)

#lasso
Lasso <- cv.glmnet(x=X_D, y=y_D, lambda=lambda, type.measure="mse", nfolds=5, 
                   gamma=1, relax=F, alpha=1, standardize=T)
#plot(Lasso)


pdf("../results/PARmse_Dem_nolag.pdf", width=11, height=6)
par(mfrow=c(1,2))
plot(Ridge)
mtext("Ridge",side=3,line=0,outer=F,cex=1.3, padj=-3.2)
plot(Lasso, ylab="")
mtext("Lasso",side=3,line=0,outer=F,cex=1.3, padj=-3.2)
dev.off()


lambda <- exp(-6.5)

mod <- glmnet(x=X_D, y=y_D, lambda=lambda, alpha=1)

pred <- mod$a0 +  X_D %*% mod$beta

openD <- open[rownames(D), ]
openD$PAR_D <- as.vector(pred)
openD$PARerror <- openD$voteshare_D_House - openD$PAR_D
openD <- openD[order(openD$PARerror, decreasing=T), ]


write.csv(data.frame(var=rownames(mod$beta), beta=as.vector(round(mod$beta, digits=4))),
          "../results/PARbetas_Dem_nolag.csv",
          row.names=F)



