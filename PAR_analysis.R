

setwd("/Users/christianbaehr/Dropbox/charisma_project/data")

congress <- read.csv("district_panel.csv")

congress$voteshare_D_House <- congress$votes_D_House / congress$totalvote_House
congress$voteshare_R_House <- congress$votes_R_House / congress$totalvote_House

congress$voteshare_D_Pres <- congress$votes_D_Pres / congress$totalvote_Pres
congress$voteshare_R_Pres <- congress$votes_R_Pres / congress$totalvote_Pres

congress$voteshare_D_Gov <- congress$votes_D_Gov / congress$totalvote_Gov
congress$voteshare_R_Gov <- congress$votes_R_Gov / congress$totalvote_Gov

congress <- congress[order(congress$state, congress$district, congress$year), ]

min_years <- tapply(congress$year, INDEX=list(paste(congress$state, congress$district)), FUN=min, na.rm=T)
if(any(min_years!=min(congress$year))) {stop("LAGGED VARIABLE WILL BE WRONG. PANEL START YEARS NOT IDENTICAL")}

congress$voteshare_D_House_l1 <- c(NA, congress$voteshare_D_House[1:(nrow(congress)-1)])
congress$voteshare_D_House_l1[congress$year==min(congress$year)] <- NA

congress$voteshare_R_House_l1 <- c(NA, congress$voteshare_R_House[1:(nrow(congress)-1)])
congress$voteshare_R_House_l1[congress$year==min(congress$year)] <- NA

#View(congress[, c("year", "state", "district", "voteshare_D_House", "voteshare_D_House_l1")])

##########

unique(congress$status_D_House)

open <- congress[which(congress$status_D_House=="Challenger" & congress$status_R_House=="Challenger"), ]

# year dummies
years <- lapply(sort(unique(open$year))[-1], function(x) (open$year == x) * 1)
yrdums <- paste0("Y", sort(unique(open$year))[-1])
open[, yrdums] <- years

# linear cubic spline for incumbency advantage

library(glmnet)

### DEM ###

D <- open[, c("voteshare_D_House", "voteshare_D_Pres", "voteshare_D_Gov",
              "gini", "mean_income", "pct_unemp", "pct_hsgrads", "pct_BAdegree", "pct_over60",
              "totalpop", "pct_black", "pct_hispanic", "pct_white",
              "voteshare_D_House_l1", yrdums)]

R <- open[, c("voteshare_R_House", "voteshare_R_Pres", "voteshare_R_Gov",
              "gini", "mean_income", "pct_unemp", "pct_hsgrads", "pct_BAdegree", "pct_over60",
              "totalpop", "pct_black", "pct_hispanic", "pct_white",
              "voteshare_R_House_l1", yrdums)]

D <- na.omit(D)
R <- na.omit(R)

X_D <- as.matrix(D[, -1])
X_R <- as.matrix(R[, -1])

y_D <- D[,1]
y_R <- R[,1]

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


pdf("../results/PARmse_Dem.pdf", width=11, height=6)
par(mfrow=c(1,2))
plot(Ridge)
mtext("Ridge",side=3,line=0,outer=F,cex=1.3, padj=-3.2)
plot(Lasso, ylab="")
mtext("Lasso",side=3,line=0,outer=F,cex=1.3, padj=-3.2)
dev.off()

#Lasso_relax <- cv.glmnet(x=X_D, y=y_D, lambda=lambda, type.measure="mse", nfolds=5, relax=T, alpha=1)
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



