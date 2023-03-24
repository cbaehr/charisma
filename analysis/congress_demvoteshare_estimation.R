
setwd("/Users/christianbaehr/Dropbox/charisma_project/data/")

options(stringsAsFactors = F)

include.lag=F
openseats.only=T
drop.unopposed=F
include.stateFE=T
include.yearFE=T

library(lattice)
library(glmnet)
library(dplyr)

set.seed(683750)

cd <- read.csv("working/condistrict_panel_full.csv")

###

## creating a lag variable within each group (= congressional district)

cd <- cd[order(cd$statenm, cd$cd, cd$con_raceyear), ]

cd <- cd %>%
  group_by(statenm, cd) %>%
  mutate(lag.con_demshare = dplyr::lag(con_demshare, n=1, default=NA))

###

## if open seats or opposed only dummy is true, then trim the data accordingly

if(openseats.only) {
  cd <- cd[which(cd$con_repstatus!="Incumbent" & cd$dem_inc_count_consecutive==0), ]
} else {
  cd <- cd
}

if(drop.unopposed) {
  cd <- cd[which(!cd$con_repunopposed & !cd$con_demunopposed), ]
} else {
  cd <- cd
}

###

## selecting features that should be squared (pct - Democrats)
if(openseats.only) {
  features <- c("pres_demshare", "gov_demshare", "sen1_demshare", "sen2_demshare",
                "pop_male_pct", "pop_over65_pct" , "pop_white_pct", "pop_black_pct", "pop_spanishorigin_pct")
} else {
  features <- c("pres_demshare", "gov_demshare", "sen1_demshare", "sen2_demshare",
                "pop_male_pct", "pop_over65_pct" , "pop_white_pct", "pop_black_pct", "pop_spanishorigin_pct",
                "dem_inc_count_consecutive", "dem_inc_count_cumulative", "dem_inc_bin")
}


## if lagged DV dummy is true, then add to feature vector
if(include.lag) {
  features <- c("lag.con_demshare", features)
} else{
  features <- features
}

cdfull <- cd[complete.cases(cd[, c("con_demshare", features)]), ] # drop rows with NA
cdfull$con_raceyear <- as.character(cdfull$con_raceyear) #easier for creating fixed effects

D <- cdfull[, features] # only keep features for variable transformation

D_names <- names(D)
D_sq <- data.frame(apply(D[ , D_names], 2, function(x) x^2)) # squared features
D_cu <- data.frame(apply(D[ , D_names], 2, function(x) x^3)) # cubed features
names(D_sq) <- paste0(D_names, "_sq")
names(D_cu) <- paste0(D_names, "_cu")
D <- do.call(cbind, list(D, D_sq, D_cu)) # bind linear, squared, and cubic features

## interactions for each pair of features
interactions <- combn(names(D), 2, FUN=paste, collapse='*')
for(i in interactions) {
  nms <- strsplit(i, "\\*")[[1]]
  D[, i] <- D[, nms[1]] * D[, nms[2]]
}

# if selected, include state fixed effects
if(include.stateFE) {
  statedums <- data.frame(model.matrix(~ 0 + statenm, cdfull)[,-1]) # create state dummies and drop first (multicollinearity)
  D <- cbind(D, statedums) #join important covariates with state dummies
} else {
  D <- D
}

# if selected, include year fixed effects
if(include.yearFE) {
  yeardums <- data.frame(model.matrix(~ 0 + con_raceyear, cdfull)[,-1]) # create state dummies and drop first (multicollinearity)
  D <- cbind(D, yeardums) #join important covariates with state dummies
} else {
  D <- D
}

###

## define outcome vector and covariate matrix
data <- cbind(cdfull[c("con_demshare")], D)

yname <- "con_demshare"

y = data[,c(yname)] # vector of y values
x = data[, names(data)!="con_demshare"] # matrix of X values
x = model.matrix(as.formula(paste(yname, "~ .", sep="")), data)[,-1] # drop intercept (included automatically in model)

##########

## create training set indices
train.indx = sample(1:nrow(data), floor(2/3 * nrow(data)))

## Ridge
cvout.ridge = cv.glmnet(x[train.indx,], y[train.indx], alpha=0) #fit
pred.ridge = predict (cvout.ridge, s="lambda.min", newx=x[-train.indx,]) #predict on test set
error.ridge = pred.ridge - y[-train.indx] #error
terror.ridge = mean(error.ridge^2) #MSE
r_sq.ridge <- 1- ( sum(error.ridge^2) / sum((y[-train.indx] - mean(y[-train.indx]))^2) )  #r squared

## Lasso
cvout.lasso = cv.glmnet(x[train.indx,], y[train.indx], alpha=1) #fit
pred.lasso = predict (cvout.lasso , s="lambda.min", newx=x[-train.indx,]) #predict on test set
error.lasso = pred.lasso - y[-train.indx] #error
terror.lasso = mean(error.lasso^2) #MSE
r_sq.lasso <- 1- ( sum(error.lasso^2) / sum((y[-train.indx] - mean(y[-train.indx]))^2) )  #r squared

## OLS

PAR.data <- data[, c("con_demshare", "pres_demshare", "gov_demshare", "sen1_demshare", "sen2_demshare")] #Bob Erikson approach (plus levels )
#statedums <- data.frame(model.matrix(~ 0 + statenm, cdfull)[,-1]) # create state dummies and drop first (multicollinearity)
yeardums <- data.frame(model.matrix(~ 0 + con_raceyear, cdfull)[,-1])
PAR.data <- cbind(PAR.data, yeardums) #join important covariates with year dummies

lmout = lm(as.formula(paste(yname, "~ .", sep="")), data=PAR.data[train.indx,]) #fit
pred.lm = predict(lmout, newdata=PAR.data[-train.indx,]) #predict on test set
error.lm = pred.lm - y[-train.indx] #error
terror.lm = mean(error.lm^2) #MSE
r_sq.lm <- 1- ( sum(error.lm^2) / sum((y[-train.indx] - mean(y[-train.indx]))^2) ) #r squared

pred.ridge <- as.vector(pred.ridge)
pred.lasso <- as.vector(pred.lasso)
error.ridge <- as.vector(error.ridge)
error.lasso <- as.vector(error.lasso)

elecinfo <- names(cdfull) %in% c("statenm", "cd", "con_raceyear", "con_repcandidate", "con_demcandidate")

write.csv(cbind(data.frame(cdfull[-train.indx, elecinfo]), pred.lm, pred.ridge, pred.lasso, error.lm, error.ridge, error.lasso, data.frame(cdfull[-train.indx, !elecinfo])),
          file = paste("../results/demvoteshare_estimation/demvoteshare_predictions.csv", sep=""),
          row.names = F)

##########

# train on 1990-2014, test on 2016-20

rsq <- function(y, yhat) return(1 - sum((y-yhat)^2) / sum((y-mean(y))^2))

train.indx <- which(cdfull$con_raceyear %in% seq(1990, 2014) )
test.indx <- which(cdfull$con_raceyear > 2014 )

reg <- lm(con_demshare ~ pres_demshare, data=PAR.data[train.indx, ])
ydem_pred = cbind(1, PAR.data$pres_demshare[test.indx]) %*% reg$coefficients
rsq1 <- rsq(PAR.data$con_demshare[test.indx], ydem_pred)
e1 <- PAR.data$con_demshare[test.indx] - ydem_pred
mse1 <- mean(e1^2)

reg <- lm(con_demshare ~ pres_demshare + gov_demshare, data=PAR.data[train.indx, ])
ydem_pred = cbind(1, as.matrix(PAR.data[test.indx, c("pres_demshare", "gov_demshare")])) %*% reg$coefficients
rsq2 <- rsq(PAR.data$con_demshare[test.indx], ydem_pred)
e2 <- PAR.data$con_demshare[test.indx] - ydem_pred
mse2 <- mean((e2)^2)

reg <- lm(con_demshare ~ pres_demshare + gov_demshare + sen1_demshare, data=PAR.data[train.indx, ])
ydem_pred = cbind(1, as.matrix(PAR.data[test.indx, c("pres_demshare", "gov_demshare", "sen1_demshare")])) %*% reg$coefficients
rsq3 <- rsq(PAR.data$con_demshare[test.indx], ydem_pred)
e3 <- PAR.data$con_demshare[test.indx] - ydem_pred
mse3 <- mean((e3)^2)

reg <- lm(con_demshare ~ pres_demshare + gov_demshare + sen1_demshare + sen2_demshare, data=PAR.data[train.indx, ])
ydem_pred = cbind(1, as.matrix(PAR.data[test.indx, c("pres_demshare", "gov_demshare", "sen1_demshare", "sen2_demshare")])) %*% reg$coefficients
rsq4 <- rsq(PAR.data$con_demshare[test.indx], ydem_pred)
e4 <- PAR.data$con_demshare[test.indx] - ydem_pred
mse4 <- mean((e4)^2)

pdf(file = "../results/demvoteshare_estimation/RSQ_demvoteshare_train1990_test2016on.pdf", width=10, height=6)
dotchart(x = c(rsq1, rsq2, rsq3, rsq4),
         labels = c("Pres", "Pres/Gov", "Pres/Gov/Sen1", "Pres/Gov/Sen1/Sen2"),
         xlab = "R^2 for Dem. vote share in test set", ylab = "Independent Variables",
         main = "R^2, Train on 1990-2014, Test 2016-20")
dev.off()

pdf(file = "../results/demvoteshare_estimation/MSE_demvoteshare_train1990_test2016on.pdf", width=10, height=6)
dotchart(x = c(mse1, mse2, mse3, mse4),
         labels = c("Pres", "Pres/Gov", "Pres/Gov/Sen1", "Pres/Gov/Sen1/Sen2"),
         xlab = "MSE for Dem. vote share in test set", ylab = "Independent Variables",
         main = "MSE, train on 1990-2014, test 2016-20")
dev.off()

pdf(file = "../results/demvoteshare_estimation/errordist_demvoteshare_train1990_test2016on.pdf", width=10, height=6)
par(mfrow=c(2, 2))
hist(e1, breaks = 20, xlim = c(-0.6, 0.6), main = " ", ylab = " ", xlab = "Pres")
hist(e2, breaks = 20, xlim = c(-0.6, 0.6), main = " ", ylab = " ", xlab = "Pres/Gov")
hist(e3, breaks = 20, xlim = c(-0.6, 0.6), main = " ", ylab = " ", xlab = "Pres/Gov/Sen1")
hist(e4, breaks = 20, xlim = c(-0.6, 0.6), main = " ", ylab = " ", xlab = "Pres/Gov/Sen1/Sen2")
mtext("Test set error dist. of Dem. vote share, train on 1990-2014, test 2016-20", side=3, outer=TRUE, line=-3)
dev.off()

###

# train on 2/3 data 1972-2020, test on 1/3

train.indx <- sample(c(1:nrow(cdfull)), floor(2/3 * nrow(cdfull)))
test.indx <- which(!rownames(cdfull) %in% train.indx)

reg <- lm(con_demshare ~ pres_demshare, data=PAR.data[train.indx, ])
ydem_pred = cbind(1, PAR.data$pres_demshare[test.indx]) %*% reg$coefficients
rsq1 <- rsq(PAR.data$con_demshare[test.indx], ydem_pred)
e1 <- PAR.data$con_demshare[test.indx] - ydem_pred
mse1 <- mean((e1)^2)

reg <- lm(con_demshare ~ pres_demshare + gov_demshare, data=PAR.data[train.indx, ])
ydem_pred = cbind(1, as.matrix(PAR.data[test.indx, c("pres_demshare", "gov_demshare")])) %*% reg$coefficients
rsq2 <- rsq(PAR.data$con_demshare[test.indx], ydem_pred)
e2 <- PAR.data$con_demshare[test.indx] - ydem_pred
mse2 <- mean((e2)^2)

reg <- lm(con_demshare ~ pres_demshare + gov_demshare + sen1_demshare, data=PAR.data[train.indx, ])
ydem_pred = cbind(1, as.matrix(PAR.data[test.indx, c("pres_demshare", "gov_demshare", "sen1_demshare")])) %*% reg$coefficients
rsq3 <- rsq(PAR.data$con_demshare[test.indx], ydem_pred)
e3 <- PAR.data$con_demshare[test.indx] - ydem_pred
mse3 <- mean((e3)^2)

reg <- lm(con_demshare ~ pres_demshare + gov_demshare + sen1_demshare + sen2_demshare, data=PAR.data[train.indx, ])
ydem_pred = cbind(1, as.matrix(PAR.data[test.indx, c("pres_demshare", "gov_demshare", "sen1_demshare", "sen2_demshare")])) %*% reg$coefficients
rsq4 <- rsq(PAR.data$con_demshare[test.indx], ydem_pred)
e4 <- PAR.data$con_demshare[test.indx] - ydem_pred
mse4 <- mean((e4)^2)

vars <- c("pres_demshare", "gov_demshare", "sen1_demshare", "sen2_demshare", paste0("con_raceyear", seq(1976,2020,2))) #omit 1974
reg <- lm(paste0("con_demshare~", paste0(vars, collapse="+")), data=PAR.data[train.indx, ])
ydem_pred = cbind(1, as.matrix(PAR.data[test.indx, vars])) %*% reg$coefficients
rsq5 <- rsq(PAR.data$con_demshare[test.indx], ydem_pred)
e5 <- PAR.data$con_demshare[test.indx] - ydem_pred
mse5 <- mean((e5)^2)

vars <- c("pres_demshare", paste0("con_raceyear", seq(1976,2020,2))) #omit 1974
reg <- lm(paste0("con_demshare~", paste0(vars, collapse="+")), data=PAR.data[train.indx, ])
ydem_pred = cbind(1, as.matrix(PAR.data[test.indx, vars])) %*% reg$coefficients
rsq6 <- rsq(PAR.data$con_demshare[test.indx], ydem_pred)
e6 <- PAR.data$con_demshare[test.indx] - ydem_pred
mse6 <- mean((e6)^2)

pdf(file = "../results/demvoteshare_estimation/RSQ_demvoteshare_traintwothirds.pdf", width=10, height=6)
dotchart(x = c(rsq1, rsq2, rsq3, rsq4, rsq5, rsq6),
         labels = c("Pres", "Pres/Gov", "Pres/Gov/Sen1", "Pres/Gov/Sen1/Sen2", "Pres/Gov/Sen1/Sen2/Year FE", "Just Pres + Year FE"),
         xlab = "R^2 for Dem. vote share in test set", ylab = "Independent Variables",
         main = "R^2, train on 2/3 of all data, test 1/3")
dev.off()

pdf(file = "../results/demvoteshare_estimation/MSE_demvoteshare_traintwothirds.pdf", width=10, height=6)
dotchart(x = c(mse1, mse2, mse3, mse4, mse5, mse6),
         labels = c("Pres", "Pres/Gov", "Pres/Gov/Sen1", "Pres/Gov/Sen1/Sen2", "Pres/Gov/Sen1/Sen2/Year FE", "Just Pres + Year FE"),
         xlab = "MSE for Dem. vote share in test set", ylab = "Independent Variables",
         main = "MSE, train on 2/3 of all data, test 1/3")
dev.off()

pdf(file = "../results/demvoteshare_estimation/errordist_demvoteshare_traintwothirds.pdf", width=8, height=8)
par(mfrow=c(3, 2))
hist(e1, breaks = 20, xlim = c(-0.6, 0.6), main = " ", ylab = " ", xlab = "Pres")
hist(e2, breaks = 20, xlim = c(-0.6, 0.6), main = " ", ylab = " ", xlab = "Pres/Gov")
hist(e3, breaks = 20, xlim = c(-0.6, 0.6), main = " ", ylab = " ", xlab = "Pres/Gov/Sen1")
hist(e4, breaks = 20, xlim = c(-0.6, 0.6), main = " ", ylab = " ", xlab = "Pres/Gov/Sen1/Sen2")
hist(e5, breaks = 20, xlim = c(-0.6, 0.6), main = " ", ylab = " ", xlab = "Pres/Gov/Sen1/Sen2/Year FE")
hist(e6, breaks = 20, xlim = c(-0.6, 0.6), main = " ", ylab = " ", xlab = "Just Pres + Year FE")
mtext("Test set error dist. of Dem. vote share, train on 2/3 of all data, test 1/3", side=3, outer=TRUE, line=-3)
dev.off()

##########

###

pdf(file="../results/demvoteshare_estimation/PARerror_histogram.pdf", width = 10, height = 6)
par(mfrow=c(1, 3))
hist(error.lm, breaks = 50, xlab = "OLS", main="")
hist(error.ridge, breaks = 50, xlab = "Ridge", main="PAR Error Distribution") 
hist(error.lasso, breaks = 50, xlab = "Lasso", main="")
dev.off()



pred.ridge.insample = predict (cvout.ridge, s="lambda.min", newx=x[train.indx,]) #predict on test set
pred.lasso.insample = predict (cvout.lasso, s="lambda.min", newx=x[train.indx,]) #predict on test set
ridge.residuals <- pred.ridge.insample-y[train.indx]
ridge.lasso <- pred.lasso.insample-y[train.indx]

pdf(file="../results/demvoteshare_estimation/PARerror_histogram_insample.pdf", width = 10, height = 6)
par(mfrow=c(1, 3))
hist(lmout$residuals, breaks = 50, xlab = "OLS", main="")
hist(ridge.residuals, breaks = 50, xlab = "Ridge", main="PAR Error Distribution") 
hist(ridge.lasso, breaks = 50, xlab = "Lasso", main="")
dev.off()

##########

## now train on pre-2016 and predict on 2016-20

## create training set indices
train.indx = which(cdfull$con_raceyear<2016)

## Ridge
cvout.ridge = cv.glmnet(x[train.indx,], y[train.indx], alpha=0) #fit
pred.ridge = predict (cvout.ridge, s="lambda.min", newx=x[-train.indx,]) #predict on test set
error.ridge = pred.ridge - y[-train.indx] #error
terror.ridge = mean(error.ridge^2) #MSE
r_sq.ridge <- 1- ( sum(error.ridge^2) / sum((y[-train.indx] - mean(y[-train.indx]))^2) )  #r squared

## Lasso
cvout.lasso = cv.glmnet(x[train.indx,], y[train.indx], alpha=1) #fit
pred.lasso = predict (cvout.lasso , s="lambda.min", newx=x[-train.indx,]) #predict on test set
error.lasso = pred.lasso - y[-train.indx] #error
terror.lasso = mean(error.lasso^2) #MSE
r_sq.lasso <- 1- ( sum(error.lasso^2) / sum((y[-train.indx] - mean(y[-train.indx]))^2) )  #r squared

## OLS
PAR.data <- data[, c("con_demshare", "pres_demshare", "gov_demshare", "sen1_demshare", "sen2_demshare")] #Bob Erikson approach (plus levels )
#statedums <- data.frame(model.matrix(~ 0 + statenm, cdfull)[,-1]) # create state dummies and drop first (multicollinearity)
yeardums <- data.frame(model.matrix(~ 0 + con_raceyear, cdfull)[,-1])
PAR.data <- cbind(PAR.data, yeardums) #join important covariates with year dummies

lmout = lm(as.formula(paste(yname, "~ .", sep="")), data=PAR.data[train.indx,]) #fit
pred.lm = predict(lmout, newdata=PAR.data[-train.indx,]) #predict on test set
error.lm = pred.lm - y[-train.indx] #error
terror.lm = mean(error.lm^2) #MSE
r_sq.lm <- 1- ( sum(error.lm^2) / sum((y[-train.indx] - mean(y[-train.indx]))^2) ) #r squared

pred.ridge <- as.vector(pred.ridge)
pred.lasso <- as.vector(pred.lasso)
error.ridge <- as.vector(error.ridge)
error.lasso <- as.vector(error.lasso)

write.csv(cbind(data.frame(cdfull[-train.indx, elecinfo]), pred.lm, pred.ridge, pred.lasso, error.lm, error.ridge, error.lasso, data.frame(cdfull[-train.indx, !elecinfo])),
          file = paste("../results/demvoteshare_estimation/demvoteshare_predictions_earlytraining_predict2016-2020.csv", sep=""),
          row.names = F)

pdf(file="../results/demvoteshare_estimation/PARerror_histogram_earlytraining_predict2016-2020.pdf", width = 10, height = 6)
par(mfrow=c(1, 3))
hist(error.lm, breaks = 50, xlab = "OLS", main="")
hist(error.ridge, breaks = 50, xlab = "Ridge", main="PAR Error Distribution") 
hist(error.lasso, breaks = 50, xlab = "Lasso", main="")
dev.off()



pred.ridge.insample = predict (cvout.ridge, s="lambda.min", newx=x[train.indx,]) #predict on test set
pred.lasso.insample = predict (cvout.lasso, s="lambda.min", newx=x[train.indx,]) #predict on test set
ridge.residuals <- pred.ridge.insample-y[train.indx]
ridge.lasso <- pred.lasso.insample-y[train.indx]

pdf(file="../results/demvoteshare_estimation/PARerror_histogram_insample_earlytraining_predict2016-2020.pdf", width = 10, height = 6)
par(mfrow=c(1, 3))
hist(lmout$residuals, breaks = 50, xlab = "OLS", main="")
hist(ridge.residuals, breaks = 50, xlab = "Ridge", main="PAR Error Distribution") 
hist(ridge.lasso, breaks = 50, xlab = "Lasso", main="")
dev.off()








