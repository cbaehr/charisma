
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

features <- c("pres_demshare", "gov_demshare", "sen1_demshare", "sen2_demshare",
              "pop_male_pct", "pop_over65_pct" , "pop_white_pct", "pop_black_pct"   , "pop_spanishorigin_pct" ,
              "dem_inc_count_consecutive", "dem_inc_count_cumulative", "dem_inc_bin")

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

###

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

write.csv(cbind(data.frame(cdfull[-train.indx, ]), pred.lm, pred.ridge, pred.lasso, error.lm, error.ridge, error.lasso),
          file = paste("../results/demvoteshare_estimation/demvoteshare_predictions.csv", sep=""),
          row.names = F)

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



