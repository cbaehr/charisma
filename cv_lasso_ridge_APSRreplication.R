
setwd("/Users/christianbaehr/Dropbox/charisma_project/data/")

#install.packages("readstata13", "glmnet")

options(expressions = 5e6)

library(lattice)
library(glmnet)
library(dplyr)
set.seed(683750)

cd <- read.csv("working/cd_panel_full.csv")

cd <- cd[which(!cd$con_repunopposed & !cd$con_demunopposed), ]
cd <- cd[which(cd$con_demshare < 0.9 & cd$con_demshare > 0.1), ]
hist(cd$con_demshare)

#cd <- cd[which(cd$con_repstatus!="Incumbent" & cd$con_demstatus!="Incumbent"), ]

##creating a lag variable within each group (= congressional district)
cd <- cd[order(cd$statenm, cd$cd, cd$con_raceyear), ]

cd <- cd %>% 
  group_by(statenm, cd) %>% 
  mutate(lag.con_demshare = dplyr::lag(con_demshare, n=1, default=NA))

cdfull <- cd[complete.cases(cd), ] # drop rows with NA

## selecting features that should be squared (pct - Democrats)
D <- cdfull[, c("lag.con_demshare","pres_demshare", "gov_demshare", "sen1_demshare", "sen2_demshare",
                "pop_male_pct", "pop_over65_pct" , "pop_white_pct", "pop_black_pct"   , "pop_spanishorigin_pct" ,
                "dem_inc_count_consecutive", "dem_inc_count_cumulative", "dem_inc_bin")]

statedums <- data.frame(model.matrix(~ 0 + statenm, cdfull)[,-1]) # create state dummies and drop first

D <- cbind(D, statedums) #join important covariates with state dummies

D_names <- names(D)
D_sq <- data.frame(apply(D[ , D_names], 2, function(x) x^2))
D_cu <- data.frame(apply(D[ , D_names], 2, function(x) x^3))
names(D_sq) <- paste0(D_names, "_sq")
names(D_cu) <- paste0(D_names, "_cu")
D <- do.call(cbind, list(D, D_sq, D_cu))

## interactions for each pair of covariates
interactions <- combn(names(D), 2, FUN=paste, collapse='*')
for(i in interactions) {
  nms <- strsplit(i, "\\*")[[1]]
  D[, i] <- D[, nms[1]] * D[, nms[2]]
}


## define outcome vector and covariate matrix
data <- cbind(cdfull[c("con_demshare")], D)
yname <- "con_demshare"

y = data[,c(yname)]
x = data[, names(data)!="con_demshare"]
x = model.matrix(as.formula(paste(yname, "~ .", sep="")), data)[,-1]

###

## split out training set
train.indx = sample(1:nrow(data), floor(2/3 * nrow(data)))

cvout.ridge = cv.glmnet(x[train.indx,], y[train.indx], alpha=0)
pred = predict (cvout.ridge, s="lambda.min", newx=x[-train.indx,])
terror.ridge = mean((pred - y[-train.indx])^2) #test error 

# Lasso
cvout.lasso = cv.glmnet(x[train.indx,], y[train.indx], alpha=1)
pred = predict (cvout.lasso , s="lambda.min", newx=x[-train.indx,])
terror.lasso = mean((pred - y[-train.indx])^2) # test error 

# Simple lm
lmout = lm(as.formula(paste(yname, "~ .", sep="")), data=data[train.indx,])
pred = predict(lmout, newdata=data[-train.indx,])
terror.lm = mean((pred - y[-train.indx])^2)

cat("Lasso test error is ", terror.lasso, "\n")
cat("Ridge test error is ", terror.ridge, "\n")
cat("Lm test error is ", terror.lm, "\n")

# Now fit the model in the entire data, first choosing between lasso and ridge
minerr = min(c(terror.lasso, terror.ridge, terror.lm))

if((terror.lasso == minerr))   {
  fit = "lasso"
  out = glmnet ( x, y, alpha =1)
  pred = predict (out, s =  cvout.lasso$lambda.min, newx=x, exact =TRUE, x=x, y=y, type="response")
  err  = y - pred    # when err>0, y>pred, and you outperformed the prediction
  coef = predict (out, s =  cvout.lasso$lambda.min, type ="coefficients")
  included.covars.lasso = names(coef[coef[,1]!=0,])
  cat("Chosen fit is lasso and the included covariates are \n")
  print(included.covars.lasso)
  
} else if ((terror.ridge  == minerr)) {
  fit = "ridge"
  out = glmnet (x , y , alpha = 0 )
  pred  = predict(out , s = cvout.ridge$lambda.min, newx = x, exact =TRUE, x=x, y=y, type="response")
  err  = y - pred 
  cat("Chosen fit is ridge \n")
} else {
  fit = "lm"
  out = lm(as.formula(paste(yname, "~ .", sep="")), data=data)
  pred = predict (out, newdata=data, type="response")
  err  = y - pred 
  cat("Chosen fit is lm \n")
}

if( nrow(pred) == nrow(cdfull) & ncol(pred) == 1 )  pred = pred[,1] else stop("pred is not a column vector")
if( nrow(err)  == nrow(cdfull) & ncol(err)  == 1  ) err  =  err[,1] else stop("err  is not a column vector")

rows <- nrow(cdfull)
pred <- as.vector(pred)
err <- as.vector(err)

write.csv(cbind(data.frame(cdfull), pred, err, terror.lasso, terror.ridge,  terror.lm, fit, rows), 
          file = paste("working/predicted-mv.csv", sep=""))

###

pdf(file = "../results/voteshare_prediction/lasso_MSE.pdf", width = 8, height = 6)
plot(cvout.lasso)
dev.off()

out = glmnet ( x, y, alpha =1) # lasso
pred = predict (out, s =  cvout.lasso$lambda.min, newx=x, exact =TRUE, x=x, y=y, type="response")
err  = y - pred    # when err>0, y>pred, and you outperformed the prediction
coef = predict (out, s =  cvout.lasso$lambda.min, type ="coefficients")

pdf(file = "../results/voteshare_prediction/lasso_coefs.pdf", width = 8, height = 6)
dotplot(coef[abs(coef[,1])>1,], xlab = sprintf("Coefficients with abs. value >1 (%s total nonzero coefficients)", sum(coef[,1]>0)))
dev.off()

###

pdf(file = "../results/voteshare_prediction/ridge_MSE.pdf", width = 8, height = 6)
plot(cvout.ridge)
dev.off()

###

result <- read.csv("working/predicted-mv.csv", stringsAsFactors = F)

result <- result[order(result$err, decreasing = T), ]

result <- result[, c("statenm", "con_raceyear", "con_demcandidate", "err", "con_demshare", "pred")]

library(xtable)

print(xtable(result[1:50,], type="latex"), file="../results/voteshare_prediction/top50residuals.tex")

pdf(file = "../results/voteshare_prediction/parError.pdf", width = 8, height = 6)
hist(result$err, main=" ", xlab = "Actual - Predicted")
dev.off()



