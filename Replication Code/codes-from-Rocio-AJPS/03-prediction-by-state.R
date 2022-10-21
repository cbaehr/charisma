#install.packages("readstata13", "glmnet")

library(readstata13)
library (glmnet )
set.seed(683750)

################################

## Prediction analysis, party by party
## For parties:  PRB PP PDT PT PTB PMDB PSTU PSL PST PTN PSC PCB PR PPS DEM PAN PSDC PRTB PTC PSB PSDB PPL PSD PCdoB
###############################

partypreds =  c("PRB", "PP", "PDT" ,"PT" ,"PTB" ,"PMDB" ,"PSTU","PSL","PST","PTN", "PSC", "PCB", "PR", "PPS", "DEM", "PAN", 
                "PSDC", "PRTB" ,"PTC", "PSB", "PSDB", "PPL" ,"PSD", "PCdoB")

for(i in 1:length(partypreds)) {
  
  party    = partypreds[i]
  cat("Starting fit for party", party, "\n")
  
  dataALL  = read.dta13(paste("./output/datapred-", party, ".dta", sep=""))
  
  # there should not be any missing values
  if(sum(colSums(is.na(dataALL))) > 0) stop(paste("Why are there missing values for party", party, "?", sep=""))

  yname = paste("mv_party_", party, "for1", sep="")
  nobs = nrow(dataALL)
  cat("There are ", nobs, "observations for party", party, "\n")
  
  # Create model matrix: first remove municipality names and year, otherwise it will create municipality dummies
  data = dataALL[,!(names(dataALL) %in% c("muni_namecl", "year", "uf", "party"))]
  x = model.matrix(as.formula(paste(yname, "~ .", sep="")), data)[,-1]
  y = data[,c(yname)]
  
  train.indx = sample(1:nrow(data), floor(2/3 * nrow(data)))
  
  # Ridge
  cvout.ridge = cv.glmnet (x[train.indx,], y[train.indx], alpha =0)
  pred = predict (cvout.ridge , s = "lambda.min", newx = x[-train.indx,])
  terror.ridge = mean((pred - y[-train.indx])^2)                          # test error 

  # Lasso
  cvout.lasso = cv.glmnet (x[train.indx,], y[train.indx], alpha =1 )
  pred = predict ( cvout.lasso , s = "lambda.min", newx = x[-train.indx,])
  terror.lasso = mean((pred - y[-train.indx])^2)                          # test error 

  # Simple lm
  lmout = lm(as.formula(paste(yname, "~ .", sep="")), data=data[train.indx,])
  pred = predict (lmout, newdata=data[-train.indx,] )
  terror.lm = mean((pred - y[-train.indx])^2)  
  
  cat("Lasso test error for party", party, " is ", terror.lasso, "\n")
  cat("Ridge test error for party", party, " is ", terror.ridge, "\n")
  cat("Lm test error for party", party, " is ", terror.lm, "\n")
  
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
  
  if( nrow(pred) == nrow(dataALL) & ncol(pred) == 1 )  pred = pred[,1] else stop("pred is not a column vector")
  if( nrow(err)  == nrow(dataALL) & ncol(err)  == 1  ) err  =  err[,1] else stop("err  is not a column vector")
  
  write.csv(cbind(dataALL, pred, err, terror.lasso, terror.ridge,  terror.lm, fit, nobs), 
            file = paste("./output/predicted-mv-", party, ".csv", sep=""))
}
