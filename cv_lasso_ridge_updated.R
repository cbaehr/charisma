library(rdrobust)
library(dplyr)
library(modelsummary)
library(data.table)
if(!require(pacman)){
  install.packages('pacman')
  library(pacman)
}
p_load(tidyverse,
       ggplot2,
       caret)


cd <- read.csv("data/working/cd_panel_full.csv")

##creating a lag variable within each group (= candidates)
cd_democrat <- cd[order(cd$con_demcandidate, cd$con_raceyear, cd$statenm, cd$cd), ]
cd_lagged_dem <- 
  cd_democrat%>%
  group_by(con_demcandidate, statenm) %>%
  mutate(lag.con_demshare = dplyr::lag(con_demshare, n = 1, default = NA)) 


cd_lagged_rep <- 
  cd %>%
  group_by(con_repcandidate, statenm) %>%
  mutate(lag.con_repshare = dplyr::lag(con_repshare, n = 1, default = NA))


## selecting features that should be squared (pct - Democrats)
D <- cd_lagged_dem[, c("lag.con_demshare","pres_demshare", "gov_demshare", "sen1_demshare", "sen2_demshare",
            "pop_male_pct", "pop_over65_pct" , "pop_white_pct", "pop_black_pct"   , "pop_spanishorigin_pct" ,   
            "dem_inc_count_consecutive", "dem_inc_count_cumulative", "dem_inc_bin")]


D_names <- names(D)
D_s <- data.frame(apply(D[ , D_names], 2, function(x) x^2))
D_c <- data.frame(apply(D[ , D_names], 2, function(x) x^3))
names(D_s) <- paste0(D_names, "_sq")
names(D_c) <- paste0(D_names, "_cu")
D <- do.call(cbind, list(D, D_s, D_c))


interactions <- combn(names(D)[-1], 2, FUN=paste, collapse='*')
interactions <- combn(names(D), 2, FUN=paste, collapse='*')
for(i in interactions) {
  nms <- strsplit(i, "\\*")[[1]]
  D[, i] <- D[, nms[1]] * D[, nms[2]]
}



#Using two thirds of the data as a training set, we fit
#a “kitchen sink” least squares model that uses all covariates, a ridge regression, and LASSO, using crossvalidation to choose the ridge and LASSO tuning parameters
#######Features : Democrats#######################
##pct-based features

set.seed(12345)
Democrats <- na.omit(cbind(cd$con_demshare, D))
y <- Democrats$`cd$con_demshare`
X <- Democrats[,-1]

# Spliting training set into two parts based on outcome: 75% (training) and 25% (testing)
index <- createDataPartition(y, p=0.75, list=FALSE)
X_train <- X[ index, ]
X_test <- X[-index, ]
y_train <- y[index]
y_test<-y[-index]


#lambda <- 10^seq(-1, 0.6, length = 20)
parameters <- c(seq(-10, 0.6, length = 30) , seq(4, 30, 5))

#10^seq(-10, 0.6, length = 30)
##IF we didn't specify \lambda,the glmnet generates its own sequence which is recommended
set.seed(12345)

lasso_caret <- train(
  X_train, y_train, 
  method = "glmnet",
  trControl = trainControl("cv", number = 5),
  tuneGrid = expand.grid(alpha=0.4, lambda = parameters),
  preProcess = c("scale")
)

ridge_caret <- train(
  X_train, y_train,
  method = "glmnet",
  trControl = trainControl("cv", number = 5),
  tuneGrid = expand.grid(alpha = 0, lambda = parameters),
  #metric= "RMSE"
  preProcess = c("scale")
)

linear <- train(
  X_train, y_train,
  method = 'lm',
  trControl = trainControl("cv", number = 5)
)


##Best Parameters
print(paste0('Lasso best parameters: ' , lasso_caret$finalModel$lambdaOpt))
print(paste0('Ridge best parameters: ' , ridge_caret$finalModel$lambdaOpt))



trellis.par.set(caretTheme())
plot(lasso_caret, metric = "RMSE")
plot(ridge_caret, metric = "RMSE")


data.frame(
  Ridge_RMSE = RMSE(predictions_ridge, y_test) , 
  Lasso_RMSE = RMSE(predictions_lasso, y_test),
  Linear_RMSE = RMSE(predictions_ridge, y_test)
)

data.frame(
  lasso = as.data.frame.matrix(coef(lasso_caret$finalModel, lasso_caret$finalModel$lambdaOpt)),
  ridge = as.data.frame.matrix(coef(ridge_caret$finalModel, ridge_caret$finalModel$lambdaOpt))
) %>%  rename(lasso = s1, ridge = s1.1)



# Make the predictions
predictions_lasso <- lasso_caret %>% predict(X_test)
predictions_ridge <- ridge_caret %>% predict(X)
predictions_lin <- linear %>% predict(X_test)
Democrats$PAR_D <- as.vector(predictions_ridge)

Democrats$PARerror <- Democrats$`cd$con_demshare` - Democrats$PAR_D 
Democrats <- Democrats[order(Democrats$PARerror, decreasing=T), ]



trellis.par.set(caretTheme())
plot(ridge_caret)  



