getwd()

# Load in all the libraries required to perform penalized regression models. 

#Note, when Lasso regression is used when aplha = 1. Alternatively, Ridge for alpha = 0

library (glmnet)
library(caTools)
library(stats)
library(ggplot2)

#set the seed number to ensure that the stimulations are reproducible
set.seed(102)
#Read the data
dat <- read.csv("data_LiMn2O4.csv", stringsAsFactor =TRUE)

#Check the structure of the dataset/Matrix
str(dat)

#Check the correlation of each variable

cor(dat)


#Make an correlation graph to see the correlation and the distribution of the data
pairs(dat, col = 'blue')

# Data_splitting
data_splitting <- function (dat, split_ratio)
{
  n <- nrow (dat)
  n_split <- round (n*split_ratio)
  
  ind <- sample(n,n_split, replace = F)
  
  train <- dat[-ind,]
  test <- dat[ind,]
  
  return (list( train = train, test = test))
}


#Split the training set and test set, we have selected the training: testing = 8/2 

first_split <- data_splitting(data,0.2)

train <- first_split$train; test <- first_split$test

X_train <- as.matrix(train[,1:6])
y_train <-train$EC

X_test <- as.matrix(test[,1:6])
y_test <- test$EC

second_split <- data_splitting(train,0.1)
train_set <- second_split$train; validate_set <-second_split$test

X_train_cv <- as.matrix(train_set[,1:6])
y_train_cv <- train_set$EC

X_validate <- as.matrix(validate_set[,1:6])
y_validate <-validate_set$EC


# Now can construct the model first with glmnet, note again, aplha =1 (lasso), alpha =2 (ridge)
penalized_reg <- glmnet(X_train,y_train, alpha = 1, lambda.min.ratio = 1e-6, nlambda = 1000)

#make a cross-validation function for determining the lambda value
penalized_reg_cv <- cv.glmnet(X_train,y_train,alpha = 1, nfolds = 10, lambda = penalized_reg$lambda)

# Plot the graphs to show the variations of lambda
plot(penalized_reg,xvar = 'lambda', label = TRUE)
plot(penalized_reg,xvar = 'dev',label =TRUE)

#Plot the resulting cross which shows the graph of MSE against log of lambda
#The dotted line indicates the optimal value of lambda
plot(penalized_reg_cv)

# Find the minimum optimun lambda value
penalized_reg_cv$lambda.min

# Find the estimated coefficients from the performed ridge regression model
ind <- penalized_reg$lambda==penalized_reg_cv$lambda.min

round (penalized_reg$a0[ind],4)

round (penalized_reg$beta[ind],4)

plot(penalized_reg_cv)
# Now construct the optimal model with the obtained model and perform the 10-fold cross validation on it

pen_re_opt <- glmnet(X_train_cv,y_train_cv,alpha=1,lambda = penalized_reg_cv$lambda.min)

#### Now try to predict it with the test_set

predict_y_test<- predict(pen_re_opt,X_test)
RMSE_test <- sqrt(mean((y_test-predict_y_test)^2))

R_squared <- (cor(predict_y_test,y_test))^2

RMSE_test
R_squared

EC_Lasso <- cbind(predict_y_test,y_test)

write.csv(EC_Lasso,"EC_lasso_pred_expermental.csv")

saveRDS(pen_re_opt,"lasso_EC.rds")

# Plot the predict variable versus experimental variable to see the data distribution

ggplotRegression <- function (fit) {
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

fit_EC <- lm(predict_y_test ~y_test)
ggplotRegression((fit_EC))

#Find the training error through 10-fold cross validation

y_train_predict <- predict(penalized_reg, X_train)
train_error <- sqrt(colMeans((y_train-y_train_predict)^2))

train_error

#Converting the test sets into the matrix form

X_test <- data.matrix(test[,1:6])
Y_test<- test$EC

Y_predict<- predict(penalized_reg,X_test)

Y_predict

#Find the mean square error of the predicted value and that from the test set
test_error <- sqrt(colMeans((Y_predict - Y_test)^2))

test_error
train_error

mean(train_error)
mean(test_error)
cor(Y_predict,test$EC)

#Plot a graph of train error/test error against the log of lambda

plot(log(penalized_reg$lambda),train_error,col='orange',type='l'
     , ylab = 'Error',xlab ='log(lambda)',ylim= c(0,30))

#ylim= c(0,30)
points(log(penalized_reg$lambda),test_error,col='blue',type='l')

abline(v=log(penalized_reg_cv$lambda.min),lty = 3)

legend('topleft', c("Training Error","Test Error"),lty = 1, col = c('orange','blue'))



