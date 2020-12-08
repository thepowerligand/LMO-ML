
# Firstly read in the libraries

library(caret)
library(e1071)
library(kernlab)
set.seed (1001)

dat <- read.csv("LMO.csv")

data_split <- function (data, splitting_ratio,a){
  
  n <- nrow (data)
  n_split <- round (n*splitting_ratio)
  mask <- sample(n,n_split,replace =a)
  
  train <- data[-mask,]
  test <- data[mask,]
  
  return (list(train = train, test = test))
}


#########################  Section 1. Data splitting ##################################
split <- data_split(dat, 0.2,F)

train <- split$train 
test <- split$test


# now from the train set, you need to split them into 1 for validation and 9 for train

split_10_fold <- data_split(train, 0.1, F)

training_set <- split_10_fold$train
validation <- split_10_fold$test


######################## Section 2. Standardise the data for SVM model input #################################
std <- function (v){ 
  
  v = (v- mean(v))/(sd(v))}

# Now apply the standarisation to both the input features from train and test set

#train_standarised <- apply(train[1:7],2,std)
#test_standarised <- apply (test[1:7],2,std)

#train_try <- scale(train[1:7],center = T,scale = T)

# Now you have to created the randomised variables with the runif command



######################## Section 3. Hyperparameter Optimisation #################################

# There are in total two tuning parameters considered in this model : cost and gamma (from the radial basis function)

################### IC  ##############################

cost_seq_ic  <- 10^(runif(100,0,2))

sigma_seq_ic <- 10^(runif(100,-2,0))


results_ic <- data.frame(0,0,0)
names(results_ic) <- c('Cost','Sigma','RMSE')

for (i in 1:100){
  for (fold in 1:10){
    
  split_10_fold <- data_split(train, 0.1, F);
  training_set <- split_10_fold$train;
  validation <- split_10_fold$test;
  #training_std <- apply(training_set[1:7],2,std);
  #validation_std <- apply(validation[1:7],2,std);
  #training <- cbind(training_std,training_set[8:10])
  #validating <- cbind(validation_std,validation[8:10])
  cost  <- cost_seq_ic[i];
  sigma_v <- sigma_seq_ic[i];
  svmFit_IC <- ksvm(IC ~ M+Mn+M_EN+Mr+LC_a+CD,data= training_set, kernel="rbfdot", C = cost,kpar = list((sigma_v)));
  prediction <- predict(svmFit_IC,newdata=validation)
  RMSE_IC <- sqrt(mean((prediction-validation$IC)^2))
  new_results <- data.frame(cost, sigma_v, RMSE_IC)
  names(new_results) <- c('Cost','Sigma','RMSE')
  results_ic <-rbind(results_ic, new_results)
    }
}
# Copy the results into another dataset_under the name results_for_mod_c
results_for_mod_c <- results_ic


#Calculating the average value of the computed RMSE to select the optimal hyperparameters
#From here, you will be able to access which set of the hyperparameter is the best in your case

averg <- function(data,a){
  n <- nrow(data);
  n_last <- n-9
  b <-  a+10;
  new_RMSE <- mean(data[a:b,3])
  
    if (a == n_last){
      new_RMSE <- mean(data[a:n,3])}
  return(list(new_RMSE))
}


a <- seq (2,nrow(results_for_mod_c),by=10) 

hyperparameters_svm_c <- data.frame(0,0,0)
names(hyperparameters_svm_c) <-c('Cost','sigma','RMSE')

for (i in a){
  Mean_RMSE <- averg(results_for_mod_c,i)
  cost <- results_for_mod_c[i,1]; sigma <-results_for_mod_c[i,2]
  new_list_c <- data.frame(cost,sigma,Mean_RMSE)
  names(new_list_c) <- c('Cost','sigma','RMSE')
  hyperparameters_svm_c <-rbind(hyperparameters_svm_c,new_list_c)
} 
 hyperparameters_svm_c <- hyperparameters_svm_c[-c(1),]
 hyperparameters_svm_c
 write.csv(hyperparameters_svm_c,'svm_ic_hp_2')

plot(log10(hyperparameters_svm_c$Cost),hyperparameters_svm_c$RMSE, xlab= c("Cost"), ylab = c("RMSE (mAhg^-1)"), 
     main=c("10-fold cross-validated RMSE value computed from the initial capacity SVM (RBF) model against the cost"))

plot(log10(hyperparameters_svm_c$sigma),hyperparameters_svm_c$RMSE, xlab= c("Sigma"), ylab = c("RMSE (mAhg^-1)"), 
     main=c("10-fold cross-validated RMSE value computed from the initial capacity SVM (RBF) predictive model against the sigma value"))

####################### EC ###################

cost_seq_ec  <- 10^(runif(100,13,3)
                    )
sigma_seq_ec <- 10^(runif(100,-3,-3))


results_ec <- data.frame(0,0,0)
names(results_ec) <- c('Cost','Sigma','RMSE')

for (i in 1:100){
  for (fold in 1:10){
    
    split_10_fold <- data_split(train, 0.1, F);
    training_set <- split_10_fold$train;
    validation <- split_10_fold$test;
    #training_std <- apply(training_set[1:7],2,std);
    #validation_std <- apply(validation[1:7],2,std);
    #training <- cbind(training_std,training_set[8:10])
    #validating <- cbind(validation_std,validation[8:10])
    cost  <- cost_seq_ec[i];
    sigma_v <- sigma_seq_ec[i];
    svmFit_EC <- ksvm(EC ~ M+Mn+M_EN+Mr+LC_a+CD,data= training_set, kernel="rbfdot", C = cost,kpar = list((sigma_v)));
    prediction <- predict(svmFit_EC,newdata=validation)
    RMSE_EC <- sqrt(mean((prediction-validation$EC)^2))
    new_results <- data.frame(cost, sigma_v, RMSE_EC)
    names(new_results) <- c('Cost','Sigma','RMSE')
    results_ec <-rbind(results_ec, new_results)
  }
}

# Copy the results into another dataset_under the name results_for_mod_c
results_for_mod_ec <- results_ec


#Calculating the average value of the computed RMSE to select the optimal hyperparameters
#From here, you will be able to access which set of the hyperparameter is the best in your case

averg <- function(data,a){
  n <- nrow(data);
  n_last <- n-9
  b <-  a+10;
  new_RMSE <- mean(data[a:b,3])
  
  if (a == n_last){
    new_RMSE <- mean(data[a:n,3])}
  return(list(new_RMSE))
}


a <- seq (2,nrow(results_for_mod_ec),by=10) 

hyperparameters_svm_ec <- data.frame(0,0,0)
names(hyperparameters_svm_ec) <-c('Cost','sigma','RMSE')

for (i in a){
  Mean_RMSE <- averg(results_for_mod_ec,i)
  cost <- results_for_mod_ec[i,1]; sigma <-results_for_mod_ec[i,2]
  new_list_ec <- data.frame(cost,sigma,Mean_RMSE)
  names(new_list_ec) <- c('Cost','sigma','RMSE')
  hyperparameters_svm_ec <-rbind(hyperparameters_svm_ec,new_list_ec)
} 
hyperparameters_svm_ec <- hyperparameters_svm_ec[-c(1),]
hyperparameters_svm_ec
write.csv(hyperparameters_svm_ec,'optimal_hyperparameters_svm_ec')

plot(log10(hyperparameters_svm_ec$Cost),hyperparameters_svm_ec$RMSE, xlab= c("Cost"), ylab = c("RMSE (mAhg^-1)"), 
     main=c("10-fold cross-validated RMSE value computed from the end capacity SVM (RBF) model against the cost"))

plot(log10(hyperparameters_svm_ec$sigma),hyperparameters_svm_ec$RMSE, xlab= c("Sigma"), ylab = c("RMSE (mAhg^-1)"), 
     main=c("10-fold cross-validated RMSE value computed from the end capacity SVM (RBF) predictive model against the sigma value"))




######################## Section 4. Model training with the optimised hyperparameters #################################


#######Section 4.1 IC ########

#Check the training error

RMSE_ic_train <- data.frame(0,0,0)
names(RMSE_ic_train) <- c('Fold','R_Squared','RMSE_train_ic')

IC_train_data <- data.frame(0,0,0)
names(IC_train_data)<- c('Fold','train_predict_ic','train_experimental_ic')

for (fold in 1:10){
    split_10_fold <- data_split(train, 0.1, F);
    training_set <- split_10_fold$train;
    validation <- split_10_fold$test;

    # Note, these values are the final optimised ones
    cost  <- 63.38783; 
    sigma_v <- 0.004430498;
    svmFit_IC <- ksvm(IC ~ M+Mn+M_EN+Mr+LC_a+CD,data= training_set, kernel="rbfdot", C = cost,kpar = list((sigma_v)));
    prediction_IC <- predict(svmFit_IC,newdata=validation)
    RMSE_IC <- sqrt(mean((prediction_IC-validation$IC)^2))
    R_squar_IC <-(cor(prediction_IC,validation$IC))^2
    new_IC_train_data <-data.frame(fold,prediction_IC,validation$IC)
    names(new_IC_train_data) <- c('Fold','train_predict_ic','train_experimental_ic')
    new_RMSE_ic_train <- data.frame(fold, R_squar_IC, RMSE_IC)
    names(new_RMSE_ic_train) <- c('Fold','R_Squared','RMSE_train_ic')
    IC_train_data<-rbind(IC_train_data,new_IC_train_data)
    RMSE_ic_train <-rbind(RMSE_ic_train, new_RMSE_ic_train)
  }

RMSE_ic_train <-RMSE_ic_train[-c(1),]

RMSE_mean_train_RMSE <- colMeans(RMSE_ic_train[3])
RMSE_mean_train_RMSE

R_mean_train <- colMeans(RMSE_ic_train[2])



# Note, these values are the final optimised ones


svmFi_IC <- ksvm(IC ~ M+Mn+M_EN+Mr+LC_a+CD,data= train, kernel="rbfdot", C = cost,kpar = list((sigma_v)))

#Test-set Validation

prediction_IC <- predict(svmFit_IC,newdata=test)
RMSE_IC <- sqrt(mean((prediction_IC-test$IC)^2))
R_squar_IC <-(cor(prediction_IC,test$IC))^2

RMSE_mean_train_RMSE
R_mean_train
RMSE_IC
R_squar_IC


##### Save all the results

IC1<-cbind(prediction_IC,test$IC)
write.csv(RMSE_ic_train,'SVM-IMO-IC-TRAIN-FOLD')
write.csv(IC_train_data,'SVM-IMO-IC-TRAIN')
write.csv(IC1,'SVM-IMO-IC-TEST')
saveRDS(svmFit_IC,"svmFit-IMO-IC.rds")

##### Section 4.2 EC ############

results_ec_train_error <- data.frame(0,0,0)
names(results_ec_train_error) <- c('Fold','R_Squared','RMSE_train_ec')

EC_train_data <- data.frame(0,0,0)
names(EC_train_data)<- c('Fold','train_predict_ec','train_experimental_ec')

for (fold in 1:10){
  split_10_fold <- data_split(train, 0.1, F);
  training_set <- split_10_fold$train;
  validation <- split_10_fold$test;

  #Note the following two values are the optimised hyperparameter values
  cost_ec  <- 12323.981;
  sigma_v_ec <- 0.000816546;
  svmFit_EC <- ksvm(EC ~ M+Mn+M_EN+Mr+LC_a+CD,data= training_set, kernel="rbfdot", C = cost_ec,kpar = list((sigma_v_ec)));
  prediction_EC <- predict(svmFit_EC,newdata=validation)
  RMSE_EC <- sqrt(mean((prediction_EC-validation$EC)^2))
  R_squar_EC <-(cor(prediction_EC,validation$EC))^2
  new_EC_train_data <-data.frame(fold,prediction_EC,validation$EC)
  names(new_EC_train_data) <- c('Fold','train_predict_ec','train_experimental_ec')
  new_results_EC <- data.frame(fold, R_squar_EC, RMSE_EC)
  names(new_results_EC) <- c('Fold','R_Squared','RMSE_train_ec')
  EC_train_data<-rbind(EC_train_data,new_EC_train_data)
  results_ec_train_error <-rbind(results_ec_train_error, new_results_EC)
}

EC_train_data <-EC_train_data[-c(1),]

RMSE_mean_train_RMSE <- colMeans(results_ec_train_error[3])

R_mean_train <- colMeans(results_ec_train_error[2])


############### Train the model with whole training dataset #########
svmFit_EC <- ksvm(EC ~ M+Mn+M_EN+Mr+LC_a+CD,data= train, kernel="rbfdot", C = cost_ec,kpar = list((sigma_v_ec)))


############## Predict the hold-out test-set with the newly optimised hyperparameters
prediction_EC <- predict(svmFit_EC,newdata=test)
RMSE_EC <- sqrt(mean((prediction_EC-test$EC)^2))
R_squar_EC <-(cor(prediction_EC,test$EC))^2
s

# Check these values
RMSE_mean_train_RMSE
R_mean_train
RMSE_EC
R_squar_EC

EC1<-cbind(prediction_EC,test$EC)

write.csv(results_ec_train_error,'SVM-IMO-EC-TRAIN-FOLD')
write.csv(EC_train_data,'SVM-IMO-EC_TRAIN')
write.csv(EC1,'SVM-IMO-EC-TEST')
saveRDS(svmFit_EC,"SVM-IMO_EC.rds")


