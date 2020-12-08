library(randomForest)
library(caret)
set.seed (1001)

data <- read.csv('LMO.csv',stringsAsFactors = F)
str(data)

#optimisation <- read.csv('Hyperparameter_EC')



#########################  Section 1. Data splitting ##################################


#Create train and test set 
data_splitting <- function (dat, split_ratio)
{
  n <- nrow (dat)
  n_split <- round (n*split_ratio)
  
  ind <- sample(n,n_split, replace = F)
  
  train <- dat[-ind,]
  test <- dat[ind,]
  
  return (list( train = train, test = test))
}

#Split into train, validate and test 
split_root <- data_splitting(data,0.2)

train = split_root$train 
test = split_root$test 


split_10_fold <- data_splitting(train,0.1)
train_model = split_10_fold$train
test_model =split_10_fold$test


#Create Random forest for the train data 



#########################  Section 2. Hyperparameter-tuning ##################################

num_trees_IC <- floor(runif(500,100,5000))

num_variables_IC <- c(1,2,3,4,5)



results_IC_train <- data.frame (0,0,0,0)
names(results_IC_train) <- c('number of variables','number of trees','RMSE_train','R_squared')


# Train the model with 10 fold cross-validation and help to optimise the hyperparameters

#########################  Section 2.1 IC-Hyperparameter-tuning ##################################

for (i in num_variables_IC){
  for (k in num_trees_IC){
    for (fold in 1:10){
      split_10_fold <-data_splitting(train,0.1)
      train_model = split_10_fold$train
      test_model =split_10_fold$test
      
      rf_EC <- randomForest(IC ~M+Mn+M_EN+Mr+LC_a+CD,
                         data=train_model, mtry = i, ntree =k, importance = TRUE)
      
      predict_y_train<- predECt(rf_EC,test_model)
      RMSE_train <- sqrt(mean((test_model$IC-predict_y_train)^2))
      SS_total <-sum((test_model$IC-mean(test_model$IC)^2))
      SS_res <- sum((test_model$IC-predict_y_train)^2)
      R_squared <- 1-(SS_res/SS_total)
      new_results_IC_train <- data.frame(i,k,RMSE_train,R_squared)
      names(new_results_EC_train) <- c('number of variables','number of trees','RMSE_train','R_squared')
      results_IC_train <- rbind(results_IC_train, new_results_IC_train)
    }
  }
}



#########################  Section 2.1 EC-Hyperparameter-tuning ##################################

num_variables_ec <- c(1,2,3,4,5)
num_trees_ec <- floor(runif(500,100,5000))


results_ec_train <- data.frame (0,0,0,0)
names(results_ec_train) <- c('number of variables','number of trees','RMSE_train','R_squared')

# Train the model with 10 fold cross-validation and help to optimise the hyperparameters
for (i in num_variables_ec){
  for (k in num_trees_ec){
    for (fold in 1:10){
      split_10_fold <-data_splitting(train,0.1)
      train_model = split_10_fold$train
      test_model =split_10_fold$test
      
      rf_ec <- randomForest(EC ~ M+Mn+M_EN+Mr+LC_a+CD,
                            data=train_model, mtry = i, ntree =k, importance = TRUE)
      
      predict_y_train<- predECt(rf_ec,test_model)
      RMSE_train <- sqrt(mean((test_model$EC-predict_y_train)^2))
      SS_total <-sum((test_model$EC-mean(test_model$EC)^2))
      SS_res <- sum((test_model$EC-predict_y_train)^2)
      R_squared <- 1-(SS_res/SS_total)
      new_results_ec_train <- data.frame(i,k,RMSE_train,R_squared)
      names(new_results_ec_train) <- c('number of variables','number of trees','RMSE_train','R_squared')
      results_ec_train <- rbind(results_ec_train, new_results_ec_train)
    }
  }
}


#################### Section 3. Hyperparameters selection ###################################

# Asssign the results into new data for modifECation purposes

results_EC_train_mod <-results_EC_train
results_ec_train_mod <-results_ec_train


# Now you need to create a averaging function to get the mean value out of 10-fold results set
averg <- function(data,a){
  n <- nrow(data);
  b <-  a+9;
  new_RMSE <- mean(data[a:b,3])
  new_R_square <- mean(data[a:b,4])
  return(list(new_RMSE,new_R_square))
}

# Now you can evaluate the results in terms of their mean average values


#################### Section 3.1 Find the mean values of different sets of hyperparameters ###################################
############################# IC ##############################################
a <- seq (2,nrow(results_IC_train_mod),by=10)

hyperparameters_rf_IC <- data.frame(0,0,0,0)
names(hyperparameters_rf_IC) <-c('num_variable','num_tree','RMSE','R_squared')

for (i in a){
  Mean_RMSE <- averg(results_IC_train_mod,i)[[1]]
  Mean_R_squared <- averg(results_IC_train_mod,i)[[2]]
  num_variable <- results_IC_train_mod[i,1]; num_trees <-results_IC_train_mod[i,2]
  new_list_c <- data.frame(num_variable,num_trees,Mean_RMSE,Mean_R_squared)
  names(new_list_c) <- c('num_variable','num_tree','RMSE','R_squared')
  hyperparameters_rf_IC <-rbind(hyperparameters_rf_IC,new_list_c)
} 

hyperparameters_rf_IC <- hyperparameters_rf_IC[-c(1),]

################################ EC ####################################
hyperparameters_rf_ec <- data.frame(0,0,0,0)
names(hyperparameters_rf_ec) <-c('num_variable','num_tree','RMSE','R_squared')

for (i in a){
  Mean_RMSE <- averg(results_ec_train_mod,i)[[1]]
  Mean_R_squared <- averg(results_ec_train_mod,i)[[2]]
  num_variable <- results_ec_train_mod[i,1]; num_trees <-results_ec_train_mod[i,2]
  new_list_c <- data.frame(num_variable,num_trees,Mean_RMSE,Mean_R_squared)
  names(new_list_c) <- c('num_variable','num_tree','RMSE','R_squared')
  hyperparameters_rf_ec <-rbind(hyperparameters_rf_ec,new_list_c)
} 

hyperparameters_rf_ec <- hyperparameters_rf_ec[-c(1),]


########## Now save these optimisation results like below ####

write.csv(hyperparameters_rf_EC,"RF-IMO-EC-HYPERPARAMETERS.csv")
write.csv(hyperparameters_rf_ec,"RF-IMO-EC-HYPERPARAMETERS.csv")


######### now you can select the sets with the minimum RMSE
# Select the row that gives you the minimum value

min_EC <- hyperparameters_rf_EC$RMSE[which.min(hyperparameters_rf_EC$RMSE)]
min_row_EC <- which(hyperparameters_rf_EC[,3] == min_EC)

min_ec <- hyperparameters_rf_ec$RMSE[which.min(hyperparameters_rf_ec$RMSE)]
min_row_ec <- which(hyperparameters_rf_ec[,3] == min_ec)


#################### Section 4. Model training with the optimal hyperparameters ###################################

########## Now you can build the model up with the full training set ################

#################### Section 4.1 IC model training ###################################

split_root <- data_splitting(data,0.2)

train = split_root$train 
test = split_root$test 

IC_train_data <- data.frame(0,0,0)
names(IC_train_data)<- c('Fold','train_predict_IC','train_experimental_IC')

results_EC_train_error <- data.frame (0,0,0)
names(results_EC_train_error) <- c('fold','RMSE_train','R_squared')



for (fold in 1:10){
      split_10_fold <-data_splitting(train,0.1)
      train_model = split_10_fold$train
      test_model =split_10_fold$test
      set.seed(1001)
      rf_EC_train <- randomForest(IC ~ï..M+Mn+M_EN+Mr+LC_a+CD,
                            data=train_model, mtry = 4, ntree =876
                            , importance = TRUE)
      
      predict_y_train<- predict(rf_IC_train,test_model)
      RMSE_train <- sqrt(mean((test_model$IC-predict_y_train)^2))
      SS_total <-sum(((test_model$IC-mean(test_model$IC))^2))
      SS_res <- sum((test_model$IC-predict_y_train)^2)
      R_squared <- 1-(SS_res/SS_total)
      new_results_IC_train <- data.frame(fold,RMSE_train,R_squared)
      names(new_results_IC_train) <- c('fold','RMSE_train','R_squared')
      new_IC_train_data <- data.frame(fold,predict_y_train,test_model$IC)
      names(new_IC_train_data) <- c('Fold','train_predict_IC','train_experimental_IC')
      results_IC_train_error <- rbind(results_IC_train_error, new_results_IC_train)
      IC_train_data<-rbind(IC_train_data,new_IC_train_data)
    }

results_IC_train_error<- results_IC_train_error[-c(1),];results_IC_train_error

IC_train_data <- IC_train_data[-c(1),]

RMSE_mean_train_RMSE <- colMeans(results_IC_train_error[2]); R_mean_train <- colMeans(results_IC_train_error[3])

# Train the model with the optimal hyperparameters
set.seed(1001)
rf_IC <- randomForest(IC ~ï..M+Mn+M_EN+Mr+LC_a+CD,
                      data=train, mtry = 4, ntree =876
                      , importance = TRUE)

#################### Section 4.2 (IC) Test the prediction power of the optimised model with the holdout test-set ###################################

# selected metrics are Root-mean-square-error (RMSE) and R_square
predict_y_IC_test<- predict(rf_IC,test)

#RMSE on testset
RMSE_test_IC <- sqrt(mean((test$IC-predict_y_IC_test)^2)) 

#R_square values on test-set
SS_total <-sum(((test$IC-mean(test$IC))^2))
SS_res <- sum((test$IC-predict_y_IC_test)^2)
R_squared_test <- 1-(SS_res/SS_total)

#View the results
RMSE_mean_train_RMSE ;R_mean_train

RMSE_test_IC; R_squared_test


IC_test_data <- cbind(test$IC,predict_y_IC_test)


saveRDS(rf_IC,'RF-IMO-IC.rds')
write.csv(IC_train_data,'RF-IMO-IC-TRAIN.csv')
write.csv(results_IC_train_error,'RF-IMO-IC-TRAIN-ERROR.csv')
write.csv(IC_1,'RF-IMO-IC-TEST-ERROR.csv')
write.csv(rf_IC$importance,'RF-IMO-IC-VAR-IMPO.csv')





#################### Section 4.3 EC model training ###################################
EC_train_data <- data.frame(0,0,0)
names(EC_train_data)<- c('Fold','train_predict_EC','train_experimental_EC')

results_EC_train_error <- data.frame (0,0,0)
names(results_EC_train_error) <- c('Fold','RMSE_train','R_squared')



for (fold in 1:10){
  split_10_fold <-data_splitting(train,0.1)
  train_model = split_10_fold$train
  test_model =split_10_fold$test
  
  rf_ec_train <- randomForest(EC ~M+Mn+M_EN+Mr+LC_a+CD,
                              data=train_model, mtry = 4, ntree =876
                              , importance = TRUE)
  
  predict_y_train<- predict(rf_ec_train,test_model)
  RMSE_train <- sqrt(mean((test_model$EC-predict_y_train)^2))
  SS_total <-sum((test_model$EC-mean(test_model$EC)^2))
  SS_res <- sum((test_model$EC-predict_y_train)^2)
  R_squared <- 1-(SS_res/SS_total)
  new_results_EC_train <- data.frame(fold,RMSE_train,R_squared)
  names(new_results_EC_train) <- c('Fold','RMSE_train','R_squared')
  new_EC_train_data <- data.frame(fold,predict_y_train,test_model$EC)
  names(new_EC_train_data) <- c('Fold','train_predict_EC','train_experimental_EC')
  results_EC_train_error <- rbind(results_EC_train_error, new_results_EC_train)
  EC_train_data<-rbind(EC_train_data,new_EC_train_data)
}

EC_train_data <- EC_train_data[-c(1),]

EC_train_data
results_ec_train_error <- results_ec_train_error[-c(1),];results_ec_train_error

#write.csv(results_ec_train_error,'RF-IMO-EC-TRAIN-ERROR.csv')

#### Validate against the test-set
rf_ec <- randomForest(EC ~M+Mn+M_EN+Mr+LC_a+CD,
                      data=train, mtry = hyperparameters_rf_ec[min_row_ec,1], ntree =hyperparameters_rf_ec[min_row_ec,2]
                      , importance = TRUE)

#################### Section 4.4 (EC) Test the prediction power of the optimised model with the holdout test-set ###################################

predict_y_ec_test<- predict(rf_ec,test)

#RMSE on testset
RMSE_test_ec <- sqrt(mean((test$EC-predict_y_ec_test)^2)) 

#R_square values on test-set
SS_total <-sum((test$EC-mean(test$EC)^2))
SS_res <- sum((test$EC-predict_y_ec_test)^2)
R_squared_test <- 1-(SS_res/SS_total)

#View the results
RMSE_mean_train_RMSE ;R_mean_train

RMSE_test_ec; R_squared_test


EC_test_data <- cbind(test$EC,predict_y_ec_test)
EC_train_data


#R_Square-EC

Res_sum_sq <- sum((EC_test_data[,1]-EC_test_data[,2])^2)
tot_var <-sum((EC_test_data[,1]-mean(EC_test_data[,1]))^2)

saveRDS(rf_EC,'RF-IMO-EC.rds')
write.csv(EC_train_data,'RF-IMO-EC-TRAIN.csv')
write.csv(results_EC_train_error,'RF-IMO-EC-TRAIN-ERROR.csv')
write.csv(EC_1,'RF-IMO-EC-TEST-ERROR.csv')
write.csv(rf_EC$importance,'RF-IMO-EC-VAR-IMPO.csv')


