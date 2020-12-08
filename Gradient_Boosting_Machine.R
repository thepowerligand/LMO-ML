# This code is for the the gradient boosting techiques, also called as the sochastic gradient boosting 
# where randomess is included on top of the randomforest


#First, load all the libraries into the system
library(caret)
library(gbm)
library(doParallel)
library(ggpubr)

#########################  Section 1. Data splitting ##################################

#Read in your data

set.seed (100)
data <-read.csv('LMO.csv')
str(data)
#Split the data into train and test
#note: the replace option in this case is closed

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
training = split_10_fold$train
validate =split_10_fold$test


######################## Section 2. Hyperparameter Optimisation #################################

#Define how you should like to train your model, for example, 10-fold cross-validation
fitControl <- trainControl(method = "cv", number =10,search = "random")


#Build your model out from all the parameters you have identified
gbmFit_IC <- train(IC ~ ï..M+Mn+M_EN+Mr+LC_a+CD, data = train, 
                 method = "gbm", 
                 trControl = fitControl,
                 tuneLength = 1000,
                 verbose = FALSE)


gbmFit_EC <- train(EC ~ ï..M+Mn+M_EN+Mr+LC_a+CD, data = train, 
                   method = "gbm", 
                   trControl = fitControl, 
                   verbose = FALSE, 
                   tuneLength = 1000)

IC <-gbmFit_IC$bestTune

EC <- gbmFit_EC$bestTune

gbm_IC_hp_op <- gbmFit_IC$results

gbm_EC_hp_op <- gbmFit_EC$results

write.csv(gbm_EC_hp_op,"gbm_EC_hp_op")
write.csv(gbm_EC_hp_op,"gbm_EC_hp_op")

saveRDS(gbmFit_EC,file = "gbmFit_EC_hyperparameter_optimisation.RDS")
saveRDS(gbmFit_EC,file = "gbmFit_EC_hyperparameter_optimisation.RDS")

#See the results of all the tuning parameters you have selected
Optimisation_results_IC <- gbmFit_IC$results
Optimisation_results_EC <- gbmFit_EC$results

#Create a csv file for these optimisation results
write.csv(Optimisation_results_EC,"Optimisation_results_gbm_EC")
write.csv(Optimisation_results_EC,"Optimisation_results_gbm_EC")
write.csv(Optimisation_results_V,"Optimisation_results_gbm_V")


########################  Section 3. choosing the best hyperparameter and train the whole model with the optimal hyperparamters ##############

########################### Section 3.1  IC model ###########################

set.seed (100)

IC_train_data <- data.frame(0,0,0)
names(IC_train_data)<- c('Fold','train_predict_IC','train_experimental_IC')

results_IC_train_error <- data.frame (0,0,0)
names(results_IC_train_error) <- c('fold','RMSE_train','R_squared')

for (fold in 1:10){
  split_10_fold <- data_splitting(train,0.1)
  training = split_10_fold$train
  validate =split_10_fold$test
  
  final_model_IC <- train(IC ~ ï..M+M_EN+Mr+LC_a+CD, data = training, 
                        method = "gbm", 
                        tuneGrid = expand.grid(interaction.depth =10,n.trees = 4082,shrinkage =0.04551847, n.minobsinnode = 5) # note, these are the pre-optimised results, 
                        # could be obtained through gbmFit_IC$bestTune 
                        )
  predict_y_train<- predict(final_model_IC,validate)
  RMSE_train <- sqrt(mean((validate$IC-predict_y_train)^2))
  SS_total <-sum(((validate$IC-mean(validate$IC))^2))
  SS_res <- sum((validate$IC-predict_y_train)^2)
  R_squared <- 1-(SS_res/SS_total)
  new_results_IC_train <- data.frame(fold,RMSE_train,R_squared)
  names(new_results_IC_train) <- c('fold','RMSE_train','R_squared')
  new_IC_train_data <- data.frame(fold,predict_IC_train,validate$IC)
  names(new_IC_train_data) <- c('Fold','train_predict_IC','train_experimental_IC')
  results_IC_train_error <- rbind(results_IC_train_error, new_results_IC_train)
  IC_train_data<-rbind(IC_train_data,new_IC_train_data)

}

#obtain the training data from 10-fold cross validations 
results_IC_train_error<- results_IC_train_error[-c(1),]
results_IC_train_error

IC_train_data <- IC_train_data[-c(1),]

RMSE_mean_train_RMSE <- colMeans(results_IC_train_error[2])

R_mean_train <- colMeans(results_IC_train_error[3])

#Train the model with the whole training set

gbm_IC <- train(IC ~ ï..M+M_EN+Mr+LC_a+CD,data = train,
                      method = "gbm", 
                      tuneGrid = expand.grid(interaction.depth =10,n.trees = 4082,shrinkage =0.04551847, n.minobsinnode = 5)
                )


# Predict againt the hold-out test set
predict_y_IC_test<- predict(gbm_IC,test)
predict_y_IC_test
test$IC

a<-(test$IC-predict_y_IC_test)^2

#Calculate the RMSE and R_Square metrics 
RMSE_test_IC <- sqrt(mean((test$IC-predict_y_IC_test)^2))

RMSE_mean_train_RMSE

RMSE_test_IC

IC_test_data = cbind(test$IC,predict_y_IC_test)

SS_total <-sum(((test$IC-mean(test$IC))^2))
SS_res <- sum((IC_test_data[,1]-IC_test_data[,2])^2)
R_squared <- 1-(SS_res/SS_total)

IC_test_data <- cbind(test$IC,predict_y_IC_test)

saveRDS(gbm_IC,'GBM-IC.RDS')
write.csv(IC_train_data,'GBM-IC_TRAIN.csv')
write.csv(results_IC_train_error,'GBM-IC-TRAIN-FOLD.csv')
write.csv(IC_test_data,'GBM-IC-TEST.csv')
write.csv(gbm_IC$importance,'GBM-IC-VAR-IMPO.csv')
write.csv(gbm_IC_hp_op,'EC_IMO_HP.csv')


###################  Section 3.2 EC model ###########################################


EC_train_data <- data.frame(0,0,0)
names(EC_train_data)<- c('Fold','train_predict_EC','train_experimental_EC')

results_EC_train_error <- data.frame (0,0,0)
names(results_EC_train_error) <- c('fold','RMSE_train','R_squared')

second_split <- data_splitting(train,0.1)

validate = second_split$train; training = second_split$test

for (fold in 1:10){
  split_10_fold <- data_splitting(train,0.1)
  training = split_10_fold$train
  validate =split_10_fold$test
  
  final_model_EC <- train(EC ~ M+Mn+M_EN+Mr+LC_a+CD, data = training, 
                          method = "gbm", 
                          tuneGrid = expand.grid(interaction.depth =10,n.trees = 2396,shrinkage =0.1024383, n.minobsinnode = 5) # note, these are the pre-optimised results, 
                        # could be obtained through gbmFit_EC$bestTune
  )
  predECt_EC_train<- predict(final_model_EC,validate)
  RMSE_train <- sqrt(mean((validate$EC-predECt_EC_train)^2))
  R_squared <- (cor(predECt_EC_train, validate$EC))^2
  new_results_EC_train <- data.frame(fold,RMSE_train,R_squared)
  names(new_results_EC_train) <- c('fold','RMSE_train','R_squared')
  new_EC_train_data <- data.frame(fold,predECt_EC_train,validate$EC)
  names(new_EC_train_data) <- c('Fold','train_predict_EC','train_experimental_EC')
  results_EC_train_error <- rbind(results_EC_train_error, new_results_EC_train)
  EC_train_data<-rbind(EC_train_data,new_EC_train_data)
  
}

results_EC_train_error<- results_EC_train_error[-c(1),]
results_EC_train_error

EC_train_data <- EC_train_data[-c(1),]

RMSE_mean_train_RMSE <- colMeans(results_EC_train_error[2])

R_mean_train <- colMeans(results_EC_train_error[3])



#Train the model with the whole training set
gbm_EC <- train(EC ~M+Mn+M_EN+Mr+LC_a+CD,data = train,
                method = "gbm", 
                tuneGrid = expand.grid(interaction.depth =10,n.trees = 2396,shrinkage =0.1024383, n.minobsinnode = 5)
)

# Predict againt the hold-out test set
predict_y_EC_test<- predict(gbm_EC,test)


#Calculate the RMSE values and R_square values to estimate the prediction power of this model
RMSE_test_EC <- sqrt(mean((test$EC-predict_y_EC_test)^2))

RMSE_mean_train_RMSE

RMSE_test_EC
R_squar_test

gbmImp <- varImp(gbm_EC, scale = FALSE)
gbmImp

EC_1 <- cbind(test$EC,predict_y_EC_test)

plot(x=test$EC,y=predict_y_EC_test,xlim=c(60,140),ylim=c(60,140))
abline(coef = c(0,1))


saveRDS(gbm_EC,'GBM-EC.RDS')
write.csv(EC_train_data,'GBM-EC_TRAIN.csv')
write.csv(results_EC_train_error,'GBM-EC-TRAIN-FOLD.csv')
write.csv(EC_1,'GBM-EC-TEST.csv')
write.csv(gbmImp,'GBM-EC-VAR-IMPO.csv')

