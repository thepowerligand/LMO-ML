
#Load in all the required libraries
library (caret)
library(rpart)
library(rpart.plot)
set.seed(1001)

#Read in the data 
data <- read.csv('LMO.csv',stringsAsFactors = F)


######
#NOTE: IC in this code stands for the initial discharge capacities
#EC stands for the end discharge capacities

#########################  Section 1. Data splitting ##################################
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



#Build the decision_tree_model

############################## Section 2. Optimise the complexity factor parameter (cp) in each case ######################

####### IC
caret.control <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
DT_IC <- train(IC ~ M + Mn + M_EN + Mr + LC_a + CD, data = train, method ="rpart",
                          trControl= caret.control)



####### EC
DT_EC <- train(EC ~ M + Mn + M_EN + Mr + LC_a + CD, data = train, method ="rpart",
                          trControl= caret.control)



DT_IC$bestTune
DT_EC$bestTune



################################ Section 3. IC model training ####################################
IC_train_data <- data.frame(0,0,0)
names(IC_train_data)<- c('Fold','train_predict_IC','train_experimental_IC')

results_IC_train_error <- data.frame (0,0)
names(results_IC_train_error) <- c('fold','RMSE_train')

for (fold in 1:10){
  split_10_fold <- data_splitting(train,0.1)
  training = split_10_fold$train
  validate =split_10_fold$test
  
  decision_tree_ic <-rpart(IC ~ M + Mn + M_EN + Mr + LC_a + CD,data = training, cp=0.03598739)
  predict_IC_train<- predict(decision_tree,validate)
  RMSE_train <- sqrt(mean((validate$IC-predict_IC_train)^2))
  new_results_IC_train <- data.frame(fold,RMSE_train)
  names(new_results_IC_train) <- c('fold','RMSE_train')
  new_IC_train_data <- data.frame(fold,predict_IC_train,validate$IC)
  names(new_IC_train_data) <- c('Fold','train_predict_IC','train_experimental_IC')
  results_IC_train_error <- rbind(results_IC_train_error, new_results_IC_train)
  IC_train_data<-rbind(IC_train_data,new_IC_train_data)
  
}

new_results_IC_train[-c(1),]; 
results_IC_train_error[-c(1),]

#Make prediction on the test side with respect to test set 
predict <- predict(decision_tree,test)

predict


#Calculate the RMSQ on the prediction

RMSE  <- sqrt(mean((predict-test$IC)^2))

RMSE
mean(new_results_IC_train$RMSE_train)

#Check for the variable importance
varImp(decision_tree)

### Combine the obv and predict variables

IC_test_data <- cbind(test$EC,predict)

IC_test_data

##### Save the results-file
saveRDS(decision_tree_ic,'DT-IC.RDS')
write.csv(IC_train_data,'DT-IC_TRAIN.csv')
write.csv(results_IC_train_error,'DT-IC-TRAIN-FOLD.csv')
write.csv(IC_test_data,'DT-IC-TEST.csv')
write.csv(varImp(decision_tree_ic),'DT-IC-VAR-IMPO.csv')







################################ Section 4. EC model training ####################################
EC_train_data <- data.frame(0,0,0)
names(EC_train_data)<- c('Fold','train_predict_EC','train_experimental_EC')

results_EC_train_error <- data.frame (0,0)
names(results_EC_train_error) <- c('fold','RMSE_train')

for (fold in 1:10){
  split_10_fold <- data_splitting(train,0.1)
  training = split_10_fold$train
  validate =split_10_fold$test
  
  decision_tree <-rpart(EC ~ M + Mn + M_EN + Mr + LC_a + CD,data = training, cp=0.06079231)
  predict_EC_train<- predict(decision_tree,validate)
  RMSE_train <- sqrt(mean((validate$EC-predict_EC_train)^2))
  new_results_EC_train <- data.frame(fold,RMSE_train)
  names(new_results_EC_train) <- c('fold','RMSE_train')
  new_EC_train_data <- data.frame(fold,predict_EC_train,validate$EC)
  names(new_EC_train_data) <- c('Fold','train_predict_EC','train_experimental_EC')
  results_EC_train_error <- rbind(results_EC_train_error, new_results_EC_train)
  EC_train_data<-rbind(EC_train_data,new_EC_train_data)
  
}

new_results_EC_train[-c(1),]; 
results_EC_train_error[-c(1),]

#View the training results 
 pre<-decision_tree_cv$result

 #Summary the model complexity 
summary(decision_tree_cv)

#Make prediction on the test side with respect to test set 
predict <- predict(decision_tree,test)

predict


#Calculate the RMSQ on the prediction

RMSE  <- sqrt(mean((predict-test$EC)^2))

RMSE
mean(new_results_EC_train$RMSE_train)

#Check for the variable importance
varImp(decision_tree)

### Combine the obv and predict variables

EC_test_data <- cbind(test$EC,predict)

EC_test_data

##### Save the results-file
saveRDS(decision_tree,'DT-EC.RDS')
write.csv(EC_train_data,'DT-EC_TRAIN.csv')
write.csv(results_EC_train_error,'DT-EC-TRAIN-FOLD.csv')
write.csv(EC_test_data,'DT-EC-TEST.csv')
write.csv(varImp(decision_tree),'DT-EC-VAR-IMPO.csv')

