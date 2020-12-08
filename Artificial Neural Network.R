

####  Load in the library ######
library(keras)
library(tensorflow)
library(tfdeploy)
set.seed (1001)
library(ggplot)
#install_keras(method='conda', tensorflow='2.0.0b1')

# load data into the system
dat <- read.csv('LMO.csv',stringsAsFactors = F)
X <- as.matrix(dat[, 1:6])

# Select the responsive variable here, switch in between IC and EC

y <- dat$IC


#########################  Section 1. Data splitting ##################################

# data_split function split data into a training set and a test set
data_split <- function(X, y, test_perc=0.1){
  n <- nrow(X)
  n_test <- round(n * test_perc)
  mask <- sample(n, n_test, replace=F)
  X_train <- X[-mask, ]
  X_test <- X[mask, ]
  y_train <- y[-mask]
  y_test <- y[mask]
  return(list(X_train=X_train, X_test=X_test, y_train=y_train, y_test=y_test))
}


ret <- data_split(X, y, 0.2)

X_training <- ret$X_train; X_test <- ret$X_test; 
y_training <- ret$y_train; y_test <- ret$y_test

re <-data_split(X_training,y_training,0.1)

X_train <- re$X_train;y_train <- re$y_train
X_validate <- re$X_test;y_validate <- re$y_test


#########################  Section 2. Define the artificial neural network  ##################################

MultiLayerNeuralNet <- function(lr, reg=0, hidden_dim, hidden_layers)
{
  initializer <- initializer_variance_scaling(scale=2)
  regularizer <- regularizer_l2(l=reg)
  layer_list <- list()
  
  for (l in 1:hidden_layers){
    if (l == 1){
      layer_list[[l]] <- layer_dense(units=hidden_dim, activation='relu', 
                                     kernel_initializer=initializer, 
                                     kernel_regularizer=regularizer,
                                     input_shape=c(6))
    } else {
      layer_list[[l]] <- layer_dense(units=hidden_dim, activation='relu', 
                                     kernel_initializer=initializer, 
                                     kernel_regularizer=regularizer)
    }
  }
  layer_list[[hidden_layers + 1]] <- layer_dense(units=1, activation='linear', 
                                                 kernel_initializer=initializer,
                                                 kernel_regularizer=regularizer)
  
  model <- keras_model_sequential(layer_list)
  compile(model, optimizer=optimizer_adam(lr=lr), 
          loss=loss_mean_squared_error)
  return(model)
}

######################  Step 3. Optimisation of the hyperparameters with the defined gride parameters for each response variable (IC and EC) ########

learning_rate <-c(0.001,0.01,0.1)
hidden_dimension <- seq(5,13, by =1)
hidden_layers <- seq(5,13, by =1)

results <-data.frame(0,0,0,0)
names(results) <- c("learning_rate","hidden_dimension","hidden_layers","rmse")
rmse <-0

for (fold in 1:10){
  for (i in learning_rate){
    for (k in hidden_dimension){
      for (l in hidden_layers){
        re <-data_split(X_training,y_training,0.1)
        X_train <- re$X_train;y_train <- re$y_train
        X_validate <- re$X_test;y_validate <- re$y_test
        model <- MultiLayerNeuralNet(lr=i, reg=0, hidden_dim = k, hidden_layers=l)
        history <- fit(model, X_train, y_train, batch_size=10, epochs=1000)
        rmse <- rmse + sqrt(evaluate(model, X_validate, y_validate))
        new_results<-data.frame(i,k,l,rmse)
        names(new_results) <- c("learning_rate","hidden_dimension","hidden_layers","rmse")
        results <- rbind(results, new_results)
      }
    }
  }
}
 
###################  Complete #################

################### Section 4. Build the model and obtain the train error #######

################### IC ##########

train_error_ic <- data.frame(0,0,0)
names(train_error_ic) <- c("Fold","RMSE","R_squared")

for (fold in 1:10){
        re <-data_split(X_training,y_training,0.1)
        X_train <- re$X_train;y_train <- re$y_train
        X_validate <- re$X_test;y_validate <- re$y_test
        model_ic <- MultiLayerNeuralNet(lr=0.001, reg=0, hidden_dim = 14, hidden_layers=5) # Note, these the optimised hyperparameters
        history <- fit(model_ic, X_train, y_train, batch_size=10, epochs=1000)
        rmse <- sqrt(evaluate(model_ic, X_validate, y_validate))
        R_squared<- (cor(predict(model_ic,X_validate),y_validate))^2
        new_results<-data.frame(fold,rmse,R_squared)
        names(new_results) <- c("Fold","RMSE","R_squared")
        train_error_ic <- rbind(train_error_ic, new_results)
}

train_error_ic

write.csv(train_error_ic,"train_error_ic")


##### train the whole model and predict againt the hold-out testset  #####

model_ic <- MultiLayerNeuralNet(lr=0.001, reg=0, hidden_dim = 14, hidden_layers=5) # Note, these the optimised hyperparameters
history <- fit(model_ic, X_training, y_training, batch_size=10, epochs=1000)


##### Use the model to predict againt the hold-out testset and calculate the RMSE and R_square value  #####
predict_y_ic_test <- predict(model_ic,X_test)

rmse_ic <- sqrt(evaluate(model_ic, X_test, y_test))
R_squared_ic<- (cor(predict(model_ic,X_test),y_test))^2

rmse_ic
R_squared_ic

############ Save the results of predicted values versus the experimental value in this case

pre_ex_ic <- cbind(predict_y_ic_test,y_test)
write.csv(pre_ex_ic,"pre_ex_ic")

fit_IC <- lm(predict_y_ic_test~y_test)

fit_IC$model
print(ggplotRegression((fit_IC))+labs(y="Predicted Initial Capacity (mAh/g)",x="Experimental Initial Capacity(mAh/g)" ))

save_model_hdf5(model_ic,"model_ic.h5")

export_savedmodel(model_ic,"model_ic")

############################# EC ######################## 

##############NOTE: NEED TO CHANGE THE RESPONSE VARIABLE AND RESPLIT THE DATASET !!! ######### 

y <- dat$EC
ret <- data_split(X, y, 0.2)

X_training <- ret$X_train; X_test <- ret$X_test; 
y_training <- ret$y_train; y_test <- ret$y_test

re <-data_split(X_training,y_training,0.1)

X_train <- re$X_train;y_train <- re$y_train
X_validate <- re$X_test;y_validate <- re$y_test



#################### Compute the training errors from 10-fold cross validation

train_error_ec <- data.frame(0,0,0)
names(train_error_ec) <- c("Fold","RMSE","R_squared")

for (fold in 1:10){
  re <-data_split(X_training,y_training,0.1)
  X_train <- re$X_train;y_train <- re$y_train
  X_validate <- re$X_test;y_validate <- re$y_test
  model_ec <- MultiLayerNeuralNet(lr=0.001, reg=0, hidden_dim = 13, hidden_layers=12) # pre-optimised hyperparamters
  history <- fit(model_ec, X_train, y_train, batch_size=10, epochs=1000)
  rmse <- sqrt(evaluate(model_ec, X_validate, y_validate))
  R_squared<- (cor(predict(model_ec,X_validate),y_validate))^2
  new_results<-data.frame(fold,rmse,R_squared)
  names(new_results) <- c("Fold","RMSE","R_squared")
  train_error_ec <- rbind(train_error_ec, new_results)
}

train_error_ec

write.csv(train_error_ec,"train_error_ec")

################ train the model over the whole dataset

history <- fit(model_ec, X_training, y_training, batch_size=10, epochs=1000)


##### Use the model to predict againt the hold-out testset and calculate the RMSE and R_square value  #####

rmse_ec <- sqrt(evaluate(model_ec, X_test, y_test))
R_squared_ec<- (cor(predict(model_ec,X_test),y_test))^2
predict_y_ec_test <- predict(model_ec,X_test)
predict_y_ec_test
rmse_ec
R_squared_ec

pre_ex_ec <- cbind(predict_y_ec_test,y_test)
write.csv(pre_ex_ic,"pre_ex_ec")

fit_EC <- lm(predict_y_ec_test~y_validate)
print(ggplotRegression((fit_EC))+labs(y="Predicted End Capacity (mAh/g)",x="Experimental End Capacity(mAh/g)" ))

save_model_hdf5(model_ec,"model_ec.h5")

export_savedmodel(model_ec,"model_ec")

###### Plot the results graph #######
ggplotRegression <- function (fit) {
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(color = "blue",size=4) +
    theme_bw()+
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))+ 
    theme(text = element_text(size = 25),axis.text.x = element_text(hjust = 1),
          axis.title = element_text(size = rel(1),face = "bold"),
          axis.text =  element_text(size = rel(1.5),face = "bold"),plot.background = element_rect(fill = "white",
                                                                                                  colour = "white"))
}

fit_IC <- lm(predict_y_ic_test ~test$IC)
fit_EC <- lm(predict_y_ec_test~test$EC)

print(ggplotRegression((fit_IC))+labs(y="Predicted Initial Capacity (mAh/g)",x="Experimental Initial Capacity(mAh/g)" ))
print(ggplotRegression((fit_EC))+labs(y="Predicted End Capacity (mAh/g)",x="Experimental End Capacity(mAh/g)" ))




