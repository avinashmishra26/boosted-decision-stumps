BoostingDecisionStump <- function(train_Data, Boosted_Tree, learning_rate){
  
  y_Train <- train_Data$medv;
  output_models = list()
  
  for(i in 1:Boosted_Tree) {
    decisionStump <- createDecisionStump(train_Data)
    #print(unlist(decisionStump))
    for(j in 1:nrow(train_Data)){
      residual <- 0
      if(decisionStump$attribute == "lstat"){
        train_value <- train_Data$lstat[j]
      } else if(decisionStump$attribute == "rm"){
        train_value <- train_Data$rm[j]
      }
 
      threshold_value <- decisionStump$thresold
      YMeanLessthan <- decisionStump$Yless_hat
      YMeanMorethan <- decisionStump$Ygreater_hat
      
      if(train_value < threshold_value){
        residual <- YMeanLessthan
      }else{
        residual <- YMeanMorethan
      }
      y_Train[j] <- y_Train[j] - learning_rate*residual
      
    }
    
    train_Data$medv <- y_Train
    output_models <- append(output_models, list(decisionStump))
  }
  return(output_models)
}

#calculating Test MSE for the Decision Stump created above
calculateBDSTestMSE <- function(decisionStump_models, Test_Data, learning_rate){
  
  B_TestMSE <- data.frame()
  predict_y <- rep(0, dim(Test_Data)[1])
  y_test <- Test_Data$medv
  i = 1
  
  for (decisionStump in decisionStump_models) {
      
      for(j in 1:nrow(Test_Data)) {
        test_feature_value <- NA
        if(decisionStump$attribute == "lstat"){
          test_feature_value <- Test_Data$lstat[j]
        } else if(decisionStump$attribute == "rm"){
          test_feature_value <- Test_Data$rm[j]
        }
        if(!is.na(test_feature_value))
        {
          if(test_feature_value < decisionStump$thresold) {
            predict_y[j] <- predict_y[j] + learning_rate*decisionStump$Yless_hat
          } else {
            predict_y[j] <- predict_y[j] + learning_rate*decisionStump$Ygreater_hat
          }
        }
      }
  
      test_mse = sum((y_test - predict_y)^2)/dim(Test_Data)[1]
      
      MSE = data.frame(B = i, TEST_MSE = test_mse)
      B_TestMSE = rbind(B_TestMSE, MSE)
      i = i+1
  }
  return (B_TestMSE)
}

