createDecisionStump = function(dataset){
  
  y_train <- dataset$medv

  RSS_Vars = find_ALL_Variables_RSS(dataset, y_train)
  
  decision_stump_model = list()
  Ylessthan <- 0
  YlessthanCount <- 0
    
  YgreaterthanCount<- 0
  Ygreaterthan<- 0
    
  Y_hat_lessthan<-  0
  Y_hat_greatethan <- 0
  
  if(min(RSS_Vars$Total_Rss_rm) < min(RSS_Vars$Total_Rss_lstat)){
    thresold_rm = dataset$rm[which.min(RSS_Vars$Total_Rss_rm)]

    for(i in 1:nrow(dataset)){
      if (dataset$rm[i] < thresold_rm)
      {
        Ylessthan = Ylessthan+ y_train[i]
        YlessthanCount =YlessthanCount+1
      }
      if(dataset$rm[i] > thresold_rm){
        Ygreaterthan = Ygreaterthan+ y_train[i]
        YgreaterthanCount = YgreaterthanCount+1
      }
      
    }
    Y_hat_lessthan <- Ylessthan/YlessthanCount
    Y_hat_greatethan <- Ygreaterthan/YgreaterthanCount
    decision_stump_model = list(attribute = "rm", thresold = thresold_rm,
                       Yless_hat = Y_hat_lessthan,Ygreater_hat = Y_hat_greatethan)
  }
  else{
    thresold_lstat = dataset$lstat[which.min(RSS_Vars$Total_Rss_lstat)]
    
    for(i in 1:nrow(dataset)){
      if (dataset$lstat[i] < thresold_lstat)
      {
        Ylessthan = Ylessthan+ y_train[i]
        YlessthanCount =YlessthanCount+1
      }
      if(dataset$lstat[i] > thresold_lstat){
        Ygreaterthan = Ygreaterthan+ y_train[i]
        YgreaterthanCount = YgreaterthanCount+1
      }
      
    }
    Y_hat_lessthan <- Ylessthan/YlessthanCount
    Y_hat_greatethan <- Ygreaterthan/YgreaterthanCount
    decision_stump_model = list(attribute = "lstat", thresold = thresold_lstat,
                       Yless_hat = Y_hat_lessthan,Ygreater_hat = Y_hat_greatethan)
  }
}

calculateRSS <- function(x_feature, y_value, X_feature_thresold){
  Ylessthan <- 0
  YlessthanCount <- 0
  
  Ygreaterthan <- 0
  YgreaterthanCount <- 0

  
  for(i in 1:length(x_feature)) {
    if (x_feature[i] < X_feature_thresold)
    {
      Ylessthan = Ylessthan + y_value[i]
      YlessthanCount = YlessthanCount + 1
    }
    else if(x_feature[i] > X_feature_thresold){
      Ygreaterthan = Ygreaterthan + y_value[i]
      YgreaterthanCount = YgreaterthanCount + 1
    }
  }
  if(YlessthanCount > 0){
      mean_Ylessthan <- Ylessthan/YlessthanCount
  } else {
    mean_Ylessthan = 0
  }
  mean_Ygreatethan <- Ygreaterthan/YgreaterthanCount
  
  RSS_Ylessthan <- 0
  RSS_Ygreaterthan <- 0
  
  for(i in 1:length(x_feature)){
    if (x_feature[i] < X_feature_thresold )
    {
      RSS_Ylessthan <- RSS_Ylessthan + (y_value[i]-mean_Ylessthan)^2
    }
    else if(x_feature[i] > X_feature_thresold)
    {
      RSS_Ygreaterthan <-  RSS_Ygreaterthan+(y_value[i]-mean_Ygreatethan)^2
    }
    
  }
  #print(RSS_Ylessthan)
  #print(RSS_Ygreaterthan)
  TotalRSS <- RSS_Ylessthan + RSS_Ygreaterthan
  return(TotalRSS)
  
}

#Evaluating All possible RSS for feature lstat and RM
find_ALL_Variables_RSS <- function(dataset, y_train){
  Total_Rss_lstat <- c()
  Total_Rss_rm <- c()
  for(i in 1:nrow(dataset)){
    lstat_feature_threshold <- dataset$lstat[i]
    Total_Rss_lstat[i] <- calculateRSS(dataset$lstat,y_train, lstat_feature_threshold)
    rm_feature_threshold <- dataset$rm[i]
    Total_Rss_rm[i]<- calculateRSS(dataset$rm,y_train,rm_feature_threshold)
  }
  return (list(Total_Rss_rm = Total_Rss_rm, Total_Rss_lstat = Total_Rss_lstat))
}

#calculating Test MSE for the Decision Stump created above
calculateTestMSE <- function(decisionStump, Test){
  
  y_test <- Test$medv
  predict_y <- rep(0, dim(Test)[1])

  for(j in 1:nrow(Test)) {
      test_feature_value <- NA
      if(decisionStump$attribute == "lstat"){
        test_feature_value <- Test$lstat[j]
      } else if(decisionStump$attribute == "rm"){
        test_feature_value <- Test$rm[j]
      }
      if(!is.na(test_feature_value))
      {
        if(test_feature_value < decisionStump$thresold) {
          predict_y[j] <- decisionStump$Yless_hat
        } else {
          predict_y[j] <- decisionStump$Ygreater_hat
        }
      }
      
        
  }
  #print(length(y_test))
  #print(length(predict_y))
  test_mse = sum((y_test - predict_y)^2)/dim(Test)[1]
  
  return (test_mse)
}




