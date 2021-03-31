
#   --------------------------------------------------------------------------------------------------
#   PART 2.4 NAIVE BAYES MODEL(NB)
#   --------------------------------------------------------------------------------------------------
# Dataset: hrtnew - standardized and encoded (dummy) variables; see Part_1
  dim(hrtnew)         # 270  25
  str(hrtnew)

# installing packages
  library(ElemStatLearn)
  library(e1071)
  library(ROCR)
  library(dplyr)                  
  library(ggplot2)
  library(scales)
  library(reshape)
  
#a. Data partitioning
  set.seed(333)   
  sample = sample.split(hrtnew,SplitRatio = 0.78) 
  train_hrtnew = subset(hrtnew,sample ==TRUE) 
  test_hrtnew = subset(hrtnew, sample==FALSE)
  dim(train_hrtnew)   # 204  25
  dim(test_hrtnew)    #  66  25
  str(train_hrtnew)    

#b. create train/test indices from hrtnew dataframe
  xTrain <- train_hrtnew[,-25]
  yTrain <- train_hrtnew$hrtDisease
  xTest <- test_hrtnew[,-25]
  yTest <- test_hrtnew$hrtDisease
  
#c fit naive bayes model with default params
  NBmodel <- naiveBayes(xTrain, yTrain)
  
#d. confusion matrix -- on "train" data
  table(predict(NBmodel, xTrain), yTrain)
  # confusion matrix -- on "test" data
  table(predict(NBmodel, xTest), yTest)
  
  #Accuracy -- on "train" data
  accuracy_NBtrain = round(mean((predict(NBmodel, xTrain)==yTrain)), 4)
  accuracy_NBtrain  # 0.8627 
  # Accuracy -- on "test" data 
  accuracy_NBtest = round(mean((predict(NBmodel, xTest)==yTest)), 4)
  accuracy_NBtest # 0.7576
  
#e. plot ROC curve
  NBpred <- predict(NBmodel, xTest, type="raw")
  NBprediction <- prediction(NBpred[, 2], yTest)
  NBperf <- performance(NBprediction, 'tpr', 'fpr')
  NBroc <- roc(yTest, NBpred[,2])
  plot(NBperf, col="red", colorize=T)
  abline(a=0, b=1)
  aucNB <- performance(NBprediction, "auc")
  aucNB <- unlist(slot(aucNB, "y.values"))
  aucNB <- round(aucNB, 4) # rounding the digits to four decimal places
  aucNB    # 0.8139
  legend(.77, .15, aucNB, title = "AUC nb", cex = 0.8)
  title("ROC Curve - Naive Bayes", adj=0)
