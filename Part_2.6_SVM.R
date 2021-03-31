
#   --------------------------------------------------------------------------------------------------
#   PART 2.6 Support Vector Machines (SVM)
#   --------------------------------------------------------------------------------------------------

# Dataset: hrtnew - standardized and encoded (dummy) variables; see Part_1
dim(hrtnew)     # 270  25
str(hrtnew)

library(caTools)      
library(ROCR)

# Splitting data into training & test data:
library(caTools)
set.seed(333)   
sample = sample.split(hrtnew,SplitRatio = 0.78) 
train_hrt = subset(hrtnew,sample ==TRUE) 
test_hrt = subset(hrtnew, sample==FALSE)

dim(train_hrt)  # 204  25
dim(test_hrt)   #  66  25

#-------------------------------------------------------------
#a. Create the SVM model with svmLinear method - Before tuning
    # first, create traincontrol function
trainctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(333)
SvmLinear <- train(hrtDisease ~., data = train_hrt, method = "svmLinear",
                    trControl=trainctrl, tuneLength = 10 )
SvmLinear # Accuracy  0.840938     
   
#b. Testing the model - on train_hrt dataset with Linear method
pred_lin_AL_train <- predict(SvmLinear, train_hrt)
cm_lin_AL_train <- confusionMatrix(train_hrt$hrtDisease, pred_lin_AL_train)
cm_lin_AL_train     # Accuracy: 0.8873

#c.                 - on test_hrt dataset
pred_lin_AL_test <- predict(SvmLinear, test_hrt)
cm_lin_AL_test <- confusionMatrix(test_hrt$hrtDisease, pred_lin_AL_test)
cm_lin_AL_test      # # Accuracy: 0.803 

accuracy_svmTrain <- table(train_hrt$hrtDisease, pred_lin_AL_train)
accuracy_svmTrain <- round(sum(diag(accuracy_svmTrain)) / sum(accuracy_svmTrain), 4)
accuracy_svmTrain 

accuracy_svmTest <- table(test_hrt$hrtDisease, pred_lin_AL_test)
accuracy_svmTest <- round(sum(diag(accuracy_svmTest)) / sum(accuracy_svmTest), 4)
accuracy_svmTest 
#---------------------------------------------------------------------------

#d. Create an SVM model with svmRadial method and compare before tuning
set.seed(333)
SvmRadial <- train(hrtDisease ~., data = train_hrt, method = "svmRadial",
                   trControl=trainctrl,
                   tuneLength = 10 )
SvmRadial 

#e. Test the model on train_hrt dataset with Linear method
pred_lin_AR_train <- predict(SvmRadial, train_hrt)
cm_lin_AR_train <- confusionMatrix(train_hrt$hrtDisease, pred_lin_AR_train)
cm_lin_AR_train     # Accuracy: 0.902 

#f. Test the model on test_hrt dataset
pred_lin_AR_test <- predict(SvmRadial, test_hrt)
cm_lin_AR_test <- confusionMatrix(as.factor(test_hrt$hrtDisease), as.factor(pred_lin_AR_test))
cm_lin_AR_test      # Accuracy: 0.7879 
 
#-------------------------------------------------------------------------
# smv Linear method gave a better test accuracy of 0.803 compared to 0.7879 of the svmRadial method
#-------------------------------------------------------------

#g. Tuning the C values to re-create a better model with svmLinear method.
 C_grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 2))
 set.seed(333)
 tune_svm_Lin_Grid <- train(hrtDisease ~., data = train_hrt, method = "svmLinear",
                           trControl=trainctrl, tuneGrid = C_grid, tuneLength = 10)
 tune_svm_Lin_Grid

 #h. Test the model svm_Lin_Grid on train_hrt dataset
 pred_lin_Grid_train <- predict(tune_svm_Lin_Grid, train_hrt)
 cm_lin_Grid_train <- confusionMatrix(as.factor(train_hrt$hrtDisease), pred_lin_Grid_train)
 cm_lin_Grid_train      # Accuracy: 0.8775
 
#i. Test the model svm_Lin_Grid on test_hrt dataset
pred_lin_Grid_test <- predict(tune_svm_Lin_Grid, test_hrt)
cm_lin_Grid_test <- confusionMatrix(as.factor(test_hrt$hrtDisease), pred_lin_Grid_test)
cm_lin_Grid_test    # Accuracy : 0.7879
#-------------------------------------------------------------------------
# The tuned model with svmLinear method gave a lower test accuracy (of (0.7879) 
# compared to its accuracy of 0.803 before tuning 
#-------------------------------------------------------------------------

# Evaluate model the model ( the one before tuning) 
#   Format variables
test_hrt$hrtDisease <- as.numeric(test_hrt$hrtDisease)
pred_lin_AL_test <- as.numeric(pred_lin_AL_test)-1

library(pROC)
prediction_SvmL <- prediction(pred_lin_AL_test, test_hrt$hrtDisease)
pref_SvmL <- performance(prediction_SvmL, "tpr", "fpr")
roc_svm <- roc(test_hrt$hrtDisease, as.numeric(pred_lin_Grid_test))

#plot ROc
par(pty = "s") 
plot(pref_SvmL,   colorize=T)
abline(a=0, b=1)

#AUC
aucSVM <- auc(roc_svm)
aucSVM <- performance(prediction_SvmL, "auc")
aucSVM <- unlist(slot(aucSVM, "y.values"))
aucSVM <- round(aucSVM, 4) # rounding the digits to four decimal places
aucSVM    # AUC 0.7877
title("ROC Curve: SVM", adj=0)
legend(.77, .15, aucSVM, title = "AUC svm", cex = 0.8)
