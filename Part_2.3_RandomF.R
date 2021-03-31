
#   --------------------------------------------------------------------------------------------------
#   PART 2.1 RANDOM FOREST MODEL (rf)
#   --------------------------------------------------------------------------------------------------
#   (Insights from the article https://www.guru99.com/r-random-forest-tutorial.html#1 were applied to this proble)

# Dataset: hrtnew - standardized and encoded (dummy) variables; see Part_1

library(caTools)
library(e1071)
library(randomForest)
library(caret)
library(pROC) 
library(ROCR)

# Data partitioning
set.seed(333)   
sample = sample.split(hrtnew,SplitRatio = 0.78) 
train_hrtnew = subset(hrtnew,sample ==TRUE) 
test_hrtnew = subset(hrtnew, sample==FALSE)

dim(train_hrtnew)   # 204  25
dim(test_hrtnew)    #  66  25
dim(hrtnew)         # 270  25
str(hrtnew)

# step 1) Building the model with the default values.
#   Define the control
trControl <- trainControl(method = "cv", number = 10, search = "grid")
 
#   Run the model
set.seed(333)
rf_default <- train(hrtDisease~.,
    data = train_hrtnew,
    method = "rf",
    metric = "Accuracy",
    trControl = trControl)
print(rf_default)

# Step 2) Search best mtry: Testing the model with values of mtry from 1 to 24
set.seed(333)
tuneGrid <- expand.grid(.mtry = c(1:24))
rf_mtry <- train(hrtDisease~., data = train_hrtnew, method = "rf",
            metric = "Accuracy", tuneGrid = tuneGrid, trControl = trControl,
            importance = TRUE, nodesize = 14, ntree = 300)
print(rf_mtry)
max(rf_mtry$results$Accuracy)  # [1] 0.862316

best_mtry <- rf_mtry$bestTune$mtry 
best_mtry    #  [1] 4

# Step 3) Search the best maxnodes: Create a loop to evaluate the different values of maxnodes
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(2: 5)) {
    set.seed(333)
    rf_maxnode <- train(hrtDisease~., data = train_hrtnew, method = "rf",
                    metric = "Accuracy", tuneGrid = tuneGrid, trControl = trControl, 
                    importance = TRUE, nodesize = 14, maxnodes = maxnodes, ntree = 300)
    current_iteration <- toString(maxnodes)
    store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)
# maxnode = 3

store_maxtrees <- list()
for (ntree in c(10, 50, 100, 200, 300)) {
    set.seed(333)
    rf_maxtrees <- train(hrtDisease~., data = train_hrtnew, method = "rf",
                         metric = "Accuracy", tuneGrid = tuneGrid, trControl = trControl,
                         importance = TRUE, nodesize = 14, maxnodes = 24, ntree = ntree)
    key <- toString(ntree)
    store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)
# max ntrees = 50    

# Final training of model: best mtry = 4; maxnode  = 3; max ntrees = 50

set.seed(333)
fit_rf <- train(hrtDisease~., data = train_hrtnew,
                method = "rf", metric = "Accuracy", tuneGrid = tuneGrid,
                trControl = trControl, importance = TRUE, nodesize = 14,
                ntree = 50,  maxnodes = 3)
print(fit_rf)
    # Accuracy   Kappa    
    # 0.8475758  0.6919936

# Step 5) Evaluate the model
set.seed(333)
pred_Rftrain <- predict(fit_rf, type="raw")
confRfTrain <- table(train_hrtnew$hrtDisease,pred_Rftrain)
accuracy_RfTrain <- round(sum(diag(confRfTrain))/sum(confRfTrain), 4)
accuracy_RfTrain  # [1] 0.9069

set.seed(333)
pred_testRf <-  predict(fit_rf, newdata = test_hrtnew, type="raw")
head(pred_testRf)
confRfTest <- table(test_hrtnew$hrtDisease,pred_testRf)
accuracy_RfTest <- round(sum(diag(confRfTest))/sum(confRfTest), 4)
accuracy_RfTest  # [1] 0.7879

#Obtaining predicted probabilites for Train and Test data
probs_rftrain <- predict(fit_rf, newdata=train_hrtnew, type="prob")
head(probs_rftrain)
probs_rftest <- predict(fit_rf, newdata=test_hrtnew, type="prob")
head(probs_rftest)

prediction_rftest <- prediction(probs_rftest[, 2], test_hrtnew$hrtDisease)
perf_Rf <- performance(prediction_rftest, "tpr", "fpr")
roc_rf <- roc(test_hrtnew$hrtDisease, probs_rftest[, 2])

#plot the ROC curve
par(pty = "s") 
plot(perf_Rf, colorize=T)
abline(a=0, b=1)
#AUC
aucRf <- auc(roc_rf)
aucRf <- round(aucRf, 4) # rounding the digits to four decimal places
aucRf # 0.8623
legend(.77, .15, aucRf, title = "AUC rf", cex = 0.8)
title("ROC Curve - Random Forest", adj=0)
