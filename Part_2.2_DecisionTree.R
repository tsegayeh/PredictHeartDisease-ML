
#   --------------------------------------------------------------------------------------------------
#   PART 2.5 DECISION TREE MODEL (D Tree)
#   --------------------------------------------------------------------------------------------------

# Dataframe:  hrtnew - standardized and encoded (dummy) variables; see Part_1

hrt_Dtree <- hrtnew # recreating the dataframe
dim(hrt_Dtree)   # [1] 270  25
str(hrt_Dtree)

library(rpart)
library(pROC) 
library(ROCR)

# Shuffle records
set.seed(333)
shuffle_index <- sample(1:nrow(hrt_Dtree))
head(shuffle_index)   #[1] 127  23 261 153   6 192
hrtDtree <- hrt_Dtree[shuffle_index, ] #renaming the dataframe

# Create Train and Test datasets
# create function create-train-Test
create_train_test <- function(hrt_Dtree, size = 0.78, train = TRUE) {
  n_row = nrow(hrt_Dtree)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (hrt_Dtree[train_sample, ])
  } else {
    return (hrt_Dtree[-train_sample, ])
  }
}
set.rseed(333)
data_train <- create_train_test(hrtDtree, 0.78, train = TRUE)
data_test <- create_train_test(hrtDtree, 0.78, train = FALSE)
dim(data_train) # [1] 210  25
dim(data_test)  # [1] 60 14

library(rpart.plot)
Mod_Dtree <- rpart(hrtDisease ~., data = data_train, method = 'class')
rpart.plot(Mod_Dtree, extra = 106)

predictd_train <-predict(Mod_Dtree, data_train, type = 'class')
cmat_train <- table(data_train$hrtDisease, predictd_train)
cmat_train

predictd_test <-predict(Mod_Dtree, data_test, type = 'class')
cmat_test <- table(data_test$hrtDisease, predictd_test)
cmat_test

accuracy_DtreeTrain <- round(sum(diag(cmat_train)) / sum(cmat_train), 4)
accuracy_DtreeTest <- round(sum(diag(cmat_test)) / sum(cmat_test), 4)

accuracy_DtreeTrain   # 0.8667
accuracy_DtreeTest    # 0.8833

# Tununing Hyper-parameters
# on Test set
  accuracy_tune <- function(Mod_Dtree) {
    predictd_test <- predict(Mod_Dtree, data_test, type = 'class')
    cmat_test <- table(data_test$hrtDisease, predictd_test)
    accuracy_DtreeTest <- sum(diag(cmat_test)) / sum(cmat_test)
    accuracy_DtreeTest
  }
control <- rpart.control(minsplit = 3, minbucket = round(5 / 3), maxdepth = 3, cp = 0)
tune_fit <- rpart(hrtDisease~., data = data_train, method = 'class', control = control)
accuracy_tune(tune_fit)   # [1] 0.9

# Calculate ROC curve
probs_Dtreetest=predict(tune_fit, newdata=data_test, type="prob")
prediction_Dtree <- prediction(probs_Dtreetest[, 2], data_test$hrtDisease)
pref_Dtree <- performance(prediction_Dtree,  "tpr", "fpr")
roc_Dtree <- roc(data_test$hrtDisease, probs_Dtreetest[, 2])

#plot the ROC curve
par(pty = "s") 
plot(pref_Dtree, col="red",   colorize=T)
abline(a=0, b=1)
#AUC
aucDtree <- auc(roc_Dtree)
aucDtree <- round(aucDtree, 4) # rounding the digits to four decimal places
aucDtree    # 0.9236
legend(.77, .15, aucDtree, title = "AUC", cex = 0.8)
