
#   PART 2.0 PREDICTION MODELS
#   --------------------------------------------------------------------------------------------------
#   PART 2.1 EXTREME GRADIENT BOOST (XGB) MODEL
#   -------------------------------------------------------------------------------------------------- 

# Dataset: hrtnew - standardized and encoded (dummy) variables; see Part_1; see in PART 1)
hrtxgb3 <- hrtnew # recreating a dataframe for this model.
str(hrtxgb3)
head(hrtxgb3$hrtDisease)

# Install packcages
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(caret)
library(pROC)
library(ROCR)


levels(hrtxgb3$hrtDisease)
#[1] "1" "2" (1=absent, 2=present)

#Converting levels to 0 and 1 (0=absent, 1=present)
levels(hrtxgb3$hrtDisease) <- c("0", "1")
levels(hrtxgb3$hrtDisease)

#Converting class to integer
hrtxgb3$hrtDisease <- as.integer(as.character(hrtxgb3$hrtDisease))
class(hrtxgb3$hrtDisease)

#Partitioning data
set.seed(333)
indxz <- sample(2, nrow(hrtxgb3), replace = T, prob = c(0.78, 0.22))
trainz <- hrtxgb3[indxz == 1,]
testz <- hrtxgb3[indxz == 2,]

dim(trainz) 
dim(testz)

#Converting train and test dataframes into matrix
trainmz <- sparse.model.matrix(hrtDisease~.-1, data = trainz)
train_labelz <- trainz[, "hrtDisease"]
train_matrixz <- xgb.DMatrix(data = as.matrix(trainmz), label = train_labelz)

testmz <- sparse.model.matrix(hrtDisease~.-1, data = testz)
test_labelz <- testz[, "hrtDisease"]
test_matrixz <- xgb.DMatrix(data = as.matrix(testmz), label = test_labelz)

dim(trainmz)
dim(testmz)

# set the number of classes
nc <-  length(unique(train_labelz))
# setting parameters for the model
xgb_params <- list("objective" = "multi:softprob", "eval_metric" = "mlogloss", "num_class" = nc)
watchlist <- list(train = train_matrixz, test = test_matrixz)

#Create and train the eXtreme Gradient Boosting Model
set.seed(333)
bst_modelxgbz <- xgb.train(params = xgb_params, data = train_matrixz, nrounds = 53, watchlist = watchlist, 
                    eta = .05, max.depth = 3, gamma = .7, subsample = .7, colsample_bytree = 1)

#Evaluating the model with error plot
e <- data.frame(bst_modelxgbz$evaluation_log)
plot(e$iter, e$train_mlogloss, col = "blue")
lines(e$iter, e$test_mlogloss, col = "red")

min(e$test_mlogloss)
e[e$test_mlogloss ==  0.298299,] # determining the number of iterations (nrounds)

#Feature Importance
fImportancez = xgb.importance(colnames(train_matrixz), model = bst_modelxgbz)
print(fImportancez)
xgb.plot.importance(fImportancez)

# prediction - On training set
p_xgbz_train <- predict(bst_modelxgbz, newdata = train_matrixz)
head(p_xgbz_train)
# Evaluating the model-on training set
pred_xgbz_train <- matrix(p_xgbz_train, nrow = nc, ncol = length(p_xgbz_train)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(label = train_labelz, max_prob = max.col(., "last")-1)
head(pred_xgbz_train)

confMxgbTrain <- table(as.factor(pred_xgbz_train$max_prob), as.factor(train_labelz))
accuracy_xgbtrain <- round(sum(diag(confMxgbTrain))/sum(confMxgbTrain), 4)
accuracy_xgbtrain    # 0.9249

# prediction - On test set
p_xgbz <- predict(bst_modelxgbz, newdata = test_matrixz)
head(p_xgbz)
# Evaluating the model-on test set
pred_xgbz <- matrix(p_xgbz, nrow = nc, ncol = length(p_xgbz)/nc) %>%
t() %>%
  data.frame() %>%
  mutate(label = test_labelz, max_prob = max.col(., "last")-1)
head(pred_xgbz)

confMxgbTest <- table(as.factor(pred_xgbz$max_prob), as.factor(test_labelz))
accuracy_xgbtest <- round(sum(diag(confMxgbTest))/sum(confMxgbTest), 4)
accuracy_xgbtest    # [1] 0.9123

predictionxgbz <- prediction(pred_xgbz[,2], test_labelz)
perfXgbz <- performance(predictionxgbz, "tpr", "fpr")
roc_xgbz <- roc(testz$hrtDisease, pred_xgbz[,2])
auc_xgbz <- performance(predictionxgbz, "auc")

#plot the ROC curve
plot(perfXgbz, colorize = T)
abline(a=0, b=1) 

# Finding AUC value
aucXgb <- auc(roc_xgbz)
aucXgb <- round(aucXgb, 4) # rounding the digits to four decimal places
aucXgb
legend(.77, .15, aucXgb, title = "AUC xgb", cex = 0.8)
