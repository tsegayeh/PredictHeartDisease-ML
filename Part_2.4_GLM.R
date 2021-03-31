
#   --------------------------------------------------------------------------------------------------
#   PART 2.1 LOGISTIC REGRESSION MODEL (OR Generalized Linear Model, GLM)
#   -------------------------------------------------------------------------------------------------- 

# Dataset: hrtnew - - standardized and encoded (dummy) variables; see Part_1
dim(hrtnew)     # 270  25
str(hrtnew)

library(caTools)      
library(ROCR)

# a. Data partitioning
set.seed(321)   
sample = sample.split(hrtnew,SplitRatio = 0.78) 
train_hrt = subset(hrtnew,sample ==TRUE) 
test_hrt = subset(hrtnew, sample==FALSE)

dim(train_hrt)  # 206  25
dim(test_hrt)   #  64  25

# b. Create model  
set.seed(321)
mod_glm <- glm(hrtDisease~., family = "binomial", data = train_hrt)
summary(mod_glm)
   
# c. Train the GLM model
pred_glmtrain <- predict(mod_glm, train_hrt, type="response")
train_glm <- cbind(train_hrt, pred_hrtDisease=ifelse(pred_glmtrain >.5, 2, 1))
table(train_glm$hrtDisease, train_glm$pred_hrtDisease)

# d. Test the GLM model
pred_glmtest <- predict(mod_glm, test_hrt, type="response")
test_glmtest <- cbind(test_hrt, pred_hrtDisease=ifelse(pred_glmtest >.5, 2, 1))
table(test_glmtest$hrtDisease, test_glmtest$pred_hrtDisease)
      accuracy_glmtrain = round(mean(train_glm$hrtDisease==train_glm$pred_hrtDisease), 4)
      accuracy_glmtrain   # [1] 0.8883
      accuracy_glmtest = round(mean(test_glmtest$hrtDisease==test_glmtest$pred_hrtDisease), 4)
      accuracy_glmtest    # [1] 0.7656 
# Note:     
      # While training the model the following warning poped up: "Warning message:
      #   In predict.lm(object, newdata, se.fit, scale = 1, type = if (type ==  :
      #   prediction from a rank-deficient fit may be misleading"
      # The warning is indicative of two things: NA's and/or high collinearity
      # In response to the warning message: 
      #   a) model was re-created excluding six variables that produced NA's. 
      #       * sex_lev_x_Male (column 8)         * chestpain_lev_x_typicalAngina (12)
      #       * fbldsugar_lev_x_gt_20mgPerdl (14) * excinducedangina_lev_x_yes (18)
      #       * Slope_ST_lev_x_upsloping (21)     * thal_lev_x_reversable (24)
      #   b) the data was checked for high collinearity; 
      #      the six variables that produced NA's were found to be highly (-0.9 to -1.0) 
      #      coorelated with one other variable
      # However, the AIC value and other results for the model did not change
      
      # recreating model without the six variables
      train_hrt2 <- train_hrt[-c(8,12,14,18,21,24)]
      set.seed(321)
      mod_glm_2 <- glm(hrtDisease~., family = "binomial", data = train_hrt2)
      summary(mod_glm_2)
      
      pred_glmtrain2 <- predict(mod_glm_2, train_hrt2, type="response")
      train_glm2 <- cbind(train_hrt2, pred_hrtDisease=ifelse(pred_glmtrain2 >.5, 2, 1))
      table(train_glm2$hrtDisease, train_glm2$pred_hrtDisease)
      
      # Checking for high Collinearity
      corrtable <- cor(train_hrt[sapply(train_hrt, is.numeric)])
      train_hrtnumeric <- train_hrt[sapply(train_hrt,is.numeric)]
      corr <- round(cor(train_hrtnumeric), 1)
      ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)
      # the afforementioned six variables were found to have a high correlation (-0.9 to 1.0)
      
  #d. Evaluate the GLM model -- # ROC AUC
      predGLM <- predict(mod_glm, test_hrt, type="response")
      predictionGLM <- prediction(predGLM, test_hrt$hrtDisease)
      eval <- performance(predictionGLM, "acc") 

      # identify best x (cutoff) and y (accuracy) values for  
      max <- which.max(slot(eval, "y.values")[[1]])
      acc <- slot(eval, "y.values")[[1]][max]
      cut <- slot(eval, "x.values")[[1]][max]
      print(c(Accuracy = acc, Cutoff = cut))
        # Accuracy  Cutoff.51 
        # 0.7968750 0.9325002 
      # Receiver Operating Characteristic (ROC) curve
      perfGLM <- performance(predictionGLM, "tpr", "fpr")
      
      par(pty = "s") 
      plot(perfGLM,  col="dodgerblue", print.auc = TRUE, colorize = T)
      abline(a=0, b=1)       
      
      # Area Under the Curve (AUC)
      aucGLM <- performance(predictionGLM, "auc")
      aucGLM <- unlist(slot(aucGLM, "y.values"))
      aucGLM <- round(aucGLM, 4) # rounding the digits to four decimal places
      aucGLM    # AUC 0.8388
      title("ROC Curve: GLM", adj=0)
      legend(.77, .15, aucGLM, title = "AUC glm", cex = 0.8)
                  
#----------------------------------------------------------------------------------------------------------
