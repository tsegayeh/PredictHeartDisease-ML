
#   --------------------------------------------------------------------------------------------------
#   PART 3. SUMMARY OF ROC/AUC VALUES AND ACCURACY
#   --------------------------------------------------------------------------------------------------
#   Models: xgboost, GLM, Random Forest, Naive Bayes, Decision Tree, SVM

#          AUC Values
aucXgb   # [1] 0.9681
aucDtree # [1] 0.9236
aucRf    # [1] 0.8623
aucGLM   # [1] 0.8588
aucNB    # [1] 0.8139
aucSVM   # [1] 0.7877

# Accuracy (training set)
accuracy_xgbtrain   # [1] 0.9249
accuracy_DtreeTrain # [1] 0.8667
accuracy_RfTrain    # [1] 0.9069
accuracy_glmtrain   # [1] 0.8883
accuracy_NBtrain    # [1] 0.8627
accuracy_svmTrain   # [1] 0.8873

# Accuracy (test set)
accuracy_xgbtest    # [1] 0.9123
accuracy_DtreeTest  # [1] 0.8833
accuracy_RfTest     # [1] 0.7879
accuracy_glmtest    # [1] 0.7656
accuracy_NBtest     # [1] 0.7576
accuracy_svmTest    # [1] 0.803

par(mfrow=c(1,2))
par(pty = "s") 
plot(perfXgbz,   colorize=T)
abline(a=0, b=1)
legend(.42, .3, aucXgb, title = "Xgboost: AUC", cex = 0.8)
legend(.7, .3, accuracy_xgbtest, title = "Accuracy Test", cex = 0.8)
legend(.7, .15, accuracy_xgbtrain, title = "AccuracyTrain", cex = 0.8)

par(pty = "s") 
plot(pref_Dtree,   colorize=T)
abline(a=0, b=1)
legend(.32, .3, aucDtree, title = "Decision Tree: AUC", cex = 0.8)
legend(.7, .3, accuracy_DtreeTest, title = "Accuracy Test", cex = 0.8)
legend(.7, .15, accuracy_DtreeTrain, title = "AccuracyTrain", cex = 0.8)

par(mfrow=c(1,2))
par(pty = "s") 
plot(perf_Rf,   colorize=T)
abline(a=0, b=1)
legend(.31, .3, aucRf, title = "Random Forest: AUC", cex = 0.8)
legend(.71, .3, accuracy_RfTest, title = "Accuracy Test", cex = 0.8)
legend(.71, .15, accuracy_RfTrain, title = "AccuracyTrain", cex = 0.8)

par(pty = "s") 
plot(perfGLM,   colorize=T)
abline(a=0, b=1)
legend(.46, .3, aucGLM, title = "GLM: AUC", cex = 0.8)
legend(.7, .3, accuracy_glmtest, title = "Accuracy Test", cex = 0.8)
legend(.7, .15, accuracy_glmtrain, title = "AccuracyTrain", cex = 0.8)

par(mfrow=c(1,2))
par(pty = "s") 
plot(NBperf,   colorize=T)
abline(a=0, b=1)
legend(.33, .3, aucNB, title = "Naive Bayes: AUC", cex = 0.8)
legend(.7, .3, accuracy_NBtest, title = "Accuracy Test", cex = 0.8)
legend(.7, .15, accuracy_NBtrain, title = "AccuracyTrain", cex = 0.8)

par(pty = "s") 
plot(pref_SvmL,   colorize=T)
abline(a=0, b=1)
legend(.47, .3, aucSVM, title = "SVM: AUC", cex = 0.8)
legend(.7, .3, accuracy_svmTest, title = "Accuracy Test", cex = 0.8)
legend(.7, .15, accuracy_svmTrain, title = "AccuracyTrain", cex = 0.8)

# Overlaid ROC Curves and AUC values of the six models
par(pty = "s") 
plot(perfXgbz, lty=1, lwd=2, col="red",main="ROC & AUC: Comparison of Six Models for
Prediction of Heart Disease", print.auc = TRUE, print.auc.x = 0.69, print.auc.y = 0.69)
plot(pref_Dtree, lty=1, lwd=2, col="purple",add=TRUE)
plot(perf_Rf, lty=1, lwd=2, col="gray",add=TRUE)
plot(perfGLM, lty=1, lwd=2, col="dodgerblue",add=TRUE)
plot(NBperf, lty=1, lwd=2, col="blue",add=TRUE)
plot(pref_SvmL, lty=1,lwd=2, col="orange",add=TRUE)

legend(0.2, 0.5, c('..Model..   ..AUC.. ..ACC test..',  'Xgboost:     .9681    .9123', 'DTree:        .9236    .8833', 'RandomF:  .8623    .7879',  
                       'GLM:         .8588    .7656', 'NaiveB:      .8139    .7576','SVM:         .7877    .803'),
       col=c('white', 'red','purple','dodgerblue', 'blue','gray','orange'),lwd=2, cex = 0.8)
 
