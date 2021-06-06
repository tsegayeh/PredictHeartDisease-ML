## ML_models_R
#### Predicting heart disease: Comparison of six ML algorithms<br />
##### Background
This work is an extension of my student project at UCLA Data Science program in Fall 2020. The dataset [heart.dat](https://github.com/tsegayeh/ml_models_in_R/blob/main/heart.dat.csv) was obtained from the [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Statlog+%28Heart%29). I have implemented the following six algorithms using the R software to predict the presence or absence of heart disease in patients:
-- [eXtreme Gradient Boost](https://github.com/tsegayeh/ml_models_in_R/blob/main/Part_2.1_Models_XGBoost.R), [Decision Tree](https://github.com/tsegayeh/ml_models_in_R/blob/main/Part_2.2_DecisionTree.R), [Random Forest](https://github.com/tsegayeh/ml_models_in_R/blob/main/Part_2.3_RandomF.R), [GLM](https://github.com/tsegayeh/ml_models_in_R/blob/main/Part_2.4_GLM.R), [Naive Bayes](https://github.com/tsegayeh/ml_models_in_R/blob/main/Part_2.5_NaiveB.R), and 
[SVM](https://github.com/tsegayeh/ml_models_in_R/blob/main/Part_2.6_SVM.R). <br />
##### The Process
Upon cleaning and preprocessing the data, explarotary data analysis (EDA) was done. Visualizations from the EDA are presented herein, including: variable tables ([vtables](https://github.com/tsegayeh/ml_models_in_R/blob/main/Image-1.1%20Variable%20Tables%20-%20Original%20vs%20Transformed.PNG)), frequency distributions ([numeric](https://github.com/tsegayeh/ml_models_in_R/blob/main/Image-1.3%20Boxplots%2C%20Numeric%20Variables.png) and [categorical](https://github.com/tsegayeh/ml_models_in_R/blob/main/Image-1.2%20Barplots%2C%20Categorical%20Variables.png) variables), and relationships ([numeric](https://github.com/tsegayeh/ml_models_in_R/blob/main/Image-1.6%20Heart%20Disease%20by%20Variable%20-%20Numeric.png) variables, [categories-1](https://github.com/tsegayeh/ml_models_in_R/blob/main/Image-1.4%20Heart%20Disease%20by%20Category-1.png) and [2](https://github.com/tsegayeh/ml_models_in_R/blob/main/Image-1.5%20Heart%20Disease%20by%20Category-2.png), and [correlations](https://github.com/tsegayeh/ml_models_in_R/blob/main/Image-1.7%20Correlation.png). Upon creation of the models, more visualizations, namely [Decision Tree plot](https://github.com/tsegayeh/ml_models_in_R/blob/main/Image-2.5%20Decision_tree_Plot.png), [error plots](https://github.com/tsegayeh/ml_models_in_R/blob/main/Image-2.1%20e%24train_mloglos%20xgb.png), [feature importance](https://github.com/tsegayeh/ml_models_in_R/blob/main/Image-2.1.1%20Feature%20Importance%20-%20Xgb.png), and ROC curves ([individual](https://github.com/tsegayeh/ml_models_in_R/blob/main/Image-3.1_ROC-AUC-eachModel.png) as well as [overlaid](https://github.com/tsegayeh/ml_models_in_R/blob/main/Image-3.2%20ROC-AUC-all-Models.png)) were used to evaluate the performances of the models.<br />
##### Results
I considered [eXtreme Gradient Boost](https://github.com/tsegayeh/ml_models_in_R/blob/main/Part_2.1_Models_XGBoost.R) model as superior in prediction, with an accuracy of 97% and covering 91% of the Area Under the Curve (AUC), followed by the [Decision Tree](https://github.com/tsegayeh/ml_models_in_R/blob/main/Part_2.2_DecisionTree.R) model, the accuracy of which was 92% and covered 88% of the AUC. A graphical summary of comparison of the models is plotted as presented in [Part_3.0](https://github.com/tsegayeh/ml_models_in_R/blob/main/Part_3.0_ROC_AUC6.R) and [Image-3.2](https://github.com/tsegayeh/ml_models_in_R/blob/main/Image-3.2%20ROC-AUC-all-Models.png) of this report.<br />
Your review and comments will be highly appreciated.<b /><br />
Thank you.
