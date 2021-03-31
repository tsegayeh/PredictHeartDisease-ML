
#                  DETECTING HEART DISEASE: Applying Machine Learning Algorithms   
#                           This is a Classification Challenge

#                  Dataset: "heart.dat"
#                  Data Source: UCI Machine Learning Repository, Center for Machine Learning and Intelligent Systems
#                    https://archive.ics.uci.edu/ml/datasets/Statlog+%28Heart%29

#   PART 1.0 DATA CLEANING AND PREPROCESSING
#   --------------------------------------------------------------------------------------------------
#   A.  LOADING AND CLEANING DATA
#   -------------------------------------------------------------------------------------------------- 

#       Installing packages
# install.packages("openintro")
# install.packages("mosaic")
# install.packages("polyclip")
# install.packages("tweenr")
# install.packages("vtable")  
# install.packages("corrplot")
# install.packages("ggcorrplot")
# install.packages("GGally")
  library(tweenr)
  library(mosaic)
  library(polyclip)
  library(vtable)
  library(ggplot2)
  library(scales)
  library(corrplot)
  library(ggcorrplot)
  library(GGally)
  library(caret)
  library(caTools)
  
# 1. Reading the dataset into R
     heart <- read.csv("heart.dat.csv", header = TRUE)  
                    # see Data Dictionary in Section 1.

# 2. Viewing the original dataset 
      str(heart)    # there are 270 records of 14 Variables
      vtable(heart) # see Variable Table in Section 2. Observed are:
                    # uninterpretable variable names and category names; 
                    # incorrect data types.
      hrt <- heart # putting the original dataset aside 
        
# 3. Rename the variables
     names(hrt) <- c("age", "sex", "chestpain", "restingbp", "serumchol", "fbldsugar",
              "restingecg", "maxhbeat", "excinducedangina", "oldpeak_ST", "Slope_ST", 
              "majorvessels", "thal", "AbsentPresent")
 
# 4. Rename the categories; correct the data types ('integer' to 'factor' for the 8 categorical variables)
    #a. sex
      hrt$sex <- as.factor(hrt$sex)
      levels(hrt$sex) <- c("Female", "Male")
    #b. chestpain
      hrt$chestpain <- as.factor(hrt$chestpain)
      levels(hrt$chestpain) <- c("typicalAngina", "atypicalAngina", "nonAnginal", "asymptomatic")
    #c. fbldsugar
      hrt$fbldsugar <- as.factor(hrt$fbldsugar)
      levels(hrt$fbldsugar) <- c("<=120mgPerdl", ">120mgPerdl")
    #d. restingecg
      hrt$restingecg <- as.factor(hrt$restingecg)
      levels(hrt$restingecg) <- c("normal", "st_twave", "l_v_hypertrophy")
    #e. excinducedangina
      hrt$excinducedangina <- as.factor(hrt$excinducedangina)
      levels(hrt$excinducedangina) <- c("no", "yes")
    #f. Slope_ST
      hrt$Slope_ST <- as.factor(hrt$Slope_ST)
      levels(hrt$Slope_ST) <- c("upsloping", "flat", "downsloping")
    #g. thal
      hrt$thal <- as.factor(hrt$thal)
      levels(hrt$thal) <- c("normal", "fixed", "reversable")
    #h. AbsetPresent: first, rename the variable to 'hrtDisease' and drop the old variable
      hrt$hrtDisease <- hrt$AbsentPresent
      hrt <- hrt[, -14]      
      hrt$hrtDisease <- as.factor(hrt$hrtDisease)
      
#5. Transformed dataset: see comparison of vtables of the original vs tranformed datasets  
      vtable(hrt) 

#6. double check for NAs
      sum(is.na(hrt)) # (there are no NA's)

#   B. EXPLORATORY DATA ANALYSIS
 
#1. Sample observations
      head(hrt, 3)
      str(hrt)
#2. Summary Statistics
    summary(hrt)
#3. Proportion of patients with and without Heart Disease
    tally(~hrtDisease, data=hrt, format="proportion")
#4. Visualization
    #a. Individual variables
    #   a1. Categorical variables: see the barplots in Section 3.
        par(mfrow=c(2,4))
        barplot(table(hrt$sex),
                main = "No. of Patients By Sex",col="orange")
        barplot(table(hrt$chestpain), 
                main = "No. of Patients By Chestpain Type",col="orange")
        barplot(table(hrt$fbldsugar), 
                main = "No. of Patients By Blood Sugar Level",col="orange")
        barplot(table(hrt$restingecg), 
                main = "No. of Patients By restingecg",col="orange")
        barplot(table(hrt$excinducedangina), 
                main = "No. of Patients By excinducedangina",col="orange")
        barplot(table(hrt$Slope_ST), 
                main = "No. of Patients By Slope_ST",col="orange")
        barplot(table(hrt$thal), 
                main = "No. of Patients By thal",col="orange")
        barplot(table(hrt$hrtDisease), 
            main = "No. of Patients By hrtDisease",col="orange")
 
    #   a2. Numeric variables: see the boxplots in Section 4.
        par(mfrow=c(2,3))
        boxplot(hrt$age,
                xlab = "Age",
                ylab = " ",col = "orange",
                border = "brown",
                horizontal = T,
                notch = TRUE
        )
        boxplot(hrt$restingbp,
                xlab = "Resting Blood Pressure",
                ylab = " ",col = "orange",
                border = "brown",
                horizontal = T,
                notch = TRUE
        )
        boxplot(hrt$serumchol,
                xlab = "Serum Cholestrol",
                ylab = " ",col = "orange",
                border = "brown",
                horizontal = T,
                notch = TRUE
        )
        boxplot(hrt$maxhbeat,
                xlab = "Maximum Heart Beat",
                ylab = " ",col = "orange",
                border = "brown",
                horizontal = T,
                notch = TRUE
        )
        boxplot(hrt$majorvessels,
                xlab = "Major Vessels",
                ylab = " ",col = "orange",
                border = "brown",
                horizontal = T,
                notch = TRUE
        )
        boxplot(hrt$oldpeak_ST,
                xlab = "OldPeak_STs",
                ylab = " ",col = "orange",
                border = "brown",
                horizontal = T,
                notch = TRUE
        )

    #b. INDIVIDUAL Variables vs TARGET variable  
        # b1. Categorical Variables - Heart disease by Category: see Sections 5 and 6.
        #1. sex
        p1 <- ggplot(hrt, aes(x= hrtDisease,  group=sex)) + 
          ggtitle("H Disease by Sex") +
          geom_bar(aes(y = ..prop.., fill = factor(..x..)), show.legend = FALSE, stat="count") +
          geom_text(aes( label = scales::percent(..prop..),
                         y= ..prop.. ), stat= "count", vjust =  1.7, size = 3) +
          labs(y = "Percent", fill="hrtDisease", x = "Heart Disease") +
          facet_grid(~sex) +
          scale_y_continuous(labels=percent) +
          theme(plot.title = element_text(size = 10))
        
        #2. chestpain
        p2 <- ggplot(hrt, aes(x= hrtDisease,  group=chestpain)) + 
          ggtitle("H Disease by Chestpain Type") +
          geom_bar(aes(y = ..prop.., fill = factor(..x..)), show.legend = FALSE, stat="count") +
          geom_text(aes( label = scales::percent(..prop..),
                         y= ..prop.. ), stat= "count", vjust =  1.7, size = 3) +
          labs(y = "Percent", fill="hrtDisease", x = "Heart Disease") +
          facet_grid(~chestpain) +
          scale_y_continuous(labels=percent) +
          theme(plot.title = element_text(size = 10))
        
        #3. fbldsugar
        p3 <- ggplot(hrt, aes(x= hrtDisease,  group=fbldsugar)) + 
          ggtitle("H Disease by fbldsugar Level") +
          geom_bar(aes(y = ..prop.., fill = factor(..x..)), show.legend = FALSE, stat="count") +
          geom_text(aes( label = scales::percent(..prop..),
                         y= ..prop.. ), stat= "count", vjust =  1.7, size = 3) +
          labs(y = "Percent", fill="hrtDisease", x = "Heart Disease") +
          facet_grid(~fbldsugar) +
          scale_y_continuous(labels=percent) +
          theme(plot.title = element_text(size = 10))
        
        #4. restingecg
        p4 <- ggplot(hrt, aes(x= hrtDisease,  group=restingecg)) + 
          ggtitle("H Disease by resting ecg Type") +  
          geom_bar(aes(y = ..prop.., fill = factor(..x..)), show.legend = FALSE, stat="count") +
          geom_text(aes( label = scales::percent(..prop..),
                         y= ..prop.. ), stat= "count", vjust =  1.7, size = 3) +
          labs(y = "Percent", fill="hrtDisease", x = "Heart Disease") +
          facet_grid(~restingecg) +
          scale_y_continuous(labels=percent) +
          theme(plot.title = element_text(size = 10))
        
        #5. excinducedangina
        p5 <- ggplot(hrt, aes(x= hrtDisease,  group=excinducedangina)) + 
          ggtitle("H Disease vs. Presence of Excercise Induced Angina") +  
          geom_bar(aes(y = ..prop.., fill = factor(..x..)), show.legend = FALSE, stat="count") +
          geom_text(aes( label = scales::percent(..prop..),
                         y= ..prop.. ), stat= "count", vjust =  1.7, size = 3) +
          labs(y = "Percent", fill="hrtDisease", x = "Heart Disease") +
          facet_grid(~excinducedangina) +
          scale_y_continuous(labels=percent) +
          theme(plot.title = element_text(size = 10))
        
        #6. Slope_ST
        p6 <- ggplot(hrt, aes(x= hrtDisease,  group=Slope_ST)) + 
          ggtitle("H Disease by Slope_ST Type") +  
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), show.legend = FALSE, stat="count") +
          geom_text(aes( label = scales::percent(..prop..),
                         y= ..prop.. ), stat= "count", vjust =  1.7, size = 3) +
          labs(y = "Percent", fill="hrtDisease", x = "Heart Disease") +
          facet_grid(~Slope_ST) +
          scale_y_continuous(labels=percent) +
          theme(plot.title = element_text(size = 10))
        
        #7. majorvessels
        p7 <- ggplot(hrt, aes(x= hrtDisease,  group=majorvessels)) + 
          ggtitle("H Disease by Number of Major Vessels Present") + 
          geom_bar(aes(y = ..prop.., fill = factor(..x..)), show.legend = FALSE, stat="count") +
          geom_text(aes( label = scales::percent(..prop..),
                         y= ..prop.. ), stat= "count", vjust = 1.7, size = 3) +
          labs(y = "Percent", fill="hrtDisease", x = "Heart Disease") +
          facet_grid(~majorvessels) +
          scale_y_continuous(labels=percent) +
          theme(plot.title = element_text(size = 10))
        
        #8. thal
        p8 <- ggplot(hrt, aes(x= hrtDisease,  group=thal)) + 
          ggtitle("H Disease by Type of Thalassemia") + 
          geom_bar(aes(y = ..prop.., fill = factor(..x..)), show.legend = FALSE, stat="count") +
          geom_text(aes( label = scales::percent(..prop..),
                         y= ..prop.. ), stat= "count", vjust = 1.7, size = 3) +
          labs(y = "Percent", fill="hrtDisease", x = "Heart Disease") +
          facet_grid(~thal) +
          scale_y_continuous(labels=percent) +
          theme(plot.title = element_text(size = 10))
      
      # Multiple plots - two pages, 4 plots per page
      source("http://peterhaschke.com/Code/multiplot.R")
      multiplot(p1, p2, p3, p4, cols = 2 ) 
      
      source("http://peterhaschke.com/Code/multiplot.R")
      multiplot(p5, p6, p7, p8, cols = 2 ) 

        # b2. Numeric Variables - Heart disease vs Numeric Variables: see Section 7.
 
          par(mfrow = c(3,2))
          #1.age
          histogram(~age|hrtDisease, data=hrt,
                    Main = "H Disease vs Age",
                    xlab="Age", layout=c(1,2), col="orange") 
          #2.restingbp
          histogram(~restingbp|hrtDisease, data=hrt,
                    xlab="restingbp", layout=c(1,2), col="orange") 
          #3.serumchol
          histogram(~serumchol|hrtDisease, data=hrt,
                    xlab="serumchol", layout=c(1,2), col="orange") 
          #4.maxhbeat
          histogram(~maxhbeat|hrtDisease, data=hrt,
                    xlab="maxhbeat", layout=c(1,2), col="orange") 
          #5. majorvessels
          histogram(~majorvessels|hrtDisease, data=hrt,
                    xlab="majorvessels", layout=c(1,2), col="orange") 
          #6.oldpeak_ST
          histogram(~oldpeak_ST|hrtDisease, data=hrt,
                    xlab="oldpeak_ST", layout=c(1,2), col="orange") 

      # c. Correlations, if any

        #Subset numeric variables and find cor cofficients: See the correlogram in section 8.
          corrtable <- cor(hrt[sapply(hrt, is.numeric)])
          hrtnumeric <- hrt[sapply(hrt,is.numeric)]
          corr <- round(cor(hrtnumeric), 1)
          ggcorrplot(corr,
                     hc.order = TRUE,
                     type = "lower", lab = TRUE) 

#   C. PREPROCESSING & ENCODING
          
#1. Preprocessing
      # a. Standardize numeric data - centering and scaling using the PreProcess function
          # summarize data
          summary(hrt[sapply(hrt, is.numeric)])
          # calculate the pre-process parameters from the dataset
          preproParams <- preProcess(hrt[sapply(hrt, is.numeric)], method=c("center", "scale"))
          print(preproParams)
          # transform the dataset using the parameters
          hrt6Numeric <- predict(preproParams, hrt[sapply(hrt, is.numeric)])
          # summarize the transformed dataset
          summary(hrt6Numeric)
          str(hrt6Numeric)
          
      # b. merge the 6 standardized numeric variables, the 7 categorical tables, and 
          #   the target variable (hrtDisease) before one-hot-coding
          hrtData <-  cbind(hrt6Numeric, categ7vars) 
          hrtData$hrtDisease <-hrt$hrtDisease
          str(hrtData)
          names(hrtData)
          
      # c. Encode categorical variables - Dummy variables
          library(vtreat)
          categ7vars <- hrtData[c(7:13)]
          # Create a treatment object
          treatment_hrtZ <- vtreat::designTreatmentsZ(
            dframe = categ7vars, varlist = colnames(categ7vars),
            minFraction = 0.05
          )
          str(categ7vars)
          # Treat the data
          categ_treated <- vtreat::prepare(treatment_hrtZ, categ7vars)
          str(categ_treated)
          # Identify and remove the not-so-important (_Catp) variables created 
            hrt7Categ <- categ_treated %>%
              dplyr::select(- dplyr::contains('_catP'))
            str(hrt7Categ)
          detach("package:vtreat", unload = TRUE)

      # d. Merging scaled and centered numeric data, and encoded categorical data
            dim(hrt6Numeric)  
            dim(hrt7Categ)   
          # Merge the standardized numeric vars, encoded categ. vars, and the traget var hrtDisease
          hrtnew_Z <- cbind(hrt6Numeric, hrt7Categ, hrt$hrtDisease)  
          dim(hrtnew_Z)
          str(hrtnew_Z) # note error in variable name "hrt$hrtDisease"
          # Correct the name of target variable from "hrt$hrtDisease" to "hrtDisease"
          hrtnew_Z$hrtDisease <- hrt$hrtDisease
          hrtnew_Z <- hrtnew_Z[-25] # Drop the wrongly named variable
          hrtnew <- hrtnew_Z
          str(hrtnew)         # Dataset ready FOR use
          
#     ---------------------------------------------------------------------------------------       
#     DATASET 'hrtnew' will be used to create and test the different Machine Learning Models
#     ---------------------------------------------------------------------------------------
          