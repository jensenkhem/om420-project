# Libraries
library(tidyverse)
library(ggplot2)
library(ISLR)
library(nnet)
library(caret) 
library(pROC)

## Read in the data set
full_covid_data_df <- read_csv("data/Provincial_Daily_Totals.csv")

## Convert the date to R Date objects
full_covid_data_df <- mutate(full_covid_data_df, SummaryDate=as.Date(SummaryDate))

## Remove the first couple months where covid was new and data was rough
full_covid_data_df <- full_covid_data_df %>% filter(SummaryDate >= "2020-04-09", SummaryDate <= "2022-02-01")

## Remove CA and RC - Remove Territories
full_covid_data_df <- full_covid_data_df %>% filter(Abbreviation != "RC", 
                                                    Abbreviation != "CA", 
                                                    Abbreviation != "YT",
                                                    Abbreviation != "NU",
                                                    Abbreviation != "NT")

full_covid_data_df <- full_covid_data_df %>% mutate(Abbreviation=as.factor(Abbreviation))


# Get correlations between Abbreviation and other variables
full_covid_data_df %>% 
  select(DailyTotals,
         DailyTested, 
         DailyRecovered, 
         DailyActive,
         DailyDeaths, 
         DailyHospitalized,
         TotalDeaths,
         TotalCases,
         TotalRecovered,
         TotalTested,
         TotalHospitalized
  ) %>% 
  cor()

## From the above table, we can see that there many of the input predictors are influencing eachother - multicolinearity
## In particular, it seems that all the predictors with prefix 'Total' TotalCases, TotalRecovered, TotalTested, and TotalHospitalized and TotalDeaths
## are influencing eachother. (Some with almost perfect correlation ~1) 
## On top of that, DailyRecovered and DailyTotals have very high correlation as well. 0.807
## After removing DailyRecovered, TotalCases, TotalRecovered, TotalTested, and TotalHospitalized from the predictors
## we can see that we can eliminate these problems, and have relatively low correlations between the predictors.

full_covid_data_df %>% 
  select(DailyTotals,
           DailyTested, 
           DailyActive,
           DailyDeaths, 
           DailyHospitalized,
           TotalDeaths,
         ) %>% 
  cor()

## Therefore, we will go ahead and carry out a multinomial logistic regression using the following set of variables
### DailyTested
### DailyActive
### DailyDeaths
### DailyHospitalized
### TotalDeaths

# Set the seed
set.seed(2)

# Split into 80% training 20% test data
sample <- sample(1:nrow(full_covid_data_df), floor(0.8 * nrow(full_covid_data_df)))
train <- full_covid_data_df[sample,]
test <- full_covid_data_df[-sample,]

# Create the model
model <- multinom(Abbreviation ~ DailyTested + 
                    DailyActive +
                    DailyDeaths +
                    DailyHospitalized +
                    TotalDeaths,
                  data=train)

# Get the predicted class for the training set rows
pred_train_class <- predict(model, type = "class")

# Calculate the error for the training data
train_error <- mean(pred_train_class != train$Abbreviation)

# Get the predicted class for the test set rows
pred_test_class <- predict(model, type = "class", newdata = test)

# Calculate the error for the test data
test_error <- mean(pred_test_class != test$Abbreviation)

cat("training_error:", train_error, "\ntest_error:", test_error)

# Calculate the test accuracy (correct classification)
test_accuracy <- 1-test_error

# Confusion matrix
test_confusion_matrix <- confusionMatrix(table(pred_test_class, test$Abbreviation))

# Precision for each class
precision_by_class <- test_confusion_matrix$byClass[,'Pos Pred Value']

# Recall for each class
recall_by_class <- test_confusion_matrix$byClass[,'Sensitivity']

# F1 score for each class
f1_by_class <- test_confusion_matrix$byClass[,'F1']

# Create data frames for plotting precision and recall by class
summary <- full_covid_data_df %>% group_by(Abbreviation) %>% summarize(precision=precision_by_class[Abbreviation],
                                                            recall=recall_by_class[Abbreviation],
                                                            F1=f1_by_class[Abbreviation]) %>% 
  distinct()

# Precision plot
ggplot() +
  geom_col(data=summary, mapping=aes(x=Abbreviation, y=precision, fill=Abbreviation))

# Recall plot
ggplot() +
  geom_col(data=summary, mapping=aes(x=Abbreviation, y=recall, fill=Abbreviation))

# F1 plot
ggplot() +
  geom_col(data=summary, mapping=aes(x=Abbreviation, y=F1, fill=Abbreviation))

# The above plots tell use that our model was best able to predict NS, PE, and QC,
# but was notably inaccurate at predicting BC, NB, NL, and SK

# Get the multiclass ROC
# Multi-class ROC area under the curve: 0.8855
# This shows us that while our model is not the best classifier, it is still performing better than chance, since the AUC is 0.8855 > 0.5
ROC <- multiclass.roc(test$Abbreviation, predict(model, newdata = test, type ="prob"))
ROC$auc



