library(dplyr)
library(tidyverse)
library(ggplot2)
library(class)

#Read in Data
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

# Split data into training and test data
set.seed(1234)
ran <- sample(1:nrow(full_covid_data_df), 0.8 *nrow(full_covid_data_df))

train_set <- full_covid_data_df[ran,]
test_set <- full_covid_data_df[-ran,]


# Run KNN for values of k from 1 -> 96 - Just Daily Columns
train_error_rate <- c()
test_error_rate <- c()
k <- c()

for (i in seq(1,96,5)) {
  knn_model <- train(
    Abbreviation ~ DailyTotals + DailyRecovered + DailyDeaths + DailyTested + DailyActive + DailyHospitalized,
    data = train_set,
    method = "knn",
    trControl = trainControl(method = "none",classProbs = TRUE),
    tuneGrid = tibble(k = i))

  pred_train <- predict(knn_model)
  pred_test <- predict(knn_model, newdata = test_set)

  train_error_rate <- append(train_error_rate, mean(train_set$Abbreviation != pred_train))
  test_error_rate <- append(test_error_rate, mean(test_set$Abbreviation != pred_test))
  k <- append(k,i)
}
plot_data <- data.frame(train_error_rate,test_error_rate,k)

# Get the value of K that produced the lowest test error rate - k=11
# test error rate 0.5512048
plot_data[plot_data$test_error_rate==min(plot_data$test_error_rate),]

ggplot(data = plot_data, mapping = aes(x = k)) +
  geom_line(aes(y = test_error_rate), color="blue") +
  geom_point(aes(y = test_error_rate), color="blue") +
  geom_line(aes(y = train_error_rate), color = "red") +
  geom_point(aes(y = train_error_rate), color = "red") +
  xlab("K") + 
  ylab("Error Rate") +
  scale_color_discrete(labels=c('Training Data', 'Test Data'))


# Try again with all daily and total columns
# Run KNN for values of k from 1 -> 96
train_error_rate <- c()
test_error_rate <- c()
k <- c()

for (i in seq(1,96,5)) {
  knn_model <- train(
    Abbreviation ~ DailyTotals + 
      DailyTested + 
      DailyRecovered + 
      DailyActive +
      DailyDeaths + 
      DailyHospitalized +
      TotalDeaths +
      TotalCases +
      TotalRecovered +
      TotalTested +
      TotalHospitalized,
    data = train_set,
    method = "knn",
    trControl = trainControl(method = "none",classProbs = TRUE),
    tuneGrid = tibble(k = i))
  
  pred_train <- predict(knn_model)
  pred_test <- predict(knn_model, newdata = test_set)
  
  train_error_rate <- append(train_error_rate, mean(train_set$Abbreviation != pred_train))
  test_error_rate <- append(test_error_rate, mean(test_set$Abbreviation != pred_test))
  k <- append(k,i)
}
plot_data <- data.frame(train_error_rate,test_error_rate,k)

# Get the value of K that produced the lowest test error rate - k=1
# test error rate 0.246988
plot_data[plot_data$test_error_rate==min(plot_data$test_error_rate),]

ggplot(data = plot_data, mapping = aes(x = k)) +
  geom_line(aes(y = test_error_rate), color="blue") +
  geom_point(aes(y = test_error_rate), color="blue") +
  geom_line(aes(y = train_error_rate), color = "red") +
  geom_point(aes(y = train_error_rate), color = "red") +
  xlab("K") + 
  ylab("Error Rate") +
  scale_color_discrete(labels=c('Training Data', 'Test Data'))

# Try again with just Total columns
# Run KNN for values of k from 1 -> 96
train_error_rate <- c()
test_error_rate <- c()
k <- c()

for (i in seq(1,96,5)) {
  knn_model <- train(
    Abbreviation ~ TotalDeaths +
      TotalCases +
      TotalRecovered +
      TotalTested +
      TotalHospitalized,
    data = train_set,
    method = "knn",
    trControl = trainControl(method = "none",classProbs = TRUE),
    tuneGrid = tibble(k = i))
  
  pred_train <- predict(knn_model)
  pred_test <- predict(knn_model, newdata = test_set)
  
  train_error_rate <- append(train_error_rate, mean(train_set$Abbreviation != pred_train))
  test_error_rate <- append(test_error_rate, mean(test_set$Abbreviation != pred_test))
  k <- append(k,i)
}
plot_data <- data.frame(train_error_rate,test_error_rate,k)

# Get the value of K that produced the lowest test error rate - k=1
# test error rate 0.2432229
plot_data[plot_data$test_error_rate==min(plot_data$test_error_rate),]

ggplot(data = plot_data, mapping = aes(x = k)) +
  geom_line(aes(y = test_error_rate), color="blue") +
  geom_point(aes(y = test_error_rate), color="blue") +
  geom_line(aes(y = train_error_rate), color = "red") +
  geom_point(aes(y = train_error_rate), color = "red") +
  xlab("K") + 
  ylab("Error Rate") +
  scale_color_discrete(labels=c('Training Data', 'Test Data'))
