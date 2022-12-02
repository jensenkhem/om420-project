# Libraries
library(tidyverse)
library(ggplot2)
library(ISLR)
library(tree)

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

# Create the classification tree with:
## Response - Abbreviation
## Input - DailyTotals, 
##         DailyRecovered, 
##         DailyDeaths, 
##         DailyTested, 
##         DailyHospitalized
##         TotalDeaths
##         TotalCases
##         TotalRecovered
##         TotalTested
##         TotalHospitalized

# Set the seed
set.seed(2)

# Split into 80% training 20% test data
train <- sample(1:nrow(full_covid_data_df), floor(0.8 * nrow(full_covid_data_df)))
test <- full_covid_data_df[-train,]

# Create and plot the tree using the training data
classification_tree <- tree(Abbreviation ~ DailyTotals + 
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
                            data=full_covid_data_df,
                            subset=train)

plot(classification_tree)
text(classification_tree, pretty=0, cex=0.6)


# Make predictions about the test data using the tree created above
classification_tree_prediction <- predict(classification_tree,
                                          newdata = test,
                                          type="class")

non_pruned_pred_table <- table(classification_tree_prediction, test$Abbreviation)
1 -(non_pruned_pred_table[1,1] + 
    non_pruned_pred_table[2,2] + 
    non_pruned_pred_table[3,3] + 
    non_pruned_pred_table[4,4] +
    non_pruned_pred_table[5,5] + 
    non_pruned_pred_table[6,6] + 
    non_pruned_pred_table[7,7] + 
    non_pruned_pred_table[8,8] + 
    non_pruned_pred_table[9,9] + 
    non_pruned_pred_table[10,10]
) / sum(non_pruned_pred_table)

## The above tree results in an error rate of 0.2597892 when applied to the test data set

# Can we do better?

# Apply Cross Validation to the tree we created - prune misclassified
cv_classification_tree <- cv.tree(classification_tree, 
                       FUN = prune.misclass,
                       K=10)

# Create a data frame of all the cross validation parameters and results
cv_df <- data.frame(size = cv_classification_tree$size,
                 dev = cv_classification_tree$dev,
                 k = cv_classification_tree$k)

# Plot the size vs CV error rate (dev)
cv_df %>% 
  ggplot(aes(size, dev)) + 
  geom_line() + 
  geom_point()

# From the above plot we can see that 19 is the size of tree that gives us the lowest error rate,
# So lets prune to the best tree size.
pruned_classification_tree <- prune.misclass(classification_tree, best = 19)
plot(pruned_classification_tree)
text(pruned_classification_tree, pretty=0, cex = 0.6)

# Re-run the predictions using this new 'best' tree on the test data
pruned_classification_tree_pred <- predict(pruned_classification_tree, 
                     test, 
                     type = "class")

# Calculate the new table and error rate for this 'best' tree.
pruned_pred_table <- table(pruned_classification_tree_pred, test$Abbreviation)
1 - (pruned_pred_table[1,1] + 
    pruned_pred_table[2,2] + 
    pruned_pred_table[3,3] + 
    pruned_pred_table[4,4] +
    pruned_pred_table[5,5] + 
    pruned_pred_table[6,6] + 
    pruned_pred_table[7,7] + 
    pruned_pred_table[8,8] + 
    pruned_pred_table[9,9] + 
    pruned_pred_table[10,10]
  ) / sum(pruned_pred_table)

# Changing K -> 5, 10, 20, 100 made no difference
# Experiment changing k
k_values <- c()
size_values <- c() 
for (i in 5:100) {
  cv_classification_tree <- cv.tree(classification_tree, 
                                    FUN = prune.misclass,
                                    K=5)
  
  # Create a data frame of all the cross validation parameters and results
  cv_df <- data.frame(size = cv_classification_tree$size,
                      dev = cv_classification_tree$dev,
                      k = cv_classification_tree$k)
  
  k_values <- append(k_values, i)
  size_values <- append(size_values, cv_df$size[1])
  
}

# Shows that for each size, the best k value was always 19
k_size_df <- data.frame(k=k_values, size=size_values)
ggplot(data=k_size_df) +
  geom_point(mapping=aes(x=k, y=size))

# Run the same experiments with a smaller set of input variables
# Create and plot the tree using the training data
classification_tree <- tree(Abbreviation ~ TotalDeaths +
                              TotalCases +
                              TotalRecovered +
                              TotalTested +
                              TotalHospitalized,
                            data=full_covid_data_df,
                            subset=train)

plot(classification_tree)
text(classification_tree, pretty=0, cex=0.6)


# Make predictions about the test data using the tree created above
classification_tree_prediction <- predict(classification_tree,
                                          newdata = test,
                                          type="class")

non_pruned_pred_table <- table(classification_tree_prediction, test$Abbreviation)
1 -(non_pruned_pred_table[1,1] + 
      non_pruned_pred_table[2,2] + 
      non_pruned_pred_table[3,3] + 
      non_pruned_pred_table[4,4] +
      non_pruned_pred_table[5,5] + 
      non_pruned_pred_table[6,6] + 
      non_pruned_pred_table[7,7] + 
      non_pruned_pred_table[8,8] + 
      non_pruned_pred_table[9,9] + 
      non_pruned_pred_table[10,10]
) / sum(non_pruned_pred_table)

## The above tree results in an error rate of 0.253012 when applied to the test data set

# Can we do better?

# Apply Cross Validation to the tree we created - prune misclassified
cv_classification_tree <- cv.tree(classification_tree, 
                                  FUN = prune.misclass,
                                  K=10)

# Create a data frame of all the cross validation parameters and results
cv_df <- data.frame(size = cv_classification_tree$size,
                    dev = cv_classification_tree$dev,
                    k = cv_classification_tree$k)

# Plot the size vs CV error rate (dev)
cv_df %>% 
  ggplot(aes(size, dev)) + 
  geom_line() + 
  geom_point()

# From the above plot we can see that 19 is the size of tree that gives us the lowest error rate,
# So lets prune to the best tree size.
pruned_classification_tree <- prune.misclass(classification_tree, best = 19)
plot(pruned_classification_tree)
text(pruned_classification_tree, pretty=0, cex = 0.6)

# Re-run the predictions using this new 'best' tree on the test data
pruned_classification_tree_pred <- predict(pruned_classification_tree, 
                                           test, 
                                           type = "class")

# Calculate the new table and error rate for this 'best' tree.
pruned_pred_table <- table(pruned_classification_tree_pred, test$Abbreviation)
1 - (pruned_pred_table[1,1] + 
    pruned_pred_table[2,2] + 
    pruned_pred_table[3,3] + 
    pruned_pred_table[4,4] +
    pruned_pred_table[5,5] + 
    pruned_pred_table[6,6] + 
    pruned_pred_table[7,7] + 
    pruned_pred_table[8,8] + 
    pruned_pred_table[9,9] + 
    pruned_pred_table[10,10]
) / sum(pruned_pred_table)

# Changing K -> 5, 10, 20, 100 made no difference
# Experiment changing k
k_values <- c()
size_values <- c() 
for (i in 5:100) {
  cv_classification_tree <- cv.tree(classification_tree, 
                                    FUN = prune.misclass,
                                    K=5)
  
  # Create a data frame of all the cross validation parameters and results
  cv_df <- data.frame(size = cv_classification_tree$size,
                      dev = cv_classification_tree$dev,
                      k = cv_classification_tree$k)
  
  k_values <- append(k_values, i)
  size_values <- append(size_values, cv_df$size[1])
  
}

# Shows that for each size, the best k value was always 20
k_size_df <- data.frame(k=k_values, size=size_values)
ggplot(data=k_size_df) +
  geom_point(mapping=aes(x=k, y=size))

## Tree of size 19 was always chosen for the set of input variables
### DailyTotals + 
### DailyTested + 
### DailyRecovered + 
### DailyActive +
### DailyDeaths + 
### DailyHospitalized +
### TotalDeaths +
### TotalCases +
### TotalRecovered +
### TotalTested +
### TotalHospitalized,

## Tree of size 20 was always chosen for the set of input variables
### TotalDeaths +
### TotalCases +
### TotalRecovered +
### TotalTested +
### TotalHospitalized,

# TotalDeaths seemed to be the most important predictor -
## If we remove this predictor, the classification error rate nearly doubles
## all of the variables with prefix 'Total' were much better predictors than the 'Daily' variables
## Removing the 'Daily' variables from the list of input variables actually slightly
## increases the accuracy of the model!
## Error rate of 0.253012 for the model without 'Daily' variables, 0.2597892 with all the original predictors

