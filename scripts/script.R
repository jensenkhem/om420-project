# Libraries
library(tidyverse)

# Read in the data set
full_covid_data_df <- read_csv("data/Provincial_Daily_Totals.csv")

# Prepare the data set for analysis
## Convert the date to R Date objects
full_covid_data_df <- mutate(full_covid_data_df, SummaryDate=as.Date(SummaryDate))

## I don't think that we need to do anything with the data set as a whole,
## but when we are trying to create models using a specific column,
## we can make a choice on a case-by-case basis

                   