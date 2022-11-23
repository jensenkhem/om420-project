# Libraries
library(tidyverse)
library(ggplot2)

# Read in the data set
full_covid_data_df <- read_csv("data/Provincial_Daily_Totals.csv")

# Prepare the data set for analysis

## Convert the date to R Date objects
full_covid_data_df <- mutate(full_covid_data_df, SummaryDate=as.Date(SummaryDate))

## Find a date range which makes sense for analysis?
## Lots of days (especially during the beginning) have little to no data available
## and will throw off any of our data analysis.
## It might make sense to remove the first few months from our analysis? -> Discuss this

## 1. Explore the relationship between the holiday season and daily covid cases (other columns too)
## 2. Compare columns between provinces
### Which provinces were more accepting to vaccines, which provinces had the most hospitalizations
###  For all these metrics we can look at the population of all the provinces to get per-capita metrics
### group by province and date, since province data is only comparable on the same day


## Data-set without country wide metrics
no_ca_covid_data_df <- full_covid_data_df %>% filter(Abbreviation != "CA")

no_ca_covid_data_df[order(-no_ca_covid_data_df$DailyTotals),]

view(no_ca_covid_data_df[order(-no_ca_covid_data_df$DailyTotals),])

## Data-set without first few months?
test_covid_data_df <- no_ca_covid_data_df %>% filter(SummaryDate >= "2020/03/01")
mean(test_covid_data_df$DailyTotals)



test_covid_data_df %>% ggplot(mapping=aes(x=DailyTotals, y=Abbreviation)) + 
  geom_boxplot()