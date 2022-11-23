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
###   For all these metrics we can look at the population of all the provinces to get per-capita metrics
### group by province and date, since province data is only comparable on the same day

## For vaccines -- remove all rows where vaccines did not exist
## Before a certain point, there was only one vaccine, so that explains cases where vaccine numbers > 0,
## but there is still NA values in the dosex columns.

## Start taking observations at 2020-03-11 --> Before this time, covid was too new, data was unavailable, very low case counts

## Find out when vaccines became available in Canada.
## From this, we can see QC got access to vaccines first - 2020-12-15 - NU and NT last - 2021-01-07
with_vaccines_covid_data_df <- full_covid_data_df %>% filter(TotalVaccinated > 0)
with_vaccines_covid_data_df[order(with_vaccines_covid_data_df$SummaryDate),]
grouped_vaccine_df <- with_vaccines_covid_data_df %>% group_by(Abbreviation) %>% arrange(.by_group = T)
summarized <- summarise(grouped_vaccine_df, first_vaccines_date=SummaryDate[1])

## Plotting the date which the provinces all administered their first vaccine
ggplot(summarized, mapping=aes(x=first_vaccines_date, y = Abbreviation, color=Abbreviation)) +
  geom_point() +
  theme(axis.title.y=element_blank())

## Population of all Provinces - For use in 'per-capita' data comparisons
### This data was retreived from: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000901&cubeTimeFrame.startMonth=07&cubeTimeFrame.startYear=2022&cubeTimeFrame.endMonth=07&cubeTimeFrame.endYear=2022&referencePeriods=20220701%2C20220701
### and for simplicity, uses the current population of the provinces (as of Q3 2022).
### NL - 525,972
### PE - 170,688
### NS - 1,019,725
### NB - 812,061
### QC - 8,695,659
### ON - 15,109,416
### MB - 1,409,223
### SK - 1,194,803
### AB - 4,543,111
### BC - 5,319,324
### YT - 43,789
### NT - 45,605
### NU - 40,526

## Data-set without country wide metrics
no_ca_covid_data_df <- full_covid_data_df %>% filter(Abbreviation != "CA")

## Data-set without first few months?
no_ca_covid_data_df <- no_ca_covid_data_df %>% filter(SummaryDate >= "2020/03/01")

view(no_ca_covid_data_df)
