# Libraries
library(tidyverse)
library(ggplot2)

# Read in the data set
full_covid_data_df <- read_csv("data/Provincial_Daily_Totals.csv")

# Prepare the data set for analysis

## Convert the date to R Date objects
full_covid_data_df <- mutate(full_covid_data_df, SummaryDate=as.Date(SummaryDate))

view(full_covid_data_df %>% filter(Abbreviation == "AB", TotalVaccinated > 0))

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
with_first_dose_covid_data_df <- full_covid_data_df %>% filter(TotalVaccinated > 0, Abbreviation != "CA")
with_first_dose_covid_data_df[order(with_first_dose_covid_data_df$SummaryDate),]
grouped_first_dose_df <- with_first_dose_covid_data_df %>% group_by(Abbreviation) %>% arrange(.by_group = T)
first_dose_summarized <- summarise(grouped_first_dose_df, first_dose_date=SummaryDate[1])

## Figure out the first time when people started getting their second dose of the vaccine in Canada
with_second_dose_covid_data_df <- full_covid_data_df %>% filter(TotalDose2 > 0, Abbreviation != "CA")
with_second_dose_covid_data_df[order(with_second_dose_covid_data_df$SummaryDate),]
grouped_second_dose_df <- with_second_dose_covid_data_df %>% group_by(Abbreviation) %>% arrange(.by_group = T)
second_dose_summarized <- summarise(grouped_second_dose_df, second_dose_date=SummaryDate[1])

## Figure out the first time when people started getting their booster shot of the vaccine in Canada
## As you can see by the following data frames, NL is missing. This is due to our source dataset having no data for 
## NL booster shot counts. -> This is not to say that nobody in NL got a booster shot during the pandemic. 
with_booster_covid_data_df <- full_covid_data_df %>% filter(TotalBooster > 0, Abbreviation != "CA")
with_booster_covid_data_df[order(with_booster_covid_data_df$SummaryDate),]
grouped_booster_df <- with_booster_covid_data_df %>% group_by(Abbreviation) %>% arrange(.by_group = T)
booster_summarized <- summarise(grouped_booster_df, booster_date=SummaryDate[1])

## Create df containing all first dates for all doses by province
all_dose_summarized <- first_dose_summarized %>% inner_join(second_dose_summarized) %>% inner_join(booster_summarized)

## Plotting the date by first administered vaccine for each dose by province
ggplot() +
  geom_point(data=first_dose_summarized, mapping=aes(x=first_dose_date, y = Abbreviation, color="Dose 1")) +
  geom_point(data=second_dose_summarized, mapping=aes(x=second_dose_date, y = Abbreviation, color="Dose 2")) +
  geom_point(data=booster_summarized, mapping=aes(x=booster_date, y = Abbreviation, color="Booster")) +
  scale_color_manual(name='Dose',
                     breaks=c('Dose 1', 'Dose 2', 'Booster'),
                     values=c('Dose 1'='blue', 'Dose 2'='red', 'Booster'='purple')) +
  labs(x = "Date", y = "Province")


## Population of all Provinces - For use in 'per-capita' data comparisons
### This data was retrieved from: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000901&cubeTimeFrame.startMonth=07&cubeTimeFrame.startYear=2022&cubeTimeFrame.endMonth=07&cubeTimeFrame.endYear=2022&referencePeriods=20220701%2C20220701
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

province_population_df <- data.frame (
  Abbreviation = c("NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YT", "NT", "NU"),
  Population = c(525972, 170688, 1019725, 812061, 8695659, 15109416, 1409223, 1194803, 4543111, 5319324, 43789, 45605, 40526)
) 

## Get the average vaccinations per day for each province
covid_data_with_avg <- full_covid_data_df %>% left_join(province_population_df, by="Abbreviation") %>%  filter(Abbreviation != "RC", Abbreviation != "CA") %>% group_by(Abbreviation) %>% mutate(avg_daily_vaccinations=mean(DailyVaccinated, na.rm=T)) %>% select(avg_daily_vaccinations, everything())

## Calculate the average daily vaccination rate for each province per capita (average proportion of population vaccinated each day)
covid_data_with_avg[!duplicated(covid_data_with_avg$Abbreviation),] %>% summarise(avg_daily_vaccination_popultion_proportion=(avg_daily_vaccinations / Population))

## Data-set without country wide metrics
no_ca_covid_data_df <- full_covid_data_df %>% filter(Abbreviation != "CA")

## Data-set without first few months?
no_ca_covid_data_df <- no_ca_covid_data_df %>% filter(SummaryDate >= "2020/03/01")

view(no_ca_covid_data_df)
