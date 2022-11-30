# Libraries
library(tidyverse)
library(ggplot2)

## Read in the data set
full_covid_data_df <- read_csv("data/Provincial_Daily_Totals.csv")

## Convert the date to R Date objects
full_covid_data_df <- mutate(full_covid_data_df, SummaryDate=as.Date(SummaryDate))

## Find out when vaccines became available in Canada for all doses in all provinces.
## From this, we can see QC got access to vaccines first - 2020-12-15 - NU and NT last - 2021-01-07
with_first_dose_covid_data_df <- full_covid_data_df %>% filter(TotalVaccinated > 0, Abbreviation != "CA")
grouped_first_dose_df <- with_first_dose_covid_data_df %>% group_by(Abbreviation) %>% arrange(.by_group = T)
first_dose_summarized <- summarise(grouped_first_dose_df, first_dose_date=SummaryDate[1])

## Figure out the first time when people started getting their second dose of the vaccine in Canada
with_second_dose_covid_data_df <- full_covid_data_df %>% filter(TotalDose2 > 0, Abbreviation != "CA")
grouped_second_dose_df <- with_second_dose_covid_data_df %>% group_by(Abbreviation) %>% arrange(.by_group = T)
second_dose_summarized <- summarise(grouped_second_dose_df, second_dose_date=SummaryDate[1])

## Calculate the first time when people started getting their booster shot of the vaccine in Canada
## As you can see by the following data frames, NL is missing. This is due to our source data-set having no data for 
## NL booster shot counts. -> This is not to say that nobody in NL got a booster shot during the pandemic. 
with_booster_covid_data_df <- full_covid_data_df %>% filter(TotalBooster > 0, Abbreviation != "CA")
grouped_booster_df <- with_booster_covid_data_df %>% group_by(Abbreviation) %>% arrange(.by_group = T)
booster_summarized <- summarise(grouped_booster_df, booster_date=SummaryDate[1])

## Plotting the date by first recorded vaccine for each dose by province
ggplot() +
  geom_point(data=first_dose_summarized, mapping=aes(x=first_dose_date, y = Abbreviation, color="Dose 1")) +
  geom_point(data=second_dose_summarized, mapping=aes(x=second_dose_date, y = Abbreviation, color="Dose 2")) +
  geom_point(data=booster_summarized, mapping=aes(x=booster_date, y = Abbreviation, color="Booster")) +
  scale_color_manual(name='Dose',
                     breaks=c('Dose 1', 'Dose 2', 'Booster'),
                     values=c('Dose 1'='blue', 'Dose 2'='red', 'Booster'='purple')) +
  labs(x = "Date", y = "Province")

# It is important to note that this is not necessarily the first day that vaccines were started to be given out,
# but rather the first day that data was recorded.


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
covid_data_with_avg <- full_covid_data_df %>% 
  left_join(province_population_df, by="Abbreviation") %>% 
  filter(Abbreviation != "RC", Abbreviation != "CA") %>% 
  group_by(Abbreviation) %>% 
  mutate(avg_daily_vaccinations=mean(DailyVaccinated, na.rm=T))

covid_data_with_avg <- covid_data_with_avg %>% filter(SummaryDate != "2021-04-24",SummaryDate !="2021-12-03", SummaryDate !="2022-02-17", SummaryDate !="2021-04-18", DailyVaccinated >= 0)

## Calculate the average daily vaccination rate for each province per-capita 
## (average proportion of population vaccinated each day)
covid_data_with_avg_summary <- covid_data_with_avg[!duplicated(covid_data_with_avg$Abbreviation),] %>% 
  summarise(avg_daily_vaccination_population_proportion=(avg_daily_vaccinations / Population))

## Bar chart plot by province for the above summary
ggplot(data = covid_data_with_avg_summary, mapping=aes(x=Abbreviation)) +
  geom_col(mapping=aes(y=avg_daily_vaccination_population_proportion, fill=Abbreviation)) +
  labs(x="Province") + 
  coord_flip()

# Boxplot for daily vaccinations
# As expected, we see that there is much more variance for some of the bigger provinces, as there is a much higher population
# and certain days saw much more vaccination than others.
ggplot(data = covid_data_with_avg, mapping=aes(x=Abbreviation, y=DailyVaccinated)) +
  geom_boxplot() +
  labs(x="Province")

# Calculate the total vaccination proportions over time for each province (at least one dose)
covid_data_total_vaccination_proportions <- full_covid_data_df %>% 
  left_join(province_population_df, by="Abbreviation") %>% 
  filter(Abbreviation != "RC", 
         Abbreviation != "CA",
         # First day where someone got vaccinated
         SummaryDate >= "2020-12-15", 
         # Remove 2021-04-23 -- No data available for any province
         SummaryDate != "2021-04-23") %>% 
  group_by(Abbreviation) %>% 
  # Modify data such that we can compare the amount of people per province who got at least 1 dose
  # even when that data is not available yet, by using the TotalVaccinated statistic until an accurate TotalDose1 
  # value is available.
  mutate(firstTotalDose1Appearance=first(TotalDose1[complete.cases(TotalDose1)])) %>% 
  # If TotalVaccinated is NA, convert to 0 or the last recorded value.
  mutate(TotalVaccinated=ifelse(is.na(TotalVaccinated), 
                                # 2021-01-07 is the first day all provinces had at least one vaccination
                                ifelse(SummaryDate <= "2021-01-07", 0, lag(TotalVaccinated)), 
                                TotalVaccinated)) %>%
  # If TotalDose1 is not available, use the TotalVaccinated instead, ONLY if it is less than the first appearing
  # TotalDose1 value in the dataset to avoid 'overcounting'
  mutate(TotalDose1=ifelse(is.na(TotalDose1), 
                           ifelse(firstTotalDose1Appearance <= TotalVaccinated, firstTotalDose1Appearance, TotalVaccinated), 
                           TotalDose1))

# Get the summary data for each province by date, per capita
covid_data_total_vaccination_proportions_summary <- covid_data_total_vaccination_proportions %>% 
  group_by(Abbreviation, SummaryDate) %>% 
  summarise(total_proportion_vaccinated=TotalDose1 / Population)

# Set the value of certain days where there was no accurate data available.
# Only occurred for the territories
summary_removed_errors <- covid_data_total_vaccination_proportions_summary %>% 
  mutate(total_proportion_vaccinated=ifelse(between(SummaryDate, as.Date("2021-03-15"), as.Date("2021-06-15")) & (Abbreviation == "NU" | Abbreviation == "YT" |Abbreviation == "NT"), NA, total_proportion_vaccinated))
summary_removed_errors <- summary_removed_errors %>% mutate(total_proportion_vaccinated=ifelse(between(SummaryDate, as.Date("2022-01-22"), as.Date("2022-01-26")) & (Abbreviation == "NL"),
                                                                                               NA, total_proportion_vaccinated))
summary_removed_errors <- summary_removed_errors %>% mutate(total_proportion_vaccinated=ifelse(between(SummaryDate, as.Date("2022-01-05"), as.Date("2022-02-18")) & (Abbreviation == "YT"),
                                                                                               NA, total_proportion_vaccinated))

  
# Provinces + Territories - full 
ggplot(data=summary_removed_errors, mapping=aes(x=SummaryDate, y=total_proportion_vaccinated, color=Abbreviation)) +
  geom_line()

# Territories - full
ggplot(data=summary_removed_errors %>% filter(Abbreviation == "NU" | Abbreviation == "YT" |Abbreviation == "NT"), mapping=aes(x=SummaryDate, y=total_proportion_vaccinated, color=Abbreviation)) +
  geom_line()

# Provinces - full
ggplot(data=summary_removed_errors %>% filter(Abbreviation != "NU" & Abbreviation != "YT" & Abbreviation != "NT"), mapping=aes(x=SummaryDate, y=total_proportion_vaccinated, color=Abbreviation)) +
  geom_line()

## Before July 2021 (Provinces + Territories) - full
# From this we can see that the territories had a much higher vaccination proportion at the start (January to Apr),
# We can also see that most provinces were vaccinating per capita at about an equal rate, until roughly July 2021,
# where noticably, provinces such as Alberta and SK's growth slowed down compared to other provinces.
ggplot(data=summary_removed_errors %>% filter(between(SummaryDate, as.Date("2020-12-15"), as.Date("2021-06-25"))), mapping=aes(x=SummaryDate, y=total_proportion_vaccinated, color=Abbreviation)) +
  geom_line()

## After July 2021 (Provinces + Territories) - full
# From this we can see that the provinces and territories became much more divided as time progressed.
ggplot(data=summary_removed_errors %>% filter(between(SummaryDate, as.Date("2021-06-25"), as.Date("2022-04-15"))), mapping=aes(x=SummaryDate, y=total_proportion_vaccinated, color=Abbreviation)) +
  geom_line()

# Summary of total percent of population vaccinated by Apr. 15 2022
summary_end_total_vaccination <- summary_removed_errors %>% summarise(end_total_vaccination_prop=max(total_proportion_vaccinated, na.rm=T))
summary_end_total_vaccination[order(-summary_end_total_vaccination$end_total_vaccination_prop),]


