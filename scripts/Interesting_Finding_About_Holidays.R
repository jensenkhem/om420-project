library(dplyr)
library(ggplot2)
library(lubridate)

covid_df <- read.csv("data/Provincial_Daily_Totals.csv")

# Change dates to R type dates
covid_df <- covid_df %>% mutate(SummaryDate = as.Date(SummaryDate)) %>% 
  mutate("Province" = ifelse(Province == "BC", "BRISTISH COLUMBIA", Province))

# Interesting finding out if there is a connection between holidays and number of cases of COVID
# COVID cases seemed to be higher during the winter holiday months from a quick sort and glance
# Going to have to group by provinces and dates by a certain range
# 1. Select relevant data
holidays_df <- covid_df %>% 
  select(Province,DailyTotals,SummaryDate,Abbreviation) %>% 
  filter(!(Abbreviation %in% c("CA","RC")))

# 2. Grouped the dates by week then summarized by total cases
## Used lead cause I assumed COVID symptoms take about a week to appear
## So then actual cases of COVID would be shifted by a week
holidays_df_summary <- holidays_df %>% 
  mutate("Weekly" = round_date(SummaryDate, "week")) %>% 
  group_by(Weekly) %>% 
  summarise("WeeklyTotals" = sum(DailyTotals)) 


# 3. Plot of Weekly cases in Canada
## In line with the 5 waves of COVID in Canada
## COVID makes a spike during the winter holiday months(Jan) - After Christmas since everyone
## went and saw each other.


holidays_df_summary_filtered <- holidays_df_summary %>% filter(Weekly >= "2021-11-22", Weekly <= "2022-02-22")

# Holidays 2021 total
ggplot(data = holidays_df_summary_filtered , aes(x = Weekly,
                               y = WeeklyTotals)) +
  geom_point() +
  labs(x="Week",y="Cases")

# Average daily totals by province in general by month
monthly_df <- holidays_df %>% 
  mutate("Monthly" = round_date(SummaryDate, "month")) %>% 
  filter(Monthly >= "2021-01-30", Monthly <= "2022-01-30") %>% 
  group_by(Abbreviation, Monthly) %>% 
  summarize(MonthlyTotals=sum(DailyTotals, na.rm=T))

# Summarize with the average for each province
monthly_summary <- monthly_df %>% summarise(AvgMonthlyTotals=mean(MonthlyTotals, na.rm=T))

# Get the difference between the average and january values.
january_difference <- monthly_df %>% 
  filter(Monthly=="2022-01-01", Abbreviation != "RC") %>% 
  left_join(monthly_summary, on="Abbreviation") %>% 
  summarise(proportion_increase=(MonthlyTotals / AvgMonthlyTotals))

# Average monthly covid cases in 2021 for each province
ggplot(data = monthly_summary %>% filter(Abbreviation != "RC"), mapping=aes(x=Abbreviation)) +
  geom_bar(mapping=aes(y=AvgMonthlyTotals, fill=Abbreviation), stat = "identity") +
  labs(x="Province")

# January 2022 covid case for each province
ggplot(data = monthly_df %>% filter(Monthly=="2022-01-01"), mapping=aes(x=Abbreviation)) +
  geom_bar(mapping=aes(y=MonthlyTotals, fill=Abbreviation), stat = "identity") +
  labs(x="Province")

# Increase from average during Jan 2022 for each province
# As you can see the difference is massive!
# Every province had a much higher monthly case count than the average during the holidays
ggplot(data = january_difference, mapping=aes(x=Abbreviation)) +
  geom_bar(mapping=aes(y=proportion_increase, fill=Abbreviation), stat = "identity") +
  labs(x="Province")






