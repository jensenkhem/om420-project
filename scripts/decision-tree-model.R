# Libraries
library(tidyverse)
library(ggplot2)

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




# Remove daily values that are NA since there aren't too many of them and we need values for input

view(full_covid_data_df %>% filter(is.na(DailyICU)))

# In the model, don't use ICU data? NL does not record data for this

view(full_covid_data_df)
