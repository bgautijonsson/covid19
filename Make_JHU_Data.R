library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)

url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"


out <- read_csv(url) %>% 
    pivot_longer(-c(1, 2, 3, 4), names_to = "date", values_to = "cases") %>% 
    mutate(date = mdy(date)) %>% 
    select(2, 5, 6) %>% 
    rename(country = 1) %>% 
    group_by(country, date) %>% 
    summarise(cases = max(cases)) %>% 
    ungroup %>% 
    inner_join(
        read_csv("Data/thyding.csv"),
        by = "country"
    ) %>% 
    inner_join(
        read_csv("Data/euro_pops.csv") %>% select(-X1),
        by = "country"
    ) %>% 
    select(country = land, date, cases, pop) %>% 
    mutate(rate = cases / pop) %>% 
    filter(cases > 1) %>% 
    group_by(country) %>% 
    mutate(days = as.numeric(date - min(date))) %>% 
    ungroup %>% 
    select(country, date, cum_cases = cases, pop, case_rate = rate, days)

# Vantaði 14. mars
out <- out %>% 
    bind_rows(
        read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-14-2020.csv") %>% 
            select(country = 2, cum_cases = 4) %>% 
            inner_join(
                read_csv("Data/thyding.csv"),
                by = "country"
            ) %>% 
            inner_join(
                read_csv("Data/euro_pops.csv") %>% select(-X1),
                by = "country"
            ) %>% 
            mutate(case_rate = cum_cases / pop,
                   date = ymd("2020-03-14")) %>% 
            select(-country, country = land) %>% 
            group_by(country, pop, date) %>% 
            summarise(cum_cases = max(cum_cases),
                      case_rate = max(case_rate)) %>% 
            ungroup
    ) %>% 
    group_by(country) %>% 
    mutate(days = as.numeric(date - min(date)))
    ungroup

out %>% 
    write_csv("Data/JHU_Data.csv")

out %>% 
    write_csv("COVID_Dashboard/Data/JHU_Data.csv")
rm(list = ls())
