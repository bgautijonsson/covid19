library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)

url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"


confirmed <- read_csv(url) %>% 
    pivot_longer(-c(1, 2, 3, 4), names_to = "date", values_to = "cases") %>% 
    mutate(date = mdy(date)) %>% 
    select(1, 2, 5, 6) %>% 
    rename(province = 1, country = 2) %>%
    filter(province == "Hubei") %>% 
    mutate(country = province,
           pop = 58500000) %>% 
    select(-province) %>%
    rename(cum_cases = cases) %>% 
    mutate(cases = c(cum_cases[1], diff(cum_cases)),
           case_rate = cum_cases / pop * 1000,
           continent = "Asia", region = "Eastern Asia",
           days = as.numeric(date - min(date)))

url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

recovered <- read_csv(url) %>% 
    pivot_longer(-c(1, 2, 3, 4), names_to = "date", values_to = "recovered") %>% 
    mutate(date = mdy(date)) %>% 
    select(1, 2, 5, 6) %>% 
    rename(province = 1, country = 2) %>%
    filter(province == "Hubei") %>% 
    mutate(country = province,
           pop = 58500000) %>% 
    select(-province) %>%
    rename(cum_recovered = recovered) %>% 
    mutate(recovered = c(cum_recovered[1], diff(cum_recovered)),
           continent = "Asia", region = "Eastern Asia",
           days = as.numeric(date - min(date)))

url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"

deaths <- read_csv(url) %>% 
    pivot_longer(-c(1, 2, 3, 4), names_to = "date", values_to = "deaths") %>% 
    mutate(date = mdy(date)) %>% 
    select(1, 2, 5, 6) %>% 
    rename(province = 1, country = 2) %>%
    filter(province == "Hubei") %>% 
    mutate(country = province,
           pop = 58500000) %>% 
    select(-province) %>%
    rename(cum_deaths = deaths) %>% 
    mutate(deaths = c(cum_deaths[1], diff(cum_deaths)),
           continent = "Asia", region = "Eastern Asia",
           days = as.numeric(date - min(date)))

hubei <- confirmed %>% 
    inner_join(recovered, by = c("country", "date", "pop", "continent", "region", "days")) %>% 
    inner_join(deaths, by = c("country", "date", "pop", "continent", "region", "days"))



hubei %>% 
    write_csv("Data/Hubei_JHU_Data.csv")

hubei %>% 
    write_csv("COVID_Dashboard/Data/Hubei_JHU_Data.csv")

rm(list = ls())
