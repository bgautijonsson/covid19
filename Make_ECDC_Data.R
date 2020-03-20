library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(RCurl)

base_url <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-"

today <- Sys.Date() 
url <- str_c(base_url, today, ".xlsx")
temp <- tempfile()
if (url.exists(url)) {
    download.file(url, temp)
} else {
    base_url <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-"
    today <- Sys.Date()
    url <- str_c(base_url, today - 1, ".xlsx")
    download.file(url, temp)
}


out <- read_xlsx(temp) %>% 
    select(date = DateRep, country = "Countries and territories", cases = Cases, deaths = Deaths) %>%
    mutate(country = str_replace(country, "_", " ")) %>% 
    inner_join(
        read_csv("Data/world_pops.csv"),
        by = "country"
    ) %>% 
    mutate(date = as.Date(date)) %>% 
    arrange(country, date) %>% 
    group_by(country) %>% 
    mutate(cum_cases = cumsum(cases),
           cum_deaths = cumsum(deaths),
           case_rate = cum_cases / pop * 1000,
           death_rate = cum_deaths / pop * 1000) %>% 
    filter(cum_cases > 1) %>% 
    mutate(days = as.numeric(date - min(date))) %>% 
    ungroup %>% 
    select(country, continent, region, pop, date, days, contains("case"), contains("death"))

hubei <- read_csv("Data/Hubei_JHU_Data.csv") %>% 
    select(-contains("death"), -contains("recovered"))

out <- out %>% 
    bind_rows(hubei)

out %>% 
    write_csv("Data/ECDC_Data.csv")

out %>% 
    write_csv("dashboard/Data/ECDC_Data.csv")
rm(list = ls())

