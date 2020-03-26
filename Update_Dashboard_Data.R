TEST <- FALSE

library(tidyverse)
library(googlesheets4)
library(readxl)
library(lubridate)


source("Make_Pop_Data.R")
source("Make_Landlaeknir_Data.R")

world_pop <- make_pop_data()

url <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.csv"

d <- read_csv(url) %>% 
    select(country = "Countries and territories", 
           date = DateRep, 
           new_cases = Cases, 
           new_deaths = Deaths) %>% 
    mutate(date = dmy(date),
           country = str_replace_all(country, "_", " ")) %>% 
    arrange(country, date) %>% 
    group_by(country) %>% 
    mutate(total_cases = cumsum(new_cases), total_deaths = cumsum(new_deaths)) %>% 
    ungroup %>% 
    filter(country != "Iceland") %>% 
    bind_rows(Make_Landlaeknir_Data()) %>% 
    mutate(country = fct_recode(country,
                                "United States" = "United States of America",
                                "Tanzania" = "United Republic of Tanzania",
                                "Congo" = "Democratic Republic of the Congo") %>% 
               str_to_lower) %>% 
    inner_join(world_pop) %>% 
    mutate(country = str_to_title(country)) %>% 
    select(continent, region, country, pop, date, everything()) %>% 
    mutate(case_rate = total_cases / pop * 1000,
           death_rate = ifelse(total_cases == 0, 0, total_deaths / total_cases))


if (TEST) {
    d %>% 
        write_csv("Input/Test/ECDC_Data.csv")
} else {
    d %>% 
        write_csv("Input/Public/ECDC_Data.csv")
}
