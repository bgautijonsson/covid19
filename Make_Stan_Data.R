Make_Stan_Data <- function(min_case_rate = 0.02, min_days = 6, upper_mult = 3) {
    library(tidyverse)
    library(googlesheets4)
    library(readxl)
    library(lubridate)
    
    source("Make_Pop_Data.R")
    source("Make_Landlaeknir_Data.R")
    world_pop <- make_pop_data() %>% 
        mutate(country = str_to_lower(country))
    
    
    url <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.csv"
    
    stan_data <- read_csv(url) %>% 
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
    
    
    stan_data %>% 
        filter(country != c("China")) %>% 
        filter(case_rate >= min_case_rate) %>% 
        group_by(country) %>% 
        mutate(days = row_number() - 1) %>% 
        filter(n() >= min_days, any(case_rate < upper_mult * min_case_rate)) %>% 
        ungroup %>% 
        mutate(country_id = as.numeric(as.factor(country)))
}
