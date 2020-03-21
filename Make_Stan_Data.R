Make_Stan_Data <- function(min_case_rate = 0.04, min_days = 7) {
    library(tidyverse)
    library(googlesheets4)
    
    source("Make_Pop_Data.R")
    source("Make_Landlaeknir_Data.R")
    
    world_pop <- make_pop_data()
    stan_data <- read_csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv") %>% 
        rename(country = location) %>% 
        filter(country != "Iceland") %>% 
        bind_rows(Make_Landlaeknir_Data()) %>% 
        inner_join(world_pop) %>% 
        select(continent, region, country, pop, date, everything()) %>% 
        mutate(case_rate = total_cases / pop * 1000,
               death_rate = ifelse(total_cases == 0, 0, total_deaths / total_cases))
    
    
    stan_data %>% 
        filter(country != c("China")) %>% 
        filter(case_rate >= min_case_rate) %>% 
        group_by(country) %>% 
        mutate(days = row_number() - 1) %>% 
        filter(any(days >= min_days)) %>% 
        ungroup %>% 
        mutate(country_id = as.numeric(as.factor(country)))
}
