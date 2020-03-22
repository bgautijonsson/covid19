library(tidyverse)
library(googlesheets4)

source("Make_Pop_Data.R")
source("Make_Landlaeknir_Data.R")

world_pop <- make_pop_data()
d <- read_csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv") %>% 
    rename(country = location) %>% 
    filter(country != "Iceland") %>% 
    bind_rows(Make_Landlaeknir_Data()) %>% 
    inner_join(world_pop) %>% 
    select(continent, region, country, pop, date, everything()) %>% 
    mutate(case_rate = total_cases / pop * 1000,
           death_rate = ifelse(total_cases == 0, 0, total_deaths / total_cases)) 
d %>% 
    write_csv("dashboard/Input/ECDC_Data.csv")

d %>% 
    write_csv("Input/ECDC_Data.csv")
