library(readr)
library(dplyr)
library(rstan)
library(magrittr)

options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
source("Make_Stan_Data.R")

d <- Make_Stan_Data() %>% 
    mutate(death_rate = total_deaths / pop * 1000) %>% 
    filter(death_rate >= 0.01) %>% 
    group_by(country) %>% 
    filter(n() >= 7) %>% 
    mutate(days = row_number()) %>% 
    ungroup %>% 
    filter(new_deaths >= 0) %>% 
    mutate(country_id = as.numeric(as.factor(country)))


N_obs <- nrow(d)
N_countries <- max(d$country_id)


days <- d$days
new_deaths <- d$new_deaths

country <- d$country_id %>% as.integer

pop <- d %>% distinct(country_id, pop) %>% arrange(country_id) %>%  .$pop

stan_data <- list(N_obs = N_obs,
                  N_countries = N_countries,
                  days = days, 
                  new_deaths = new_deaths, 
                  country = country,
                  pop = pop)

m <- stan(
    file = "Stan/Logistic/Hierarchical_GenLogistic_Deaths_NegBin.stan", 
    data  = stan_data, 
    chains = 4, 
    iter = 1000, 
    warmup = 500,
    cores = 4,
    save_warmup = FALSE
)

write_rds(m, "Stan/Logistic/Hierarchical_Model_GenLogistic_Deaths.rds")
write_rds(m, "Stan/Logistic/Interactive Model Checking/Hierarchical_Model_GenLogistic_Deaths.rds")
write_csv(d, "Stan/Logistic/Interactive Model Checking/stan_data_GenLogistic_Deaths.csv")
write_csv(d, str_c("Input/Stan_Data/Stan_Data_GenLogistic_Deaths_", Sys.Date(), ".csv"))

d %>% 
    group_by(country) %>% 
    summarise(First = min(date),
              Days_In_Data = n(),
              Start_Rate = min(case_rate),
              End_Rate = max(case_rate)) %>%
    write_csv(str_c("Output/Stan_Data_Info/Stan_Data_Info_GenLogistic_Deaths_", Sys.Date(), ".csv"))
