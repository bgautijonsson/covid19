library(readr)
library(dplyr)
library(rstan)
library(magrittr)

options(mc.cores = parallel::detectCores())
source("Make_Stan_Data_RW.R")

d <- Make_Stan_Data()

new_cases <- d %>% 
    mutate(new_cases = as.integer(new_cases)) %>% 
    select(date, country, new_cases) %>% 
    pivot_wider(names_from = country, values_from = new_cases) %>% 
    select(-date) %>% 
    as.matrix

N_countries <- ncol(new_cases)
N_days <- nrow(new_cases)

pop <- d %>% distinct(country_id, pop) %>% arrange(country_id) %>%  .$pop
min_rate <- d %>% 
    group_by(country, country_id) %>% 
    summarise(min_rate = min(case_rate) / 1000) %>% 
    arrange(country_id) %>% 
    .$min_rate

stan_data <- list(N_days = N_days,
                  N_countries = N_countries,
                  new_cases = new_cases, 
                  pop = pop,
                  min_rate = min_rate)


m <- sampling(stan_model("Stan/Logistic/Hierarchical_Logistic_Cases_NegBin_RW.stan"), 
              data  = stan_data, chains = 4, iter = 4000, warmup = 2000,
              control = list(adapt_delta = 0.99))

write_rds(m, "Stan/Logistic/Hierarchical_Model_NegBin_RW.rds")
write_rds(m, str_c("Stan/Logistic/Saved_Models/Hierarchical_Model_NegBin_RW_", Sys.Date(), ".rds"))
write_rds(m, "Stan/Logistic/Interactive Model Checking/Hierarchical_Model_NegBin_RW.rds")
write_csv(d, "Stan/Logistic/Interactive Model Checking/stan_data_RW.csv")
write_csv(d, str_c("Input/Stan_Data/Stan_Data_RW_", Sys.Date(), ".csv"))

d %>% 
    group_by(country) %>% 
    summarise(First = min(date),
              Days_In_Data = n(),
              Start_Rate = min(case_rate),
              End_Rate = max(case_rate)) %>%
    write_csv(str_c("Output/Stan_Data_Info/Stan_Data_RW_Info_", Sys.Date(), ".csv"))
