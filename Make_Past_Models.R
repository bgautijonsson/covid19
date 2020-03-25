library(readr)
library(dplyr)
library(rstan)
library(magrittr)
library(stringr)

options(mc.cores = parallel::detectCores())
source("Make_Stan_Data.R")

cur_date <- Sys.Date()
i <- 0

while (TRUE) {
    
    d <- Make_Stan_Data(min_case_rat = 0.02, min_days = 8 + i) %>% 
        filter(date <= cur_date)
    
    N_obs <- nrow(d)
    N_countries <- max(d$country_id)
    
    if (N_countries < 8) break
    
    
    days <- d$days
    new_cases <- d$new_cases
    total_cases <- d$total_cases
    country <- d$country_id %>% as.integer
    
    pop <- d %>% distinct(country_id, pop) %>% arrange(country_id) %>%  .$pop
    
    N_preds <- 60
    pred_days <- seq_len(60) - 1
    country_id = d %>% filter(country == "Iceland") %>% .$country_id %>% unique
    
    stan_data <- list(N_obs = N_obs,
                      N_countries = N_countries,
                      days = days, 
                      new_cases = new_cases, 
                      total_cases = total_cases, 
                      country = country,
                      pop = pop,
                      N_preds = N_preds,
                      pred_days = pred_days,
                      country_id = country_id)
    
    m <- sampling(stan_model("Stan/Logistic/Hierarchical_Logistic_Cases.stan"), 
                  data  = stan_data, chains = 4, iter = 2000, warmup = 1000)
    
    write_rds(m, str_c("Stan/Logistic/Past_Models/Model_", cur_date, ".rds"))
    write_csv(d, str_c("Stan/Logistic/Past_Models/Stan_Data_", cur_date, ".csv"))
    
    cur_date <- cur_date - 1
    i <- i + 1
}
