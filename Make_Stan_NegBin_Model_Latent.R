library(readr)
library(dplyr)
library(rstan)
library(magrittr)
library(broom)

options(mc.cores = parallel::detectCores())
source("Make_Stan_Data.R")

d <- Make_Stan_Data()


N_obs <- nrow(d)
N_countries <- max(d$country_id)


days <- d$days
new_cases <- d$new_cases
total_cases <- d %>% 
    group_by(country_id) %>% 
    summarise(total_cases = max(total_cases)) %>% 
    arrange(country_id)  %>% 
    .$total_cases

total_deaths <- d %>% 
    group_by(country_id) %>% 
    summarise(total_deaths = max(total_deaths)) %>% 
    arrange(country_id)  %>% 
    .$total_deaths

total_days <- d %>% 
    group_by(country_id) %>% 
    summarise(total_days = n()) %>% 
    arrange(country_id)  %>% 
    .$total_days %>% 
    cumsum

country <- d$country_id %>% as.integer
pop <- d %>% distinct(country_id, pop) %>% arrange(country_id) %>%  .$pop

stan_data <- list(N_obs = N_obs,
                  N_countries = N_countries,
                  days = days, 
                  new_cases = new_cases, 
                  country = country,
                  pop = pop,
                  total_deaths = total_deaths,
                  total_days = total_days)


m <- sampling(stan_model("Stan/Logistic/Hierarchical_Logistic_Cases_NegBin_Latent.stan"), 
              data  = stan_data, chains = 1, iter = 2000, warmup = 1000)



