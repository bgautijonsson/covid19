library(readr)
library(dplyr)
library(rstan)
library(magrittr)
library(broom)

options(mc.cores = parallel::detectCores())
source("Make_Stan_Data.R")

d <- Make_Stan_Data(min_case_rate = 0.25) %>% 
    filter(new_cases >= 0)


N_obs <- nrow(d)
N_countries <- max(d$country_id)


days <- d$days
new_cases <- d$new_cases

total_cases <- d$total_cases
total_deaths <- d$total_deaths

country <- d$country_id %>% as.integer
pop <- d %>% distinct(country_id, pop) %>% arrange(country_id) %>%  .$pop

stan_data <- list(N_obs = N_obs,
                  N_countries = N_countries,
                  days = days, 
                  new_cases = new_cases, 
                  country = country,
                  pop = pop,
                  total_deaths = total_deaths)


m <- sampling(stan_model("Stan/Logistic/Hierarchical_Logistic_Cases_NegBin_Latent.stan"), 
              data  = stan_data, chains = 1, iter = 2000, warmup = 1000)



