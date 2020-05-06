library(readr)
library(dplyr)
library(rstan)
library(magrittr)
library(broom)

options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
source("Make_Stan_Data.R")

d <- Make_Stan_Data() %>% 
    filter(new_cases >= 0)


N_obs <- nrow(d)
N_countries <- max(d$country_id)


days <- d$days
new_cases <- d$new_cases
total_deaths <- d$total_deaths

country <- d$country_id %>% as.integer
pop <- d %>% distinct(country_id, pop) %>% arrange(country_id) %>%  .$pop

stan_data <- list(N_obs = N_obs,
                  N_countries = N_countries,
                  days = days, 
                  new_cases = new_cases, 
                  country = country,
                  pop = pop,
                  total_deaths = new_deaths)


m <- sampling(stan_model("Stan/Logistic/Hierarchical_Logistic_Cases_NegBin_Latent.stan"), 
              data  = stan_data, chains = 1, iter = 4000, warmup = 2000)


tidyMCMC(m, pars = c("mu_alpha_cases", "mu_beta_cases", "mu_s_cases",
                     "mu_alpha_deaths", "mu_beta_deaths", "mu_s_deaths"), rhat = T, ess = T, conf.int = T)    


