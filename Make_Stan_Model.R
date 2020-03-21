library(readr)
library(dplyr)
library(rstan)
library(magrittr)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
source("Make_Stan_Data.R")

d <- Make_Stan_Data()

N_obs <- nrow(d)
N_countries <- max(d$country_id)


days <- d$days
cases <- d$total_cases
country <- d$country_id %>% as.integer


pop <- d %>% distinct(country_id, pop) %>% arrange(country_id) %>%  .$pop

# iceland_d <- d %>% filter(country == "Iceland")
# 
# iceland_id <- unique(iceland_d$country_id)
# N_preds <- 60
# N_agegroups <- 9
# pred_days <- seq_len(N_preds) - 1
# age_dist <- aldur$p_tilfelli
# hospital_dist <- aldur$p_spitali
# icu_dist <- aldur$p_alvarlegt

stan_data <- list(N_obs = N_obs,
                  N_countries = N_countries,
                  days = days, 
                  obs_cases = cases, 
                  country = country,
                  pop = pop)

m <- sampling(stan_model("Stan/Logistic/Hierarchical_Logistic_Cases.stan"), 
              data  = stan_data, chains = 4, iter = 3000, warmup = 1000,
              control = list(max_treedepth = 15))

write_rds(m, "Stan/Logistic/Hirearchical_Model.rds")