library(readr)
library(dplyr)
library(rstan)
library(magrittr)

options(mc.cores = parallel::detectCores())
source("Make_Stan_Data.R")

d <- Make_Stan_Data() %>% 
    filter(new_cases >= 0)


N_obs <- nrow(d)
N_countries <- max(d$country_id)


days <- d$days
new_cases <- d$new_cases

country <- d$country_id %>% as.integer

pop <- d %>% distinct(country_id, pop) %>% arrange(country_id) %>%  .$pop

X_GP <- d %>% 
    mutate(X = days + 1000 * country_id) %>% 
    .$X

stan_data <- list(N_obs = N_obs,
                  N_countries = N_countries,
                  days = days, 
                  new_cases = new_cases, 
                  country = country,
                  pop = pop,
                  X_GP = X_GP)


m <- sampling(stan_model("Stan/Logistic/Hierarchical_Logistic_Cases_GaussianProcess.stan"), 
              data  = stan_data, chains = 4, iter = 4000, warmup = 2000)

write_rds(m, "Stan/Logistic/Hierarchical_Model_GaussianProcess.rds")
write_rds(m, "Stan/Logistic/Interactive Model Checking/Hierarchical_Model_GaussianProcess.rds")
write_csv(d, "Stan/Logistic/Interactive Model Checking/stan_data_GaussianProcess.csv")
write_csv(d, str_c("Input/Stan_Data/Stan_Data_GaussianProcess_", Sys.Date(), ".csv"))

d %>% 
    group_by(country) %>% 
    summarise(First = min(date),
              Days_In_Data = n(),
              Start_Rate = min(case_rate),
              End_Rate = max(case_rate)) %>%
    write_csv(str_c("Output/Stan_Data_Info/Stan_Data_Info_GaussianProcess_", Sys.Date(), ".csv"))
