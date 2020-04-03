library(readr)
library(dplyr)
library(rstan)
library(magrittr)

options(mc.cores = parallel::detectCores())
source("Make_Stan_Data.R")

d <- Make_Stan_Data()


N_obs <- nrow(d)
N_countries <- max(d$country_id)


days <- d$days
new_cases <- d$new_cases

country <- d$country_id %>% as.integer

pop <- d %>% distinct(country_id, pop) %>% arrange(country_id) %>%  .$pop

stan_data <- list(N_obs = N_obs,
                  N_countries = N_countries,
                  days = days, 
                  new_cases = new_cases, 
                  country = country,
                  pop = pop)


m <- sampling(stan_model("Stan/Logistic/Hierarchical_Logistic_Cases_NegBin.stan"), 
              data  = stan_data, chains = 4, iter = 4000, warmup = 2000,
              control = list(adapt_delta = 0.99))

write_rds(m, "Stan/Logistic/Hierarchical_Model_NegBin.rds")
write_rds(m, str_c("Stan/Logistic/Saved_Models/Hierarchical_Model_NegBin_", Sys.Date(), ".rds"))
write_rds(m, "Stan/Logistic/Interactive Model Checking/Hierarchical_Model_NegBin.rds")
write_csv(d, "Stan/Logistic/Interactive Model Checking/stan_data.csv")
write_csv(d, str_c("Input/Stan_Data/Stan_Data_", Sys.Date(), ".csv"))

d %>% 
    group_by(country) %>% 
    summarise(First = min(date),
              Days_In_Data = n(),
              Start_Rate = min(case_rate),
              End_Rate = max(case_rate)) %>%
    write_csv(str_c("Output/Stan_Data_Info/Stan_Data_Info_", Sys.Date(), ".csv"))
