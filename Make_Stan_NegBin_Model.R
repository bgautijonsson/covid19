TEST <- TRUE

library(readr)
library(dplyr)
library(rstan)
library(magrittr)

options(mc.cores = parallel::detectCores())
source("Make_Stan_Data.R")

d <- Make_Stan_Data(min_case_rate = 0.02, min_days = 6, upper_mult = 3)


N_obs <- nrow(d)
N_countries <- max(d$country_id)


days <- d$days
new_cases <- d$new_cases
total_cases <- d$total_cases
country <- d$country_id %>% as.integer

pop <- d %>% distinct(country_id, pop) %>% arrange(country_id) %>%  .$pop

stan_data <- list(N_obs = N_obs,
                  N_countries = N_countries,
                  days = days, 
                  new_cases = new_cases, 
                  total_cases = total_cases, 
                  country = country,
                  pop = pop)


if (TEST) {
    m <- sampling(stan_model("Stan/Test/Logistic/Hierarchical_Logistic_Cases_NegBin.stan"), 
                  data  = stan_data, chains = 4, iter = 2000, warmup = 1000)
    
    write_rds(m, "Stan/Test/Logistic/Hierarchical_Model_NegBin.rds")
    write_rds(m, str_c("Stan/Test/Logistic/Saved_Models/Hierarchical_Model_NegBin_", Sys.Date(), ".rds"))
    write_rds(m, "Stan/Test/Logistic/Interactive Model Checking/Hierarchical_Model_NegBin.rds")
    write_csv(d, "Stan/Test/Logistic/Interactive Model Checking/stan_data.csv")
    write_csv(d, str_c("Input/Test/Stan_Data/Stan_Data_", Sys.Date(), ".csv"))
    
    d %>% 
        group_by(country) %>% 
        summarise(First = min(date),
                  Days_In_Data = n(),
                  Start_Rate = min(case_rate),
                  End_Rate = max(case_rate)) %>%
        write_csv(str_c("Output/Test/Stan_Data_Info/Stan_Data_Info_", Sys.Date(), ".csv"))
    
} else {
    m <- sampling(stan_model("Stan/Public/Logistic/Hierarchical_Logistic_Cases_NegBin.stan"), 
                  data  = stan_data, chains = 4, iter = 2000, warmup = 1000)
    
    write_rds(m, "Stan/Public/Logistic/Hierarchical_Model_NegBin.rds")
    write_rds(m, str_c("Stan/Public/Logistic/Saved_Models/Hierarchical_Model_NegBin_", Sys.Date(), ".rds"))
    write_rds(m, "Stan/Public/Logistic/Interactive Model Checking/Hierarchical_Model_NegBin.rds")
    write_csv(d, "Stan/Public/Logistic/Interactive Model Checking/stan_data.csv")
    write_csv(d, str_c("Input/Public/Stan_Data/Stan_Data_", Sys.Date(), ".csv"))
    
    d %>% 
        group_by(country) %>% 
        summarise(First = min(date),
                  Days_In_Data = n(),
                  Start_Rate = min(case_rate),
                  End_Rate = max(case_rate)) %>%
        write_csv(str_c("Output/Public/Stan_Data_Info/Stan_Data_Info_", Sys.Date(), ".csv"))
}
