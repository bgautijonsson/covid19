library(tidyverse)
library(rstan)
library(tidybayes)
library(magrittr)
library(broom)
library(googlesheets4)
sheets_auth(email = "bgautijonsson@gmail.com")

source("Make_Stan_Data.R")

d <- Make_Stan_Data()
aldur <- sheets_read("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA", sheet = "Aldur") %>% 
    mutate(p_tilfelli = tilfelli / sum(tilfelli)) %>% 
    select(aldur, tilfelli, p_tilfelli, everything())

m <- read_rds("Stan/Logistic/Hirearchical_Model.rds")

iceland_d <- d %>% filter(country == "Iceland")

id <- unique(iceland_d$country_id)
pop <- unique(iceland_d$pop)
start_date <- min(iceland_d$date)

results <- spread_draws(m, alpha[country], beta[country], maximum[country]) %>% 
    ungroup %>% 
    filter(country == id) %>% 
    mutate(iter = row_number()) %>% 
    select(iter, alpha, beta, maximum) %>% 
    expand_grid(days = seq(0, 45)) %>% 
    mutate(linear = alpha + beta * days,
           rate = maximum / (1 + exp(-linear)),
           cases = rpois(n(), rate * pop)) %>% 
    group_by(iter) %>% 
    mutate(recovered = lag(cases, n = 21, default = 0),
           active_cases = cases - recovered) %>% 
    ungroup %>% 
    select(iter, days, active_cases)



age_results <- results %>% 
    filter(iter >= max(iter) - 2000) %>% 
    rowwise %>% 
    mutate(age_cases = list(tibble(age = aldur$aldur, 
                                   cases = as.vector(rmultinom(1, 
                                                               size = active_cases, 
                                                               prob = aldur$p_tilfelli))))) %>% 
    unnest(age_cases) %>% 
    ungroup


all_results <- age_results %>% 
    group_by(iter, days) %>% 
    mutate(hospital = rbinom(n(), size = cases, prob = aldur$p_spitali),
           icu = rbinom(n(), size = hospital, prob = aldur$p_spitali))  %>% 
    ungroup %>% 
    pivot_longer(c(cases, hospital, icu)) %>% 
    group_by(iter, days, name) %>% 
    mutate(total = case_when(name == "cases" ~ as.integer(active_cases),
                             name %in% c("hospital", "icu") ~ sum(value))) %>% 
    ungroup %>% 
    select(-active_cases) %>% 
    pivot_wider(names_from = "age", values_from = "value") %>% 
    pivot_longer(c(-iter, -days, -name), names_to = "age", values_to = "value") %>% 
    group_by(date = days + start_date, name, age) %>% 
    summarise(median = median(value),
              upper = quantile(value, .99))

out_path <- str_c("Output/Iceland_Predictions_", Sys.Date(), ".csv")

write_csv(all_results, out_path)
           