##### Info ####

# Fjöldi virkra smita
# Stan model gefur predictions á cumulative cases, ekki current cases.
# Gefum okkur að manneskja sé 3 vikur (21 dag) að jafna sig. 
# Þá væri active_cases á tíma t = cumulative_t - cumulative_(t - 21).
# Getum því fengið cumulative predictions úr líkani og differencað til að fá active numbers.
# 
# 
# Fjöldi fólks á sjúkrahúsi
# Við höfum tölur frá 


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
    mutate(tilfelli = tilfelli + 1,
           p_tilfelli = tilfelli / sum(tilfelli)) %>% 
    select(aldur, tilfelli, p_tilfelli, everything())

m <- read_rds("Stan/Logistic/Hierarchical_Model.rds")

iceland_d <- d %>% filter(country == "Iceland")

id <- unique(iceland_d$country_id)
pop <- unique(iceland_d$pop)
start_date <- min(iceland_d$date)

results <- spread_draws(m, alpha[country], beta[country], maximum[country]) %>% 
    ungroup %>% 
    filter(country == id) %>% 
    mutate(iter = row_number()) %>% 
    select(iter, alpha, beta, maximum) %>% 
    expand_grid(days = seq(0, 60)) %>% 
    mutate(linear = alpha + beta * days,
           rate = maximum / (1 + exp(-linear)),
           cases = rate * pop) %>% 
    group_by(iter) %>% 
    mutate(recovered = lag(cases, n = 21, default = 0),
           active_cases = pmax(0, cases - recovered)) %>% 
    ungroup %>% 
    select(iter, days, cumulative_cases = cases, active_cases)



age_results <- results %>% 
    filter(iter >= max(iter) - 1000) %>% 
    rowwise %>% 
    mutate(age_cases = list(tibble(age = aldur$aldur, 
                                   cases_active = as.vector(rmultinom(1, 
                                                                      size = active_cases, 
                                                                      prob = aldur$p_tilfelli)),
                                   cases_cumulative = as.vector(rmultinom(1, 
                                                                      size = cumulative_cases, 
                                                                      prob = aldur$p_tilfelli))))) %>% 
    unnest(age_cases) %>% 
    ungroup

all_results <- age_results %>% 
    group_by(iter, days) %>% 
    mutate(hospital_active = rbinom(n(), size = cases_active, prob = aldur$p_spitali),
           hospital_cumulative = rbinom(n(), size = cases_cumulative, prob = aldur$p_spitali)) %>% 
    group_by(iter, age) %>% 
    mutate(hospital_active = lag(hospital_active, 7, default = 0),
           hospital_cumulative = lag(hospital_cumulative, 7, default = 0),
           hospital_active = pmax(hospital_active - lag(hospital_active, 14, default = 0), 0)) %>% 
    group_by(iter, days) %>% 
    mutate(icu_active = rbinom(n(), size = hospital_active, prob = aldur$p_spitali),
           icu_cumulative = rbinom(n(), size = hospital_cumulative, prob = aldur$p_spitali))  %>% 
    group_by(iter, age) %>% 
    mutate(icu_active = lag(icu_active, 3, default = 0),
           icu_cumulative = lag(icu_cumulative, 3, default = 0),
           icu_active = pmax(icu_active - lag(icu_active, n = 10, default = 0), 0)) %>% 
    ungroup %>% 
    pivot_longer(c(cases_active, hospital_active, icu_active,
                   cases_cumulative, hospital_cumulative, icu_cumulative), 
                 names_to = c("name", "type"),
                 names_pattern = "(.*)_(.*)",
                 values_to = "value") %>% 
    group_by(iter, days, name, type) %>% 
    mutate(total = case_when(name == "cases" & type == "active" ~ active_cases,
                             name == "cases" & type == "cumulative" ~ cumulative_cases,
                             name %in% c("hospital", "icu") ~ sum(value))) %>% 
    ungroup %>% 
    select(-active_cases, -cumulative_cases) %>% 
    pivot_wider(names_from = "age", values_from = "value") %>% 
    pivot_longer(c(-iter, -days, -name, -type), names_to = "age", values_to = "value") %>% 
    group_by(date = days + start_date, type, name, age,) %>% 
    summarise(median = median(value),
              upper = quantile(value, .975))

out_path <- str_c("Output/Iceland_Predictions/Iceland_Predictions_", Sys.Date(), ".csv")

write_csv(all_results, out_path)
