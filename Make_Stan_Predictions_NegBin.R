##### Parameters #####
lag_infected_to_hospital <- 7
lag_hospital_to_icu <- 3
days_from_infection_to_healthy <- 21
days_in_hospital <- 14
days_in_icu <- 10

N_iter <- 2000

##### Packages #####
library(tidyverse)
library(rstan)
library(tidybayes)
library(magrittr)
library(broom)
library(googlesheets4)
library(lubridate)
sheets_auth(email = "bgautijonsson@gmail.com")

##### Data and Functions #####
source("Make_Stan_Data.R")

d <- Make_Stan_Data()

daily_cases <- function(alpha, beta, S, t) {
    z <- alpha + beta * t
    beta * S * exp(-z) / (exp(-z) + 1)^2
}

aldur <- sheets_read("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA/edit#gid=0", sheet = "Aldur") %>% 
    slice(1:9) %>% 
    mutate(tilfelli = tilfelli,
           p_tilfelli = tilfelli / sum(tilfelli))


m <- read_rds("Stan/Logistic/Hierarchical_Model_NegBin.rds")


iceland_d <- d %>% filter(country == "Iceland")

id <- unique(iceland_d$country_id)
pop <- unique(iceland_d$pop)
start_date <- min(iceland_d$date)
start_cases <- min(iceland_d$total_cases)

##### Start simulations #####

# Sample cumulative cases and calculate active cases
results <- spread_draws(
    m, 
    alpha[country], 
    beta[country], 
    S[country],
    phi[country]
) %>% 
    ungroup %>% 
    filter(country == id) %>% 
    mutate(iter = row_number()) %>% 
    select(
        iter, 
        alpha, 
        beta, 
        S,
        phi
    ) %>% 
    expand_grid(days = seq(1, 60)) %>% 
    mutate(
        daily_rate = daily_cases(alpha = alpha, beta = beta, S = S, t = days),
        new_cases = rnbinom(n(), mu = daily_rate * pop, size = phi),
    ) %>% 
    group_by(iter) %>% 
    mutate(total_cases = cumsum(new_cases) + start_cases,
           recovered_cases = lag(total_cases, n = 21, default = 0),
           active_cases = total_cases - recovered_cases) %>%  
    ungroup %>% 
    select(iter, days, new_cases, total_cases, recovered_cases, active_cases)


# Calculate cases in each age-group
age_results <- results %>% 
    filter(iter >= max(iter) - N_iter) %>% 
    rowwise %>% 
    mutate(
        age_cases = list(
            tibble(
                age = aldur$aldur, 
                cases_new = as.vector(rmultinom(1, 
                                                size = new_cases, 
                                                prob = aldur$p_tilfelli)),
            )
        )
    ) %>% 
    unnest(age_cases) %>% 
    group_by(iter, age) %>% 
    mutate(cases_cumulative = cumsum(cases_new),
           cases_recovered = lag(cases_cumulative, n = 21, default = 0),
           cases_active = cases_cumulative - cases_recovered) %>% 
    ungroup


all_results <- age_results %>% 
    group_by(iter, days) %>% 
    # Sample hospitalizations with binomial distribution fom Ferguson et al.
    mutate(hospital_new = ifelse(cases_new == 0, 0, rbinom(n(), size = cases_new, prob = aldur$p_spitali)),
           icu_new = ifelse(hospital_new == 0, 0, rbinom(n(), size = hospital_new, prob = aldur$p_alvarlegt))) %>% 
    group_by(iter, age) %>% 
    mutate(hospital_new = lag(hospital_new, n = lag_infected_to_hospital, default = 0),
           icu_new = lag(icu_new, n =  lag_infected_to_hospital + lag_hospital_to_icu, default = 0),
           hospital_cumulative = cumsum(hospital_new),
           icu_cumulative = cumsum(icu_new),
           hospital_discharged = lag(hospital_cumulative, n = days_in_hospital, default = 0),
           icu_discharged = lag(icu_cumulative, n = days_in_icu, default = 0),
           hospital_active = hospital_cumulative - hospital_discharged,
           icu_active = icu_cumulative - icu_discharged) %>% 
    select(iter, days, new_cases, total_cases, recovered_cases, active_cases, age, starts_with("cases"), starts_with("hospital"), starts_with("icu")) %>%  
    pivot_longer(c(cases_new, cases_cumulative, cases_recovered, cases_active,
                   hospital_new, hospital_cumulative, hospital_discharged, hospital_active,
                   icu_new, icu_cumulative, icu_discharged, icu_active),
                 names_to = c("name", "type"),
                 names_pattern = "(.*)_(.*)",
                 values_to = "value") %>% 
    group_by(iter, days, name, type) %>% 
    mutate(total = sum(value)) %>% 
    ungroup %>% 
    mutate(total = case_when(name %in% c("hospital", "icu") ~ total,
                             type == "new" ~ new_cases,
                             type == "cumulative" ~ total_cases,
                             type == "recovered" ~ recovered_cases,
                             type == "active" ~ active_cases)) %>% 
    select(-new_cases, -total_cases, -recovered_cases, -active_cases) %>% 
    arrange(iter, days, name, type) %>% 
    pivot_wider(names_from = age, values_from = value) %>% 
    pivot_longer(c(-iter, -days, -name, -type), names_to = "age", values_to = "value") %>% 
    group_by(date = days + start_date, name, type, age) %>% 
    summarise(median = median(value),
              upper = quantile(value, 0.975))

out <- all_results %>% 
    mutate(aldursdreifing = "gögn")

# Calculate cases in each age-group
age_results <- results %>% 
    filter(iter >= max(iter) - N_iter) %>% 
    rowwise %>% 
    mutate(
        age_cases = list(
            tibble(
                age = aldur$aldur, 
                cases_new = as.vector(rmultinom(1, 
                                                size = new_cases, 
                                                prob = aldur$smoothed_dreifing)),
            )
        )
    ) %>% 
    unnest(age_cases) %>% 
    group_by(iter, age) %>% 
    mutate(cases_cumulative = cumsum(cases_new),
           cases_recovered = lag(cases_cumulative, n = 21, default = 0),
           cases_active = cases_cumulative - cases_recovered) %>% 
    ungroup


all_results <- age_results %>% 
    group_by(iter, days) %>% 
    # Sample hospitalizations with binomial distribution fom Ferguson et al.
    mutate(hospital_new = ifelse(cases_new == 0, 0, rbinom(n(), size = cases_new, prob = aldur$p_spitali)),
           icu_new = ifelse(hospital_new == 0, 0, rbinom(n(), size = hospital_new, prob = aldur$p_alvarlegt))) %>% 
    group_by(iter, age) %>% 
    mutate(hospital_new = lag(hospital_new, n = lag_infected_to_hospital, default = 0),
           icu_new = lag(icu_new, n =  lag_infected_to_hospital + lag_hospital_to_icu, default = 0),
           hospital_cumulative = cumsum(hospital_new),
           icu_cumulative = cumsum(icu_new),
           hospital_discharged = lag(hospital_cumulative, n = days_in_hospital, default = 0),
           icu_discharged = lag(icu_cumulative, n = days_in_icu, default = 0),
           hospital_active = hospital_cumulative - hospital_discharged,
           icu_active = icu_cumulative - icu_discharged) %>% 
    select(iter, days, new_cases, total_cases, recovered_cases, active_cases, age, starts_with("cases"), starts_with("hospital"), starts_with("icu")) %>%  
    pivot_longer(c(cases_new, cases_cumulative, cases_recovered, cases_active,
                   hospital_new, hospital_cumulative, hospital_discharged, hospital_active,
                   icu_new, icu_cumulative, icu_discharged, icu_active),
                 names_to = c("name", "type"),
                 names_pattern = "(.*)_(.*)",
                 values_to = "value") %>% 
    group_by(iter, days, name, type) %>% 
    mutate(total = sum(value)) %>% 
    ungroup %>% 
    mutate(total = case_when(name %in% c("hospital", "icu") ~ total,
                             type == "new" ~ new_cases,
                             type == "cumulative" ~ total_cases,
                             type == "recovered" ~ recovered_cases,
                             type == "active" ~ active_cases)) %>% 
    select(-new_cases, -total_cases, -recovered_cases, -active_cases) %>% 
    arrange(iter, days, name, type) %>% 
    pivot_wider(names_from = age, values_from = value) %>% 
    pivot_longer(c(-iter, -days, -name, -type), names_to = "age", values_to = "value") %>% 
    group_by(date = days + start_date, name, type, age) %>% 
    summarise(median = median(value),
              upper = quantile(value, 0.975))

out <- out %>% 
    bind_rows(all_results %>% 
                  mutate(aldursdreifing = "óhagstæð"))

##### Output #####
out_path <- str_c("Output/Iceland_Predictions/Iceland_Predictions_", Sys.Date(), ".csv")
write_csv(out, out_path)


out_path_posterior <- str_c("Output/Iceland_Posterior/Iceland_Posterior_", Sys.Date(), ".csv")
results %>% 
    mutate(date = days - 1 + min(iceland_d$date)) %>% 
    select(date, iter, new_cases) %>% 
    write_csv(out_path_posterior)

