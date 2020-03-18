#### Parameters ####

pred_days <- 60 # Hversu langt fram á við á að spá?
n_simulations <- 2e5 # Hversu margar ítarnir í hermun
days_until_recovered <- 21 # Hve lengu ertu veik(ur)
days_in_hospital <- 14 # Hve lengi ertu á spítala
days_in_icu <- 100 # Hve lengi ertu í gjörgæslu

#### Setup ####
library(tidyverse) 
library(lubridate)
library(propagate)
select <- dplyr::select

#### Make data and read in ####
source("Make_Landlaeknir_Data.R")

d <- read_csv("Data/smit.csv") %>% 
    filter(tegund == "Samtals", fjoldi > 0)
aldur <- read_csv("Data/aldur.csv")

#### Fit Model ####

ice.g <- nls(fjoldi ~ SSlogis(dagar, phi1, phi2, phi3), data = d)

#### Obtain prediction intervals ####

preds_obj <- predictNLS(ice.g, newdata = tibble(dagar = seq(0, pred_days)), interval = "prediction")

#### Tidy into dataframes and write out ####

preds_cumulative <- preds_obj$summary %>% 
    as_tibble %>%
    select(pred = "Sim.Mean", upper = "Sim.97.5%") %>% 
    mutate(dagar = row_number() - 1)

preds_cumulative %>% write_csv("Data/preds_cumulative.csv")

preds_active <- preds_cumulative %>% 
    pivot_longer(-dagar) %>% 
    arrange(dagar, name) %>% 
    group_by(name) %>% 
    mutate(lag_value = lag(value, n = days_until_recovered, default = 0)) %>% 
    ungroup %>% 
    mutate(value = value - lag_value) %>% 
    select(-lag_value) %>% 
    pivot_wider() 

preds_active %>% write_csv("Data/preds_active.csv")

#### Simulation function ####

make_pred <- function(estimate, upper, aldur, n = 1000, q = 0.95) {
    cases_est <- rmultinom(n = n, size = estimate, prob = aldur$p_tilfelli)
    cases_upper <- rmultinom(n = n, size = upper, prob = aldur$p_tilfelli)
    
    rows <- nrow(cases_est)
    cols <- ncol(cases_est)
    
    hospital_est <- matrix(0, nrow = rows, ncol = cols)
    hospital_upper <- matrix(0, nrow = rows, ncol = cols)
    serious_est <- matrix(0, nrow = rows, ncol = cols)
    serious_upper <- matrix(0, nrow = rows, ncol = cols)
    ### This could be paralellized ###
    for (i in seq_len(cols)) {
        hospital_est[, i] <- rbinom(rows, size = cases_est[, i], prob = aldur$p_spitali)
        serious_est[, i] <- rbinom(rows, size = hospital_est[, i], prob = aldur$p_alvarlegt)
        
        hospital_upper[, i] <- rbinom(rows, size = cases_upper[, i], prob = aldur$p_spitali)
        serious_upper[, i] <- rbinom(rows, size = hospital_upper[, i], prob = aldur$p_alvarlegt)
    }
    
    
    median_cases <- apply(cases_est, 1, median)
    upper_cases <- apply(cases_upper, 1, quantile, probs = q)
    
    median_hospital <- apply(hospital_est, 1, median)
    upper_hospital <- apply(hospital_upper, 1, quantile, prob = q)
    
    median_serious <- apply(serious_est, 1, median)
    upper_serious <- apply(serious_upper, 1, quantile, prob = q)
    
    tibble(aldur = aldur$aldur,
           median_cases = median_cases,
           upper_cases = upper_cases,
           median_hospital = median_hospital,
           upper_hospital = upper_hospital,
           median_serious = median_serious,
           upper_serious = upper_serious) %>% 
        list
}

#### Perform simulation ####
simulations_cumulative <- preds_cumulative %>% 
    rowwise %>% 
    mutate(simulation = make_pred(pred, upper, aldur, n = n_simulations)) %>% 
    unnest(simulation) %>% 
    mutate(dags = dagar + min(d$dags)) %>% 
    pivot_longer(c(starts_with("median_"), starts_with("upper_"))) %>% 
    separate(name, into = c("type", "variable"), sep = "_") %>% 
    select(dags, aldur, variable, type, value) %>% 
    pivot_wider(names_from = "type", values_from = "value") %>% 
    pivot_longer(c(median, upper)) %>% 
    arrange(dags, variable, aldur) %>% 
    group_by(dags, variable, name, aldur) %>% 
    pivot_wider()

simulations_cumulative %>% write_csv("Data/simulations_cumulative.csv")

simulations_active <- preds_active %>% 
    rowwise %>% 
    mutate(simulation = make_pred(pred, upper, aldur, n = n_simulations)) %>% 
    unnest(simulation) %>% 
    mutate(dags = dagar + min(d$dags)) %>% 
    pivot_longer(c(starts_with("median_"), starts_with("upper_"))) %>% 
    separate(name, into = c("type", "variable"), sep = "_") %>% 
    select(dags, aldur, variable, type, value) %>% 
    pivot_wider(names_from = "type", values_from = "value") %>% 
    pivot_longer(c(median, upper)) %>% 
    arrange(dags, variable, aldur) %>% 
    group_by(dags, variable, name, aldur)  %>% 
    mutate(lag_value = case_when(variable == "hospital" ~ lag(value, n = days_in_hospital, default = 0),
                                 variable == "serious" ~ lag(value, n = days_in_icu),
                                 TRUE ~ lag(value, n = 14, default = 0)),
           value = case_when(variable == "hospital" ~ pmax(0, value - lag_value),
                             variable == "serious" ~ pmax(0, value - lag_value),
                             TRUE ~ value)) %>% 
    ungroup %>% 
    select(-lag_value) %>% 
    pivot_wider()

simulations_active %>% write_csv("Data/simulations_active.csv")
