library(tidyverse)
library(rstan)
library(magrittr)
library(broom)
library(tidybayes)

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


m <- sampling(stan_model("Stan/Bi-Logistic/Hierarchical_BiLogistic_Cases_NegBin.stan"), 
              data  = stan_data, chains = 1, iter = 2000, warmup = 1000)


daily_cases <- function(alpha1, beta1, S1, alpha2, beta2, S2, t) {
    z1 <- alpha1 + beta1 * t
    z2 <- alpha2 + beta2 * t
    m1 <- beta1 * S1 * exp(-z1) / (exp(-z1) + 1)^2
    m2 <- beta2 * S2 * exp(-z2) / (exp(-z2) + 1)^2
    
    m1 + m2
}

daily_cases_split <- function(alpha, beta, S, t) {
    z <- alpha + beta * t
    S  / (exp(-z) + 1)
}


plot_dat <- d %>% filter(country == "Iceland")
id <- unique(plot_dat$country_id)
pop <- unique(plot_dat$pop)
start_cases <- min(plot_dat$total_cases)

days_in_data <- max(plot_dat$days) + 1

tidyMCMC(m, par = c("mu_alpha", "mu_alpha2", "sigma_alpha", "sigma_alpha2",
                    "mu_beta", "mu_beta2", "sigma_beta", "sigma_beta2"), rhat = T, ess = T)

results <- spread_draws(m, 
             alpha[country], beta[country], S[country], 
             alpha2[country], beta2[country], S2[country],
             phi[country]
             ) %>% 
    ungroup %>% 
    filter(country == id) %>% 
    mutate(iter = row_number()) %>% 
    select(iter, 
           alpha, beta, S, 
           alpha2, beta2, S2,
           phi
           ) %>% 
    expand_grid(days = seq(1, days_in_data + 100)) %>% 
    mutate(daily_rate1 = daily_cases_split(alpha, beta, S, days),
           daily_rate2 = daily_cases_split(alpha2, beta2, S2, days),
           daily_rate3 = daily_rate1 + daily_rate2,
           daily_rate = daily_cases(alpha, beta, S, alpha2, beta2, S2, days),
           daily_cases = rnbinom(n(), mu = daily_rate * pop, size = phi)) %>% 
    group_by(iter) %>% 
    mutate(cases = as.numeric(cumsum(daily_cases)) + start_cases) %>% 
    ungroup %>% 
    select(iter, days, new_cases = daily_cases, cumulative_cases = cases,
           daily_rate1, daily_rate2, daily_rate3)


results %>% 
    select(days, daily_rate1, daily_rate2, daily_rate3) %>% 
    pivot_longer(-days) %>% 
    group_by(days, name) %>% 
    summarise(median = median(value),
              upper = quantile(value, 0.975)) %>% 
    ggplot(aes(days, median, group = name)) +
    geom_line() +
    geom_line(aes(y = upper), lty = 2)
