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
library(scales)
sheets_auth(email = "bgautijonsson@gmail.com")

source("Make_Stan_Data.R")

d <- Make_Stan_Data()

daily_cases <- function(alpha, beta, maximum, t) {
    z <- alpha + beta * t
    beta * maximum * exp(-z) / (exp(-z) + 1)^2
}

aldur <- sheets_read("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA", sheet = "Aldur") %>% 
    mutate(tilfelli = tilfelli + 1,
           p_tilfelli = tilfelli / sum(tilfelli)) %>% 
    select(aldur, tilfelli, p_tilfelli, everything())


m_total_cases <- read_rds("Stan/Logistic/Hierarchical_Model_NegBin.rds")


iceland_d <- d %>% filter(country == "Iceland")

id <- unique(iceland_d$country_id)

pars <- spread_draws(m_total_cases, 
                        alpha[country], 
                        beta[country], 
                        maximum[country],
                        phi[country]
) %>% 
    ungroup %>% 
    filter(country == id) %>% 
    mutate(iter = row_number()) %>% 
    select(
        iter, 
        alpha, 
        beta, 
        maximum,
        phi
    ) %>% 
    pivot_longer(-iter) %>% 
    group_by(name) %>% 
    summarise(mean = mean(value), var = var(value)) %>% 
    pivot_longer(-name, names_to = "par") %>% 
    pivot_wider(names_from = name, values_from = value)

N_days <- 60
N_agegroups <- nrow(aldur)
pop <- unique(iceland_d$pop)

alpha_prior <- pars$alpha
beta_prior <- pars$beta
maximum_prior <- pars$maximum
phi_prior <- pars$phi

days <- seq_len(N_days) - 1

age_cases <- aldur$tilfelli %>% as.integer

stan_data <- list(
    N_days = N_days,
    N_agegroups = N_agegroups,
    pop = pop,
    age_cases = age_cases,
    days = days,
    age_cases = age_cases,
    alpha_prior = alpha_prior,
    beta_prior = beta_prior,
    maximum_prior = maximum_prior,
    phi_prior = phi_prior
)

str(stan_data)


m_iceland <- sampling(stan_model("Stan/Iceland_Simulation/Iceland_Simulation.stan"), 
              data  = stan_data, chains = 1, iter = 1000, warmup = 500)


tidyMCMC(m_iceland, conf.int = T, ess = T, rhat = T,
         pars = c("theta"))



spread_draws(m_iceland, new_cases[days]) %>% 
    ungroup %>% 
    mutate(days = days - 1) %>% 
    select(days, new_cases) %>% 
    group_by(days) %>% 
    mutate(iter = row_number()) %>% 
    group_by(iter) %>% 
    mutate(total_cases = cumsum(new_cases)) %>% 
    group_by(days) %>% 
    summarise(median = median(total_cases),
              upper = quantile(total_cases, 0.975)) %>% 
    ggplot(aes(days, median)) +
    geom_line() +
    geom_line(aes(y = upper), lty = 2) +
    scale_y_continuous(breaks = pretty_breaks(8))

