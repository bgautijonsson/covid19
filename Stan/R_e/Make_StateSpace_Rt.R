library(tidyverse)
library(rstan)
library(lubridate)

options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")


d <- read_csv("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA/export?format=csv&id=1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA&gid=1788393542") %>% 
    select(
        date = "Dagsetning",
        new_cases = "Ny_Smit",
        new_deaths = "Dauðsföll",
        new_recovered = "Batnað", 
        total_cases = "Smit_Samtals",
        total_deaths = "Dauðsföll_Samtals",
        total_recovered = "Batnað_Samtals",
        total_removed = "Removed_Samtals",
        active_cases = "Virk_Smit")

d <- d %>% 
    mutate(date = ymd(date),
           N = 364220,
           S = N - total_cases,
           S = lag(S, 1, default = 0),
           active_cases = lag(active_cases, 1, default = 0)) %>% 
    select(date, dI_dt = new_cases, total_cases, S,  I = active_cases, R = total_removed, N) %>% 
    filter(date >= ymd("2020-03-01"))

N_days <- nrow(d)

S <- d$S
I <- d$I
dI_dt <- d$dI_dt
N <- 364220


days <- seq_len(N_days) - 1

stan_data <- list(
    N_days = N_days,
    N = N,
    S  = S,
    I = I,
    dI_dt = dI_dt,
    days = days
)


m <- sampling(stan_model("SIR_Model_Discrete_StateSpace.stan"), 
              data  = stan_data, chains = 4, iter = 4000, warmup = 2000,
              control = list(max_treedepth = 15))

write_rds(m, "Rt_Model_StateSpace.rds")




