library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(rstan)
library(here)
library(broom)
library(stringr)

options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

d <- read_csv("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA/export?format=csv&gid=1788393542") %>% 
    mutate(N = 364134,
           S = N - Smit_Samtals,
           R = Removed_Samtals) %>% 
    select(date = Dagsetning, 
           N, 
           S, I = Virk_Smit, R) %>% 
    filter(date >= ymd("2020-03-05")) %>% 
    mutate(betanum = 1 + (date >= ymd("2020-03-16")))



N_days <- nrow(d) - 1

counts <- d %>% select(S, I, R) %>% slice(-1) %>%  as.matrix


y0 <- d %>% select(S, I, R) %>% slice(1) %>% unlist

betanum <- d$betanum[-1]

stan_data <- list(N_days = N_days,
                  counts = counts,
                  y0 = y0,
                  N = 364134,
                  betanum)

m <- sampling(stan_model(here("Stan", "SIR", "SIR.stan")), data = stan_data, 
         chains = 4, iter = 4000)


tidyMCMC(m, pars = c("beta", "gamma", "R0", "recovery_time"), rhat = T, ess = T, conf.int = T)


tidyMCMC(m, pars = "y") %>% 
    mutate(day = str_match(term, "([0-9]+),")[, 2] %>% as.numeric,
           par = c("S", "I", "R")[as.numeric(str_match(term, ",([0-9]+)")[, 2])]) %>% 
    ggplot(aes(day, estimate)) +
    geom_line() +
    facet_wrap("par", scales = "free") +
    geom_point(data = d %>% 
                   mutate(day = as.numeric(date - ymd("2020-03-15")) + 1) %>% 
                   pivot_longer(cols = c(S, I, R), names_to = "par", values_to = "estimate") )
