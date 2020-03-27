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
    mutate(tilfelli = round(tilfelli / 4),
           p_tilfelli = tilfelli / sum(tilfelli)) %>% 
    select(aldur, tilfelli, p_tilfelli, everything())

N_agegroups <- nrow(aldur)

age_cases <- aldur$tilfelli %>% as.integer

stan_data <- list(
    N_agegroups = N_agegroups,
    age_cases = age_cases
)

str(stan_data)


m_iceland <- sampling(stan_model("Stan/Iceland_Simulation/Iceland_Simulation.stan"), 
              data  = stan_data, chains = 4, iter = 2000, warmup = 100)

tidyMCMC(m_iceland, conf.int = T)

write_rds(m_iceland, "age_dist.rds")
