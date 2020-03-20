library(tidyverse); library(knitr); library(kableExtra); library(broom); library(cowplot); 
library(rstan); library(tidybayes); library(scales); library(googlesheets4)

sheets_auth(email = "bgautijonsson@gmail.com")


d <- read_csv("https://www.dropbox.com/s/br9kjy0pbrzscq3/ECDC_Data.csv?dl=1") %>% 
    filter(case_rate > 0.01, 
           !country %in% c("China", "Hubei")) %>% 
    select(country, pop, date, days, cases = cum_cases, case_rate) %>% 
    group_by(country) %>% 
    mutate(days = row_number() - 1) %>% 
    filter(any(days > 10)) %>% 
    ungroup %>% 
    mutate(country_id = as.numeric(as.factor(country)))

smit <- sheets_read("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA", sheet = "Smit") %>% 
    mutate(fjoldi = Smit_Samtals + cumsum(IE_Smit)) %>% 
    select(date = Dagsetning, cases = fjoldi) %>% 
    mutate(country = "Iceland",
           pop = 339031,
           case_rate = cases / pop * 1000,
           country_id = 6,
           date = lubridate::as_date(date)) %>% 
    filter(case_rate >= 0.01) %>% 
    mutate(days = row_number() - 1)

d <- d %>% 
    filter(country != "Iceland") %>% 
    bind_rows(smit)

write_csv(d, "stan_dat.csv")