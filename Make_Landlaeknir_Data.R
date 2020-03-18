library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(googlesheets4)

sheets_auth(email = "bgautijonsson@gmail.com")

d_smit <- sheets_read("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA", sheet = "Smit") %>% 
    mutate(Dagsetning = ymd(Dagsetning) %>% as.Date) %>% 
    rename(dags = Dagsetning) %>% 
    pivot_longer(contains("Smit"), 
                 names_to = "tegund", names_prefix = "Smit_",
                 values_to = "fjoldi") %>% 
    arrange(tegund, dags) %>% 
    group_by(tegund) %>% 
    mutate(dagar = as.numeric(dags - min(dags))) %>% 
    ungroup

d_sottkvi <- sheets_read("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA", sheet = "Sottkvi") %>% 
    mutate(Dagsetning = ymd(Dagsetning) %>% as.Date) %>% 
    rename(dags = Dagsetning) %>% 
    pivot_longer(-dags, 
                 names_to = "stadur", 
                 values_to = "fjoldi") %>% 
    arrange(stadur, dags) %>% 
    group_by(stadur) %>% 
    mutate(dagar = as.numeric(dags - min(dags))) %>% 
    ungroup

d_spitali <- sheets_read("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA", 
                         sheet = "Spitali")  %>% 
    mutate(Dagsetning = ymd(Dagsetning) %>% as.Date) %>% 
    dplyr::select(dags = Dagsetning, fjoldi = Fjöldi)

d_syni <- sheets_read("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA", 
                      sheet = "Syni") %>% 
    mutate(Dagsetning = ymd(Dagsetning) %>% as.Date) %>% 
    dplyr::select(dags = Dagsetning, fjoldi = Fjöldi, cum_fjoldi = Samtals)

d_aldur <- sheets_read("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA", 
                       sheet = "Aldur") %>% 
    mutate(tilfelli = tilfelli + 1,
           p_tilfelli = tilfelli / sum(tilfelli),
           p_spitali = c(0.001, 0.003, 0.012, 0.032, 0.049, 0.102, 0.166, 0.243, 0.273),
           p_alvarlegt = c(0.05, 0.05, 0.05, 0.05, 0.063, 0.122, 0.274, 0.432, 0.709))


d_smit %>% 
    write_csv("Data/smit.csv")
d_sottkvi %>% 
    write_csv("Data/sottkvi.csv")
d_spitali %>% 
    write_csv("Data/spitali.csv")
d_syni %>% 
    write_csv("Data/syni.csv")
d_aldur %>% 
    write_csv("Data/aldur.csv")

d_smit %>% 
    write_csv("Modeling/Logistic Growth/smit.csv")
d_aldur %>% 
    write_csv("Modeling/Logistic Growth/aldur.csv")


d_smit %>% 
    write_csv("COVID_Dashboard/Data/smit.csv")
d_sottkvi %>% 
    write_csv("COVID_Dashboard/Data/sottkvi.csv")
d_spitali %>% 
    write_csv("COVID_Dashboard/Data/spitali.csv")
d_syni %>% 
    write_csv("COVID_Dashboard/Data/syni.csv")
d_aldur %>% 
    write_csv("COVID_Dashboard/Data/aldur.csv")








