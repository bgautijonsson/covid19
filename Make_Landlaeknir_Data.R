Make_Landlaeknir_Data <- function() {
    library(dplyr)
    library(tidyr)
    library(readr)
    library(lubridate)
    
    
    
    read_csv("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA/export?format=csv&gid=1788393542") %>%
        rename(new_deaths = "Dauðsföll", new_cases = Ny_Smit, total_cases = Smit_Samtals, total_deaths = Dauðsföll_Samtals) %>% 
        mutate(date = ymd(Dagsetning),
               country = "Iceland") %>% 
        select(country, date, total_cases, new_cases, new_deaths, total_deaths)
}



    





