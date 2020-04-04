Make_Landlaeknir_Data <- function() {
    library(dplyr)
    library(tidyr)
    library(readr)
    library(lubridate)
    
    
    
    read_csv("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA/export?format=csv&gid=0") %>% 
        rename(new_deaths = "Dauðsföll") %>% 
        mutate(ny_smit = Ny_Smit + IE_Smit,
               date = ymd(Dagsetning),
               total_deaths = cumsum(new_deaths),
               country = "Iceland") %>% 
        select(country, date, total_cases = Smit_Samtals, new_cases = ny_smit, new_deaths, total_deaths)
}



    





