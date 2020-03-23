Make_Landlaeknir_Data <- function(email = "bgautijonsson@gmail.com") {
    library(dplyr)
    library(tidyr)
    library(readr)
    library(lubridate)
    library(googlesheets4)
    
    sheets_auth(email = email)
    
    sheets_read("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA", sheet = "Smit") %>% 
        mutate(ny_smit = Ny_Smit + IE_Smit,
               date = as_date(Dagsetning),
               new_deaths = 0,
               total_deaths = 0,
               country = "Iceland") %>% 
        select(country, date, total_cases = Smit_Samtals, new_cases = ny_smit, new_deaths, total_deaths) %>% 
        filter(date >= ymd("2020-03-09"))
}



    





