Make_Landlaeknir_Data <- function() {
    library(dplyr)
    library(tidyr)
    library(readr)
    library(lubridate)
    
    
    
    read_csv("https://docs.google.com/spreadsheets/d/1ODqwILD6yBfE5mgKmI4mE-t-wzBDaw8l27gERUiAMgo/export?format=csv&gid=0") %>%
        rename(new_deaths = "Dauðsföll") %>% 
        mutate(ny_smit = Ny_Smit + IE_Smit,
               date = ymd(Dagsetning),
               total_deaths = cumsum(new_deaths),
               country = "Iceland") %>% 
        select(country, date, total_cases = Smit_Samtals, new_cases = ny_smit, new_deaths, total_deaths) 
}



    





