Make_Landlaeknir_Data <- function(email = "bgautijonsson@gmail.com") {
    library(dplyr)
    library(tidyr)
    library(readr)
    library(lubridate)
    library(googlesheets4)
    
    sheets_auth(email = email)
    
    sheets_read("https://docs.google.com/spreadsheets/d/1ODqwILD6yBfE5mgKmI4mE-t-wzBDaw8l27gERUiAMgo/edit", sheet = "Smit") %>% 
        mutate(ny_smit = Ny_Smit + IE_Smit,
               date = as_date(Dagsetning),
               new_deaths = 0,
               total_deaths = 0,
               country = "Iceland") %>% 
        select(country, date, total_cases = Smit_Samtals, new_cases = ny_smit, new_deaths, total_deaths)
}



    





