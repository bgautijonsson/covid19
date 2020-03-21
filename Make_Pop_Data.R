make_pop_data <- function() {
    library(rvest);
    library(tidyverse)
    
    url <- "https://en.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)"
    
    d <- read_xml(url) %>% 
        html_table(fill = T) %>% 
        .[[4]] %>% 
        as_tibble %>% 
        select(1, 2, 3, 5) %>% 
        set_names(c("country", "continent", "region", "pop")) %>% 
        mutate(pop = parse_number(pop),
               country = str_replace(country, "\\[.*\\]$", "")) %>% 
        filter(country != "World")
    
    
    return(d)
    
}
