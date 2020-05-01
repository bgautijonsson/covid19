library(scales)

tidyMCMC(m,
         pars = c("mu_death_rate", "mu_detected", "kappa_detected",  "mu_beta", "mu_alpha"), 
         ess = T, rhat = T, conf.int = T)


results1 <- tidyMCMC(m,
         pars = c("country_mu_detected"), 
         ess = T, rhat = T, conf.int = T) %>% 
    mutate(country_id = row_number()) %>% 
    inner_join(d %>% distinct(country, country_id), by = "country_id")


results1 %>% 
    mutate(country = fct_reorder(country, estimate)) %>% 
    ggplot(aes(country, estimate, ymin = conf.low, ymax = conf.high)) +
    geom_linerange() +
    geom_point() +
    scale_y_log10() +
    coord_flip() 


results2 <- tidyMCMC(m,
                    pars = c("death_rate"), 
                    ess = T, rhat = T, conf.int = T) %>% 
    mutate(country_id = row_number()) %>% 
    inner_join(d %>% distinct(country, country_id), by = "country_id")


results2 %>% 
    mutate(country = fct_reorder(country, estimate)) %>% 
    ggplot(aes(country, estimate, ymin = conf.low, ymax = conf.high)) +
    geom_linerange() +
    geom_point() +
    scale_y_log10(labels = percent) +
    coord_flip() 


results1 %>% 
    select(country, detected = estimate) %>% 
    inner_join(
        results2 %>% 
            select(country, death_rate = estimate)
    ) %>% 
    ggplot(aes(detected, death_rate)) +
    geom_point()

d %>% 
    group_by(country) %>% 
    filter(days == max(days)) %>% View
    ungroup %>% 
    select(country, country_id, days, total_cases, total_deaths) %>% 
    mutate(death_rate = total_deaths / total_cases) %>% 
    inner_join(results2) %>% 
    select(country, total_cases, total_deaths, days, death_rate, estimate) %>% 
    print(n = 80)


d %>% 
    group_by(country, country_id) %>% 
    summarise(pop = unique(pop),
              total_deaths = max(total_deaths),
              total_cases = max(total_cases)) %>% 
    mutate(death_rate = total_deaths / total_cases,
           est_cases = total_deaths / 0.0004) %>% 
    print(n = 80)
