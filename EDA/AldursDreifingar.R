aldur <- sheets_read("https://docs.google.com/spreadsheets/d/1ODqwILD6yBfE5mgKmI4mE-t-wzBDaw8l27gERUiAMgo/edit#gid=2147090221", sheet = "Aldur") %>% 
    slice(1:9) %>% 
    mutate(tilfelli = fj_smit_data,
           p_tilfelli = tilfelli / sum(tilfelli))


aldur %>% 
    select(aldur, p_spitali_ic, p_icu_ic, 
           fj_smit_data, fj_spitali_data, fj_icu_data) %>% 
    mutate(p_spitali_data = fj_spitali_data / fj_smit_data,
           p_icu_data = ifelse(fj_spitali_data == 0, 0, fj_icu_data / fj_spitali_data)) %>% 
    select(aldur, starts_with("p_spitali"), starts_with("p_icu")) %>% 
    pivot_longer(c(-aldur), names_to = c("p", "place", "type"), names_sep = "_") %>% 
    mutate(type = fct_recode(type,
                             "IC" = "ic",
                             "Gögn" = "data"),
           place = fct_relevel(place, "spitali")) %>% 
    ggplot(aes(aldur, value, group = type, col = type)) +
    geom_line() +
    facet_wrap("place") +
    scale_colour_brewer(type = "qual", palette = "Set1") +
    scale_y_continuous(labels = percent) +
    labs(col = " ",
         title = "Dreifing á spítala og ICU í IC, gögnum og blöndu") +
    theme(legend.position = "top",
          axis.title = element_blank())

prior_tilfelli <- tibble(aldur = rep(aldur$aldur, 4),
                         tilfelli_prior = c(rep(10, 9), rep(50, 9), rep(100, 9),
                                            10, 10, 50, 50, 100, 50, 50, 10, 10),
                         tegund = c(rep("10", 9), rep("50", 9), rep("100", 9), rep("Breytilegt", 9)))

aldur %>% 
    select(aldur, p_spitali_ic, p_icu_ic, 
           fj_smit_data, fj_spitali_data, fj_icu_data) %>% 
    mutate(p_spitali_data = fj_spitali_data / fj_smit_data,
           p_icu_data = ifelse(fj_spitali_data == 0, 0, fj_icu_data / fj_spitali_data)) %>% 
    inner_join(prior_tilfelli) %>% 
    mutate(fj_spitali_ic = tilfelli_prior * p_spitali_ic,
           fj_icu_ic = fj_spitali_ic * p_icu_ic) %>% 
    mutate(fj_smit_total = tilfelli_prior + fj_smit_data,
           fj_spitali_total = fj_spitali_ic + fj_spitali_data,
           fj_icu_total = fj_icu_ic + fj_icu_data,
           p_spitali_total = fj_spitali_total / fj_smit_total,
           p_icu_total = fj_icu_total / fj_spitali_total) %>% 
    select(aldur, tilfelli_prior, tegund, starts_with("p_spitali"), starts_with("p_icu")) %>% 
    pivot_longer(c(-aldur, -tilfelli_prior, -tegund), names_to = c("p", "place", "type"), names_sep = "_") %>% 
    mutate(type = fct_recode(type,
                             "IC" = "ic",
                             "Gögn" = "data",
                             "Saman" = "total"),
           place = fct_relevel(place, "spitali")) %>% 
    ggplot(aes(aldur, value, group = type, col = type)) +
    geom_line() +
    facet_grid(tegund ~ place) +
    scale_colour_brewer(type = "qual", palette = "Set1") +
    scale_y_continuous(labels = percent) +
    labs(col = " ",
         title = "Dreifing á spítala og ICU í IC, gögnum og blöndu") +
    theme(legend.position = "top",
          axis.title = element_blank())
