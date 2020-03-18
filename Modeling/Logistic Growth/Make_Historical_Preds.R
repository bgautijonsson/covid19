library(tidyverse); library(cowplot); library(kableExtra); library(scales); library(gganimate)
library(lubridate); library(emmeans); library(broom); library(propagate)
theme_set(theme_classic(base_size = 12) + 
              background_grid(color.major = "grey90", 
                              color.minor = "grey95", 
                              minor = "xy", major = "xy") +
              theme(legend.position = "none"))
select <- dplyr::select


d <- read_csv("smit.csv") %>% 
    filter(tegund == "Samtals",
           fjoldi > 0)



make_hist_preds <- function(index) {
    model_d <- d %>% slice(seq_len(index))
    ice.g <- nls(fjoldi ~ SSlogis(dagar, phi1, phi2, phi3), data = model_d)
    preds_obj <- predictNLS(ice.g, newdata = tibble(dagar = seq(0, 60)), interval = "prediction")
    preds <- preds_obj$summary %>% 
        as_tibble %>%
        select(pred = "Sim.Mean", upper = "Sim.97.5%") %>% 
        mutate(dagar = row_number() - 1, 
               last_day = ymd("2020-02-29") + index - 1)
}

map_df(seq(12, 18), make_hist_preds) %>% 
    write_csv("historical_preds.csv")