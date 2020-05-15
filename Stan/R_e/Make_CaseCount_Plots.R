library(tidyverse)
library(knitr)
library(cowplot)
library(rstan)
library(broom)
library(scales)
library(lubridate)
library(ggtext)
library(here)
library(slider)


theme_set(theme_bw(base_size = 14) + 
              panel_border(colour = "grey30", size = 1) + 
              background_grid(color.major = "grey90", 
                              color.minor = "grey95", 
                              minor = "xy", major = "xy"))
wd <- here()
if (!str_detect(here(), "R_e")) wd <- here("Stan", "R_e")

m <- read_rds(paste0(wd, "/Rt_Model_StateSpace.rds"))

d <- read_csv("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA/export?format=csv&id=1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA&gid=1788393542") %>% 
    select(
        date = "Dagsetning",
        new_cases = "Ny_Smit",
        new_deaths = "Dauðsföll",
        new_recovered = "Batnað", 
        total_cases = "Smit_Samtals",
        total_deaths = "Dauðsföll_Samtals",
        total_recovered = "Batnað_Samtals",
        total_removed = "Removed_Samtals",
        active_cases = "Virk_Smit")

d <- d %>% 
    mutate(date = ymd(date),
           N = 364220,
           S = N - total_cases,
           S = lag(S, 1, default = 0),
           active_cases = lag(active_cases, 1, default = 0),
           weekly_average = slide_index_dbl(new_cases, date, mean, .before = 3, .after = 3)) %>% 
    select(date, dI_dt = new_cases, total_cases, S,  I = active_cases, R = total_removed, N, weekly_average) %>% 
    filter(date >= ymd("2020-02-28"))

lims <- tibble(name = rep(c("R", "New cases", "Total cases"), each = 2),
               type = rep(c("min", "max"), 3),
               lim = c(0, 5,
                       -2, 115,
                       -2, 1900),
               x = ymd("2020-02-28"))

plot_dat_line  <- d %>% 
    select(-S, -I, -R, -N) %>% 
    rename("New cases" = weekly_average, "Total cases" = total_cases) %>% 
    pivot_longer(c("New cases", "Total cases")) %>% 
    bind_rows(
        tidyMCMC(m, pars = "r", conf.int = T) %>% 
            mutate(date = ymd("2020-03-01") + row_number() - 1) %>% 
            mutate(name = quote("R"), value = estimate) %>% 
            select(date, name, value, conf.low, conf.high)
    ) %>% 
    inner_join(lims, by = "name") %>% 
    mutate(name = fct_recode(name, 
                             "R[t]" = "R",
                             "New~cases" = "New cases",
                             "Total~cases" = "Total cases"),
           name = fct_relevel(name, "R[t]", "New~cases", "Total~cases"),
           conf.high = pmin(conf.high, 9.2),
           hline = ifelse(name == "R[t]", 1, NA))

plot_dat_point <- d %>% 
    select(-S, -I, -R, -N) %>% 
    rename("New cases" = dI_dt, "Total cases" = total_cases) %>% 
    pivot_longer(c("New cases", "Total cases")) %>% 
    bind_rows(
        tidyMCMC(m, pars = "r", conf.int = T) %>% 
            mutate(date = ymd("2020-03-01") + row_number() - 1) %>% 
            mutate(name = quote("R"), value = estimate) %>% 
            select(date, name, value, conf.low, conf.high)
    ) %>% 
    inner_join(lims, by = "name") %>% 
    mutate(name = fct_recode(name, 
                             "R[t]" = "R",
                             "New~cases" = "New cases",
                             "Total~cases" = "Total cases"),
           name = fct_relevel(name, "R[t]", "New~cases", "Total~cases"),
           conf.high = pmin(conf.high, 9.2),
           hline = ifelse(name == "R[t]", 1, NA))




plot_dat_line %>% 
    ggplot(aes(date, value)) +
    geom_point(aes(x = x, y = lim), alpha = 0) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3) +
    geom_line() +
    geom_point(data = plot_dat_point %>% filter(name != "R[t]"), alpha = 0.5, size = 1.6) +
    # Fyrra samkomubann
    geom_vline(xintercept = ymd("2020-03-16"), lty = 2) +
    # Seinna samkomubann
    geom_vline(xintercept = ymd("2020-03-24"), lty = 2) +
    # Fyrsta tilslökun
    geom_vline(xintercept = ymd("2020-05-04"), lty = 2) +
    geom_hline(aes(yintercept = hline), lty = 2, col = "grey50") +
    facet_wrap("name", scales = "free_y", ncol = 1, labeller = label_parsed) +
    scale_x_date(breaks = ymd(c("2020-02-28", 
                                "2020-03-16", "2020-03-24",
                                "2020-04-01", 
                                "2020-05-04")),
                 labels = c(quote("February 28^th"), 
                            quote("March 16^th"), quote("March 24^th"),
                            quote("April 1^st"), 
                            quote("May 4^th"))) +
    scale_y_continuous(expand = expansion(mult = 0.02),
                       breaks = pretty_breaks(7)) +
    theme(axis.title = element_blank(),
          axis.text.x = element_markdown()) +
    background_grid(major = "none", minor = "none") +
    ggsave(paste0(wd, "/Figures/Cases/Rt_Cases_Iceland.png"), width = 5, height = 0.6 * 5, scale = 2.5)







# Table
# library(kableExtra)
# options(knitr.kable.NA = '')
# plot_dat %>% 
#     mutate(value = ifelse(name == "R[t]",
#                           paste0(round(value, 2),
#                                  ",",
#                                  round(conf.low, 2),
#                                  ",",
#                                  round(conf.high, 2),
#                                  ","),
#                           value)) %>% 
#     select(date, name, value) %>% 
#     mutate(name = fct_recode(name,
#                              "R" = "R[t]",
#                              "Ný" = "New~cases",
#                              "Uppsöfnuð" = "Total~cases")) %>% 
#     distinct %>% 
#     pivot_wider(names_from = name, values_from = value) %>% 
#     separate(R, into = c("R", "Neðri", "Efri"), sep = ",", convert = T) %>% 
#     rename(Dagsetning = date) %>% 
#     kable(digits = 2,
#           align = c("l", rep("c", ncol(.) - 1))) %>% 
#     kable_styling() %>% 
#     add_header_above(c("", "Greind smit" = 2, "", "95% Mörk" = 2))



