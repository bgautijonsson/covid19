library(broom)
library(scales)
library(tidybayes)
library(tidyverse)
library(rstan)
library(lubridate)
library(tidyverse)
library(cowplot)
library(ggtext)
library(here)
wd <- here()
if (!str_detect(here(), "R_e")) wd <- here("Stan", "R_e")


theme_set(theme_bw(base_size = 14) + 
              panel_border(colour = "grey30", size = 1) + 
              background_grid(color.major = "grey90", 
                              color.minor = "grey95", 
                              minor = "xy", major = "xy"))

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
           active_cases = lag(active_cases, 1, default = 0)) %>% 
    select(date, dI_dt = new_cases, total_cases, S,  I = active_cases, R = total_removed, N) %>% 
    filter(date >= ymd("2020-03-01"))

tidyMCMC(m, pars = c("sigma_beta", "phi_inv_sqrt"), ess = T, rhat = T, conf.int = T)

tidyMCMC(m, pars = "log_beta", ess = T, conf.int = T, rhat = T) %>% 
    mutate(day = row_number(),
           date = ymd("2020-03-01") + day - 1) %>% 
    ggplot(aes(date, estimate, ymin = conf.low, ymax = conf.high)) +
    geom_ribbon(alpha = 0.3) +
    geom_line() +
    geom_hline(yintercept = 1, lty = 2) +
    scale_y_continuous(breaks = pretty_breaks(8)) +
    geom_vline(xintercept = ymd("2020-03-16"))

gather_draws(m, log_beta[day], slope_log_beta[day], r[day], pred_cases[day], n = 100) %>% 
    group_by(day, .variable) %>% 
    mutate(iter = row_number()) %>% 
    select(iter, day, .variable, .value) %>% 
    pivot_wider(names_from = .variable, values_from = .value) %>% 
    group_by(iter) %>% 
    mutate(total_cases = cumsum(pred_cases)) %>% 
    pivot_longer(c(-iter, -day), names_to = ".variable", values_to = ".value") %>%  
    na.omit %>% 
    group_by(date = ymd("2020-03-01") + day - 1 + (.variable == "slope_log_beta"), .variable) %>% 
    summarise(estimate = median(.value),
              lower = quantile(.value, .025),
              upper = quantile(.value, .975)) %>% 
    ungroup %>% 
    mutate(.variable = fct_recode(.variable,
                                  "New~cases" = "pred_cases",
                                  "Total~cases" = "total_cases",
                                  "R[t]" = "r",
                                  "ln(beta)" = "log_beta",
                                  "nabla~ln(beta)" = "slope_log_beta"),
           .variable = fct_relevel(.variable, "New~cases", "Total~cases", "R[t]", "ln(beta)"),
           vline = case_when(.variable == "nabla~ln(beta)" ~ 0,
                             .variable == "R[t]" ~ 1,
                             TRUE ~ NA_real_),
           upper = ifelse(.variable == "R[t]", pmin(9.3, upper), upper)) %>% 
    ggplot(aes(date, estimate, ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.3) +
    geom_line() +
    geom_hline(aes(yintercept = vline), lty = 2) +
    geom_vline(xintercept = ymd("2020-03-16"), lty = 2) +
    geom_vline(xintercept = ymd("2020-03-24"), lty = 2) +
    geom_vline(xintercept = ymd("2020-05-05"), lty = 2) +
    geom_point(data = d %>% 
                   select(date, "New~cases" = dI_dt, "Total~cases" = total_cases) %>% 
                   pivot_longer(c(`New~cases`, `Total~cases`), 
                                names_to = ".variable",
                                values_to = "estimate") %>% 
                   mutate(.variable = as_factor(.variable)),
               aes(x = date, y = estimate), inherit.aes = F) +
    facet_wrap(".variable", scales = "free_y", ncol = 1,
           labeller = label_parsed) +
    scale_x_date(breaks = ymd(c("2020-03-01", 
                                "2020-03-16", "2020-03-24",
                                "2020-04-01", 
                                "2020-05-04")),
                 labels = c(quote("March 1^st"), 
                            quote("March 16^th"), quote("March 24^th"),
                            quote("April 1^st"), 
                            quote("May 4^th"))) +
    scale_y_continuous(expand = expansion(mult = 0.02),
                       breaks = pretty_breaks(7)) +
    theme(axis.title = element_blank(),
          axis.text.x = element_markdown()) +
    background_grid(major = "none", minor = "none") +
    # ggtitle(latex2exp::TeX("Evolution of $\\ln(\\beta)$ and its derivative, and the effect on $R_{t}$ and new diagnoses"),
    #         subtitle = latex2exp::TeX("$\\beta$ is the expected number of infectious contacts per day for an infected individual")) +
    ggsave(paste0(wd, "/Figures/Beta/Log_Beta_Iceland.png"), width = 0.7 * 5, height = 5, scale = 2.5)

