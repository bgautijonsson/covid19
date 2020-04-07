library(shiny); library(dplyr); library(tidyr);
library(ggplot2); library(readr); library(cowplot); 
library(lubridate); 
library(scales);
library(slider)
library(gganimate)
library(ggtext)
library(lme4)
library(purrr)
library(gganimate)
library(forcats)
library(stringr)
library(patchwork)
theme_set(theme_classic(base_size = 12) + 
              background_grid(color.major = "grey90", 
                              color.minor = "grey95", 
                              minor = "xy", major = "xy") +
              theme(legend.position = "none"))

d <- read_csv("Input/ECDC_Data.csv") %>% 
    filter(continent == "Europe", total_cases > 0, country != "San Marino", date <= ymd("2020-04-01")) %>% 
    group_by(country) %>% 
    filter(any(date == ymd("2020-03-01"))) %>% 
    ungroup


get_rates <- function(start_date) {
    end_date <- start_date + 4
    
    m <- d %>%
        arrange(country, date) %>% 
        filter(date >= start_date, 
               date <= end_date) %>% 
        mutate(days = as.numeric(date - start_date)) %>% 
        lmer(log(case_rate) ~ days + (days | country), data = ., control = lmerControl(optimizer = "bobyqa"))
    
    evo <- exp(coef(m)[[1]][, 2, drop = F]) - 1
    
    tibble(start_date = start_date, 
           rate = list(tibble(country = rownames(evo),
                              rate = evo[, 1])))
}

dates <- ymd("2020-03-01") + seq_len(26)

results <- map_df(dates, get_rates)


results %>% 
    unnest(rate) %>%
    mutate(plot_lab = str_c(start_date, "_", country),
           plot_lab = fct_reorder(plot_lab, rate)) %>% 
    ggplot(aes(country, rate, col = country == "Iceland")) +
    geom_point(show.legend = F) +
    geom_segment(aes(xend = country, yend = 0), show.legend = F) +
    scale_y_continuous(labels = percent, 
                       breaks = pretty_breaks(5),
                       expand = expansion(mult = 0.02)) +
    scale_colour_manual(values = c("black", "blue")) +
    scale_x_discrete(labels = function(x) str_replace(x, "^.+_", "")) +
    coord_flip(ylim = c(0, 0.6)) +
    labs(title = "Dagleg aukning á tíðni tilfella (per 1000 íbúa)",
         subtitle = "Tímabil: {frame_time} til {frame_time + 4}") +
    theme(axis.title = element_blank(),
          text = element_text(size = 12)) +
    background_grid(major = "none", minor = "none") +
    transition_time(start_date) +
    ease_aes()

anim_save("rate_evolution.gif")



p1 <- results %>% 
    unnest(rate) %>% 
    ggplot(aes(start_date, rate, group = country, col = country == "Iceland")) +
    geom_line() +
    geom_vline(xintercept = ymd("2020-03-03"), lty = 2) +
    geom_vline(xintercept = ymd("2020-03-17"), lty = 2) +
    scale_colour_manual(values = c("grey","blue")) +
    scale_y_continuous(labels = percent) +
    scale_x_date(breaks = ymd(c("2020-03-03", "2020-03-10", "2020-03-17", "2020-03-24")),
                 date_labels = "%d. %B", expand = expansion(mult = c(0.03, 0.035))) +
    labs(y = "Daily % increase per capita") +
    theme(axis.title.x = element_blank())




m <- d %>%
    arrange(country, date) %>% 
    filter(date >= ymd("2020-03-03"), 
           date <= ymd("2020-03-17")) %>% 
    mutate(days = as.numeric(date - ymd("2020-03-03"))) %>% 
    lmer(log(case_rate) ~ days + (days | country), data = ., control = lmerControl(optimizer = "bobyqa"))

evo1 <- exp(coef(m)[[1]][, 2, drop = F]) - 1

p2 <- tibble(start_date = ymd("2020-03-03"), 
       rate = list(tibble(country = rownames(evo),
                          rate = evo1[, 1]))) %>% 
    unnest(rate) %>% 
    mutate(country = fct_reorder(country, rate)) %>% 
    ggplot(aes(country, rate, col = country == "Iceland")) +
    geom_point(show.legend = F) +
    geom_segment(aes(xend = country, yend = 0), show.legend = F) +
    scale_y_continuous(labels = function(x) percent(x, accuracy = 1), 
                       breaks = pretty_breaks(5),
                       expand = expansion(mult = 0.02)) +
    scale_colour_manual(values = c("black", "blue")) +
    coord_flip(ylim = c(0, 0.56)) +
    theme(axis.title = element_blank(),
          text = element_text(size = 11)) +
    background_grid(major = "none", minor = "none")

m2 <- d %>%
    arrange(country, date) %>% 
    filter(date >= ymd("2020-03-17")) %>% 
    mutate(days = as.numeric(date - ymd("2020-03-17"))) %>% 
    lmer(log(case_rate) ~ days + (days | country), data = ., control = lmerControl(optimizer = "bobyqa"))

evo2 <- exp(coef(m2)[[1]][, 2, drop = F]) - 1


p3 <- tibble(start_date = ymd("2020-03-03"), 
             rate = list(tibble(country = rownames(evo),
                                rate = evo2[, 1],
                                rate_old = evo1[, 1]))) %>% 
    unnest(rate) %>% 
    mutate(country = fct_reorder(country, rate_old)) %>% 
    ggplot(aes(country, rate, col = country == "Iceland")) +
    geom_point(show.legend = F) +
    geom_segment(aes(xend = country, yend = 0), show.legend = F) +
    scale_y_continuous(labels = function(x) percent(x, accuracy = 1), 
                       breaks = pretty_breaks(5),
                       expand = expansion(mult = 0.02)) +
    scale_colour_manual(values = c("black", "blue")) +
    coord_flip(ylim = c(0, 0.56)) +
    theme(axis.title = element_blank(),
          text = element_text(size = 11)) +
    background_grid(major = "none", minor = "none")


plot_grid(p1, plot_grid(p2, p3, nrow = 1, rel_widths = c(0.95, 1), scale = 0.95, labels = list("ii", "iii")), 
          nrow = 2, labels = list("i", ""), rel_heights = c(1, 0.8)) +
    ggsave("lancet_plot.png", scale = 0.9)



