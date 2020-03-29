library(shiny); library(dplyr); library(tidyr);
library(ggplot2); library(readr); library(cowplot); 
library(lubridate); 
library(scales);
library(slider)
library(gganimate)
library(plotly)
theme_set(theme_classic(base_size = 12) + 
              background_grid(color.major = "grey90", 
                              color.minor = "grey95", 
                              minor = "xy", major = "xy") +
              theme(legend.position = "none"))

d <- read_csv("Input/ECDC_Data.csv")

p <- d %>% 
    arrange(country, date) %>% 
    group_by(country) %>% 
    mutate(weekly_cases = slide_index_dbl(new_cases, date, sum, .before = 6)) %>% 
    ungroup %>% 
    filter(total_cases > 0) %>% 
    filter(country %in% c("Italy", "United States", "Spain", "China",
                          "Iceland", "South Korea", "Norway", "Denmark"),
           date >= ymd("2020-01-26")) %>% 
    ggplot(aes(total_cases, weekly_cases, 
               group = country,
               col = country == "Iceland",
               size = country == "Iceland",
               alpha = country == "Iceland")) +
    geom_abline(intercept = 0, slope = 1, lty = 2, size = 2,
                col = "grey", alpha = 0.5) +
    geom_line() +
    scale_x_log10(breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000,
                             10000, 30000, 100000, 300000, 1e6),
                  labels = label_number(accuracy = 1, big.mark = ".")) +
    scale_y_log10(breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000,
                             10000, 30000, 100000, 300000),
                  labels = label_number(accuracy = 1, big.mark = ".")) +
    scale_colour_manual(values = c("black", "blue")) +
    scale_size_manual(values = c(1, 2)) +
    scale_alpha_manual(values = c(0.5, 1)) +
    coord_cartesian(xlim = c(1, 1e6),
                    ylim = c(1, 1e5)) +
    labs(title = "Þróun COVID-19 á heimsvísu",
         x = "Heildarfjöldi smita",
         y = "Nýgreind smit (Undanfarna viku)")

ggplotly(p)
