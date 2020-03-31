library(shiny); library(dplyr); library(tidyr);
library(ggplot2); library(readr); library(cowplot); 
library(lubridate); 
library(scales);
library(slider)
library(gganimate)
library(ggtext)
theme_set(theme_classic(base_size = 12) + 
              background_grid(color.major = "grey90", 
                              color.minor = "grey95", 
                              minor = "xy", major = "xy") +
              theme(legend.position = "none"))

d <- read_csv("Input/ECDC_Data.csv")

d %>% 
    arrange(country, date) %>% 
    group_by(country) %>% 
    mutate(weekly_cases = slide_index_dbl(new_cases, date, sum, .before = 6)) %>% 
    ungroup %>% 
    filter(total_cases > 0, case_rate > 0.0009 | country == "China") %>% 
    filter(country %in% c("Italy", "United States", "Spain", "Germany", "France", "China",
                          "Iceland", "South Korea", "Denmark", "Norway", "Sweden"),
           date >= ymd("2020-01-26")) %>% 
    mutate(colour = case_when(
        country == "Iceland" ~ "Iceand",
        country %in% c("Norway", "Denmark", "Sweden") ~ "Nordic",
        country %in% c("South Korea", "China") ~ "Escaped",
        TRUE ~ "Stuck"
    )) %>% 
    ggplot(aes(total_cases, weekly_cases, 
               group = country,
               col = colour,
               size = country == "Iceland",
               alpha = country == "Iceland")) +
    geom_abline(intercept = 0, slope = 1, lty = 2, size = 2,
                col = "grey", alpha = 0.5) +
    geom_line() +
    geom_text(
        data = tribble(
            ~total_cases, ~weekly_cases, ~date, ~label,
            150000,  10000, ymd("2020-02-27"), "Kína",
        ),
        aes(x = total_cases,
            y = weekly_cases,
            label = label),
        inherit.aes = F
    ) +
    geom_text(
        data = tribble(
            ~total_cases, ~weekly_cases, ~date, ~label,
            20000, 2000, ymd("2020-03-14"), "Suður\nKórea"
        ),
        aes(x = total_cases,
            y = weekly_cases,
            label = label),
        inherit.aes = F
    ) +
    # geom_text(
    #     data = tibble(
    #         date = ymd("2020-02-30"),
    #         y = 1e5, 
    #         x = 10, 
    #         label = "Bandaríkin, Þýskaland, Spánn og Ítalia\nhafa ekki náð stjórn á faraldrinum        "
    #     ), 
    #     aes(x = x, y = y, label = label), colour = "red",
    #     inherit.aes = F) +
    # geom_text(data = tibble(
    #     date = ymd("2020-02-30"),
    #     y = 3e4, 
    #     x = 5.5, 
    #     label = "Kína og Suður Kórea              \nbeittu hörðum aðgerðum        "
    # ), 
    # aes(x = x, y = y, label = label), colour = "black",
    # inherit.aes = F) +
    # geom_text(data = tibble(
    #     date = ymd("2020-02-30"),
    #     y = 1e4, 
    #     x = 5.5, 
    #     label = "Danmörk, Noregur og Svíþjóð\n                                    heftu útbreiðslu en héldu svo áfram á sömu leið aftur"
    # ), 
    # aes(x = x, y = y, label = label), colour = "goldenrod",
    # inherit.aes = F) +
    # geom_text(data = tibble(
    #     date = ymd("2020-02-30"),
    #     y = 3e3, 
    #     x = 5.5,  
    #     label = "            Mun Ísland fylgja Suður Kóreu og Kína\neða norðurlöndunum               "
    # ), 
    # aes(x = x, y = y, label = label), colour = "blue",
    # inherit.aes = F) +
    scale_x_log10(breaks = c(1, 10, 1e2, 1e3, 1e4, 1e5, 1e6),
                  labels = label_number(accuracy = 1)) +
    scale_y_log10(breaks = c(1, 10, 1e2, 1e3, 1e4, 1e5, 1e6),
                  labels = label_number(accuracy = 1)) +
    scale_colour_manual(values = c("black", "blue", "goldenrod", "red")) +
    scale_size_manual(values = c(1, 2)) +
    scale_alpha_manual(values = c(0.5, 1)) +
    coord_cartesian(xlim = c(1, 1e6),
                    ylim = c(1, 1e5)) +
    theme(aspect.ratio = 0.621,
          plot.title = element_markdown()) +
    labs(title = "Þróun COVID-19 á <i style='color:blue'>Íslandi</i> og heimsvísu",
         subtitle = "Staðan {frame_along}",
         x = "Heildarfjöldi smita",
         y = "Nýgreind smit (Undanfarna viku)")  +
    transition_reveal(date) -> p

animate(p, renderer = gifski_renderer(loop = F))

anim_save("evolution.gif", p, renderer = gifski_renderer(loop = F))

