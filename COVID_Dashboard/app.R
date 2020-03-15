library(shiny); library(dplyr); library(tidyr);
library(ggplot2); library(readr); library(cowplot); 
library(googlesheets4); library(shinythemes);
library(lubridate); library(kableExtra); library(scales)
library(emmeans); library(broom); library(forcats);
library(lme4); library(stringr); library(plotly);
library(mgcv)
theme_set(theme_classic(base_size = 12) + 
              background_grid(color.major = "grey90", 
                              color.minor = "grey95", 
                              minor = "xy", major = "xy") +
              theme(legend.position = "none"))
d_smit <- read_csv("Data/smit.csv") %>% 
    filter(tegund != "Annad") %>% 
    mutate(tegund = fct_recode(tegund, "Erlend" = "Erlendis", "Innlend" = "Innanlands"),
           date = ymd(dags))
d_sottkvi <- read_csv("Data/sottkvi.csv") %>% 
    mutate(date = ymd(dags))
d <- d_smit %>% 
    filter(tegund == "Samtals")
euro_smit <- read_csv("Data/JHU_Data.csv")

Sys.setlocale("LC_TIME", "is_IS")

ui <- navbarPage(
    title = "Ísland og COVID19", 
    theme = shinytheme(theme = "flatly"),
    ##### Smitaþróun #####
    tabPanel(title = "Smitaþróun",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "countries", 
                                 label = "Lönd", 
                                 choices = unique(euro_smit$country),
                                 multiple = T, selectize = T,
                                 selected = c("Danmörk", "Noregur", "Finnland", "Svíþjóð", "Ísland")),
                     uiOutput("countries_to_choose"),
                     selectInput(inputId = "x_var",
                                 label = "Sýna þróun eftir",
                                 choices = c("Dagsetningu", "Dögum frá öðru smiti"),
                                 multiple = F,
                                 selected = "Dagsetningu"),
                     div(actionButton(inputId = "gobutton1", label = "Birta", width = "120px"), 
                         class = "center", align = "middle"),
                     h6("Höfundur:"),
                     h6("Brynjófur Gauti Jónsson,"),
                     h6("Tölfræðiráðgjöf Heilbrigðisvísindasviðs Háskóla Íslands"),
                     div(img(src = "hi_hvs_horiz.png", width = "80%"), align = "middle", class = "center"),
                     h6("Byggt á daglega uppfærðum gögnum John Hopkins University sem fást í hlekki að neðan"),
                     a("Hlekkur á gögn", href = "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data"),
                     h6("Síðast uppfært 15. mars 2020 klukkan 00:00")
                     
                 ),
                 
                 mainPanel(
                     tabsetPanel(type = "tabs",
                                 tabPanel("Fjöldi",
                                          plotlyOutput("euro_plot_n", height = "600px")),
                                 tabPanel("Tíðni",
                                          #textOutput("euro_plot_p_priortext"), br(),
                                          plotlyOutput("euro_plot_p", height = "600px")),
                                 tabPanel("Samanburður",
                                          plotlyOutput("lmer_plot", height = "600px"))
                     )
                 )
             )
    ),
    ##### Töfluyfirlit #####
    tabPanel(title = "Töfluyfirlit",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "countries_table", 
                                 label = "Lönd", 
                                 choices = unique(euro_smit$country),
                                 multiple = T, selectize = T,
                                 selected = c("Danmörk", "Noregur", "Finnland", "Svíþjóð", "Ísland")),
                     uiOutput("countries_to_table"),
                     selectInput(inputId = "sort_col", label = "Raða eftir",
                                 choices = c("Landi", "Tilfellum", "Tíðni", "Fyrsta smiti"), 
                                 selected = "Landi"),
                     div(actionButton(inputId = "gobutton2", label = "Birta", width = "120px"), 
                         class = "center", align = "middle"),
                     h6("Höfundur:"),
                     h6("Brynjófur Gauti Jónsson,"),
                     h6("Tölfræðiráðgjöf Heilbrigðisvísindasviðs Háskóla Íslands"),
                     div(img(src = "hi_hvs_horiz.png", width = "80%"), align = "middle", class = "center"),
                     h6("Byggt á daglega uppfærðum gögnum John Hopkins University sem fást í hlekk að neðan"),
                     a("Hlekkur á gögn", href = "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data"),
                     h6("Síðast uppfært 15. mars 2020 klukkan 00:00")
                 ),
                 mainPanel(
                     tableOutput("summary_table")
                 )
             )),
    ##### Ísland #####
    tabPanel(title = "Ísland",
             sidebarLayout(
                 sidebarPanel(
                     numericInput(inputId = "forspa_dagar",
                                  label = "Lengd forspár (í dögum)",
                                  min = 1, max = 21, value = 7, step = 1),
                     numericInput(inputId = "confidence",
                                  label = "Öryggi", 
                                  min = 0.9, max = 0.99,
                                  value = 0.95, step = 0.01),
                     selectInput(inputId = "island_x_var",
                                 label = "Sýna þróun eftir",
                                 choices = c("Dagsetningu", "Dögum frá fyrsta smiti"),
                                 multiple = F,
                                 selected = "Dagsetningu"),
                     div(actionButton(inputId = "gobutton3", label = "Birta", width = "120px"), 
                         class = "center", align = "middle"),
                     h6("Höfundur:"),
                     h6("Brynjófur Gauti Jónsson,"),
                     h6("Tölfræðiráðgjöf Heilbrigðisvísindasviðs"),
                     h6("Háskóla Íslands"),
                     img(src = "hi_hvs_horiz.png", width = "80%", align = "middle", class = "center"),
                     h6("Byggt á gögnum frá Embætti Landlæknis sem fást á covid.is og í hlekki að neðan"),
                     a("Hlekkur á gögn", href = "https://www.landlaeknir.is/um-embaettid/greinar/grein/item38863/Stoduskyrslur---Ovissustig-vegna-koronaveiru-(2019-nCoV)"),
                     h6("Síðast uppfært 15. mars 2020 klukkan 00:00")
                     
                 ),
                 
                 mainPanel(
                     tabsetPanel(type = "tabs",
                                 tabPanel("Smit",
                                          plotOutput("smit_plot", height = "600px"))
                     )
                 )
             )
    )
)

server <- function(input, output) {
    ##### Europe Counts #####
    
    output$countries_to_choose <- renderUI({
        req(input$countries)
        if ("Ísland" %in% input$countries) {
            selectInput(inputId = "chosen", 
                        label = "Samanburðarland", 
                        choices = input$countries,
                        selectize = T,
                        selected =  "Ísland")
        } else {
            selectInput(inputId = "chosen", 
                        label = "Samanburðarland", 
                        choices = input$countries,
                        selectize = T,
                        selected =  input$countries[1])
        }
    })
    
    euro_plot_n <- eventReactive(input$gobutton1, {
        req(input$countries, input$chosen)
        if (input$x_var == "Dögum frá öðru smiti") {
            p <- euro_smit %>% 
                filter(country %in% c(input$countries)) %>% 
                mutate(days_from_first = as.numeric(date - min(date)),
                       chosen = ifelse(country == input$chosen, "chosen", "other")) %>% 
                ggplot(aes(days, cum_cases, 
                           col = chosen, 
                           size = chosen,
                           alpha = chosen,
                           group = country)) +
                geom_line(show.legend = F) +
                scale_y_log10() +
                scale_colour_manual(values = c("Blue", "Black")) +
                scale_size_manual(values = c(1.2, 0.8)) + 
                scale_alpha_manual(values = c(1, 0.3)) +
                labs(title = "Þróun fjölda smitaðra á Íslandi og annars staðar í Evrópu",
                     subtitle = "Sýnd eftir dögum frá öðru smiti hvers lands",
                     x = "Dagar",
                     y = "Fjöldi smitaðra") 
            ggplotly(p, tooltip = c("x", "y", "country"))
        } else {
            p <- euro_smit %>% 
                filter(country %in% c(input$countries)) %>% 
                mutate(days_from_first = as.numeric(date - min(date)),
                       chosen = ifelse(country == input$chosen, "chosen", "other")) %>% 
                ggplot(aes(date, cum_cases, 
                           col = chosen, 
                           size = chosen,
                           alpha = chosen,
                           group = country)) +
                geom_line(show.legend = F) +
                scale_x_date(date_labels = "%d. %B", breaks = pretty_breaks(8)) +
                scale_y_log10() +
                scale_colour_manual(values = c("Blue", "Black")) +
                scale_size_manual(values = c(1.2, 0.8)) + 
                scale_alpha_manual(values = c(1, 0.3)) +
                labs(title = "Þróun fjölda smitaðra á Íslandi og annars staðar í Evrópu",
                     subtitle = "Sýnd eftir dagsetningu",
                     y = "Fjöldi smitaðra") +
                theme(axis.title.x = element_blank())
            ggplotly(p, tooltip = c("x", "y", "country"))
        }})
    
    
    output$euro_plot_n <- renderPlotly({
        euro_plot_n()
    })
    
    output$euro_plot_n_info <- renderText({
        data <- nearPoints(euro_smit %>% filter(country %in% c(input$countries)), 
                           input$euro_plot_n_click, threshold = 40, addDist = T,
                           xvar = "days", yvar = "cum_cases")
        data <- data[which.min(data$dist_), ]
        if (length(data$country) == 0) return ("Smelltu á línu til að sjá hvaða landi hún tilheyrir")
        today <- Sys.Date() %>% ymd
        timi <- as.numeric(today - data$date)
        out <- paste0(data$country, ": Fjöldi var ", round(data$cum_cases, 3), " fyrir ", timi, " dögum")
        return(out)
    })
    
    ##### Europe Rates #####
    
    euro_plot_p <- eventReactive(input$gobutton1, {
        req(input$countries, input$chosen)
        
        if (input$x_var == "Dögum frá öðru smiti") {
            p <- euro_smit %>% 
                filter(country %in% c(input$countries)) %>% 
                mutate(days_from_first = as.numeric(date - min(date)),
                       chosen = ifelse(country == input$chosen, "chosen", "other")) %>% 
                ggplot(aes(days, case_rate, 
                           col = chosen, 
                           size = chosen,
                           alpha = chosen,
                           group = country)) +
                geom_line(show.legend = F) +
                scale_y_log10() +
                scale_colour_manual(values = c("Blue", "Black")) +
                scale_size_manual(values = c(1.2, 0.8)) + 
                scale_alpha_manual(values = c(1, 0.3)) +
                labs(title = "Þróun tíðni smitaðra á Íslandi og annars staðar í Evrópu",
                     subtitle = "Sýnd sem fjöldi per 1000 íbúar eftir dögum frá öðru smiti hvers lands",
                     x = "Dagar",
                     y = "Fjöldi smitaðra (per 1000 íbúar)")
            ggplotly(p, tooltip = c("x", "y", "country"))
        } else {
            p <- euro_smit %>% 
                filter(country %in% c(input$countries)) %>% 
                mutate(days_from_first = as.numeric(date - min(date)),
                       chosen = ifelse(country == input$chosen, "chosen", "other")) %>% 
                ggplot(aes(date, case_rate, 
                           col = chosen, 
                           size = chosen,
                           alpha = chosen,
                           group = country)) +
                geom_line(show.legend = F) +
                scale_y_log10() +
                scale_x_date(date_labels = "%d. %B", breaks = pretty_breaks(8)) +
                scale_colour_manual(values = c("Blue", "Black")) +
                scale_size_manual(values = c(1.2, 0.8)) + 
                scale_alpha_manual(values = c(1, 0.3)) +
                labs(title = "Þróun tíðni smitaðra á Íslandi og annars staðar í Evrópu",
                     subtitle = "Sýnd sem fjöldi per 1000 íbúar eftir dögum frá öðru smiti hvers lands",
                     y = "Fjöldi smitaðra (per 1000 íbúar)") +
                theme(axis.title.x = element_blank())
            ggplotly(p, tooltip = c("x", "y", "country"))
        }
    })
    
    output$euro_plot_p <- renderPlotly({
        euro_plot_p()
    })
    
    output$euro_plot_p_info <- renderText({
        data <- nearPoints(euro_smit %>% filter(country %in% c(input$countries)), 
                           input$euro_plot_p_click, threshold = 40, addDist = T,
                           xvar = "days", yvar = "case_rate")
        data <- data[which.min(data$dist_), ]
        if (length(data$country) == 0) return ("Smelltu á línu til að sjá hvaða landi hún tilheyrir")
        today <- Sys.Date() %>% ymd
        timi <- as.numeric(today - data$date)
        
        out <- paste0(data$country, ": Tíðni var ", round(data$case_rate, 3), " per 1000 íbúa fyrir ", timi, " dögum")
        return(out)
    })
    
    ##### Europe LMER #####
    
    output$lmer_plot <- renderPlotly({
        d <- euro_smit %>% 
            filter(date >= ymd("2020-03-02")) %>% 
            mutate(days = as.numeric(date - min(date)))
        
        
        m <- lmer(log(case_rate) ~ days + (days | country), data = d,
                  control = lmerControl(optimizer = "bobyqa"))
        
        evo <- coef(m)[[1]][, 2, drop = F] %>%  exp
        evo <- evo[order(evo), , drop = F]
        
        which_iceland <- which(rownames(evo) == "Ísland")
        evo_iceland <- evo[which_iceland, ]
        evo_iceland <- round(evo_iceland - 1, 3)
        
        p <- tibble(country = rownames(evo),
                    change = evo[, 1]) %>% 
            mutate(country = ifelse(str_detect(country, "Bosnía"), "Bosnía & H", country),
                   country = ifelse(str_detect(country, "Norður-M"), "N-Makedónía", country),
                   country = fct_reorder(country, change),
                   col = case_when(country == "Ísland" ~ "blue",
                                   country == "Danmörk" ~ "red",
                                   TRUE ~ "grey")) %>% 
            ggplot(aes(country, change - 1)) +
            geom_point(aes(col = col), show.legend = F) +
            geom_segment(aes(xend = country, yend = 0, col = col), show.legend = F) +
            geom_hline(yintercept = exp(summary(m)$coefficients[2, 1]) - 1, lty = 2) +
            # Meðalaukning
            geom_text(data = tibble(), 
                      aes(label = "Meðalaukning\ní Evrópu        ", 
                          x = 15, y = 0.41),
                      size = 4) +
            # Danmörk
            geom_text(data = tibble(), 
                      aes(label = "       Danmörk upplifði\nmikla aukningu tilfella", 
                          x = 35, y = 0.58),
                      col = "red",
                      size = 4) +
            # Label Iceland
            geom_text(data = tibble(),
                      aes(label = percent(evo_iceland),
                          x = which_iceland,
                          y = evo_iceland + 0.02),
                      col = "blue", size = 4) +
            scale_y_continuous(labels = percent, 
                               breaks = pretty_breaks(5),
                               expand = expansion(mult = 0.02)) +
            scale_colour_manual(values = c("blue", "grey", "red")) +
            coord_flip() +
            labs(title = "Dagleg aukning á tíðni tilfella (per 1000 íbúa) eftir 2. mars",
                 subtitle = "Aukning á Íslandi er rétt í kringum meðaltal") +
            theme(axis.title = element_blank(),
                  text = element_text(size = 12)) +
            background_grid(major = "none", minor = "none")
        
        ggplotly(p, tooltip = c("x", "y", "country"))
    })
    
    ##### Europe Table #####
    
    output$countries_to_table <- renderUI({
        req(input$countries)
        if ("Ísland" %in% input$countries) {
            selectInput(inputId = "chosen_table", 
                        label = "Samanburðarland", 
                        choices = input$countries_table,
                        selectize = T,
                        selected =  "Ísland")
        } else {
            selectInput(inputId = "chosen_table", 
                        label = "Samanburðarland", 
                        choices = input$countries_table,
                        selectize = T,
                        selected =  input$countries_table[1])
        }
    })
    
    summary_table <- eventReactive(input$gobutton2, {
        req(input$sort_col)
        cols <- list("Landi" = "country", 
                     "Tilfellum" = "cases", 
                     "Tíðni" = "incidence", 
                     "Fyrsta smiti" = "days")
        out <- euro_smit %>% 
            filter(country %in% input$countries_table) %>% 
            group_by(country) %>% 
            summarise(cases = max(cum_cases),
                      incidence = round(cases / max(pop) * 1000, 4),
                      days = max(days),
                      first = min(date)) %>% 
            arrange(desc(!!sym(cols[[input$sort_col]])))
        
        if (input$sort_col == "Landi") {
            out <- out %>% 
                arrange(!!sym(cols[[input$sort_col]]))
        } else {
            out <- out %>% 
                arrange(desc(!!sym(cols[[input$sort_col]])))
        }
        names(out) <- c("Land", "Tilfelli", "Tíðni (per 1000)", "Dagar frá öðru smiti", "Dagsetning annars smits")
        
        icel <- which(out$Land == input$chosen_table)
        
        out %>% 
            kable(format = "html", align = c("l", rep("c", ncol(.) - 1))) %>% 
            kable_styling(bootstrap_options = c("striped", "hover")) %>% 
            row_spec(icel, bold = T, background = "#b3cde3")
    })
    
    output$summary_table <- function() {
        summary_table()
    }
    
    ##### Iceland Smit #####
    
    get_iceland_preds <- eventReactive(input$gobutton3, {
        
        alpha <- 1 - input$confidence
        
        
        dat_days <- max(d$dagar)
        
        m <- gam(fjoldi ~ s(dagar, bs = "cr", k = 4), 
                 knots = list(dagar = c(0, 2,  6, 10)),
                 data = d, family = quasipoisson)
        
        pred_dat <- emmeans(m, ~ dagar, 
                            at = list(dagar = seq(0, dat_days + input$forspa_dagar)), 
                            type = "response", data = d) %>% 
            tidy(level = input$confidence) %>% 
            mutate(lower = qpois(alpha / 2, conf.low),
                   upper = qpois(1 - alpha/2, conf.high),
                   fjoldi = rate) %>% 
            mutate(first_dags = min(d$dags),
                   dags = first_dags + dagar,
                   tegund = "Samtals")
        
        
        
        text_dat <- pred_dat %>%
            group_by(tegund) %>%
            summarise(dags = max(dags),
                      dagar = max(dagar),
                      fjoldi = max(fjoldi),
                      upper = max(upper) %>% round(-1),
                      lower = max(lower) %>% round(-1)) %>%
            mutate(texti = str_c(lower, " - ", upper, "\ntilfelli"),
                   dags = dags + 1,
                   fjoldi = case_when(
                       tegund == "Samtals" ~ fjoldi * 1.1,
                       tegund == "Erlend" ~ fjoldi * 0.95,
                       TRUE ~ fjoldi
                   ),
                   texti = ifelse(tegund == "Samtals", texti, NA))
        
        if (input$island_x_var == "Dagsetningu") {
            d_smit %>% 
                mutate(x = ifelse(dags == ymd("2020-03-08"), "2020-03-08", NA) %>% ymd,
                       y = ifelse(dags == ymd("2020-03-08"), fjoldi, NA),
                       y = case_when(tegund == "Innlend" ~ y - 4,
                                     tegund == "Erlend" ~ y - 8,
                                     TRUE ~ y + 25),
                       type = "obs") %>% 
                ggplot(aes(dags, fjoldi, col = tegund)) +
                geom_ribbon(data = pred_dat, aes(ymin = lower, ymax = upper), fill = "#4daf4a", 
                            col = NA, alpha = 0.2, show.legend = F) +
                geom_line(data = pred_dat, aes(dags, fjoldi), show.legend = F, lty = 2) +
                geom_line(show.legend = F, size = 1.2) +
                geom_text(aes(label =  tegund, x = x, y = y), show.legend = F, size = 6) +
                geom_text(data = text_dat, aes(x = dags - 1, 
                                               y = lower * 0.6, label = texti),
                          show.legend = F, size = 5) +
                geom_point(data = text_dat, aes(x = dags + 1), col = NA) +
                scale_y_log10(breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000, 10000, 30000, 100000, 300000)) +
                scale_x_date(date_labels = "%d. %B", breaks = pretty_breaks(8)) +
                scale_colour_brewer(type = "qual", palette = "Set1") +
                scale_fill_brewer(type = "qual", palette = "Set1") +
                labs(y = "Fjöldi smitaðra",
                     title = "Fjöldi smitaðra á Íslandi auk spábila",
                     subtitle = "Fyrst eru aðallega smit erlendis frá, en með tímanum verða þau aðallega innanlands") +
                theme(axis.title.x = element_blank(),
                      text = element_text(size = 14))
        } else {
            
            d_smit %>% 
                mutate(x = ifelse(dags == ymd("2020-03-08"), as.numeric(ymd("2020-03-08") - min(dags)), NA),
                       y = ifelse(dags == ymd("2020-03-08"), fjoldi, NA),
                       y = case_when(tegund == "Innlend" ~ y - 2,
                                     tegund == "Erlend" ~ y - 5,
                                     TRUE ~ y + 18),
                       type = "obs") %>% 
                ggplot(aes(dagar, fjoldi, col = tegund)) +
                geom_ribbon(data = pred_dat, aes(ymin = lower, ymax = upper),  fill = "#4daf4a",
                            col = NA, alpha = 0.2, show.legend = F) +
                geom_line(data = pred_dat, aes(dagar, fjoldi), show.legend = F, lty = 2) +
                geom_line(show.legend = F, size = 1.2) +
                geom_text(aes(label =  tegund, x = x + 0.5, y = y), show.legend = F, size = 6) +
                geom_text(data = text_dat, aes(x = dagar - 1, 
                                               y = lower * 0.6, label = texti),
                          show.legend = F, size = 5) +
                scale_y_log10(breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000, 10000, 30000, 100000, 300000)) +
                scale_colour_brewer(type = "qual", palette = "Set1") +
                scale_fill_brewer(type = "qual", palette = "Set1") +
                labs(y = "Fjöldi smitaðra",
                     x = "Dagar frá fyrsta smiti",
                     title = "Fjöldi smitaðra á Íslandi auk spábila",
                     subtitle = "Fyrst eru aðallega smit erlendis frá, en með tímanum verða þau aðallega innanlands") +
                theme(text = element_text(size = 14))
        }
        
    })
    
    output$smit_plot <- renderPlot({
        get_iceland_preds()
    })
}

shinyApp(ui = ui, server = server)
