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
    mutate(tegund = fct_recode(tegund, "Erlend" = "Erlendis", "Innlend" = "Innanlands"))
d_sottkvi <- read_csv("Data/sottkvi.csv")
d_spitali <- read_csv("Data/spitali.csv") 
d_syni <- read_csv("Data/syni.csv")

d_samtals <- d_smit %>% 
    filter(tegund == "Samtals")

euro_smit <- read_csv("Data/ECDC_Data.csv")
sidast_uppfaert <- "Síðast uppfært 19. mars 2020 klukkan 17:00"

Sys.setlocale("LC_TIME", "is_IS")

ui <- navbarPage(
    title = "Ísland og COVID19", 
    theme = shinytheme(theme = "flatly"),
    ##### Smitaþróun #####
    tabPanel(title = "Smitaþróun",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "continent",
                                 label = "Heimsálfa",
                                 choices = unique(euro_smit$continent),
                                 multiple = T, selectize = T,
                                 selected = "Europe"),
                     uiOutput("countries"),
                     div(actionButton(inputId = "selectall", label = "Velja/Afvelja öll lönd"),
                         class = "center", align = "middle"),
                     uiOutput("countries_to_choose"),
                     selectInput(inputId = "x_var",
                                 label = "Sýna þróun eftir",
                                 choices = c("Dagsetningu", "Dögum síðan skilyrði að neðan var náð"),
                                 multiple = F,
                                 selected = "Dagsetningu"),
                     fluidRow(column(6,
                                     selectInput(inputId = "filtervar",
                                                 label = "Sýna gögn þar sem",
                                                 choices = c("Fjöldi tilvika", "Tíðni tilvika per milljón"),
                                                 multiple = F,
                                                 selected = "Fjöldi tilvika")),
                              column(6,
                                     numericInput(inputId = "filtervalue",
                                                  label = "Er hærri en", 
                                                  min = 0, max = 100, value = 50))),
                     selectInput(inputId = "scale",
                                 label = "Kvarði",
                                 choices = c("Upprunalegur", "Logra"),
                                 multiple = F, selected = "Logra"),
                     div(actionButton(inputId = "gobutton1", label = "Birta", width = "120px"), 
                         class = "center", align = "middle"),
                     h6("Höfundur:"),
                     h6("Brynjófur Gauti Jónsson,"),
                     h6("Tölfræðiráðgjöf Heilbrigðisvísindasviðs Háskóla Íslands"),
                     div(img(src = "hi_hvs_horiz.png", width = "80%"), align = "middle", class = "center"),
                     h6("Byggt á daglega uppfærðum gögnum ECDC sem fást í hlekki að neðan"),
                     a("Hlekkur á gögn", href = "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide"),
                     h6(sidast_uppfaert),
                     a("Allan kóða má nálgast hér", href = "https://github.com/bgautijonsson/covid19")
                     
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
                                 selected = c("Denmark", "Norway", "Finnland", "Sweden", "Iceland")),
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
                     h6("Byggt á daglega uppfærðum gögnum ECDC sem fást í hlekk að neðan"),
                     a("Hlekkur á gögn", href = "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide"),
                     h6(sidast_uppfaert),
                     a("Allan kóða má nálgast hér", href = "https://github.com/bgautijonsson/covid19")
                 ),
                 mainPanel(
                     tableOutput("summary_table")
                 )
             ))
)

server <- function(input, output, session) {
    
    observe({
        if (input$selectall > 0) {
            if (input$selectall %% 2 == 1){
                updateCheckboxGroupInput(session=session, inputId="countries",
                                         selected = euro_smit %>% 
                                             filter(continent %in% input$continent) %>% 
                                             .$country)
                
            }
            else {
                updateCheckboxGroupInput(session=session, 
                                         inputId="countries",
                                         selected = c("Denmark", "Norway", "Finnland", "Sweden", "Iceland"))
                
            }}
    })
    
    ##### Europe Counts #####
    
    output$countries <- renderUI({
        req(input$continent)
        
        if ("Europe" %in% input$continent) {
            selectInput(inputId = "countries", 
                        label = "Lönd", 
                        choices = euro_smit %>% filter(continent %in% input$continent) %>% .$country %>% unique,
                        multiple = T, selectize = T,
                        selected = c("Denmark", "Norway", "Finnland", "Sweden", "Iceland"))
        } else {
            selectInput(inputId = "countries", 
                        label = "Lönd", 
                        choices = euro_smit %>% filter(continent %in% input$continent) %>% .$country %>% unique,
                        multiple = T, selectize = T)
        }
    })
    
    output$countries_to_choose <- renderUI({
        req(input$countries)
        if ("Iceland" %in% input$countries) {
            selectInput(inputId = "chosen", 
                        label = "Samanburðarland", 
                        choices = input$countries,
                        selectize = T,
                        selected =  "Iceland")
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
        if (input$filtervar == "Fjöldi tilvika") {
            filtervar <- "cum_cases"
            filtervalue <- input$filtervalue 
        }
        else {
            filtervar <- "case_rate"
            filtervalue <- input$filtervalue / 1000
        }
        if (input$x_var == "Dögum síðan skilyrði að neðan var náð") {
            p <- euro_smit %>% 
                filter(country %in% input$countries, continent %in% input$continent,
                       !!sym(filtervar) > filtervalue) %>% 
                group_by(country) %>% 
                mutate(days = row_number(),
                       chosen = ifelse(country == input$chosen, "chosen", "other")) %>% 
                ungroup %>% 
                ggplot(aes(days, cum_cases, 
                           col = chosen, 
                           size = chosen,
                           alpha = chosen,
                           group = country)) +
                geom_line(show.legend = F) +
                scale_colour_manual(values = c("Blue", "Black")) +
                scale_size_manual(values = c(1.2, 0.8)) + 
                scale_alpha_manual(values = c(1, 0.3)) +
                labs(title = "Þróun fjölda smitaðra á Íslandi og annars staðar",
                     subtitle = "Sýnd eftir dögum frá öðru smiti hvers lands",
                     x = "Dagar",
                     y = "Fjöldi smitaðra") 
            
            if (input$scale == "Logra") p <- p + scale_y_log10()
            ggplotly(p, tooltip = c("x", "y", "country"))
        } else {
            p <- euro_smit %>% 
                filter(country %in% input$countries, continent %in% input$continent,
                       !!sym(filtervar) > filtervalue) %>% 
                mutate(days_from_first = as.numeric(date - min(date)),
                       chosen = ifelse(country == input$chosen, "chosen", "other")) %>% 
                ggplot(aes(date, cum_cases, 
                           col = chosen, 
                           size = chosen,
                           alpha = chosen,
                           group = country)) +
                geom_line(show.legend = F) +
                scale_x_date(date_labels = "%d. %B", breaks = pretty_breaks(8)) +
                
                scale_colour_manual(values = c("Blue", "Black")) +
                scale_size_manual(values = c(1.2, 0.8)) + 
                scale_alpha_manual(values = c(1, 0.3)) +
                labs(title = "Þróun fjölda smitaðra á Íslandi og annars staðar",
                     subtitle = "Sýnd eftir dagsetningu",
                     y = "Fjöldi smitaðra") +
                theme(axis.title.x = element_blank())
            if (input$scale == "Logra") p <- p + scale_y_log10()
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
        if (input$filtervar == "Fjöldi tilvika") {
            filtervar <- "cum_cases"
            filtervalue <- input$filtervalue 
        }
        else { 
            filtervar <- "case_rate"
            filtervalue <- input$filtervalue / 1000
        }
        
        if (input$x_var == "Dögum síðan skilyrði að neðan var náð") {
            p <- euro_smit %>% 
                filter(country %in% input$countries, 
                       continent %in% input$continent,
                       !!sym(filtervar) > filtervalue) %>% 
                group_by(country) %>% 
                mutate(days = row_number(),
                       chosen = ifelse(country == input$chosen, "chosen", "other")) %>% 
                ungroup() %>% 
                ggplot(aes(days, case_rate, 
                           col = chosen, 
                           size = chosen,
                           alpha = chosen,
                           group = country)) +
                geom_line(show.legend = F) +
                scale_colour_manual(values = c("Blue", "Black")) +
                scale_size_manual(values = c(1.2, 0.8)) + 
                scale_alpha_manual(values = c(1, 0.3)) +
                labs(title = "Þróun tíðni smitaðra á Íslandi og annars staðar",
                     subtitle = "Sýnd sem fjöldi per 1000 íbúar eftir dögum frá öðru smiti hvers lands",
                     x = "Dagar",
                     y = "Fjöldi smitaðra (per 1000 íbúar)")
            if (input$scale == "Logra") p <- p + scale_y_log10()
            ggplotly(p, tooltip = c("x", "y", "country"))
        } else {
            p <- euro_smit %>% 
                filter(country %in% input$countries, continent %in% input$continent,
                       !!sym(filtervar) > filtervalue) %>% 
                mutate(chosen = ifelse(country == input$chosen, "chosen", "other")) %>% 
                ggplot(aes(date, case_rate, 
                           col = chosen, 
                           size = chosen,
                           alpha = chosen,
                           group = country)) +
                geom_line(show.legend = F) +
                scale_x_date(date_labels = "%d. %B", breaks = pretty_breaks(8)) +
                scale_colour_manual(values = c("Blue", "Black")) +
                scale_size_manual(values = c(1.2, 0.8)) + 
                scale_alpha_manual(values = c(1, 0.3)) +
                labs(title = "Þróun tíðni smitaðra á Íslandi og annars staðar",
                     subtitle = "Sýnd sem fjöldi per 1000 íbúar eftir dögum frá öðru smiti hvers lands",
                     y = "Fjöldi smitaðra (per 1000 íbúar)") +
                theme(axis.title.x = element_blank())
            if (input$scale == "Logra") p <- p + scale_y_log10()
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
    
    lmer_plot <- eventReactive(input$gobutton1, {
        req(input$continent)
        d <- euro_smit %>% 
            filter(date >= ymd("2020-03-02"),
                   continent %in% input$continent) %>% 
            mutate(days = as.numeric(date - min(date)))
        
        n_obs <- length(unique(d$country))
        
        
        m <- lmer(log(case_rate) ~ days + (days | country), data = d,
                  control = lmerControl(optimizer = "bobyqa"))
        
        evo <- coef(m)[[1]][, 2, drop = F] %>%  exp
        evo <- evo[order(evo), , drop = F]
        
        which_chosen <- which(rownames(evo) == input$chosen)
        evo_chosen <- evo[which_chosen, ]
        evo_chosen <- round(evo_chosen - 1, 3)
        mean_evo <- exp(fixef(m)[2]) - 1
        
        p <- tibble(country = rownames(evo),
                    change = evo[, 1]) %>% 
            mutate(country = fct_reorder(country, change),
                   col = case_when(country == input$chosen ~ "blue",
                                   country == "Denmark" ~ "red",
                                   TRUE ~ "grey")) %>% 
            ggplot(aes(country, change - 1)) +
            geom_point(aes(col = col), show.legend = F) +
            geom_segment(aes(xend = country, yend = 0, col = col), show.legend = F) +
            geom_hline(yintercept = exp(summary(m)$coefficients[2, 1]) - 1, lty = 2) +
            # Meðalaukning
            geom_text(data = tibble(), 
                      aes(label = "Meðalaukning", 
                          x = 2, y = mean_evo + 0.05),
                      size = 4) +
            # Label Iceland
            geom_text(data = tibble(),
                      aes(label = percent(evo_chosen),
                          x = which_chosen,
                          y = evo_chosen + 0.02),
                      col = "blue", size = 4) +
            scale_y_continuous(labels = percent, 
                               breaks = pretty_breaks(5),
                               expand = expansion(mult = 0.02)) +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            scale_colour_manual(values = c("blue", "grey", "red")) +
            coord_flip() +
            labs(title = "Dagleg aukning á tíðni tilfella (per 1000 íbúa) eftir 2. mars") +
            theme(axis.title = element_blank(),
                  text = element_text(size = 12)) +
            background_grid(major = "none", minor = "none")
        
        if (n_obs > 60) {
            p <- p + theme(axis.text.y = element_text(size = 5))
        }
        
        ggplotly(p, tooltip = c("x", "y", "country"))
    })
    
    output$lmer_plot <- renderPlotly({
        lmer_plot()
    })
    
    ##### Europe Table #####
    
    output$countries_to_table <- renderUI({
        req(input$countries)
        if ("Iceland" %in% input$countries) {
            selectInput(inputId = "chosen_table", 
                        label = "Samanburðarland", 
                        choices = input$countries_table,
                        selectize = T,
                        selected =  "Iceland")
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
}

shinyApp(ui = ui, server = server)
