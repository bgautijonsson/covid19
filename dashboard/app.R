library(shiny); library(dplyr); library(tidyr);
library(ggplot2); library(readr); library(cowplot); 
library(shinythemes);library(lubridate); library(kableExtra)
library(scales); library(broom); library(forcats);
library(lme4); library(stringr); library(plotly);
library(writexl); library(DT)
theme_set(theme_classic(base_size = 12) + 
              background_grid(color.major = "grey90", 
                              color.minor = "grey95", 
                              minor = "xy", major = "xy") +
              theme(legend.position = "none"))

url.exists <- RCurl::url.exists

fileurl <- local({
    today <- Sys.Date()
    baseurl <- "https://raw.githubusercontent.com/bgautijonsson/covid19/master/Output/"
    url <- paste0(baseurl, today, ".csv")
    # Ef er komin inn spá fyrir daginn, annars prófa frá í gær
    if (url.exists(url)) {
        url
    } else {
        paste0(baseurl, "Iceland_Predictions_", "2020-03-22", ".csv")
    }
})

d_spa <- read_csv(
    fileurl
) %>% 
    mutate_at(vars(median, upper), round)

d <- read_csv("https://raw.githubusercontent.com/bgautijonsson/covid19/master/Input/ECDC_Data.csv")
sidast_uppfaert <- "Síðast uppfært 23. mars 2020 klukkan 19:30"

Sys.setlocale("LC_TIME", "is_IS")

ui <- navbarPage(
    title = "Ísland og COVID19", 
    theme = shinytheme(theme = "flatly"),
    ##### Smitafjöldi #####
    tabPanel(title = "Þróun",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "continent",
                                 label = "Heimsálfa",
                                 choices = unique(d$continent),
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
                                          plotlyOutput("euro_plot_p", height = "600px"))
                     )
                 )
             )
    ),
    ##### Aukning #####
    tabPanel(title = "Aukning",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "continent_samanburdur",
                                 label = "Heimsálfa",
                                 choices = unique(d$continent),
                                 multiple = T, selectize = T,
                                 selected = "Europe"),
                     uiOutput("countries_to_choose_samanburdur"),
                     selectInput(inputId = "tegund_samanburdur",
                                 label = "Hvernig er tími valinn í reikninga?",
                                 choices = c("Dagsetning", "Dagar eftir að skylirði var náð"),
                                 multiple = F, selectize = F,
                                 selected = "Dagsetning"),
                     uiOutput("param_selection_samanburdur"),
                     div(actionButton(inputId = "gobutton_samanburdur", label = "Birta", width = "120px"), 
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
                                 tabPanel("Höfðatala",
                                          plotlyOutput("lmer_plot", height = "600px"))
                     )
                 )
             )),
    ##### Töfluyfirlit #####
    tabPanel(title = "Töfluyfirlit",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "countries_table", 
                                 label = "Lönd", 
                                 choices = unique(d$country),
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
                     tableOutput("summary_table"),
                     conditionalPanel(
                         condition = "input.gobutton2>0",
                         downloadButton(outputId = "table_download", label = "Sækja gögn")
                     )
                 )
             )
    ),
    ##### Forspá #####
    tabPanel(
        title = "Spá",
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    inputId = "tegund_forspa",
                    label = "Sjá spá fyrir",
                    choices = c("Uppsafnaðan fjölda" = "cumulative", "Virkan fjölda" = "active")
                ),
                selectInput(
                    inputId = "breyta_forspa",
                    label = "Sjá spá fyrir fjölda",
                    choices = c("Greindra smita" = "cases", "Á spítala" = "hospital", "Á gjörgæslu" = "icu")
                ),
                selectInput(inputId = "byage_forspa", label = "Birta eftir aldurs hópum?",
                            choices = c("Aldursskipting", "Heild"), 
                            selected = "Heild"),
                fluidRow(
                    column(6,
                           dateInput("date_from_forspa", 
                                     label = "Frá",
                                     value = "2020-03-04", 
                                     min = "2020-03-02", 
                                     max = "2020-05-01")),
                    column(6,
                           dateInput("date_to_forspa", 
                                     label = "Til",
                                     value = "2020-03-21", 
                                     min = "2020-03-02", 
                                     max = "2020-05-01"))),
                div(actionButton(inputId = "gobutton_forspa", label = "Birta gögn", width = "120px"), 
                    class = "center", align = "middle"),
                HTML("<br>"),
                downloadButton("downloadData_forspa", label = "Sækja töflu"),
                HTML("<br>"),
                h6("Höfundar:"),
                h6("Brynjófur Gauti Jónsson og Sindri Baldur"),
                h6("Tölfræðiráðgjöf Heilbrigðisvísindasviðs Háskóla Íslands"),
                div(img(src = "hi_hvs_horiz.png", width = "80%"), align = "middle", class = "center"),
                h6("Byggt á daglega uppfærðum gögnum ..."),
                h6(sidast_uppfaert),
                a("Allan kóða má nálgast hér", href = "https://github.com/bgautijonsson/covid19")
            ),
            mainPanel(
                tabsetPanel(
                    type = "tabs",
                    tabPanel(
                        "Tafla",
                        dataTableOutput(outputId = "tafla")
                    )
                )
            )
        )
    ),
    ##### Fróðleikur #####
    tabPanel(title = "Fróðleikur", 
             sidebarLayout(
                 sidebarPanel(
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
                     tabsetPanel(type = "tabs",
                                 tabPanel("Lögmál smárra talna",
                                          includeHTML("www/LawOfSmallNumbers.html")
                                 )
                     )
                 )
             )
    )
    
)

server <- function(input, output, session) {
    
    observe({
        if (input$selectall > 0) {
            if (input$selectall %% 2 == 1){
                updateCheckboxGroupInput(session=session, inputId="countries",
                                         selected = d %>% 
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
                        choices = d %>% filter(continent %in% input$continent) %>% .$country %>% unique,
                        multiple = T, selectize = T,
                        selected = c("Denmark", "Norway", "Finnland", "Sweden", "Iceland"))
        } else {
            selectInput(inputId = "countries", 
                        label = "Lönd", 
                        choices = d %>% filter(continent %in% input$continent) %>% .$country %>% unique,
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
            filtervar <- "total_cases"
            filtervalue <- input$filtervalue 
        }
        else {
            filtervar <- "case_rate"
            filtervalue <- input$filtervalue / 1000
        }
        if (input$x_var == "Dögum síðan skilyrði að neðan var náð") {
            p <- d %>% 
                filter(country %in% input$countries, continent %in% input$continent,
                       !!sym(filtervar) > filtervalue) %>% 
                group_by(country) %>% 
                mutate(days = row_number(),
                       chosen = ifelse(country == input$chosen, "chosen", "other")) %>% 
                ungroup %>% 
                ggplot(aes(days, total_cases, 
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
            p <- d %>% 
                filter(country %in% input$countries, continent %in% input$continent,
                       !!sym(filtervar) > filtervalue) %>% 
                mutate(days_from_first = as.numeric(date - min(date)),
                       chosen = ifelse(country == input$chosen, "chosen", "other")) %>% 
                ggplot(aes(date, total_cases, 
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
        data <- nearPoints(d %>% filter(country %in% c(input$countries)), 
                           input$euro_plot_n_click, threshold = 40, addDist = T,
                           xvar = "days", yvar = "total_cases")
        data <- data[which.min(data$dist_), ]
        if (length(data$country) == 0) return ("Smelltu á línu til að sjá hvaða landi hún tilheyrir")
        today <- Sys.Date() %>% ymd
        timi <- as.numeric(today - data$date)
        out <- paste0(data$country, ": Fjöldi var ", round(data$total_cases, 3), " fyrir ", timi, " dögum")
        return(out)
    })
    
    ##### Europe Rates #####
    
    euro_plot_p <- eventReactive(input$gobutton1, {
        req(input$countries, input$chosen)
        if (input$filtervar == "Fjöldi tilvika") {
            filtervar <- "total_cases"
            filtervalue <- input$filtervalue 
        }
        else { 
            filtervar <- "case_rate"
            filtervalue <- input$filtervalue / 1000
        }
        
        if (input$x_var == "Dögum síðan skilyrði að neðan var náð") {
            p <- d %>% 
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
            p <- d %>% 
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
        data <- nearPoints(d %>% filter(country %in% c(input$countries)), 
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
    
    output$countries_to_choose_samanburdur <- renderUI({
        req(input$continent_samanburdur)
        if ("Europe" %in% input$continent_samanburdur) {
            selectInput(inputId = "chosen_samanburdur", 
                        label = "Samanburðarland", 
                        choices = d %>% 
                            filter(continent == input$continent_samanburdur) %>% 
                            .$country %>% 
                            unique,
                        selectize = T,
                        selected =  "Iceland")
        } else {
            selectInput(inputId = "chosen_samanburdur", 
                        label = "Samanburðarland", 
                        choices = d %>% 
                            filter(continent == input$continent_samanburdur) %>% 
                            .$country %>% 
                            unique,
                        selectize = T)
        }
    })
    
    output$param_selection_samanburdur <- renderUI({
        req(input$tegund_samanburdur)
        if (input$tegund_samanburdur == "Dagsetning") {
            h4("Veldu tímabil til að bera saman aukningu í tíðni smita")
            fluidRow(
                column(6,
                       dateInput("date_from_samanburdur", 
                                 label = "Frá",
                                 value = "2020-03-04", 
                                 min = "2020-03-02", 
                                 max = max(d$date) - 3)),
                column(6,
                       dateInput("date_to_samanburdur", 
                                 label = "Til",
                                 value = max(d$date), 
                                 min = "2020-03-08", 
                                 max = max(d$date))))
        } else {
            fluidRow(column(6,
                            selectInput(inputId = "type_filt_samanburdur",
                                        label = "Sýna gögn þar sem",
                                        choices = c("Fjöldi tilvika", "Tíðni tilvika per milljón"),
                                        multiple = F,
                                        selected = "Fjöldi tilvika")),
                     column(6,
                            numericInput(inputId = "filtervalue_samanburdur",
                                         label = "Er hærri en", 
                                         min = 0, max = 100, value = 50)))
        }
    })
    
    lmer_plot <- eventReactive(input$gobutton_samanburdur, {
        req(input$continent_samanburdur)
        
        if (input$tegund_samanburdur == "Dagsetning") {
            d <- d %>% 
                filter(date >= ymd(input$date_from_samanburdur),
                       date <= ymd(input$date_to_samanburdur),
                       continent %in% input$continent_samanburdur) %>% 
                mutate(days = as.numeric(date - min(date)))
            
            n_obs <- length(unique(d$country))
        } else {
            if (input$type_filt_samanburdur == "Fjöldi tilvika") {
                filter_var <- "total_cases"
                filter_value <- input$filtervalue_samanburdur
            }
            else {
                filter_var <- "case_rate"
                filter_value <- input$filtervalue_samanburdur / 1000
            }
            
            d <- d %>% 
                filter(continent %in% input$continent_samanburdur,
                       !!sym(filter_var) >= filter_value) %>% 
                mutate(days = as.numeric(date - min(date)))
            
            n_obs <- length(unique(d$country))
        }
        
        
        m <- lmer(log(case_rate) ~ days + (days | country), data = d,
                  control = lmerControl(optimizer = "bobyqa"))
        
        evo <- coef(m)[[1]][, 2, drop = F] %>%  exp
        evo <- evo[order(evo), , drop = F]
        
        which_chosen <- which(rownames(evo) == input$chosen_samanburdur)
        evo_chosen <- evo[which_chosen, ]
        evo_chosen <- round(evo_chosen - 1, 3)
        mean_evo <- exp(fixef(m)[2]) - 1
        
        p <- tibble(country = rownames(evo),
                    change = evo[, 1]) %>% 
            mutate(country = fct_reorder(country, change),
                   col = case_when(country == input$chosen_samanburdur ~ "blue",
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
            # Label Chosen
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
            labs(title = "Dagleg aukning á tíðni tilfella (per 1000 íbúa) á völdu tímabili") +
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
        out <- d %>% 
            filter(country %in% input$countries_table) %>% 
            group_by(country) %>% 
            summarise(cases = max(total_cases),
                      incidence = round(cases / max(pop) * 1000, 4),
                      days = as.numeric(max(date) - min(date)),
                      first = min(date)) %>% 
            arrange(desc(!!sym(cols[[input$sort_col]])))
        
        if (input$sort_col == "Landi") {
            out <- out %>% 
                arrange(!!sym(cols[[input$sort_col]]))
        } else {
            out <- out %>% 
                arrange(desc(!!sym(cols[[input$sort_col]])))
        }
        names(out) <- c("Land", "Tilfelli", "Tíðni (per 1000)", "Dagar frá fyrsta smiti", "Dagsetning fyrsta smits")
        
        icel <- which(out$Land == input$chosen_table)
        
        out 
    })
    
    output$summary_table <- function() {
        out <- summary_table()
        
        icel <- which(out$Land == input$chosen_table)
        
        out %>% 
            kable(format = "html", align = c("l", rep("c", ncol(.) - 1))) %>% 
            kable_styling(bootstrap_options = c("striped", "hover")) %>% 
            row_spec(icel, bold = TRUE, background = "#b3cde3")
    }
    
    output$table_download <- downloadHandler(
        filename = function() {
            str_c("tafla_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
            write_xlsx(summary_table(), file)
        }
    )
    
    ##### Forspá #####
    
    out_gogn <- eventReactive(input$gobutton_forspa, {
        out <- d_spa %>% 
            filter(type == input$tegund_forspa,
                   name == input$breyta_forspa,
                   date >= ymd(input$date_from_forspa),
                   date <= ymd(input$date_to_forspa))
        
        
        
        if (input$byage_forspa == "Heild") {
            out <- out %>% 
                filter(age == "total") %>% 
                select(-type, -age, -name,
                       Dagsetning = date, "Líklegasta spá" = median, "Svartsýn spá" = upper)
        } else {
            out <- out %>% 
                filter(age != "total") %>% 
                select(-name, -type,
                       Dagsetning = date, Aldur = age, "Líklegasta spá" = median, "Svartsýn spá" = upper)
        }
        
        out
    })
    output$downloadData_forspa <- downloadHandler(
        filename = function() {
            paste0(
                Sys.Date(),
                "_covid_",
                input$tegund_forspa, "_",
                input$breyta_forspa, "_",
                if (input$byage_forspa != "Heild") "eftir_aldri" else "",
                "_spagildi.xlsx"
            )
        },
        content = function(file) {write_xlsx(out_gogn(), file)}
    )
    output$tafla <- renderDataTable({
        datatable(
            out_gogn(),
            rownames= FALSE,
            options = list(dom = 't', pageLength = nrow(out_gogn()))
        )
    })
    
    
}

shinyApp(ui = ui, server = server)
