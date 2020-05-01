library(shiny)
library(DT)
library(shinyWidgets)
library(shinythemes)

source("helpers.r")


# Global data
data <- covid_data()
totals <- covid_totals(data)

last_day <- max(data$dateRep)
total_cases <- sum(totals$cases)
total_deaths <- sum(totals$deaths)

yesterday <-
  data %>% filter(dateRep == last_day - 1) %>% summarise(cases = sum(cases), deaths = sum(deaths))
today <-
  data %>% filter(dateRep == last_day) %>% summarise(cases = sum(cases), deaths = sum(deaths))
today_percentage <- (today * 100 / yesterday) - 100

countries <- totals$country

totals$lethality <- totals$deaths / totals$cases * 100
totals$mortality <- totals$deaths / totals$population * 10000


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("simplex"),
  
  navbarPage(
    title = "Covid-19",
    
    # World Tab
    tabPanel("World",
             
             fluidRow(
               column(
                 3,
                 tags$h3(
                   "In 2019 a new virus came into existence all around the world. This is the covid-19 impact until today.",
                 ),
                 wellPanel(tags$h3(
                   tags$b(icon("globe"), "Total cases: "),
                   textOutput("total_cases", inline = T)
                 )),
                 wellPanel(tags$h3(
                   tags$b(icon("globe"), "Total deaths: "),
                   textOutput("total_deaths", inline = T)
                 )),
                 wellPanel(tags$h3(
                   tags$b(icon("calendar"), sprintf("Today cases (%s):", last_day)), textOutput("today_cases", inline = T)
                 )),
                 wellPanel(tags$h3(
                   tags$b(icon("calendar"), sprintf("Today deaths (%s):", last_day)), textOutput("today_deaths", inline = T)
                 )),
                 wellPanel(
                   selectInput(
                     "map_display",
                     label = h3("Display/sort by:"),
                     choices = list("Total Cases" = "cases", "Total Deaths" = "deaths"),
                     selected = "cases"
                   )
                 )
               ),
               column(9,
                      fluidRow(column(
                        12, plotOutput("map", height = "auto")
                      )),
                      fluidRow(
                        tags$h3(
                          "These are the top 10 affected countries sorted by the chosen criteria:"
                        ),
                        column(12, plotOutput("top10"))
                      ))
             )),
    
    # Compare Tab
    tabPanel("Compare",
             
             fluidRow(
               column(3,
                      
                      wellPanel(
                        selectInput(
                          "select_timeline",
                          label = h3("Compare:"),
                          choices = list("Cases per day" = "Per day",
                                         "Cummulative cases" = "Cummulative"),
                          selected = "Per day"
                        )
                        
                      ),
                      
                      wellPanel(
                        pickerInput(
                          "picker_timeline",
                          label = "Select countries for comparison:",
                          choices = countries,
                          selected = c("Portugal", "Spain"),
                          multiple = TRUE
                        )
                      )),
               column(9,
                      plotOutput("plot_timeline"))
               
             )),
    
    # Data Tab
    tabPanel(
      "Data",
      
      tags$p(
        "Data obtained from ",
        tags$a("ecdc.europa.eu", href = "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide")
      ),
      
      DTOutput("data_table")
      
    ),
    
    # World Tab cummulative
    tabPanel("Cummulative",
             fluidRow(
               column(3,
                      
                      wellPanel(
                        dateInput("date", "Date:", value = "2020-04-30", format = "yy/mm/dd"),
                        
                      )),
               column(9,
                      fluidRow(column(
                        12, plotOutput("map_cummulative", height = "auto")
                      )))
             )),
    # World Tab
    tabPanel("Lethality",
             
             fluidRow(
               column(3,
                      wellPanel(
                        selectInput(
                          "lethality_map_display",
                          label = h3("Display/sort by:"),
                          choices = list("Lethality" = "lethality", "Mortality" = "mortality"),
                          selected = "lethality"
                        )
                      )),
               column(9,
                      fluidRow(column(
                        12, plotOutput("lethality_map", height = "auto")
                      )))
             ))
    
    
  ),
  
  includeHTML("footer.html")
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Render Table data
  output$data_table <- renderDT({
    data
  })
  
  # Render Timeline Comparison
  output$plot_timeline <- renderPlot({
    req(input$picker_timeline)
    
    plot_covid_timeline_comparison(data, input$picker_timeline, input$select_timeline)
  }, res = 120)
  
  # Render Top10 Plot
  output$top10 <- renderPlot({
    plot_covid_top10(totals, input$map_display)
  }, res = 120)
  
  # Render World Map
  output$map <- renderPlot({
    plot_covid_totals(totals, input$map_display)
  }, height = function() {
    session$clientData$output_map_width / 2
  }, res = 120)
  
  # Render Individual values
  output$total_cases <- renderText({
    sum(totals$cases)
  })
  
  output$total_deaths <- renderText({
    sum(totals$deaths)
  })
  
  output$today_cases <- renderText({
    paste(today$cases, " (", round(today_percentage$cases, 0), "%)")
  })
  
  output$today_deaths <- renderText({
    paste(today$deaths, " (", round(today_percentage$deaths, 0), "%)")
  })
  
  # Render Cummulative Map
  output$map_cummulative <- renderPlot({
    plot_country_centroids(covid_data(), input$date)
  }, height = function() {
    session$clientData$output_map_width / 2
  }, res = 120)
  
  # Render Lethality map
  output$lethality_map <- renderPlot({
    plot_lethality_totals(totals, input$lethality_map_display)
  }, height = function() {
    session$clientData$output_map_width / 2
  }, res = 120)
  
}

# Run the application
shinyApp(ui = ui, server = server)