#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinymaterial)
library(shinydashboard)
library(tidyverse)
library(MASS)
library(fBasics)    
library(urca)
library(TSA)


ui <- dashboardPage(
  dashboardHeader(title = "Madres solteras"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets",
               badgeLabel = "new", badgeColor = "green")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "dashboard",
              h2("Dashboard tab content"),
                  textInput("textAge", h3("Ingrese la edad:")),
                  textInput("textMunicipality", h3("Ingrese el municipio:")),
                  selectInput("state", h2("Choose a state:"),
                          list('East Coast' = c("NY", "NJ", "CT"),
                               'West Coast' = c("WA", "OR", "CA"),
                               'Midwest' = c("MN", "WI", "IA"))
                  ),
                  textOutput("result"),
                  textInput("textLevelSecurity", h3("Salud:")),
                  textInput("textWork", h3("Nivel de seguridad:")),
                  textInput("textLevelHappy", h3("Trabajo:")),
                  textInput("textLive", h3("Nivel de felicidad:")),
                  textInput("textEconomicHome", h3("vale vivir:")),
                  textInput("textEnterEconomic", h3("Hogares completos:")),
                  textInput("textTranquility", h3("Ingreso económico:")),
                  textInput("textEconomicHome", h3("Nivel de tranquilidad:")),
                  textInput("textEnterEconomic", h3("Municipio del desplazado:")),
                  textInput("textTranquility", h3("Niños :")),
                  textInput("textTranquility", h3("Padre del hogar :")),
                  tags$head(
                    tags$style
                    (
                      HTML
                      (
                        '#run{background-color:#8BC5ED}'
                      )
                    ) 
                  ), actionButton("run","Run Analysis"),
                  #material_button(
                  #  input_id = "example_button",
                  #  label = "DEEP_ORANGE",
                  #  color = "blue"
                  #)
              
              h2("este es un resultado")
      ),
      
      tabItem(tabName = "widgets",
              h2("Widgets tab content"),
              fluidRow(
                box(title = "Histogram", status = "primary", plotOutput("plot2", height = 250)),
                box(
                  title = "Inputs", status = "warning",
                  "Box content here", br(), "More box content",
                  sliderInput("slider", "Slider input:", 1, 100, 50),
                  textInput("text", "Text input:")
                )
              )
      )
    )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #output$hist <- renderPlot({hist(rnorm(input$num), main = input$title)})
  
  histdata <- rnorm(500)
  output$plot2 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(rnorm(data), main = input$text)
  })
  
  output$result <- renderText({
    paste("You chose", input$state)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)