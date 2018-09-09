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
              h2("Parámetros para hallar el resultado de la predicción de la satisfacción de una madre soltera"),
                  numericInput("textAge", h3("Ingrese la edad de la madre soltera:"),  0, min = 15, max = 100),
                  numericInput("textAgeMunicipality", h3("Ingrese el número de años que ha vivido la madre soltera en un municipio:"),  1, min = 1, max = 100),
                  # razón del desplazamiento
                  selectInput("textReasonForDisplacement", h3("Ingrese el número que indica la razón del desplazamiento:"),
                              list('Razón de desplazamiento' = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
                  ),
                  textOutput("resultReasonForDisplacement"),
                  selectInput("textHealth", h3("Elija el nivel de satisfacción con los servicios de salud prestados:"),
                              list('Nivel de satisfacción' = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
                  ),
                  numericInput("textLevelSecurity", h3("Nivel de seguridad:"), 0, min = 0, max = 10),
                  selectInput("textWork", h3("Nivel de satisfacción con el trabajo que poseen:"),
                              list('Nivel satusfacción del trabajo' = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
                  ),
                  selectInput("textLevelHappy", h3("Nivel de felicidad de la semana anterior:"),
                              list('Nivel de felicidad' = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
                  ),
                  selectInput("textLive", h3("Nivel de deseo de vivir por parte de la madre soltera:"),
                              list('Nivel de de deseo de vivir' = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
                  ),
                  selectInput("textCompletHome", h3("Elija el número de familias que viven en el hogar:"),
                              list('Cantidad de familias' = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
                  ),
                  selectInput("textEnterEconomic", h3("Nivel de sagtisfacción por el ingreso económico:"),
                              list('Nivel de ingreso económico' = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
                  ),
                  selectInput("textTranquility", h3("Elija el nivel de tranquilidad de la madre soltera:"),
                              list('Nivel de tranquilidad' = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
                  ),
                  selectInput("textDisplaced", h3("¿La madre soltera es desplazada?:"),
                              list('Elija si o no si la madre soltera es desplazada' = c("si", "no"))
                  ),
                  selectInput("textBoys", h3("Elija el número de hijos en el hogar:"),
                              list('Número de hijos' = c("si", "no"))
                  ),
                  selectInput("textFather", h3("¿Actualmente el padre de la madre soltera vive con ella? :"),
                              list('Elija si wk padre de la madre soltera está vivo o muerto' = c("si", "no"))
                  ),
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
  
  output$resultReasonForDisplacement <- renderText({
    paste("You chose", input$textReasonForDisplacement)
  })
  
  #-------------------------------------------------------------------------------------------------------
  ## llamado de la base de datos y c+alculo de la predicción 
  
  setwd("/home/jose/") ## esta es la posicion de la base de datos.txt
  data<-read.csv("/home/jose/data.txt", header = TRUE, sep = " ")
  
  ## transformar variable
  
  data$satisfecho<-as.numeric(data$satisfecho)
  y<-data$satisfecho+1
  z<-y^2.414084
  
  ### convertir variables 
  
  data$edad<-as.numeric(data$edad)
  data$desplazado_municipio<-as.factor(data$desplazado_municipio)
  data$anios_viviendo_municipio<-as.numeric(data$anios_viviendo_municipio)
  data$razon_desplazamiento<-as.factor(data$razon_desplazamiento)
  data$padre_vive_hogar<-as.factor(data$padre_vive_hogar)
  data$ingreso_economico<-as.numeric(data$ingreso_economico)
  data$salud<-as.numeric(data$salud)
  data$nivel_seguridad<-as.numeric(data$nivel_seguridad)
  data$trabajo<-as.numeric(data$trabajo)
  data$feliz<-as.numeric(data$feliz)
  data$tranquilidad<-as.numeric(data$tranquilidad)
  data$vale_vivir<-as.numeric(data$vale_vivir)
  data$hogares_completos<-as.numeric(data$hogares_completos)
  
  ## BAckWARd (proceso hacia atras) 
  
  modback <- lm(z ~ edad + desplazado_municipio 
                + anios_viviendo_municipio + razon_desplazamiento 
                + padre_vive_hogar + ingreso_economico + salud 
                + nivel_seguridad + trabajo + feliz + tranquilidad 
                + vale_vivir + ninos + hogares_completos,
                data = data)
  
  summary(modback)
  
  ## Prediccion formulas
  
  aux <-  output$resultReasonForDisplacement <- renderText({ 
    input$textReasonForDisplacement
  })
  
  p <- predict(object = modback,
               newdata=data.frame(edad = aux,
                                  anios_viviendo_municipio = aux,
                                  razon_desplazamiento = aux, 
                                  salud = aux, 
                                  nivel_seguridad = aux, 
                                  trabajo = aux, 
                                  feliz = aux, 
                                  vale_vivir = aux, 
                                  hogares_completos = aux, 
                                  ingreso_economico = aux,
                                  tranquilidad = aux,
                                  desplazado_municipio = aux,
                                  ninos = aux, 
                                  padre_vive_hogar = aux),
               type = "response")
  prediccion <- p^(1/2.414084) - 1
  prediccion <-round(prediccion,0) - 1 ### revisar si es -1
  prediccion
  
  #-------------------------------------------------------------------------------------------------------
}

# Run the application 
shinyApp(ui = ui, server = server)