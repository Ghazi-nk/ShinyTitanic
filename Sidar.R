
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

titanic_data <- read.csv2("titanic_data.csv", header = TRUE, sep= ",")
# This assumes you have a CSV file named "titanic_data.csv" with the provided data.

# Define UI for the application
ui <- fluidPage(
  titlePanel("Titanic Survival Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose a variable:",
                  choices = c("Age", "Pclass")),
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    mainPanel(
      plotOutput("barChart")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins2",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    mainPanel(
      plotOutput("pClassChart")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("mplot_merkmal", "Wähle ein Merkmal zum Vergleich:",
                  choices = c("PClass", "Gender", "Age", "Fare")),
      
      conditionalPanel(
        condition = "input.mplot_merkmal == 'Age'",
        selectInput("mplot_intervall", "Wähle ein Intervall für Age:",
                    choices = c("Alle Passagiere",
                                "Passagiere bis zu 20 Jahren",
                                "Passagiere zwischen 20 - 40 Jahren",
                                "Passagiere zwischen 40 - 60 Jahren",
                                "Passagiere zwischen 60 - 85 Jahren"))),
      conditionalPanel(
        condition = "input.mplot_merkmal == 'Fare'",
        selectInput("mplotfare_intervall", "Wähle ein Intervall für Fare:",
                    choices = c("Alle Passagiere",
                                "Tickets zwischen 100 - 200 GE",
                                "Tickets zwischen 200 - 550 GE")))
    ),
    mainPanel(
      plotOutput("mplotOutput")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  
  output$barChart <- renderPlot({
    # Choose the variable based on user input
    x_var <- switch(input$variable,
                    "Age" = cut(as.numeric(titanic_data$Age), breaks = seq(0, 51, by = input$bins)),
                    "Pclass" = as.factor(titanic_data$Pclass))
    
    # Filter out missing values
    titanic_data_filtered <- na.omit(titanic_data)
    
    # Calculate survival rate
    survival_rate <- tapply(titanic_data_filtered$Survived, x_var, mean)
    
    # Create a bar chart
    ggplot(data.frame(x = names(survival_rate), y = survival_rate), aes(x = x, y = y)) +
      geom_bar(stat = "identity", fill = "steelblue", color = "black") +
      labs(
        title = "Survival Analysis",
        x = input$variable,
        y = "Survival Rate"
      )
  })
  
  output$pClassChart <- renderPlot({
    x = cut(as.numeric(titanic_data$Age), breaks = seq(0, 100, by = input$bins2));
    survival_rate <- tapply(titanic_data$Survived, x, mean);
    
    ggplot(data.frame(x = names(survival_rate), y = survival_rate), aes(x = x, y = y)) +
      geom_bar(stat = "identity", fill = "steelblue", color = "black") +
      labs(
        title = "Survival Rate based on Age",
        x = "Gender",
        y = "Survival Rate"
      )
  })
  
  output$mplotOutput <- renderPlot({
    titanic_data_filtered <- na.omit(titanic_data)
    breaks <- switch(input$mplot_intervall,
                     "Alle Passagiere" = c(0, 18, 30, 60, 100),
                     "Passagiere bis zu 20 Jahren" = c(0, 5, 10, 15, 20),
                     "Passagiere zwischen 20 - 40 Jahren" = c(20, 25, 30, 35, 40),
                     "Passagiere zwischen 40 - 60 Jahren" = c(40, 45, 50, 55, 60),
                     "Passagiere zwischen 60 - 85 Jahren" = c(60, 65, 70, 85))
    breaks_fare <- switch(input$mplotfare_intervall,
                          "Alle Passagiere" = c(0, 25, 50, 100, 550),
                          "Tickets zwischen 100 - 200 GE" = c(100, 125, 150, 200),
                          "Tickets zwischen 200 - 550 GE" = c(200, 230, 270, 550))
    chosen <- switch(input$mplot_merkmal, 
                "PClass" = titanic_data_filtered$Pclass,
                "Gender" = titanic_data_filtered$Sex,
                "Age" = cut(as.numeric(titanic_data_filtered$Age), breaks = breaks),
                "Fare" = cut(as.numeric(titanic_data_filtered$Fare), breaks = breaks_fare))
    
    survival <- titanic_data_filtered$Survived
    
    table_data <- data.frame(chosen, survival)
    colnames(table_data) <- c(input$mplot_merkmal, "Survived")
    mosaic_data <- table(table_data)
    
    mosaicplot(mosaic_data, labs(title = "Mosaikplot"))
  })
}
# Run the application
shinyApp(ui = ui, server = server)