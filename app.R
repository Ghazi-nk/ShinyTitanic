
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

install.packages("shiny")
install.packages("ggplot2")
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
  )
)

# Define server logic
server <- function(input, output) {
  
  
  output$barChart <- renderPlot({
    # Choose the variable based on user input
    x_var <- switch(input$variable,
                    "Age" = cut(as.numeric(titanic_data$Age), breaks = seq(0, 100, by = input$bins)),
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
View(titanic_data)
}
# Run the application
shinyApp(ui = ui, server = server)