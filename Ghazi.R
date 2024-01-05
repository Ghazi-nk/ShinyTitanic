library(shiny)
library(dplyr)
library(ggplot2)
library(palmerpenguins)

titanic_data <- read.csv2("titanic_data.csv", header = TRUE, sep = ",")

ui <- fluidPage(
  titlePanel("Titanic Überlebenswahrscheinlichkeit"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("class_selector", "Passagier Klasse",
                   choices = c("1", "2", "3"),
                   selected = "1"),
      sliderInput("age_group_selector", "Altersgruppen",
                  min = 10, max = 25, value = 10, step = 5)
    ),
    mainPanel(
      plotOutput("survival_histogram")
    )
  )
)

server <- function(input, output) {
  calculate_survival_probability <- function(data, class, age_group) {
    total_passengers <- nrow(data)
    survived_passengers <- sum(data$Survived == 1)
    survival_probability <- (survived_passengers / total_passengers) * 100
    return(survival_probability)
  }
  
  output$survival_histogram <- renderPlot({
    selected_class <- input$class_selector
    age_group <- input$age_group_selector
    
    survival_prob <- calculate_survival_probability(titanic_data, selected_class, age_group)
    
    hist_data <- titanic_data %>%
      filter(Pclass == as.numeric(selected_class),
             !is.na(Age)) %>%
      mutate(Age_Group = cut(as.numeric(Age), breaks = seq(0, 100, by = age_group),
                             labels = seq(0, 100, by = age_group)[-1]))
    
    survival_probabilities <- hist_data %>%
      group_by(Age_Group) %>%
      summarize(Survival_Probability = sum(Survived == 1) / n(),
                Absolute_Count = n())
    
    ggplot(survival_probabilities, aes(x = as.factor(Age_Group), y = Survival_Probability * 100) +
      geom_bar(stat = "identity", fill = "skyblue", color = "black") +
      labs(title = paste("Überlebenswahrscheinlichkeit in Pclass", selected_class),
           x = "Altersgruppen", y = "Überlebenswahrscheinlichkeit (%)") +
      theme_minimal() +
      scale_x_discrete(na.translate = FALSE)  })
}

shinyApp(ui, server)
