library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

# Laden Sie Ihre Daten hier
titanic_data <- read.csv2("titanic_data.csv", header = TRUE, sep = ",")

# Shiny-App definieren
ui <- fluidPage(
  titlePanel("Titanic Überlebenswahrscheinlichkeit"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("class_selector", "Passagier Klasse",
                   choices = c("1", "2", "3"),
                   selected = "1"),
      sliderInput("age_group_selector", "Altersgruppen",
                  min = 0, max = 100, value = 10,
                  step = 5)
    ),
    mainPanel(
      plotOutput("survival_histogram")
    )
  )
)

server <- function(input, output) {
  # Funktion zur Berechnung der Überlebenswahrscheinlichkeit
  calculate_survival_probability <- function(data, class, age_group) {
    filtered_data <- data %>%
      filter(!is.na(Age),
             Pclass == as.numeric(class),
             Age >= age_group[1], Age <= age_group[2])
    total_passengers <- nrow(filtered_data)
    survived_passengers <- sum(filtered_data$Survived == 1)
    survival_probability <- (survived_passengers / total_passengers) * 100
    return(survival_probability)
  }
  
  # Histogramm erstellen
  output$survival_histogram <- renderPlot({
    selected_class <- input$class_selector
    age_group <- input$age_group_selector
    
    # Überlebenswahrscheinlichkeit für ausgewählte Klasse und Altersgruppe berechnen
    survival_prob <- calculate_survival_probability(titanic_data, selected_class, age_group)
    
    # Daten für das Histogramm erstellen
    hist_data <- titanic_data %>%
      filter(!is.na(Age),
             Pclass == as.numeric(selected_class)) %>%
      mutate(Age_Group = cut(as.numeric(Age), breaks = seq(0, 100, by = age_group),
                             labels = seq(0, 100, by = age_group)[-1]))
    
    # Überlebenswahrscheinlichkeit für jede Altersgruppe berechnen
    survival_probabilities <- hist_data %>%
      group_by(Age_Group) %>%
      summarize(Survival_Probability = sum(Survived == 1) / n())
    
    # Use tidyr's complete function to include missing bins (NA)
    survival_probabilities <- tidyr::complete(survival_probabilities, Age_Group = seq(0, 100, by = age_group)[-1], fill = list(Survival_Probability = 0))
    
    # Histogramm zeichnen
    ggplot(survival_probabilities, aes(x = as.factor(Age_Group), y = Survival_Probability * 100)) +
      geom_bar(stat = "identity", fill = "skyblue", color = "black") +
      labs(title = paste("Überlebenswahrscheinlichkeit in Pclass", selected_class),
           x = "Altersgruppen", y = "Überlebenswahrscheinlichkeit (%)") +
      theme_minimal() +
      scale_x_discrete(drop = FALSE)  # to include empty bins
  })
}

shinyApp(ui, server)
