library(shiny)
library(dplyr)
library(ggplot2)
library(palmerpenguins)

titanic_data <- read.csv2("titanic_data.csv", header = TRUE, sep = ",")

ui <- fluidPage(
  titlePanel("Titanic Überlebenswahrscheinlichkeit"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxInput("pclass_toggle", "Gesamt Übersicht", value = FALSE),
      conditionalPanel(
        condition = "input.pclass_toggle == false",
        radioButtons("class_selector", "Passagier Klasse",
                     choices = c("1", "2", "3"),
                     selected = "1"),
      ),
      sliderInput("age_group_selector", "Sequenz der Altersgruppen festlegen:",
                  min = 10, max = 25, value = 10, step = 5)
    ),
    mainPanel(
      plotOutput("survival_histogram")
    )
  )
)

server <- function(input, output) {
  
  output$survival_histogram <- renderPlot({
    age_group <- input$age_group_selector
    
    if (input$pclass_toggle) {
      hist_data <- titanic_data %>%
        filter(!is.na(Age)) %>%
        mutate(Age_Group = cut(as.numeric(Age), breaks = seq(0, 100, by = age_group),
                               labels = seq(0, 100, by = age_group)[-1]))
      

      survival_probabilities <- hist_data %>%
        group_by(Age_Group = cut(as.numeric(Age), breaks = seq(0, 100, by = age_group)), Pclass) %>%
        summarize(Survival_Probability = sum(Survived == 1) / n())
                  
      
      
      ggplot(survival_probabilities, aes(x = as.factor(Age_Group), y = Survival_Probability, fill = as.factor(Pclass))) +
        geom_bar(stat = "identity", position = "dodge", color = "black") +
        labs(title = "Überlebenswahrscheinlichkeit nach Altersgruppen und Passagiere Klasse",
             x = "Altersgruppen", y = "Absolute Anzahl der Passagiere",
             fill = "Passagier Klasse") +  # Hier wird die Legendenbeschriftung hinzugefügt
        scale_x_discrete(na.translate = FALSE) +
        theme_minimal() +
        scale_fill_manual(values = c("#FFD700", "#C0C0C0", "#CD7F32"), name = "Passagier Klasse") +  # Hier wird die Legendenüberschrift angepasst
        theme(axis.title.y.right = element_text(color = "skyblue"),
              axis.text.y.right = element_text(color = "skyblue"))
      
    } else {
      
      
      
      selected_class <- as.numeric(input$class_selector)
      
      hist_data <- titanic_data %>%
        filter(Pclass == selected_class, !is.na(Age)) %>%
        mutate(Age_Group = cut(as.numeric(Age), breaks = seq(0, 100, by = age_group),
                               labels = seq(0, 100, by = age_group)[-1]))
      
      survival_probabilities <- hist_data %>%
        group_by(Age_Group = cut(as.numeric(Age), breaks = seq(0, 100, by = age_group))) %>%
        summarize(Survival_Probability = sum(Survived == 1) / n(),
                  Absolute_Count = n())
      
      
      ggplot(survival_probabilities, aes(x = as.factor(Age_Group), y = Survival_Probability * 100)) +
        geom_bar(stat = "identity", fill = "skyblue", color = "black") +
        geom_text(aes(label = Absolute_Count), vjust = -0.5, color = "black") +
        labs(title = paste("Überlebenswahrscheinlichkeit in Passagiere Klasse", selected_class),
             x = "Altersgruppen", y = "Überlebenswahrscheinlichkeit (%)") +
        theme_minimal() +
        scale_x_discrete(na.translate = FALSE) +
        scale_y_continuous(limits = c(0, 100), name = "Überlebenswahrscheinlichkeit (%)") +
        theme(axis.title.y.right = element_text(color = "skyblue"),
              axis.text.y.right = element_text(color = "skyblue"))
    }
  })
  
}

shinyApp(ui, server)