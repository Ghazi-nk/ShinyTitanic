
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
library(dplyr)
library(palmerpenguins)

titanic_data <- read.csv2("titanic_data.csv", header = TRUE, sep= ",")
# This assumes you have a CSV file named "titanic_data.csv" with the provided data.

# Define UI for the application
ui <- fluidPage(
  titlePanel("Titanic Survival Analysis"),

  sidebarLayout(
    sidebarPanel(
      h2("Titanic Survival Analysis"),
      h4("Bitte wähle die zu überprüfenden Daten aus."),
      selectInput("sex", "Geschlecht:", c("male", "female")),
      sliderInput("alter", "Alter:", 1, 90, value = 25, step = 1),
      selectInput("hafen", "An diesem Hafen zugestiegen:", c("Cherbourg" = 'C', "Queenstown" = 'Q', "Southhampton" = 'S')),
      numericInput("klasse", "Ticketklasse:", value = 3, min = 1, max = 3, step = 1)
    ),
    
    mainPanel(
      h2(textOutput("prob")),
    )
  ),
  
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
  ),

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
  # Logik für Überlebenschance-Vorhersage:
  output$prob <- renderText({
    titanic_data_filtered <- select(titanic_data, -c(PassengerId, Name, Ticket, Cabin, SibSp, Parch, Fare))
    
    titanic_data_filtered <- titanic_data_filtered %>%
      as_tibble() %>%
      mutate(
        Survived = factor(Survived),
        Pclass = factor(Pclass),
        Age = factor(Age),
        Sex = factor(Sex),
        Embarked = factor(Embarked)
      ) %>%
      filter(!is.na(Survived), !is.na(Pclass), !is.na(Age))
    
    prediction_model <- glm(formula=Survived ~. , family = binomial(link = "logit"), data = titanic_data_filtered)
    
    prediction_input_data <- data.frame(
      Sex = as.factor(input$sex),
      Age = as.factor(as.character(input$alter)),
      Embarked = as.factor(input$hafen),
      Pclass = as.factor(as.character(input$klasse))
    )
    
    prob <- predict(prediction_model, newdata = prediction_input_data, type = "response")
    
    paste(round(as.numeric(as.character(prob))*100, 2), "%")
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
    
    #Ghazi Plot
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
      
    }
    )
  })
}
# Run the application
shinyApp(ui = ui, server = server)
