
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Transmission type prediction based on Motor Trend Car Road Tests dataset."),

  # Sidebar with a slider input for number of bins
  sidebarPanel(
    sliderInput("mpg",
                "Miles/(US) gallon:",
                min = 10,
                max = 40,
                value = 20),
    sliderInput("wt",
                "Weight (lb/1000):",
                min = 1.5,
                max = 5.4,
                value = 2),
    sliderInput("qsec",
                "1/4 mile time:",
                min = 10.0,
                max = 25.0,
                value = 20),
    submitButton("Predict Transmission Type", icon("refresh"))
  ),

  # Show a plot of the generated distribution
  mainPanel(
      h3("Predicted transmission type"),
      hr(),
      span("In 1973-74, the car withe the specified mgp, weight and 1/4 mile time would most likely have: "),
      strong(textOutput("prediction", inline = "true")),
      span("transmission."),
      h3("About the application"),
      p("The application tries to predict cars transmission type based on its Miles/(US) gallon fuel consumption, weight and 1/4 mile time."),
      p("This dataset, has been extracted from 1974 Motor Trend US magazine and covers the car models between 1973-1974."),
      p("To use the application, please select the value for Miles/(US) gallon, weight for the car and it's 1/4 mile time, then click: \"Predict Transmision Type\", the perdicted transmission type will be shown in the Predicted transmission type section."),
      h3("Confussion matrix"),
      verbatimTextOutput("confusionMatrix")
  )
))
