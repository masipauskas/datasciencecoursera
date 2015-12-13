
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://www.rstudio.com/shiny/
#

library(shiny)
library(datasets)
library(caret)
require(randomForest)
require(e1071)

set.seed(20150718)

ds <- mtcars[, c(1, 6, 7, 9)]
in_training  <- createDataPartition(ds$am, p = 0.6, list = FALSE)

training       <- ds[in_training, -4]
testing      <- ds[-in_training, -4 ]
training_class <- as.factor(ds[in_training, c(4)])
testing_class <- as.factor(ds[-in_training, c(4)])

buildModel <- function(train_data, train_class) {
    model <- train(
        train_data, train_class, method = "rf",
        tuneGrid = data.frame(.mtry = 3)
    )

    model
}


testModel <- function(model, test_data, test_class) {
    predictions <- predict(model, test_data, type = "raw")
    confusionMatrix(predictions, test_class)
}

shinyServer(function(input, output) {
  model <- buildModel(training, training_class)

  output$prediction <- renderText({
      iData <- data.frame(mpg = as.numeric(input$mpg), wt = as.numeric(input$wt), qsec = as.numeric(input$qsec))
      result <- predict(model, iData, type = "raw")
      if (result == "0") "Manual" else "Automatic"
  })

  output$confusionMatrix <- renderPrint({
      testModel(model, testing, testing_class)
  })

})
