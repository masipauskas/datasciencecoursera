best <- function(state, outcome) {
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  validate_state <- function(state, data) {
    if (!(state %in% data[,7])) {
      stop("invalid state")
    }
  }
  
  validate_outcome_to_column <- function(outcome) {
    if (outcome == "heart attack") {
      11
    } else if (outcome == "heart failure") {
      17
    } else if (outcome == "pneumonia") {
      23
    } else {
      stop("invalid outcome")  
    }
  }
  
  validate_state(state, data)
  column <- validate_outcome_to_column(outcome)
  data <- data[order(data[,2]), ]
  data <- data[data[,7] == state, ]
  data[which.min(data[, column]), 2]
}