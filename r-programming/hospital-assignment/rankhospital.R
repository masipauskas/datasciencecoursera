rankhospital <- function(state, outcome, num = "best") {
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
    
    num_to_row <- function(num) {
      if (num == "best" || num == "worst") {
        1
      } else if (is.numeric(num)) {
        as.numeric(num)
      } else {
        stop("invalid num")
      }
    }
    
    validate_state(state, data)
    column <- validate_outcome_to_column(outcome)
    row <- num_to_row(num)
    
    data <- data[data[,7] == state, ]
    data <- data[data[, column] != "Not Available", ]
    data <- data[order(data[,2]), ]
    
    if (num == "worst") {
      data <- data[order(as.numeric(data[, column]), decreasing = TRUE),]
    } else {
      data <- data[order(as.numeric(data[, column])),]
    }
    
    data[row, 2]
}