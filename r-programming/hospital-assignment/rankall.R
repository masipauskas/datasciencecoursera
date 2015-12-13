rankall <- function(outcome, num = "best") {
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
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
  
  get_name <- function(data, col, n) {
    row <- num_to_row(n)
    
    if (n == "worst") {
      data <- data[order(as.numeric(data[,col]), decreasing = TRUE),]
    } else {
      data <- data[order(as.numeric(data[,col])),]
    }
    cbind(hospital=data[row, 2], state=data[1, 7])
  }
  
  column <- validate_outcome_to_column(outcome)
  row <- num_to_row(num)
  data <- data[order(data[, 7], data[, 2]), ]
  result <- lapply(split(data, data[, 7]), function(x) get_name(x, column, num))
  frame <- as.data.frame(do.call(rbind, result))
}