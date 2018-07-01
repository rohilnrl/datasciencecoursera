best <- function(state, outcome) {
  if (outcome == "heart attack") {
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop("invalid outcome")
  }
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if (length(which(data$State == state)) == 0) {
    stop("invalid state")
  } else {
    data <- data[which(data$State == state), ]
  }
  
  data[[outcome]] <- as.numeric(data[[outcome]])
  outcome_data <- data[[outcome]][!is.na(data[[outcome]])]
  lowest_death <- min(outcome_data)
  lowest_death <- which(lowest_death == data[[outcome]])
  
  if (length(lowest_death) == 1) {
    return (data[["Hospital.Name"]][lowest_death])
  } else {
    hospitals <- sort(data[["Hospital.Name"]][lowest_death])
    return (hospitals[1])
  }
}
