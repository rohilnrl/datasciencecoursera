rankhospital <- function(state, outcome, num = "best") {
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
  
  if (num == "best") {
    lowest_death <- min(outcome_data)
    lowest_death <- which(lowest_death == data[[outcome]])
  } else if (num == "worst") {
    outcome_data_sorted <- sort(outcome_data)
    lowest_death <- outcome_data_sorted[length(outcome_data_sorted)]
    count <- length(outcome_data_sorted[outcome_data_sorted < lowest_death])
    lowest_death <- which(lowest_death == data[[outcome]])
  } else {
    if (num > length(outcome_data)) {
      return (NA)
    } else {
      outcome_data_sorted <- sort(outcome_data)
      lowest_death <- outcome_data_sorted[num]
      count <- length(outcome_data_sorted[outcome_data_sorted < lowest_death])
      lowest_death <- which(lowest_death == data[[outcome]])
    }
  }
    
  if (length(lowest_death) == 1) {
    return (data[["Hospital.Name"]][lowest_death])
  } else if (num == "worst") {
    hospitals <- sort(data[["Hospital.Name"]][lowest_death])
    return (hospitals[length(hospitals)])
  } else if (num == "best") {
    hospitals <- sort(data[["Hospital.Name"]][lowest_death])
    return (hospitals[1])
  } else {
    hospitals <- sort(data[["Hospital.Name"]][lowest_death])
    return (hospitals[num - count])
  }
}
