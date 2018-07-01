rankall <- function(outcome, num = "best") {
  if (outcome == "heart attack") {
  } else if (outcome == "heart failure") {
  } else if (outcome == "pneumonia") {
  } else {
    stop("invalid outcome")
  }
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  rank_data <- data.frame(hospital = character(), state = character())

  for (state in unique(data[["State"]])) {
    hospital <- rankhospital(state, outcome, num)
    rank_data <- rbind(rank_data, data.frame(hospital = hospital, state = state))
  }
  
  rank_data <- rank_data[order(rank_data$state), ]
  return (rank_data)
}
