complete <- function(directory, id = 1:332) {
  complete_data <- data.frame(id = numeric(), nobs = numeric())
  
  for (i in id) {
    old_i <- i
    
    # ID name preprocessing - adding leading zeroes
    if (i < 10) {
      i <- paste("00", as.character(i), collapse = "", sep = "")
    } else if (i < 100) {
      i <- paste("0", as.character(i), collapse = "", sep = "")
    }
    
    path <- file.path(directory, paste(i, ".csv", sep = ""))
    
    data = read.csv(path)
    count <- length(data[[2]][!is.na(data[["sulfate"]]) & !is.na(data[["nitrate"]])])
    
    complete_data <- rbind(complete_data, data.frame(id = old_i, nobs = count))
  }
  complete_data
}