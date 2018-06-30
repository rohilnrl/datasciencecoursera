pollutantmean <- function(directory, pollutant, id = 1:332) {
  summer <- 0
  counter <- 0
  
  for (i in id) {
    # ID name preprocessing - adding leading zeroes
    if (i < 10) {
      i <- paste("00", as.character(i), collapse = "", sep = "")
    } else if (i < 100) {
      i <- paste("0", as.character(i), collapse = "", sep = "")
    }
    
    path <- file.path(directory, paste(i, ".csv", sep = ""))
    
    data = read.csv(path)
    registered_pollutants <- data[[pollutant]][!is.na(data[[pollutant]])]
    summer <- summer + sum(registered_pollutants)
    counter <- counter + length(registered_pollutants)
  }
  
  summer / counter
}