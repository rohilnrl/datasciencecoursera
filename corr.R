corr <- function(directory, threshold = 0) {
  complete_data <- complete("specdata")
  nobs = numeric()
  
  for (i in 1:332) {
    old_i <- i
    
    if (complete_data[["nobs"]][i] <= threshold) {
      next
    }
    
    # ID name preprocessing - adding leading zeroes
    if (i < 10) {
      i <- paste("00", as.character(i), collapse = "", sep = "")
    } else if (i < 100) {
      i <- paste("0", as.character(i), collapse = "", sep = "")
    }
    
    path <- file.path(directory, paste(i, ".csv", sep = ""))
    
    data = read.csv(path)
    sulfate <- data[["sulfate"]][!is.na(data[["sulfate"]]) & !is.na(data[["nitrate"]])]
    nitrate <- data[["nitrate"]][!is.na(data[["sulfate"]]) & !is.na(data[["nitrate"]])]
    
    nobs <- c(nobs, cor(sulfate, nitrate))
  }
  nobs
}