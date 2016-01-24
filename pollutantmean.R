pollutantmean <- function(directory, pollutant, id=1:332) {
  
  output <- vector()
  
  for (i in seq_along(id)) {
    if (id[i]<10) {
      filename <- paste(directory, "/00", id[i], ".csv", sep="")
    }
    else if (id[i]>=10 && id[i]<99) {
      filename <- paste(directory, "/0", id[i], ".csv", sep="")
    }
    else if (id[i]>100) {
      filename <- paste(directory, "/", id[i], ".csv", sep="")
    }
    
    my_table <- read.csv(filename)
    my_pollutant <- my_table[,pollutant]
    
    output <- c(output, my_pollutant)

  }
  
  mean(output, na.rm = TRUE)
}