source("complete.R")

corr <- function(directory, threshold=0) {
  
  output <- vector()

  my_files <- complete(directory,1:332)

  id <- my_files$id[my_files$nobs >= threshold]
  
  if (length(id)>0) {
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
      mask <- (!is.na(my_table[2]) == TRUE & !is.na(my_table[3]) == TRUE)
      corrr <- cor(my_table[mask,2], my_table[mask,3])
      output <- c(output, corrr)      

    }
    
    return(output)

  }
  else {
    
    return(numeric(0))
    
  }
}