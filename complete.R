complete <- function(directory, id=1:332) {
  
  output <- data.frame()
  
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
    
    full_exp <- sum(!is.na(my_table[2]) == TRUE & !is.na(my_table[3]) == TRUE)
    if(full_exp>0) {
    output <- rbind(output, data.frame(id[i],full_exp))
    }
    
  }
  colnames(output) <- c("id", "nobs")
  
  output
}