best <- function(state, outcome) {
  ## Read outcome data
  
  data_file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
## Check that state and outcome are valid
    
  if (any(unique(data_file$State) == state)) {

    if (any(c("heart attack", "heart failure", "pneumonia") == outcome)) {
      
      if (outcome == "heart attack") {
        data <- data_file[data_file$State == state, c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
        data <- data[order(suppressWarnings(as.double(data[,2]))), ]
      }
      if (outcome == "heart failure") {
        data <- data_file[data_file$State == state, c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
        data <- data[order(suppressWarnings(as.double(data[,2]))), ]
      }
      if (outcome == "pneumonia") {
        data <- data_file[data_file$State == state, c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
        data <- data[order(suppressWarnings(as.double(data[,2]))), ]
      }      

      ## Return hospital name in that state with lowest 30-day death
      ## rate
          data$Hospital.Name[1]

    }
    else {stop("invalid outcome")}
  }
  else {stop("invalid state")}
    
}

