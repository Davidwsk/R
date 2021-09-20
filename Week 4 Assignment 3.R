best <- function(state, outcome) {
  ## Read outcome data
  df_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that state and outcome are valid
  if (!state %in% df_outcome$State) {
    stop("invalid state")
  }
  if (outcome == "heart attack") {
    df_final <- data.frame(Name = df_outcome$Hospital.Name, State = df_outcome$State,
                           compare = suppressWarnings(as.numeric(df_outcome[,11])))
  } else if (outcome == "heart failure") {
    df_final <- data.frame(Name = df_outcome$Hospital.Name, State = df_outcome$State,
                           compare = suppressWarnings(as.numeric(df_outcome[,17])))
  } else if (outcome == "pneumonia") {
    df_final <- data.frame(Name = df_outcome$Hospital.Name, State = df_outcome$State,
                           compare = suppressWarnings(as.numeric(df_outcome[,23])))
  } else {
    stop("invalid outcome")
  }

  ## Return hospital name in that state with lowest 30-day death
  ## rate
  df_final <- df_final[complete.cases(df_final),]
  df_final <- df_final[df_final$State == state,]
  result <- df_final$Name[df_final$compare == min(df_final$compare, na.rm = TRUE)]
  
  print(result)
  
}


best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")

