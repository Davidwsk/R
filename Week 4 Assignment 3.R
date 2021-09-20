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


rankhospital <- function(state, outcome, num = "best") {
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
  if (is.numeric(num)) {
    rank <- num
  } else if (num == "best") {
    rank <- 1
  } else if (num == "worst") {
    rank <- 0
  } else {
    stop("invalid num")
  }
  
  print(rank)
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  df_final <- df_final[complete.cases(df_final),]
  df_final <- df_final[df_final$State == state,]
  df_final <- df_final[order(df_final$compare, df_final$Name),]
  
  print(df_final)
  print(df_final[1,])
  print(df_final["4077",])
  print(df_final[nrow(df_final),])
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
