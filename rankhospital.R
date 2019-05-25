rankhospital <- function(state, outcome, num = "best") {
  
  outcomefile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if (!(state %in% outcomefile$State)) {
    stop("invalid state")
  }
  
  else if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) {
    stop("invalid outcome")
  }
  
  else {
    colnames(outcomefile)[c(11,17,23)] <- c("heart.attack","heart.failure","pneumonia")
    outcome <- sub(" ",".",outcome)
  }
  
  outcomefile[,c(11,17,23)] <- sapply(outcomefile[,c(11,17,23)], as.numeric)
  subsetteddf <- subset(outcomefile, State == state & !is.na(outcomefile[[outcome]]), select = c(Hospital.Name, heart.attack,heart.failure,pneumonia))
  ordereddf <- subsetteddf[order(subsetteddf[[outcome]], subsetteddf[,1]),]
  
  if (num == "best") {
    num <- as.numeric(1)
  }
  
  else if (num == "worst") {
    num <- as.numeric(length(ordereddf[[outcome]]))
  }
  
  else if (as.numeric(num) > length(ordereddf[[outcome]])) {
    NA
  }
  
  ordereddf[[1]][as.numeric(num)]
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}