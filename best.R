best <- function(state, outcome) {
  state <- state
  outcometype <- outcome
  outcomefile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if (!(state %in% outcomefile$State)) {
    stop("invalid state")
  }
  
  else if (!(outcometype %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  
  else {
    colnames(outcomefile)[c(11,17,23)] <- c("heart.attack","heart.failure","pneumonia")
    outcometype <- sub(" ",".",outcometype)
  }
  
  outcomefile[,c(11,17,23)] <- sapply(outcomefile[,c(11,17,23)],as.numeric)
  subsetteddf <- subset(outcomefile, State == state, select = c(Hospital.Name, heart.attack,heart.failure,pneumonia))
  
  getminhospnames <- subsetteddf[which(subsetteddf[[outcometype]]==min(subsetteddf[[outcometype]], na.rm = TRUE )), 1]
  
  if (length(getminhospnames>1)) {
     sort(getminhospnames)
     return(getminhospnames[[1]])
  }
  else
  {
     return(getminhospnames)
  }
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}
