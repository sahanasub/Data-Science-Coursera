rankall <- function(outcome, num = "best") {
  outcomefile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) {
    stop("invalid outcome")
  }
  
  else {
    colnames(outcomefile)[c(2,7,11,17,23)] <- c("Hospital.Name","State", "heart.attack","heart.failure","pneumonia")
    outcome <- sub(" ",".",outcome)
    rankalldf <- data.frame(hospital = character(),state = character(), stringsAsFactors = FALSE)
  }
  
  outcomefile[,c(11,17,23)] <- sapply(outcomefile[,c(11,17,23)], as.numeric)
  subsetteddf <- outcomefile[!is.na(outcomefile[[outcome]]),]
  splitdf <- split(subsetteddf, as.factor(subsetteddf[["State"]]))
  for (i in 1:length(splitdf)) {
    splitdf[[i]] <- splitdf[[i]][order(splitdf[[i]][[outcome]], splitdf[[i]]$Hospital.Name),]
    flag <- TRUE
    if (num == "best") {
      num1 <- as.numeric(1)
      flag <- TRUE
    }
    
    else if (num == "worst") {
      num1 <- as.numeric(length(splitdf[[i]][[outcome]]))
      flag <- TRUE
    }
    
    else if (as.numeric(num) > length(splitdf[[i]][[outcome]])) {
      rankalldf[nrow(rankalldf)+1,] <- list(NA,names(splitdf[i]))
      flag <- FALSE
    }
    
    else {
      num1 <- num
    }
    
    if (flag) {  
      rankalldf[nrow(rankalldf)+1,] <- list(splitdf[[i]][["Hospital.Name"]][as.numeric(num1)],names(splitdf[i]))
    }
    
    i <- i+1
  }
  
  return(rankalldf)
  }
  
  
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name