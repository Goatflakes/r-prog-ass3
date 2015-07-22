outcomeData <- NULL
states <- NULL
outcomes <- c("heart attack", "heart failure", "pneumonia")
outcomeNumber <- c(11, 17, 23)
names(outcomeNumber) <- outcomes

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  if (is.null(outcomeData)){
    outcomeData <<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    states <<- unique(outcomeData$State)
    
    # convert the outcomes columns to numbers
    for(i in outcomeNumber) {
      outcomeData[, i] <<- as.numeric(outcomeData[, i])
    }
  }
  
  ## Check that state and outcome are valid
  if(!state %in% states) {
    stop("invalid state")
  }
  if(!outcome %in% outcomes) {
    stop("invalid outcome")
  }
  
  # keep only hospitals (rows) which match state, keep
  # the hospital name column (2 in the original data)
  # and outcomes (columns) that match outcome
  stateData <- outcomeData[outcomeData$State ==
                             state,c(2, outcomeNumber[[outcome]])]
  # remove NAs
  stateData <- stateData[complete.cases(stateData), ]
  # now sort that data in ascending order of outcome, breaking
  # ties on the ascending alphabetical order of the hospital name
  stateData <- stateData[order(stateData[ ,2], stateData[ ,1]),]
  
  best <- 1
  worst <- length(stateData[[1]])
  
  # check if num is valid. it may be numeric, "best" or "worst"
  if(is.numeric(num)) {
    if(num > worst) {
      return(NA)
    }
  } else if (num == "best"){
    num <- 1
  } else if (num == "worst"){
    num <- worst
  } else {
    return(NA)
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  stateData[num,1]
}