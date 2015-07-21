outcomeData <- NULL
states <- NULL
outcomes <- c("heart attack", "heart failure", "pneumonia")
outcomeNumber <- c(11, 17, 23)
names(outcomeNumber) <- outcomes

best <- function(state, outcome) {
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
  # now sort that data in ascending order of outcome, breaking
  # ties on the ascending alphabetical order of the hospital name
  stateData <- stateData[order(stateData[ ,2], stateData[ ,1]),]
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  stateData[1,1]
}