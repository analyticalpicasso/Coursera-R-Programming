rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  frameSupplied <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomeCol <- list("heart attack" = 11, "heart failure"=17,"pneumonia" = 23)
  cols <- c(1,2,7)
  validOutcomeList <- c("heart attack","heart failure","pneumonia")
  outcomeColumnNum <- outcomeCol[[outcome]]
  cols <- c(cols,outcomeColumnNum)
  filteredData <- frameSupplied[,cols]
  validStateList <- levels(factor(filteredData$State))
  fcs <- (factor(filteredData$State))
  grouped <- split(filteredData,filteredData$State)
  df <- data.frame(hospital=character(),state=character() ) 
  for (state in validStateList){
    rankedRow <- numeric()
    flag <- TRUE
    stateWiseGrouped <- (grouped[state])
    frameData <- stateWiseGrouped[[state]]
    frameData <- data.frame(frameData, ot = as.numeric(frameData[,4]))
    if(num=="best"){
      orderedData <- frameData[order(frameData$ot, frameData$Hospital.Name), ]
      rankedRow <- orderedData[1,]
    } else if(num=="worst"){
      orderedData <- frameData[order(-frameData$ot, frameData$Hospital.Name), ]
      rankedRow <- orderedData[1,]
    } else if(num > nrow(frameData)) {
      flag<-FALSE
    } else{
      orderedData <- frameData[order(frameData$ot, frameData$Hospital.Name), ]
      rankedRow <- orderedData[num,]
    }
    
    if (flag){
      hospital <- (rankedRow$Hospital.Name)
      st <-  (rankedRow$State)
      newRow <- data.frame(hospital=hospital, state =st)
      #newRow <- data.frame(c(hospital,st))
      
    } else{
      newRow <- data.frame(hospital="<NA>", state=state)
      #newRow <- data.frame("<NA>",state)
    }
    df<- rbind(df,newRow)
    
    
  }
  
  return(df)
  
}