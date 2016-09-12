## R Programming Assignment 3
## September 8, 2016
## Dale Richardson

## Create a function, rankhospital, that outputs the rank of a hospital based on state and outcome for 
## heart failure, heart attack or pneumonia

rankhospital <- function(state, outcome, num = "best"){
        ## test outcome
        #outcome <- "heart attack"
        #state <- "TX"
        
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
        
        outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23 ) #create vector of columns for selection
        ## Check that state and outcome are valid by check for membership in the built-in state abbreviations vector
        if (!(state %in% state.abb)){
                stop("invalid state")
        }
        if (!(outcome %in% names(outcomes))){
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death rate
        
        ## split by outcome
        byOutcome <- outcome_data[, c(2,7,outcomes[outcome])]
        ## Remove NAs and subset by state
        byState <- na.omit(byOutcome[ which(byOutcome$State == state),])
        ## Really want to use dplyr here, but will use base instead
        ## dd[ order(-dd[,4], dd[,1]), ]
        sorted <- byState[order(byState[,3], byState[,1]), ]
        ## return best hospital name
        ## set the row and column index 
        if (num == "best"){
                sorted[1,1]
        }
        
        if (num == "worst"){
                sorted[nrow(sorted), 1]
        }
        else {
                sorted[num, 1]  
        }
        
        
}