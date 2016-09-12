## R Programming Assignment 3
## September 7, 2016
## Dale Richardson

## Plot the 30-day mortality rates for heart attack

# outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
# head(outcome)
# str(outcome)
# 
# names(outcome)

## make a simple histogram for the 30-day death rates from heart attack, column 11

# outcome[, 11] <- as.numeric(outcome[, 11]) # seems like this column was chr by default
# 
# hist(outcome[, 11])


## Finding the best hospital in a state

best <- function(state, outcome){
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
        sorted[1,1]
        
}