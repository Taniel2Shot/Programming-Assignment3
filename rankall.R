## R Programming Assignment 3
## September 8, 2016
## Dale Richardson

## Many thanks to the week 4 forum post by our mentor, Al Warren. His tips really helped sort this one out. 

## Create a function, rankall, that outputs the rank of a hospital based on outcome for 
## heart failure, heart attack or pneumonia

rankall <- function(outcome, num = "best"){
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
        
        ## For each state, find the hospital of the given rank
        
        # Get data into 3 relevant columns and remove NAs
        byOutcome <- na.omit(outcome_data[, c(2,7,outcomes[outcome])])
        
        # Rename the columns to make life easy
        names(byOutcome) <- c("hospital", "state", "outcome")
        
        ## Sort this data frame by outcome, state and hospital name
        sorted <- byOutcome[order(byOutcome[,3], byOutcome[,2], byOutcome[,1]), ]
        
        ## Use the split function to split this sorted data frame into a list organized by state
        myList <- split(sorted, sorted$state)
        
        ## Create a function that takes as input a dataframe and extracts best, worst or integer number
        myFuncforlapply <- function(adataframe, num){
                if (num == "best"){
                        return(adataframe[1,1]) #you must use the return statement to get these value in the output!
                }
                
                if (num == "worst"){
                        return(adataframe[nrow(adataframe), 1])
                }
                else {
                        return(adataframe[num, 1])  
                }
        }
        
        ## Get the hospital names according to queried rank, which could be "best", "worst" or integer num
        hospitalNames <- sapply(myList, myFuncforlapply, num)
        
        ## Get the names of the states
        stateNames <- names(hospitalNames)
        
        ## Unlist the list of hospital names
        #unlisted <- unlist(lapply(myList, myFuncforlapply, num))
        
        outputDataframe <- data.frame(hospital=hospitalNames, state=stateNames, row.names=stateNames)
        
        
}
