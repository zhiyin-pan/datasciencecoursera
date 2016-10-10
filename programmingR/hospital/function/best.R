# Write a function called best that take two arguments: the 2-character
# abbreviated name of a state and an outcome name. The function reads the
# outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the best (i.e. lowest) 30-day mortality for the
# specified outcome in that state. The hospital name is the name provided in the
# Hospital.Name variable. The outcomes can be one of “heart attack”, “heart
# failure”, or “pneumonia”. Hospitals that do not have data on a particular 
# outcome should be excluded from the set of hospitals when deciding the
# rankings. 

# Handling ties. If there is a tie for the best hospital for a given
# outcome, then the hospital names should be sorted in alphabetical order and
# the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”, 
# and “f” are tied for best, then hospital “b” should be returned).
# 

# The function should check the validity of its arguments. If an invalid state
# value is passed to best, the function should throw an error via the stop
# function with the exact message “invalid state”. If an invalid outcome value
# is passed to best, the function should throw an error via the stop function
# with the exact message “invalid outcome”.

# Here is some sample output from the function.
# > source("best.R")
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
# > best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome

library(dplyr)

best <- function(state, outcome) {
    
    ## Check that state is valid
    if (!state %in% rawdata$State) {
        stop("invalid state")
    }
    
    ## Check that outcome is valid
    if( !outcome %in% condition) {
        stop("invalid outcome")
    }
    
    ## Read outcome data
    rawdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    condition <- c("heart attack", "heart failure", "pneumonia")

    ## clean up the table to only contain 5 variables needed. 
    columns = c('Hospital.Name', 
                     'State', 
                     'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack', 
                     'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure', 
                     'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')
    data <- rawdata[, columns]
    
    ## filter the state
    data <- subset(data, State == state)
    
    ## make column 3-5 numeric
    data[,3:5] = suppressWarnings(lapply(data[,3:5], function(x) as.numeric(x)))
    
    ## rename column 3-5 to be the same as the condition passed in, for easy processing. 
    names(data)[3:5] <- condition

    ## sort by the passed in outcome, and also the hostpical name. 
    ## sort is too expensive, better to improve it by getting all the rows with the min
    ## and then sort by hostpical name. 
    sorted <- na.omit(arrange(data, data[, outcome], data[, 'Hospital.Name']))
    
    ## rate
    sorted[1, 'Hospital.Name']
}