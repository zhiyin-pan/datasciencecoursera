# Write a function called rankhospital that takes three arguments: the
# 2-character abbreviated name of a state (state), an outcome (outcome), and the
# ranking of a hospital in that state for that outcome (num). The function reads
# the outcome-of-care-measures.csv file and returns a character vector with the
# name of the hospital that has the ranking specified by the num argument. For
# example, the call rankhospital("MD", "heart failure", 5) would return a
# character vector containing the name of the hospital with the 5th lowest
# 30-day death rate for heart failure. The num argument can take values “best”,
# “worst”, or an integer indicating the ranking (smaller numbers are better). If
# the number given by num is larger than the number of hospitals in that state,
# then the function should return NA. Hospitals that do not have data on a
# particular outcome should be excluded from the set of hospitals when deciding
# the rankings. 

# Handling ties. It may occur that multiple hospitals have the same 30-day
# mortality rate for a given cause of death. In those cases ties should be
# broken by using the hospital name. For example, in Texas (“TX”), the hospitals
# with lowest 30-day mortality rate for heart failure are shown here.

# > head(texas)
# Hospital.Name Rate Rank
# 3935 FORT DUNCAN MEDICAL CENTER 8.1 1
# 4085 TOMBALL REGIONAL MEDICAL CENTER 8.5 2
# 4103 CYPRESS FAIRBANKS MEDICAL CENTER 8.7 3
# 3954 DETAR HOSPITAL NAVARRO 8.7 4
# 4010 METHODIST HOSPITAL,THE 8.8 5
# 3962 MISSION REGIONAL MEDICAL CENTER 8.8 6

# Note that Cypress Fairbanks Medical Center and Detar Hospital Navarro both
# have the same 30-day rate (8.7). However, because Cypress comes before Detar
# alphabetically, Cypress is ranked number 3 in this scheme and Detar is ranked
# number 4. One can use the order function to sort multiple vectors in this 
# manner (i.e. where one vector is used to break ties in another vector). The
# function should use the following template.

# The function should check the validity of its arguments. If an invalid state
# value is passed to best, the function should throw an error via the stop
# function with the exact message “invalid state”. If an invalid outcome value
# is passed to best, the function should throw an error via the stop function
# with the exact message “invalid outcome”.

library(dplyr)

rankhospital <- function(state, outcome, num = "best") {
    ## Check that outcome is valid
    if( !outcome %in% condition) {
        stop("invalid outcome")
    }
    
    ## check num is either "best" or "worst" or an integer 
    if ( (!is.numeric(num)) && num != 'best' && num != 'worst') {
        stop("invalid ranking")
    }
    
    if (num == 'best' || num == 'worst') {
        ranking <- 1
    }
    else {
        ranking <- as.integer(num)
    }
    
    ## Read outcome data
    rawdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    condition <- c("heart attack", "heart failure", "pneumonia")
    ## Check that state is valid
    if (!state %in% rawdata$State) {
        stop("invalid state")
    }
    
    ## clean up the table to only contain 6 variables needed. 
    columns = c('Provider.Number',
                'Hospital.Name', 
                'State', 
                'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack', 
                'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure', 
                'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')
    data <- rawdata[, columns]
    
    ## filter the state
    data <- subset(data, State == state)
    
    ## make column 3-5 numeric
    data[,4:6] = suppressWarnings(lapply(data[,4:6], function(x) as.numeric(x)))
    
    ## rename column 3-5 to be the same as the condition passed in, for easy processing. 
    names(data)[4:6] <- condition
    
    ## sort by the passed in outcome, and also the hostpical name. 
    ## sort is too expensive, better to improve it by getting all the rows with the min
    ## and then sort by hostpical name. 
    if (num == 'worst') {
        # descending order on outcome. 
        sorted <- na.omit(arrange(data, desc(data[, outcome]), data[, 'Hospital.Name']))
    }
    else {
        # ascending order.
        sorted <- na.omit(arrange(data, data[, outcome], data[, 'Hospital.Name']))
    }
    
    # print(sorted)
    sorted[ranking,'Hospital.Name']
    sorted
}

