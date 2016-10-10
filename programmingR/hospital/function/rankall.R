# Write a function called rankall that takes two arguments: an outcome name
# (outcome) and a hospital ranking (num). The function reads the
# outcome-of-care-measures.csv file and returns a 2-column data frame containing
# the hospital in each state that has the ranking specified in num. For example
# the function call rankall("heart attack", "best") would return a data frame
# containing the names of the hospitals that are the best in their respective
# states for 30-day heart attack death rates. The function should return a value
# for every state (some may be NA). The first column in the data frame is named
# hospital, which contains the hospital name, and the second column is named
# state, which contains the 2-character abbreviation for the state name.
# Hospitals that do not have data on a particular outcome should be excluded
# from the set of hospitals when deciding the rankings.
# 
# Handling ties. The rankall function should handle ties in the 30-day mortality
# rates in the same way that the rankhospital function handles ties.

# NOTE: For the purpose of this part of the assignment (and for efficiency),
# your function should NOT call the rankhospital function from the previous
# section. The function should check the validity of its arguments. If an
# invalid outcome value is passed to rankall, the function should throw an error
# via the stop function with the exact message “invalid outcome”. The num 
# variable can take values “best”, “worst”, or an integer indicating the ranking
# (smaller numbers are better). If the number given by num is larger than the
# number of hospitals in that state, then the function should return NA.


# Here is some sample output from the function.
# > source("rankall.R")
# > head(rankall("heart attack", 20), 10)
# hospital state
# AK <NA> AK
# AL D W MCMILLAN MEMORIAL HOSPITAL AL
# AR ARKANSAS METHODIST MEDICAL CENTER AR
# 4
# AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
# CA SHERMAN OAKS HOSPITAL CA
# CO SKY RIDGE MEDICAL CENTER CO
# CT MIDSTATE MEDICAL CENTER CT
# DC <NA> DC
# DE <NA> DE
# FL SOUTH FLORIDA BAPTIST HOSPITAL FL

# > tail(rankall("pneumonia", "worst"), 3)
# hospital state
# WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
# WV PLATEAU MEDICAL CENTER WV
# WY NORTH BIG HORN HOSPITAL DISTRICT WY


# > tail(rankall("heart failure"), 10)
# hospital state
# TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
# TX FORT DUNCAN MEDICAL CENTER TX
# UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
# VA SENTARA POTOMAC HOSPITAL VA
# VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
# VT SPRINGFIELD HOSPITAL VT
# WA HARBORVIEW MEDICAL CENTER WA
# WI AURORA ST LUKES MEDICAL CENTER WI
# WV FAIRMONT GENERAL HOSPITAL WV
# WY CHEYENNE VA MEDICAL CENTER WY

library(dplyr)

rankall <- function(outcome, num = "best") {
    ## Check that state is valid
    if (!state %in% rawdata$State) {
        stop("invalid state")
    }
    
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
    
    ## clean up the table to only contain 6 variables needed. 
    columns = c('Provider.Number',
                'Hospital.Name', 
                'State', 
                'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack', 
                'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure', 
                'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')
    data <- rawdata[, columns]
    
    ## make column 4-6 numeric
    data[,4:6] = suppressWarnings(lapply(data[,4:6], function(x) as.numeric(x)))
    
    ## rename column 4-6 to be the same as the condition passed in, for easy processing. 
    names(data)[4:6] <- condition
    
    sortByRate <- function(x) {
        if (num == 'worst') {
            # descending order on outcome. 
            sorted <- na.omit(arrange(x, desc(x[, outcome]), x[, 'Hospital.Name']))
        }
        else {
            # ascending order.
            sorted <- na.omit(arrange(x, x[, outcome], x[, 'Hospital.Name']))
        }
        sorted[ranking,c('Hospital.Name', 'State')]
    }

    l <- split(data, data$State)
    l2 <- sapply(l, sortByRate)
    
    l2
    # print(sorted)
}

