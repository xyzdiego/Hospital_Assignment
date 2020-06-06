# This is a function created for Coursera R-Programming course
# The function return a character vector containing the name of the hospital with 
# the 5th lowest 30-day death rate for heart failure. The num argument can take 
# values "best", "worst", or an integer indicating the ranking (smaller numbers are better)
# The results are according for ranking data in treatment of pneumonia, heart attacks 
# or heart failure based in U.S. Department of Health and Human Services data from 2012 
# ( (http://hospitalcompare.hhs.gov)). The data is on the archive outcome-of-care-measures.csv 
# included in this GitHub project. 
# This function was created by Diego Andres Benitez.

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    directory <- paste(getwd(), "/", "outcome-of-care-measures.csv", sep = "")
    data <- read.csv(directory, header = T, colClasses = "character", 
                     na.strings = "Not Available")
    # Only take the columns we need
    hospital.data <- data[,c(2, 7, 11, 17, 23)]
    # 2 = Hospital Name
    # 7 = State
    # 11 = heart attack 30-day mortality rate
    # 17 = heart failure 30-day mortality rate
    # 23 = pneumonia  30-day mortality rate
    
    ## Check that state and outcome are valid
    if (!state %in% unique(hospital.data$State)) {
        stop("invalid state")
    }
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop("invalid outcome")
    }
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    index <-  match(outcome, c("heart attack", "heart failure", "pneumonia"))
    state.hosp.data <- na.omit(hospital.data[hospital.data$State == state, ])[,c(1:2, (2 + index))]
    state.hosp.data <- state.hosp.data[order(state.hosp.data$Hospital.Name),]
    state.hosp.data$Rank <- rank(as.numeric(state.hosp.data[,3]), ties.method = "first")
    if (num == "best") {
        state.hosp.data$Hospital.Name[which.min(as.numeric(state.hosp.data[,3]))]
    } else if (num == "worst") {
        state.hosp.data$Hospital.Name[which.max(as.numeric(state.hosp.data[,3]))]
    } else if (num >= min(state.hosp.data$Rank) & num <= max(state.hosp.data$Rank)) {
        state.hosp.data$Hospital.Name[which(state.hosp.data$Rank == num)]
    } else print("NA")
}
