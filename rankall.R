# This is a function created for Coursera R-Programming course
# The function return a character vector containing the name of the hospital with 
# the 5th lowest 30-day death rate for heart failure. The num argument can take 
# values "best", "worst", or an integer indicating the ranking (smaller numbers are better)
# The results are according for ranking data in treatment of pneumonia, heart attacks 
# or heart failure based in U.S. Department of Health and Human Services data from 2012 
# ( (http://hospitalcompare.hhs.gov)). The data is on the archive outcome-of-care-measures.csv 
# included in this GitHub project. 
# This function was created by Diego Andres Benitez.

rankall <- function(outcome, num = "best") {
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
    
    ## Check outcome is valid
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop("invalid outcome")
    }
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    index <- match(outcome, c("heart attack", "heart failure", "pneumonia"))
    hosp.data<- hospital.data[,c(1:2, (2 + index))]
    hosp.data <- hosp.data[order(hosp.data$Hospital.Name),]
    state = sort(unique(hosp.data$State))
    data_list = list()
    for (i in 1:length(state)) {
        data_list[[i]] = subset(hosp.data, State == state[i])
    }
    ranking = function(x) {
        Rank_ <- rank(as.numeric(x[,3]), na.last = "keep", ties.method = "first")
        x <- data.frame(x, Rank = Rank_)
    }
    data_list = lapply(data_list, ranking)
    if (num == "best") {
        num <-  1
        hospitals <-  unlist(lapply(data_list, function(x) {x$Hospital.Name[match(num, x$Rank)]}))
        data.frame(hospital = hospitals, state = state)
    } else if (num == "worst") {
        hospitals <- unlist(lapply(data_list, function(x) {x$Hospital.Name[match(max(x$Rank, na.rm = TRUE), x$Rank)]}))
        data.frame(hospital = hospitals, state = state)
    } else if (is.numeric(num) == TRUE & num < max(unlist(lapply(data_list, function(x) {max(x$Rank, na.rm = TRUE)})))) {
        hospitals = unlist(lapply(data_list, function(x) {x$Hospital.Name[match(num, x$Rank)]}))
        data.frame(hospital = hospitals, state = state)
    } else print("NA")
}