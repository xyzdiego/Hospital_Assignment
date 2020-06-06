# This is a function created for Coursera R-Programming course
# The function find the best hospital in a state according for ranking data in 
# treatment of pneumonia, heart attacks or heart failure based in U.S. Department 
# of Health and Human Services data from 2012 ( (http://hospitalcompare.hhs.gov)). 
# The data is on the archive outcome-of-care-measures.csv included in this 
# GitHub project. 
# This function was created by Diego Andres Benitez.

best <- function(state, outcome){
    ## Read outcome data
    ## read the data file in local directory
    directory <- paste(getwd(), "/", "outcome-of-care-measures.csv", sep = "")
    data <- read.csv(directory, colClasses = "character")
    
    ## One auxiliary function for getting the hospital name
    auxiliar <- function(data, col_num, state){
        state_subset <- data[data[,7] == state, ] # Logical values vector
        outcome_arrs <- state_subset[, col_num]   # Select outcomes variable (i.e. Pneumonia)
        minim <- min(outcome_arrs, na.rm = T)     # Search the min value
        min_index <- which(outcome_arrs == minim)
        hosp_name <- state_subset[min_index, 2]       # Search the hospital name
        return(hosp_name)
    }
    
    ## change dat type from character to numeric in three interesting values
    data[,11] <- as.numeric(data[,11])  # heart attack variable
    data[,17] <- as.numeric(data[,17])  # heart failure variable
    data[,23] <- as.numeric(data[,23])  # pneumonia variable
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if(!state %in% data$State){
        stop("invalid state, only two letters")
    } else if(!outcome %in% outcomes){
        stop("invalid outcome, plese select only one of these diseases: \n 
             'heart attack', 'heart failure', 'pneumonia'")
    } else {
        if(outcome == "heart attack"){
            hosp_name <- auxiliar(data, 11, state)
        } else if(outcome == "heart failure"){
            hosp_name <- auxiliar(data, 17, state)
        } else {
            hosp_name <- auxiliar(data, 23, state)
        }
        result <- hosp_name
        return(result)
    }
}


# tests
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")   #Error
best("NY", "cerebral death") #Error