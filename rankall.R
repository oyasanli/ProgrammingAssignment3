##Function finds the hospital in a state in giving outcome and rank. 
##Oya Şanlı
rankAll <- function(outcome, num = "best") {

    dataAll <- data.frame(hospital = character(), state = character())
  
## Read data

    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
## Check that outcome and num are valid

    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        dataAll <- "invalid outcome"
    }
    else {
        keys <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        outcomeKey <- keys[outcome]

## For each state, find the hospital of the given rank
##split data by state
        data_state <- split(data, data$State)
##for every state find the best hospitals
        for (stat in names(data_state)) {
           our_state <- data_state[[stat]]
           data_outcome <- suppressWarnings(as.numeric(our_state[, outcomeKey]))
           good <- complete.cases(data_outcome)
           data_outcome <- data_outcome[good]
           our_state <- our_state[good,]
##order by outcome and hospital name
           our_state <- our_state[ order(data_outcome, our_state["Hospital.Name"]), ]
        
           if (num == "best") {
               numState <- c(1)
           } else {
               if (num == "worst") {
                   numState <- length(data_outcome)
               } else {
                   numState <- num
               }
           }
    
        dataPart <- data.frame(hospital = our_state[numState, "Hospital.Name"], state = stat, row.names = stat)
##combine all data
        dataAll <- rbind(dataAll, dataPart)
        }
    }

## Return the data frame with the hospital names and state

    dataAll
}