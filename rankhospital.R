##Function finds the hospital in a state in giving outcome and rank. 
##Oya Şanlı
rankhospital <- function(state, outcome, num) {
## Read outcome data
    
    data <- read.csv("outcome-of-care-measures.csv", 
                          colClasses = "character",
                          header = TRUE)
    
## Get data we need
    
    our_data <- as.data.frame(cbind(data[, 2],  # hospital 
                                data[, 7],      # state
                                data[, 11],     # heart attack 
                                data[, 17],     # heart failure 
                                data[, 23]),    # pneumonia
                           stringsAsFactors = FALSE)
    
## Rename columns
    
    colnames(our_data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
## Check that state, outcome are valid
    
    if(!state %in% our_data[,"state"]){
        stop('invalid state')
    }
    
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    }
    
    
## Return hospital name in that state with lowest 30-day death rate
    
## Get only the hospitals in giving state
    our_state <- our_data[(our_data[, "state"] == state), ]
    
## Convert outcome to numberic
    our_state[, outcome] <- suppressWarnings(as.numeric(our_state[, outcome]))
    
## Remove NA values
    our_state <- our_state[!is.na(our_state[, outcome]), ]
## if rank is not a number convert to number    
    if(num== "best") {
        num<- 1 
    }
    
    if (num== "worst") {
        num<- nrow(our_state) 
    }
    
## Order by outcome and hospital name
    our_state <- our_state[order(our_state[, outcome], our_state[, "hospital"]), ]

## Get names of hospital with h the (num)th lowest 30-day death rate
    hNames <- our_state[num, 1]
    hNames
}