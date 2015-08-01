#State variable is column 7
# first part of function binds data frame info based on state input argument
# second part finds minimum for outcome argument, returns hospital 


library(dplyr)
setwd('C:/Users/Travis/Documents/R programming/rprog-data-ProgAssignment3-data')

best<- function(state,outcome){
    data<-read.csv('outcome-of-care-measures.csv', colClasses='character')
    by_state<-data.frame() #creates empty data frame 
    states<-unique(data[,7]) #creates factor of all states
    if (state %in% states == FALSE) stop('invalid state') #error 
    if (outcome != 'heart attack' & outcome != 'heart failure' & 
        outcome != 'pneumonia') stop('invalid state')
    for (i in 1:nrow(data)) {
        st<-data[i,7] #selects state of each row in loop 
        if(st == state) {
            by_state<-rbind(by_state,data[i,]) #adds all columns of data
        }
    }
    #returns hospital with minimum outcome 
    if (outcome == 'heart attack') {
        out<- as.numeric(by_state[,11]) #convert character to numbers, get NAs
        low<-min(out, na.rm = T) 
        low<-sprintf(low,fmt = '%#.3g') #specifies one decimal place 
        low_info<-filter(by_state,by_state[11] == low) #finds row with min value
        
    } else if (outcome == 'heart failure') {
        out<- as.numeric(by_state[,17])
        low<-min(out, na.rm = T)
        low_info<-filter(by_state,by_state[17] == low)
       
    } else if (outcome == 'pneumonia') {
        out<-as.numeric(by_state[,23])
        low<-min(out, na.rm = T)
        low_info<-filter(by_state,by_state[23] == low)
        
    }
    hospital<- as.character(low_info[,2])
    hospital<- sort(hospital) # sorts alphabetically if >1 hospital
    return(hospital)
}


