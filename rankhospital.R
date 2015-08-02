
library(dplyr)
setwd(setwd('C:/Users/Travis/Documents/R programming/rprog-data-ProgAssignment3-data'))


rankhospital<-function(state,outcome,num) {
    data<-read.csv('outcome-of-care-measures.csv', colClasses='character')
    by_state<-data.frame() #creates empty data frame 
    states<-unique(data[,7]) #creates factor of all states
    if (state %in% states == FALSE) stop('invalid state') #error 
    if (outcome != 'heart attack' & outcome != 'heart failure' & 
        outcome != 'pneumonia') stop('invalid outcome')
    for (i in 1:nrow(data)) {
        st<-data[i,7] #selects state of each row in loop a=
        if(st == state) {
            by_state<-rbind(by_state,data[i,]) #adds all columns of data
        }
    }
    if (outcome == 'heart attack'){
        ha_order<-order(by_state[,11]) #creates rank for each hosp based on ha
        by_state<-cbind(by_state,ha_order) #adds rank column to state data
        by_state<-rename(by_state,
          Rate = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
          Rank = ha_order) #renames variables 
        by_state<-select(by_state, Hospital.Name, Rate, Rank) # data of interest
        
    }
}