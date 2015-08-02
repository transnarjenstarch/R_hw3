
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
        st<-data[i,7] #selects state of each row in loop 
        if(st == state) {
            by_state<-rbind(by_state,data[i,]) #adds all columns of data
        }
    }
    if (outcome == 'heart attack'){
       Rate <- as.numeric(by_state[,11])
       by_state <- cbind(by_state,Rate)
       by_state<- select(by_state,Hospital.Name,Rate)
       sorted <-by_state[order(by_state[,2],by_state[,1], na.last = NA),]
       Rank<-seq_along(sorted[,2])
    
    }
    if (outcome == 'heart failure'){
        Rate <- as.numeric(by_state[,17])
        by_state <- cbind(by_state,Rate)
        by_state<- select(by_state,Hospital.Name,Rate)
        sorted <-by_state[order(by_state[,2],by_state[,1], na.last = NA),]
        Rank<-seq_along(sorted[,2])
    }
    
    if (outcome == '[pnuemonia'){
        Rate <- as.numeric(by_state[,23])
        by_state <- cbind(by_state,Rate)
        by_state<- select(by_state,Hospital.Name,Rate)
        sorted <-by_state[order(by_state[,2],by_state[,1], na.last = NA),]
        Rank<-seq_along(sorted[,2])
    }
    output<-cbind(sorted,Rank)
    if (num == 'best'){ num<- 1}
    if (num == 'worst'){ num<-max(output[,3])}
    return(output[num,1])
   
      
}