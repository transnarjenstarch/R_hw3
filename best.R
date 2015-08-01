#State variable is column 7

library(dplyr)


best<- function(state,outcome){
    data<-read.csv('outcome-of-care-measures.csv', colClasses='character')
    by_state<-data.frame() #creates empty data frame 
    for (i in 1:nrow(data)) {
        st<-data[i,7] #selects state of each row in loop 
        if(st == state) {
            by_state<-rbind(by_state,data[i,]) #adds all columns of data
        }
    }
    if (outcome == 'heart attack') {
        out<- as.numeric(by_state[,11]) #convert character to numbers, get NAs
        low<-min(out, na.rm = T) 
        low_info<-filter(by_state,by_state[11] == low) #finds row with min value
        hospital<-low_info[1,2]
    } else if (outcome == 'heart failure') {
        out<- as.numeric(by_state[,17])
        low<-min(out, na.rm = T)
        low_info<-filter(by_state,by_state[17] == low)
        hospital<-low_info[1,2]
    } else if (outcome == 'pneumonia') {
        out<-as.numeric(by_state[,23])
        low<-min(out, na.rm = T)
        low_info<-filter(by_state,by_state[23] == low)
        hospital<-low_info[1,2]
    }
    return(hospital)
}


