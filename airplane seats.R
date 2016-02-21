#for the February 19th puzzler
#http://fivethirtyeight.com/features/will-someone-be-sitting-in-your-seat-on-the-plane/

### This is the function to run the simulation a single time.
simulation <- function(){
        #one hundred people, one hundred seats.
        seats <- 1:100
        for(i in 1:100){
                if(i==1){ #Enter the Villain.
                        seatpicked <- as.numeric(sample(seats,1))
                        seats[seatpicked] <- NaN #Taken seats become NaN
                        next
                
                }
                if(i==100){ #This is you. True means your seat is open.
                        result <- ifelse(is.na(unlist(seats[100])), FALSE, TRUE)
                        return(result)
                }
                else{ #for everybody else... Follow this procedure.
                      if(is.na(unlist(seats[i]))){ #if their seat is taken, pick a random one.
                              seatpicked <- as.numeric(sample(seats[!is.na(seats)],1))
                              seats[seatpicked] <- NaN
                              next
                      }
                      else{
                              seats[i] <- NaN #Take their seat if it is open.
                              next
                      }  
                }
        }
}

### This is the meta function to run the sim a given number of times.
runsim <- function(iterations=1000){
        results <- data.frame()
        for(j in 1:iterations){
                results <- rbind(results,simulation())
        }
        return(results)
}
        
        