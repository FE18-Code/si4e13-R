#!/usr/bin/env Rscript

montyhall <- function(){
  goat <- 'Goat'
  car <- 'Car'
  
  list <- c(car, goat, goat) #compose
  list <- sample(list, length(list)) #randomize
  
  #print(list)
  
  return(list[1]) #select first item of randomized array
}

montyhall()
