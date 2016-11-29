#!/usr/bin/Rscript


# read from stdin
readinteger <- function(){ #unused
  n <- readline("Enter an integer: ") #ohoh -> interactive mode ONLY = not working in shell
  return(as.integer(n))
}


# main game fct
montyhall <- function(){
  cat('\n===== New game =====\n')
  ### setup ###
  retval <- -1
  i <- 1 #misc loop counter (OMG arrays start at 1)
  str.goat <- 'Goat'
  str.car <- 'Car'
  
  doors <- c(str.car, str.goat, str.goat) #compose
  doors <- sample(doors) #randomize
  
  ### player : select door ###
  player.choice <- sample(1:length(doors) , size=1)
  cat("[Player] Choice =", player.choice, '\n')
  
  for(i in 1:length(doors)){
    if((i!=player.choice) && (doors[i]!=str.car)){
      #search door to open : not the player's choice & not the car
      presenter.choice <- i
      cat("[Presenter] Choice =", presenter.choice, "(", doors[presenter.choice], ')\n')
      break()
    }
  }
  
  ### player : keep same door ? ###
  keep <- sample(c(TRUE,FALSE), size=1)
  #keep <- TRUE
  cat('[Player] Keep door', player.choice, '?', keep, '\n')
  
  if(keep==FALSE){
    for(i in 1:length(doors)){
      if((i!=player.choice) && (i!=presenter.choice)){
        #search last door
        player.choice <- i
        break()
      }
    }
  }

  ### final : open door ###
  cat("[Player] Final choice =", player.choice, "(", doors[player.choice], ')\n')
  cat("[Presenter] Doors were :", doors, '\n')
  
  retval=doors[player.choice]
  return(retval) #select first item of randomized array
}


# simulation fct (tested nb.iter times)
stats <- function(nb.iter){
  results <- c() #init empty

  for(i in 1:nb.iter){
    results <- c(results, montyhall())
  }

  return(results)
}


table(stats(100))

