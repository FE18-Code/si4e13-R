#!/usr/bin/Rscript


# read from stdin
readinteger <- function(){ #unused
  n <- readline("Enter an integer: ") #ohoh -> interactive mode ONLY = not working in shell
  return(as.integer(n))
}


# main game fct
montyhall <- function(door.keep='random'){
  cat('\n===== New game =====\n')
  ### setup ###
  retval <- -1
  i <- 1 #misc loop counter (OMG arrays start at 1)
  str.goat <- 'Goat'
  str.car <- 'Car'
  
  if((door.keep!='random') && (door.keep!='keep') && (door.keep!='change')){
    door.keep <- 'random' #whooooo, what have you done with the config child ??
  }
  cat('Strategy : door =', door.keep, '\n')

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
  if(door.keep=='keep'){
    keep <- TRUE
  }else if(door.keep=='change'){
    keep <- FALSE
  }else{ #random : redundant with keep.door='random' 
    keep <- sample(c(TRUE,FALSE), size=1)
  }
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


# simulation fct
stats <- function(){
  nb.iter <- 1000
  results.random <- c() #init empty
  results.keepdoor <- c() #init empty
  results.changedoor <- c() #init empty

  #run tests
  for(i in 1:nb.iter){
    results.random <- c(results.random, montyhall(door.keep='random'))
  }
  for(i in 1:nb.iter){
    results.keepdoor <- c(results.keepdoor, montyhall(door.keep='keep'))
  }
  for(i in 1:nb.iter){
    results.changedoor <- c(results.changedoor, montyhall(door.keep='change'))
  }

  #format results
  table.random <- table(results.random)
  table.keepdoor <- table(results.keepdoor)
  table.changedoor <- table(results.changedoor)

  results.all <- matrix(c(table.random, table.keepdoor, table.changedoor), ncol=2, byrow=TRUE)
  colnames(results.all) <- c("Cars", "Goats")
  rownames(results.all) <- c("Random", "Keep door", "Change door")
  results.all <- as.table(results.all)
  
  #display results
  cat('\n\n===== RESULTS (results vs strategy for', nb.iter, 'tests) =====\n\n')
  print(results.all)
  cat('\n===== RESULTS (results vs strategy for', nb.iter, 'tests) =====\n\n')


  return(0)
}

stats()
