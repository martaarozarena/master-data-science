wheel <- c('D', '7', 'BBB', 'BB', 'B', 'C', '0')
wheel.prob <- c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52)
names(wheel.prob) = wheel

get_symbols <- function(wheel, wheel.prob){
  tirada <- sample(wheel, size = 3, replace = TRUE, prob = wheel.prob)
  return(tirada)
}

score <- function(tirada){
  equal <- length(unique(tirada))==1
  bars <- all(tirada %in% c('B', 'BB', 'BBB'))
  nc <- sum(tirada == 'C')
  
  if (equal & tirada[1]!='0'){
    if (tirada[1]=='DD'){prize <- 100}
    else if (tirada[1]=='7'){prize <- 80}
    else if (tirada[1]=='BBB'){prize <- 40}
    else if (tirada[1]=='BB'){prize <- 25}
    else{prize <- 10}
    print(paste('Congrats ',prize))
  }else if(bars | nc==2){
    prize <- 5
    print(paste('Congrats ',prize))
  }else if(nc==1){
    prize <- 2
    print(paste('Congrats ',prize))
  }else{
    print('No price')
    prize <- 0
  }
  return(prize)
}

play <- function(){
  
  tirada <- get_symbols(wheel, wheel.prob)
  print(tirada)  
  prize <- score(tirada)
  return(prize)
  
}
