
#' Dice Game (Version 1)
#' 
#' This version uses a loop to get the result.
#' 
#' Play a dice game "n" times 
#' Each game costs $2 
#' If you roll a 3 or 5, you win twice the value of your roll.  
#' Display the total winnings (winning - costs) as the result.
#'
#' @param n a positive integer
#'
#' @return Total winning from this dice game
ver1 <- function(n) {
  if(!is.numeric(n)) {
    # This code is from class note (STATS 506)
    stop("n must be a numeric")
  }
  if(n <= 0) { 
    # This code is from class note (STATS 506)
    stop("n must be a positive integer")  
  }
  T_winning <- 0
  dice <- vector(length = n)
  for(i in 1:n) {
    dice[i] <- sample(1:6, 1) 
    if(dice[i] == 3) {
      T_winning <- T_winning + 6 - 2 
    }
    else if(dice[i] == 5) {
      T_winning <- T_winning + 10 - 2 
    }
    else {
      T_winning <- T_winning - 2
    }
  }
  return(T_winning)
}
ver1(10)

#' Dice Game (Version 2)
#' 
#' This version uses builted-in R vectorized functions to get the result.
#' 
#' Play a dice game "n" times 
#' Each game costs $2 
#' If you roll a 3 or 5, you win twice the value of your roll.  
#' Display the total winnings (winning - costs) as the result.
#'
#' @param n a positive integer
#'
#' @return Total winning from this dice game
ver2 <- function(n) {
  if(!is.numeric(n)) {
    # This code is from class note (STATS 506)
    stop("n must be a numeric")
  }
  if(n <= 0) { 
    # This code is from class note (STATS 506)
    stop("n must be a positive integer")  
  }
  dice <- sample(1:6, n, replace = TRUE)
  winning <- sum(dice == 3) * 6 + sum(dice == 5) * 10
  T_winning <- winning - 2*n
  return(T_winning)
}
ver2(10)

#' Dice Game (Version 3)
#' 
#' This version uses a table to get the result.
#' 
#' Play a dice game "n" times 
#' Each game costs $2 
#' If you roll a 3 or 5, you win twice the value of your roll.  
#' Display the total winnings (winning - costs) as the result.
#'
#' @param n a positive integer
#'
#' @return Total winning from this dice game
ver3 <- function(n) { 
  if(!is.numeric(n)) {
    # This code is from class note (STATS 506)
    stop("n must be a numeric")
  }
  if(n <= 0) { 
    # This code is from class note (STATS 506)
    stop("n must be a positive integer")  
  }
  dice <- sample(1:6, n, replace = TRUE)
  dice_table <- table(dice)
  winning <- sum(dice_table["3"]*6, dice_table["5"]*10, na.rm = TRUE)
  T_winning <- winning - 2*n
  return(T_winning)
    }
ver3(10)

#' Dice Game (Version 4)
#' 
#' This version uses "apply" functioins to get the result.
#' 
#' Play a dice game "n" times 
#' Each game costs $2 
#' If you roll a 3 or 5, you win twice the value of your roll.  
#' Display the total winnings (winning - costs) as the result.
#'
#' @param n a positive integer
#'
#' @return Total winning from this dice game
ver4 <- function(n) { 
  if(!is.numeric(n)) {
    # This code is from class note (STATS 506)
    stop("n must be a numeric")
  }
  if(n <= 0) { 
    # This code is from class note (STATS 506)
    stop("n must be a positive integer")  
  }
  dice <- sample(1:6, n, replace = TRUE)
  dice_matrix <- matrix(dice, nrow=1)
  dice_matrix
  winning <- apply(dice_matrix, 1, function(x) { 
    sum(x == 3) * 6 + sum(x == 5) * 10
  }
  )
  T_winning <- winning - 2*n
  return(T_winning)
}

ver1(3)
ver1(3000)
ver2(3)
ver2(3000)
ver3(3)
ver3(3000)
ver4(3)
ver4(3000)

library(microbenchmark)
microbenchmark(ver1(3), ver2(3), ver3(3), ver4(3))
microbenchmark(ver1(3000), ver2(3000), ver3(3000), ver4(3000))

