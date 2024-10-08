# Problem 1 - Dice Game
# a. Implementation of dice games

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
ver1(10) # a game result using a loop

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
ver2(10) # a game result using built-in R vectorized function 

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
ver3(10) # a game result collapsing the dice rolls into a table


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
ver4(10) # a game result using one of the "apply" functions

# b. Demonstration that all versions work

ver1(3)    # Version 1 with input 3
ver1(3000)  # Version 1 with input 3,000
ver2(3)    # Version 2 with input 3
ver2(3000)  # Version 2 with input 3,000
ver3(3)    # Version 3 with input 3
ver3(3000)  # Version 3 with input 3,000
ver4(3)   # Version 4 with input 3
ver4(3000) # Version 4 with input 3,000

# c. demonstration that all versions work
?set.seed

set.seed(3) # Control the randomization for input 3
ver1(3)    # Version 1 with input 3
set.seed(3) # Control the randomization for input 3
ver2(3)    # Version 2 with input 3
set.seed(3) # Control the randomization for input 3
ver3(3)    # Version 3 with input 3
set.seed(3) # Control the randomization for input 3
ver4(3)   # Version 4 with input 3

set.seed(3000) # Control the randomization for input 3,000
ver1(3000)  # Version 1 with input 3,000
set.seed(3000) # Control the randomization for input 3,000
ver2(3000)  # Version 2 with input 3,000
set.seed(3000) # Control the randomization for input 3,000
ver3(3000)  # Version 3 with input 3,000
set.seed(3000) # Control the randomization for input 3,000
ver4(3000) # Version 4 with input 3,000

#d. Demonstration of the speed of the 

library(microbenchmark)

# The speed with input 1,000
microbenchmark(ver1(1000), ver2(1000), ver3(1000), ver4(1000))

# The speed with input 100,000
microbenchmark(ver1(100000), ver2(100000), ver3(100000), ver4(100000))

#e. Decision on whether this game is fair or not

# From "problem c" above, you can see that the total returns from large trials amount to 2,124. The total expected return for one trial of this game is calculated as 2/3(6*1/6 + 10*1/6 - 2). Because the player can expect a positive total income, this game is not fair.
