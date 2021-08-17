# upload packages

library(dplyr)


# upload data

x <- unlist (read.csv("femaleControlsPopulation.csv"))


# Exercise

## Sampling for 1000 times------------------------------------------------------------------------

set.seed(1)

n = 1000



nulls <- vector("numeric", n)

for (i in 1:n) {
  placebo <- sample( x, 5)
  
  nulls[i] <- mean(placebo)
}

diff <- nulls - mean(x)

mean( abs( diff ) > 1 )

## sampling for 10,000 times------------------------------------------------------------------------------
set.seed(1)

n = 10000



nulls <- vector("numeric", n)

for (i in 1:n) {
  placebo <- sample( x, 5)
  
  nulls[i] <- mean(placebo)
}

diff <- nulls - mean(x)

mean( abs( diff ) > 1 )

## sampling 50 mice for 1000 times------------------------------------------------------------------------------
set.seed(1)

n = 1000



nulls <- vector("numeric", n)

for (i in 1:n) {
  placebo <- sample( x, 50)
  
  nulls[i] <- mean(placebo)
}

diff <- nulls - mean(x)

mean( abs( diff ) > 1 )