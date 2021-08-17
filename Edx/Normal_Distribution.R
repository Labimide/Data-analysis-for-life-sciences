# Load libraries
library(dplyr)

# Load data
x <- read.csv("femaleControlsPopulation.csv") %>% unlist


#make averages5
set.seed(1)

n <- 1000

averages5 <- vector("numeric", n)

for (i in 1:n) {
  averages5[i] <- mean( sample( x, 5) )
}

#make averages50
set.seed(1)

n <- 1000

averages50 <- vector("numeric", n)

for (i in 1:n) {
  averages50[i] <- mean( sample( x, 50) )
}

par(mfrow = c(1, 2))

hist(averages5)

hist(averages50)

mean(averages50 >= 23 & averages50 <= 25)

a <- pnorm(25, 23.9, 0.43)

b <- pnorm(23, 23.9, 0.43)

a - b
