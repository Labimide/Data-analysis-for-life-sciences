##-----------------------Load Data-----------------------------------------------------------------------

babies <- read.table("babies.txt", header=TRUE)

##----------------------Load Libraries----------------------------------------------------------------------

library(dplyr)
library(rafalib)

##----------------------------------------------------------------------------------------------------------

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

##------------------------------------------------------------------------------------------

set.seed(1)

N <- 5

B <- 10000

reject <- function(N, alpha = 0.05){
    bwt.ns <- sample(bwt.nonsmoke, N)
    bwt.s <- sample(bwt.smoke, N)
    pval <- t.test(bwt.ns, bwt.s)$p.value
    pval < alpha
}


power <- function(N, alpha) {
  set.seed(1)
  rejections <- replicate(B, reject(N, alpha = alpha))
  mean(rejections)
}

set.seed(1)
power(60, 0.01) 
              
##------------------------------------------------------------------------------------------

set.seed(1)

power(120)

##-----------------------------------------------------------------------------------------

Ns <- seq(30, 120, 30)

res <- sapply(Ns, function(N, alpha = 0.01){
  power(N, alpha = alpha)
})

Ns[which.min(abs(res - 0.8))]
