## Load Libraries---------------------------------------------------------------------
library(rafalib)
library(dplyr)
## sampling a n dices-------------------------------------------------------------------------

mypar(1, 1)
n = 100

p = 1/6

set.seed(1)

y = replicate(10000, mean((sample(1:6, n, replace = T)) == 6))

z = (y - p) / sqrt(p*(1-p)/n)

mean(abs(z) >= 2)

qqnorm(z)
qqline(z)

##--------------------------------------------------------------------------------

mypar(2,2)

## p = 0.5, n = 5-----------------------------------------------------------------------------------------------
n = 5

p = 0.5

set.seed(1)

y = replicate(10000, mean((sample(1:6, n, replace = T)) == 6))

z = (y - p) / sqrt(p*(1-p)/n)

mean(abs(z) >= 2)

qqnorm(z)
qqline(z)

## p = 0.5, n = 30-----------------------------------------------------------------------------------------------
n = 30

p = 0.5

set.seed(1)

y = replicate(10000, mean((sample(1:6, n, replace = T)) == 6))

z = (y - p) / sqrt(p*(1-p)/n)

mean(abs(z) = 2)

qqnorm(z)
qqline(z)

## p = 0.01, n = 30-----------------------------------------------------------------------------------------------
n = 30

p = 0.01

set.seed(1)

y = replicate(10000, mean((sample(1:6, n, replace = T)) == 6))

z = (y - p) / sqrt(p*(1-p)/n)

mean(abs(z) > 2)

qqnorm(z)
qqline(z)

## p = 0.01, n = 100-----------------------------------------------------------------------------------------------
n = 100

p = 0.01

set.seed(1)

y = replicate(10000, mean((sample(1:6, n, replace = T)) == 6))

z = (y - p) / sqrt(p*(1-p)/n)

mean(abs(z) >= 2)

qqnorm(z)
qqline(z)

##---------------------------------------------------------------------------------------

ps <- c(0.5,0.5,0.01,0.01)
ns <- c(5,30,30,100)
library(rafalib)
mypar(4,2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
    x <- sample(1:sides,n,replace=TRUE)
    (mean(x==1) - p) / sqrt(p*(1-p)/n)
  }) 
  hist(zs,nclass=7)
  qqnorm(zs)
  abline(0,1)
}

##---------------------------------------------------------------------------------------
dat <- read.csv("femaleMiceWeights.csv")

X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist

mean(X)
sd(X)

n = 2

z = sqrt(12)*(n/sd(X))

pnorm(-z) + (1- pnorm(z))

SE <- sqrt((var(Y) + var(X))/12)

t <- (mean(Y) -  mean(X))/SE

##---------------------------------------------------------------------------------------

1 - pt(3,df=3)
1 - pt(3,df=15)
1 - pt(3,df=30)
1 - pnorm(3)

##------------------------------------------------------------------------------------------

2*(1 - pnorm(t))

##------------------------------------------------------------------------------------

t.test(Y, X)
