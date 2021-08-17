# Download dataset
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 

dat <- na.omit(dat)

## Load Libraries-----------------------------------------------------------------------------------

library(dplyr)
library(rafalib)

## Filter for Male-----------------------------------------------------------------------------------------

x <- filter(dat, Sex == "M" & Diet == "chow") %>%
  select(Bodyweight) %>%
    unlist

mx <- mean(x)

sdx <- popsd(x)

##------------------------------------------------------------------------------

set.seed(1)

X <- sample(x, 25)

mX <- mean(X)

##-------------------------------------------------------------------------------------

y <- filter(dat, Sex == "M" & Diet == "hf") %>%
  select(Bodyweight) %>%
  unlist

my <- mean(y)

sdy <- popsd(y)

##------------------------------------------------------------------------------------------------

set.seed(1)

Y <- sample(y, 25)

mY <- mean(Y)

##--------------------------------------------------------------------------------------------

abs(my - mx) - abs(mY - mX)

## Filter for Female------------------------------------------------------------------------------------------
x <- filter(dat, Sex == "F" & Diet == "chow") %>%
  select(Bodyweight) %>%
  unlist

mx <- mean(x)

sdx <- popsd(x)

##------------------------------------------------------------------------------

set.seed(2)

X <- sample(x, 25)

mX <- mean(X)

##-------------------------------------------------------------------------------------

y <- filter(dat, Sex == "F" & Diet == "hf") %>%
  select(Bodyweight) %>%
  unlist

my <- mean(y)

sdy <- popsd(y)

##------------------------------------------------------------------------------------------------

set.seed(2)

Y <- sample(y, 25)

mY <- mean(Y)

##--------------------------------------------------------------------------------------------

abs(my - mx) - abs(mY - mX)

## Checking Normal Distribution for data--------------------------------------------------------------------------------------------

hist(x)
qqnorm(x)

y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
mean( abs(z) <=1 )


## Proportion in male 2sd from mean--------------------------------------------------------------------------------------------

mean( abs(z) <= 2 )

## Proportion in male 3sd from mean--------------------------------------------------------------------------------------------

mean( abs(z) <= 3 )


##-------------------------------------------------------------------------------------
mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)

## using replicate() function----------------------------------------------------------------------------------------------

y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
set.seed(1)
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)

mean(avgs)

popsd(avgs)
