##Loading data-----------------------------------------------------------------------------------

dat <- read.csv("mice_pheno.csv")
chowPopulation <- dat[dat$Sex=="F" & dat$Diet=="chow",3]

##------------------------------------------------------------------------------------------------

B <- 250

mypar()

plot(mean(chowPopulation)+c(-7,7),c(1,1),type="n",
     xlab="weight",ylab="interval",ylim=c(1,B))
abline(v=mean(chowPopulation))

qnorm(1-0.05/2)

##---------------------Loading Libraries-----------------------------------------------------------------------

library(dplyr)
library(rafalib)

##---------------------Loading Data-------------------------------------------------------------------------------

babies <- read.table("babies.txt", header=TRUE)

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

##--------------------------------------------------------------------------------------

mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

##-----------------Sampling Data-----------------------------------------------------------------------------------------

N <- 25

set.seed(1)
dat.ns <- sample(bwt.nonsmoke, N)
dat.s <- sample(bwt.smoke, N)

df <- 2*N-2

Q = 0.01/2

qnorm(0.01/2)

##---------------------------------------------------------------------------------------------

N <- 5

set.seed(1)
dat.ns <- sample(bwt.nonsmoke, N)
dat.s <- sample(bwt.smoke, N)

t.test(dat.s, dat.ns)

t.test(dat.ns, dat.s)


