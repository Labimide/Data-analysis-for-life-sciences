## Load Data-----------------------------------------------------------------------------------

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

## Load libraries--------------------------------------------------------------------------------------------

library(dplyr)
library(rafalib)

##--------------------------------------------------------------------------------------------

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

##-------------------------------------------------------------------------------------------

mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

##------------------------------------------------------------------------------------------

set.seed(1)

dat.ns <- sample(bwt.nonsmoke, 25)
dat.s <- sample(bwt.smoke, 25)

tval <- t.test(dat.ns, dat.s)

pval <- 1-(pnorm(abs(tval$statistic))-pnorm(-abs(tval$statistic)))

