x <- c(2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23)
sum(x)/length(x)

y <- 1:25

ss <- 0

for (i in y) {
  if (i <= 25) ss = ss + i^2
}

ss


class(cars)


length(cars)

head(cars)

?which()

which((1:12)%%2 == 0) # which are even?

which(1:10 > 3, arr.ind = F)

which(ll <- c(TRUE, FALSE, TRUE, NA, FALSE, FALSE, TRUE)) #> 1 3 7

names(ll) <- letters[seq(ll)]
which(ll)

which(cars[,2] == 85)

dat <- read.csv("femaleMiceWeights.csv")
head(dat)

dat[12,2]

dat$Bodyweight[11]

length(dat$Diet)

View(dat)

mean(dat$Bodyweight[13:24])

?sample

set.seed(1)
sample(dat$Bodyweight[13:24], size = 1)

library (dplyr)
dat1 <- read.csv("msleep_ggplot2.csv")
class(dat1)

View(dat1)

primates <- filter(dat1, order == "Primates") %>% select(sleep_total) %>% unlist

nrow(primates)

class(primates)

mean(primates)

filter(dat1, order == "Primates") %>% summarize(mean = mean(sleep_total))
