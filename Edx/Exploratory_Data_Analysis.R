install.packages("UsingR")
library(UsingR)

head(father.son)

x = father.son$fheight

## -------------------------------------------------------------------

hist(x, main = "Histo 1")

hist(x, breaks = seq(floor(min(x)), ceiling(max(x))))

## -------------------------------------------------------------------

xs <- seq(floor(min(x)), ceiling(max(x)))
plot(xs, ecdf(x)(xs), type = "l")


## -------------------------------------------------------------------------

load("skew.RData")
dim(dat)
par(mfrow = c(3, 3))

for (i in 1:9) {
 qqnorm(dat[,i])
  qqline(dat[,i])
}

## ---------------------------------------------------------------------------

head(InsectSprays)

values = InsectSprays$count
factor = InsectSprays$spray

par(mfrow = c(1, 2))

boxplot(split(values, factor), main = "Type A")

boxplot(values ~ factor, main = "Type B")

## ---------------------------------------------------------------------------

library(dplyr)
data(nym.2002, package="UsingR")

head(nym.2002)

par(mfrow = c(2, 3))

boxplot(split(nym.2002$time, nym.2002$gender))

hist(nym.2002$time)

female <- filter(nym.2002, gender == "Female") %>% select(time) %>% unlist


male <- filter(nym.2002, gender == "Male") %>% select(time) %>% unlist

hist(female)

hist(male)

mean(female) - mean(male)

boxplot(female, male)
