install.packages("gapminder")


##------------------------------------------------------------------------------------
library(gapminder)
data(gapminder)
head(gapminder)

library(dplyr)

x <- filter(gapminder,year == 1952) %>% select(lifeExp) %>% unlist

hist(x)

mean(x <= 40)

mean(x>= 40 & x <= 60)

prop = function(q) {
  mean(x <= q)
}

prop(40)

qs = seq(from=min(x), to=max(x), length = 20)


props = sapply(qs, prop)

plot(qs, props)

par(1,2)

plot(ecdf(x))
