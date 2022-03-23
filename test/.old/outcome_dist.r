# t3 distribution
rt(10, df=3)

# truncated
outcome <- rnorm(100)
trunc_point <- quantile(outcome, c(.1, .9)) 

outcome[outcome < trunc_point[1]] <- trunc_point[1]
outcome[outcome > trunc_point[2]] <- trunc_point[2]

outcome <- rnorm(1000000)
outcome[outcome < -2] <- -2
outcome[outcome > 2] <- 2

library(tidyverse)
data.frame(y=outcome) %>% 
  ggplot() +
  # geom_histogram(aes(x=y), 
  #                bins = 20, fill = "white", color = "black") +
  geom_density(aes(x=y), 
               fill = "grey", color = "black") +
  theme_bw()

hist(outcome)

outcome <- rnorm(1000000)
outcome<- outcome[outcome > -2 & outcome < 2]

hist(outcome)

data.frame(y=outcome) %>% 
  ggplot() +
  # geom_histogram(aes(x=y), 
  #                bins = 20, fill = "white", color = "black") +
  geom_density(aes(x=y), 
               fill = "grey", color = "black") +
  theme_bw()



hist(rnorm(10000))

sort(outcome)


p1 <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) + ylab("") +
  scale_y_continuous(breaks = NULL)

mean = 0
sd = 1

c1 = c(-1)
c2 = c(1)

p1 <- pnorm(c1)
p2 <- 1-pnorm(c2)


outcome[outcome < c1]
outcome[outcome > c2]

p1*p2*dnorm(-1:1)





qnorm(3)



