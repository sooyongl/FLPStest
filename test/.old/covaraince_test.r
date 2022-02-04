library(lavaan)

models<-"
y1 ~ -1*x1 + 0.5*x2
y2 ~ -1*x1 + 0.5*x2

y1 ~~ .8*y1
y2 ~~ .8*y2

x1 ~~ 1*x1
x2 ~~ 1*x2

y1 ~~ 0.1*y2
x1 ~~ 0*x2

y1 ~ 3*1
y2 ~ 0.2*1

x1 ~ 0*1
x2 ~ 0*1
"
sem(models, meanstructure = T) %>% summary()
sem(models,meanstructure = T) %>% lavInspect(what="est")
sem(models,meanstructure = T) %>% fitted()

round(cov(data), 3)
round(colMeans(data), 3)
