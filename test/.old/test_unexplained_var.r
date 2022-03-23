X <- rnorm(1000, 3, 5)

Y <- 0.5 * X

exvar <- 0.5

unexpvar <- exvar / (1-exvar) * var(Y)

Y <- Y + rnorm(1000, 0, sqrt(unexpvar))


fit <- lm(Y ~ X, data = data.frame(Y=Y, X=X))
summary(fit)
