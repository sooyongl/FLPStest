library(tidyverse)
library(ggpubr)

parsFromMod <- list(
  N = 1000, # sample size
  R2Y = 0.2, ## from app
  omega = 0.2,
  tau0 = 0.13, ## from paper
  tau1 = 0, ## from paper "a difference of one IQR in etaT was associated with a reduction of 0.083 in the effect size" 0.083/1.35~~ 0.06
  lambda = 40, ## from data used in model
  R2eta = 0.5, ## from app
  nsec = 130, ## from data used in model
  lvmodel = "rasch" # tag for latent variable model
)


# set.seed(i+N+nsec)
reg_model <- function() {
  sdat <- do.call("makeDat", parsFromMod)
  # str(sdat)
  pop.data <- with(sdat, data.frame(Y, Z, X))
  fit <- lm(Y ~ Z + x1 + x2, pop.data)
  c(coefficients(fit), Y = mean(sdat$Y))
}

pop_dist <- map_df(1:1000, ~ reg_model())
saveRDS(pop_dist, "pop_dist.rds")

pop_dist <- readRDS("pop_dist.rds") %>%
  rename("Intercept" = "(Intercept)")

pop_dist <- pop_dist %>%
  rename("Intercept" = "(Intercept)")

0.13  - sd(pop_dist$Z) * 1.96

tau0 <- pop_dist %>% 
  ggplot() +
  geom_histogram(aes(x = Z)) +
  geom_vline(xintercept = mean(pop_dist$Z)) +
  annotate("label", label = round(mean(pop_dist$Z), 3),
           x = round(mean(pop_dist$Z), 3), y = 0) +
  xlab("coef of Z (tau0)") +
  scale_x_continuous(n.breaks = 10) +
  theme_bw(base_size = 14)

intercept <- pop_dist %>% 
  ggplot() +
  geom_density(aes(x = Intercept)) +
  geom_vline(xintercept = mean(pop_dist$Intercept)) +
  annotate("label", label = round(mean(pop_dist$Intercept), 3),
           x = round(mean(pop_dist$Intercept), 3), y = 0) +
  xlab("Intercept") +
  scale_x_continuous(n.breaks = 10) +
  theme_bw(base_size = 14)


Y <- pop_dist %>% 
  ggplot() +
  geom_density(aes(x = Y)) +
  geom_vline(xintercept = mean(pop_dist$Y)) +
  annotate("label", label = round(mean(pop_dist$Y), 3),
           x = round(mean(pop_dist$Y), 3), y = 0) +
  xlab("Mean of Y") +
  scale_x_continuous(n.breaks = 10) +
  theme_bw(base_size = 14)

pp <- ggarrange(Y, intercept, tau0, ncol = 3)
annotate_figure(pp, 
                top = text_grob("Y = b0 + b1*Z + b2*X1 + b3*X2",
                                color = "blue", face = "bold", size = 14))





# -------------------------------------------------------------------------

res <- lapply(1:5000, function(x) 
{
    N <- 1000; R2Y <- 0.2; tau0 <- 0.13; tau1 <- -0.06; omega <- 0.2
  ### Treatment or Control
  Z <- rep(c(1,0),each=N/2)
  # Generate True eta -------------------------------------------------------
  ## generate etas
  random.e <- rnorm(N,0,sqrt(1)); random.e <- random.e - mean(random.e); random.e <- random.e* (sqrt(1-R2eta) / sd(random.e));
  
  eta <- random.e
  
  ### simulate Y -------------------------------------------------------------
  random.Y <- rnorm(N,0,sqrt(1-R2Y)); random.Y <- random.Y - mean(random.Y); random.Y <- random.Y * (sqrt(1-R2Y)/sd(random.Y))
  
  Y <- tau0*Z + omega*eta + tau1*eta*Z + random.Y
  
  # check ----------------------------
  data <- data.frame(Y, Z, eta); apply(data, 2, sd); var(random.Y)
  # fit <- lm(Y ~ Z + eta, data = data)
  # var(fit$residuals)
  # coefficients(fit)
  # 
  # fit <- lm(Y ~ Z, data = data)
  # var(fit$residuals)
  # coefficients(fit)
  # 
  fit <- lm(Y ~ Z*eta, data = data)
  
  c(coefficients(fit),var(fit$residuals))
  
})
res2 <- do.call('rbind',res)
colMeans(res2)
apply(res2, 2, var)

plot(density(res2[,2]))


