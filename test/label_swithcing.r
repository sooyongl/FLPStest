rm(list = ls())
library(rstan); library(tidyverse)
for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)
source_funs <- ls()

res_list <- fs::dir_ls("results", regexp = "rds$")

i <- 6

model_tykepe <- str_split(res_list[i], "_", simplify = T)[2]
scaley <- str_split(res_list[i], "_", simplify = T)[3]

res <- readRDS(res_list[i])

fit <- res$fit
sdat <- res$sdat

traceplot(fit, c("b00","b0","b1","a1"))

library("label.switching")

post_par <- rstan::extract(fit,
                           c("b00","b0","b1","a1","lp__"),
                           permuted = TRUE)


post_par
