
## Fully Latent Principal Stratification (FLPS)

Fully Latent Principal Stratification (**FLPS**) is an extension of
principal stratification.

## Install

Install the latest release from CRAN:

``` r
devtools::install_github("sooyongl/FLPS")
```

The documentation is available at (…)

## Basic working example

### Running with the package

-   Set up simulation factors

For latent variable models, Rasch, 2PL, GPCM, and sem (one-factor CFA)
are available.

``` r
parsFromMod <- list(
  N = 500, # sample size
  R2Y = 0.2, # r^2 of outcome
  R2eta = 0.2, ## r^2 of eta by two covariates
  omega = 0.2, # the effect of eta
  tau0 = 0.13, ## direct effect
  tau1 = -0.06, ## interaction effect between Z and eta
  lambda = 10, # the mean of administered items
  nsec = 20, # the number of items
  lvmodel = "Rasch" # tag for latent variable model
)
```

-   Generate a set of simulated data for
    [\`rstan\`\`](https://github.com/stan-dev/rstan) package.

``` r
dt <- FLPS::makeDat()
```

-   Fit your FLPS model

``` r
res <- FLPS::runSimulation(parsFromMod, iter = 4000, warmup = 1000, cores = 1, chains = 1)
```

### Running directly from the R project (without installing the package)

``` r
for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)

library(coda)
library(rstan)
library(tidyverse)

parsFromMod <- list(
  N = 1000, 
  R2Y = 0.2,
  omega = 0.2,
  tau0 = 0.13,
  tau1 = -0.06,
  lambda = 10,
  R2eta = 0.2,
  nsec = 20,
  lvmodel = "2PL"
)

sdat <- do.call("makeDat", parsFromMod)

fit <- stan(
  cores = 1,
  "inst/stan/psIRT.stan",
  data = sdat,
  # iter = 4000,
  # warmup = 1000,
  chains = 1
)
```
