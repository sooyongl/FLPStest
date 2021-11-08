
## FLPS

Fully Latent Principal Stratification

Fully Latent Principal Stratification (**FLPS**) is an extension of
principal stratification.

## Install

Install the latest release from CRAN:

``` r
devtools::install_github("sooyongl/FLPS")
```

The documentation is available at (…)

## Basic working example

-   Set up simulation factors

``` r
# simulation factors ------------------------------------------------------
parsFromMod <- list(
  N = 500, # sample size
  R2Y = 0.2, ## from app
  omega = 0.2,
  tau0 = 0.13, ## from paper
  tau1 = -0.06, ## from paper "a difference of one IQR in etaT was associated with a reduction of 0.083 in the effect size" 0.083/1.35~~ 0.06
  lambda = 10, ## from data used in model
  R2eta = 0.2, ## from app
  nsec = 20, ## from data used in model
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
res <- FLPS::runSimulation(parsFromMod)
```
