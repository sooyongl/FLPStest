
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
  N = 500,              # sample size
  R2Y = 0.2,            # r^2 of outcome
  R2eta = 0.2,          # r^2 of eta by two covariates
  omega = 0.2,          # the effect of eta
  tau0 = 0.13,          # direct effect
  tau1 = -0.06,         # interaction effect between Z and eta
  lambda = 10,          # the mean of administered items
  nsec = 20,            # the number of items
  lvmodel = "Rasch"     # tag for latent variable model; case-nonsensitive
)
```

-   Generate a set of simulated data for
    [`rstan`](https://github.com/stan-dev/rstan) package.

``` r
dt <- do.call(FLPS::makeInpData, parsFromMod)
```

`dt` contains three variables: `lv.par`, `true_eta`, and `inp_data`.

-   `lv.par`: information about latent variable models

-   `true_eta`: True factor scores

-   `inp_data`: a matrix containing all the data for FLPS. It is used in
    `runFLPS` function.

``` r
# Input data matrix
head(dt$inp_data,5)
```

    ##            Y Z          x1         x2 X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12
    ## 1  0.6659478 1 -0.46700688  2.0108626  1 NA NA NA NA NA  1 NA NA  NA  NA  NA
    ## 2  1.9291701 1  1.22900561  0.2660477 NA  0  0 NA  1 NA  0 NA  1  NA  NA  NA
    ## 3  1.1205595 1 -0.55433973 -0.6977031 NA NA NA NA NA NA NA  1 NA  NA  NA  NA
    ## 4 -0.4231403 1 -0.89700570 -0.5831692 NA  1  1 NA NA NA  0 NA NA  NA  NA  NA
    ## 5 -0.2065087 1  0.02736169  0.7790387  0 NA NA NA  0  0  0  1  0   0   0   0
    ##   X13 X14 X15 X16 X17 X18 X19 X20
    ## 1   1   0   1  NA   1  NA  NA  NA
    ## 2   0  NA  NA  NA  NA   1  NA  NA
    ## 3  NA  NA  NA  NA  NA  NA  NA  NA
    ## 4   0  NA  NA   0  NA  NA   0   0
    ## 5   0   0  NA   0   0   0   0  NA

-   Fit your FLPS model

Now, provide information about your model. `runFLPS` coverts `inp_data`
into the data format for `rstan` given the information, and runs FLPS.

``` r
res <- FLPS::runFLPS(
  inp_data = sim_dt$inp_data,
  outcome = "Y",
  group = "Z",
  covariate = c("x1","x2"),
  lv_type = "rasch",
  lv_model = paste0("F =~ ", paste(paste0("X", 1:20), collapse = "+")),
  stan_options = list(iter = 4000, warmup = 1000, cores = 1, chains = 4)
)
```
