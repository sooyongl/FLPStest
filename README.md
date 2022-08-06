
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

-   Generate a simulated rectangular data.
-   This data will be converted to a list of data for
    [`rstan`](https://github.com/stan-dev/rstan) package.
-   For latent variable models, Rasch, 2PL, GPCM, and sem (one-factor
    CFA) are available.

``` r
sim_dt <- FLPS::makeInpData(
  N       = 500,  # sample size
  R2Y     = 0.2,  # r^2 of outcome
  R2eta   = 0.5,  # r^2 of eta by two covariates
  omega   = 0.2,  # the effect of eta
  tau0    = 0.13, # direct effect
  tau1    = -0.06,# interaction effect between Z and eta
  linear  = T,    # linearity between the outcome and the covariates
  ydist   = 'n',  # outcome distribution
  lambda  = 0.6,  # the proportion of administered items
  nsec    = 20,   # the total number of items
  nfac    = 1,    # the number of latent factors
  lvmodel = 'rasch' # tag for latent variable model; case-sensitive (use lower-case letters)
)
```

`sim_dt` contains three variables: `sim_info` and `inp_data`.

-   `sim_info`: information about FLPS models

-   `inp_data`: a data frame containing all the data for FLPS. It is
    used in `runFLPS` function.

``` r
# Input data matrix
head(sim_dt$inp_data,5)
```

    ##            Y Z         x1 x2 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15
    ## 1  1.5003687 1  0.9972527  1  0  0  0 NA  0  0 NA  1 NA   0   1   0  NA   1  NA
    ## 2 -1.9171469 1 -1.3221610  0 NA  0  1 NA  0  1  1 NA  1   0  NA  NA   0   1   1
    ## 3 -2.9871150 1 -2.0303319  0  1  1  1 NA  1  1 NA NA NA  NA   1   1   1   1   0
    ## 4 -0.4899533 1 -0.9069553  1  0 NA NA NA NA  1  1  0  0  NA   0   0  NA  NA   1
    ## 5  1.6518906 1  0.4944915  1 NA NA  0 NA NA  0  1  0 NA   0   0   0  NA   1  NA
    ##   i16 i17 i18 i19 i20
    ## 1   0  NA  NA   1  NA
    ## 2   0  NA  NA  NA   0
    ## 3  NA  NA   1   1  NA
    ## 4  NA   1   0   0   0
    ## 5   0   0   0   1  NA

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
