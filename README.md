
<!-- README.md is generated from README.Rmd. Please edit that file -->
little listeners
================

installation
------------

First install the devtools package, then install the package from this repository.

``` r
install.packages("devtools")
devtools::install_github("tjmahr/littlelisteners")
```

implemented functions
---------------------

Functions are being added as needed. As a first step, I've migrated `empirical_logit` functions from [lookr](https://github.com/tjmahr/lookr).

``` r
library("littlelisteners")
n_to_target <- c(10, 0, 1, 0)
n_to_distractor <- c(0, 1, 0, 0)

# undefined log-odds
log(n_to_target / n_to_distractor)
#> [1]  Inf -Inf  Inf  NaN

# add .5 trick
log((n_to_target + .5) / (n_to_distractor + .5))
#> [1]  3.044522 -1.098612  1.098612  0.000000

# shortcut
empirical_logit(n_to_target, n_to_distractor)
#> [1]  3.044522 -1.098612  1.098612  0.000000

# weighting function
empirical_logit_weight(n_to_target, n_to_distractor)
#> [1] 2.095238 2.666667 2.666667 4.000000
```
