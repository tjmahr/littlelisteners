
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Little Listeners

<!-- badges: start -->

[![R-CMD-check](https://github.com/tjmahr/littlelisteners/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tjmahr/littlelisteners/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

Tools for working with data from word recognition eyetracking
experiments.

## Installation

First install the devtools package, then install the package from this
repository.

``` r
install.packages("devtools")
devtools::install_github("tjmahr/littlelisteners")
```

## Learn more

See the [documentation
website](https://tjmahr.github.io/littlelisteners/) for package
tutorials and documentation.

<!-- ### Other features -->
<!-- I've also migrated `empirical_logit` functions from [lookr](https://github.com/tjmahr/lookr). -->
<!-- ```{r} -->
<!-- n_to_target <- c(10, 0, 1, 0) -->
<!-- n_to_distractor <- c(0, 1, 0, 0) -->
<!-- # undefined log-odds -->
<!-- log(n_to_target / n_to_distractor) -->
<!-- # add .5 trick -->
<!-- log((n_to_target + .5) / (n_to_distractor + .5)) -->
<!-- # shortcut -->
<!-- empirical_logit(n_to_target, n_to_distractor) -->
<!-- # weighting function -->
<!-- empirical_logit_weight(n_to_target, n_to_distractor) -->
<!-- ``` -->
