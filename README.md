
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

## Background

This package was developed to support eyetracking word recognition
experiments for the [Little
Listeners](https://littlelisteners.waisman.wisc.edu/) project. In these
experiments, images are placed onscreen and followed by a spoken prompt
to view one of the images. We record the participant’s gaze location
over the course of a trial. By aggregating this gaze location over many
trials, we can describe and measure a participant’s word recognition by
showing how their gaze location changes in response to speech.

littlelisteners is my (Tristan Mahr’s) second or third attempt at making
an eyetracking processing package in R. My design goals were to be
generic (work on dataframes of eyetracking data) and not just bespoke
outputs for individual eyetrackers and experiments. The package’s code
tries to play well with the tidyverse as well.
