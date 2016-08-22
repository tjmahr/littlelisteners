
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

overview
--------

``` r
library(dplyr)
library(littlelisteners)
```

### eyetracking data

Here's some made-up data for a hypothetical two-image eyetracking experiment.

``` r
two_image_data
#> # A tibble: 1,640 x 7
#>    Subject TrialNo TargetImage Condition  Time GazeByTarget GazeByLocation
#>      <chr>   <int>       <chr>     <chr> <dbl>        <chr>          <chr>
#> 1      S01       1      ImageL         C -1000       Target         ImageL
#> 2      S01       1      ImageL         C  -950   Distractor         ImageR
#> 3      S01       1      ImageL         C  -900       Target         ImageL
#> 4      S01       1      ImageL         C  -850       Target         ImageL
#> 5      S01       1      ImageL         C  -800         <NA>           <NA>
#> 6      S01       1      ImageL         C  -750       Target         ImageL
#> 7      S01       1      ImageL         C  -700       Target         ImageL
#> 8      S01       1      ImageL         C  -650   Distractor         ImageR
#> 9      S01       1      ImageL         C  -600       Target         ImageL
#> 10     S01       1      ImageL         C  -550      tracked        tracked
#> # ... with 1,630 more rows
```

Gazes are coded in the `GazeByTarget` column as looks to the `"Target"` or `"Distractor"` image with other looks coded as `"tracked"` (ambiguous/intermediate location) or `NA` (offscreen or missing).

### response definitions

To deal with eyetracking data in a generic way, we need a way to describe eyetracking responses. We assume that there are four basic gaze types.

-   Primary responses: A gaze to a primary or target image.
-   Others: A gaze to a competing image.
-   Elsewhere: A gaze that is onscreen but not a primary or other response. Typically, this occurs when the participant is shifting between images.
-   Missing: A missing or offscreen gaze.

A *response definition* is a programmatic way of mapping gaze codes to these response categories. In the code below, `response_def` is a response definition for a two-image experiment with a `"Target"` image and `"Distractor"` and other looks are either `"tracked"` or missing (`NA`).

``` r
response_def <- list(
  primary = "Target",
  others = "Distractor",
  elsewhere = "tracked",
  missing = NA
)
```

### `aggregate_looks`

These response definitions allow us to aggregate looking data in a generic way. The function `aggregate_looks` counts the number of looks to each of the four response categories using an aggregation formula. For example, we can count looks by participant.

``` r
aggregate_looks(two_image_data, response_def, Subject ~ GazeByTarget)
#> # A tibble: 4 x 11
#>   Subject Distractor Target Elsewhere Missing Others Primary Looks
#>     <chr>      <dbl>  <dbl>     <dbl>   <dbl>  <dbl>   <dbl> <dbl>
#> 1     S01         85    212        73      40     85     212   410
#> 2     S02        110    130       117      53    110     130   410
#> 3     S03        112    148       124      26    112     148   410
#> 4     S04         61    253        52      44     61     253   410
#> # ... with 3 more variables: Prop <dbl>, PropSE <dbl>, PropNA <dbl>
```

Or looks by participant x condition:

``` r
aggregate_looks(two_image_data, response_def, Subject + Condition ~ GazeByTarget)
#> # A tibble: 12 x 12
#>    Subject Condition Distractor Target Elsewhere Missing Others Primary
#>      <chr>     <chr>      <dbl>  <dbl>     <dbl>   <dbl>  <dbl>   <dbl>
#> 1      S01         A         29     61        24       9     29      61
#> 2      S01         B          8     16        10       7      8      16
#> 3      S01         C         48    135        39      24     48     135
#> 4      S02         A         36     43        28      16     36      43
#> 5      S02         B         11     13        11       6     11      13
#> 6      S02         C         63     74        78      31     63      74
#> 7      S03         A         36     43        37       7     36      43
#> 8      S03         B         14     13        11       3     14      13
#> 9      S03         C         62     92        76      16     62      92
#> 10     S04         A         18     76        16      13     18      76
#> 11     S04         B          6     24         9       2      6      24
#> 12     S04         C         37    153        27      29     37     153
#> # ... with 4 more variables: Looks <dbl>, Prop <dbl>, PropSE <dbl>,
#> #   PropNA <dbl>
```

We can also perform other kind of aggregations using different response definitions. For instance, we can compare left vs right images by writing a new response definition.

``` r
location_def <- list(
  primary = "ImageR",
  others = "ImageL",
  elsewhere = "tracked",
  missing = NA
)

aggregate_looks(two_image_data, location_def, Subject ~ GazeByLocation)
#> # A tibble: 4 x 11
#>   Subject ImageL ImageR Elsewhere Missing Others Primary Looks      Prop
#>     <chr>  <dbl>  <dbl>     <dbl>   <dbl>  <dbl>   <dbl> <dbl>     <dbl>
#> 1     S01    187    110        73      40    187     110   410 0.3703704
#> 2     S02    126    114       117      53    126     114   410 0.4750000
#> 3     S03    137    123       124      26    137     123   410 0.4730769
#> 4     S04    209    105        52      44    209     105   410 0.3343949
#> # ... with 2 more variables: PropSE <dbl>, PropNA <dbl>
```

### others

I've also migrated `empirical_logit` functions from [lookr](https://github.com/tjmahr/lookr).

``` r
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
