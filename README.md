
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
#>  1     S01       1      ImageR         C -1000      tracked        tracked
#>  2     S01       1      ImageR         C  -950         <NA>           <NA>
#>  3     S01       1      ImageR         C  -900   Distractor         ImageL
#>  4     S01       1      ImageR         C  -850         <NA>           <NA>
#>  5     S01       1      ImageR         C  -800         <NA>           <NA>
#>  6     S01       1      ImageR         C  -750       Target         ImageR
#>  7     S01       1      ImageR         C  -700       Target         ImageR
#>  8     S01       1      ImageR         C  -650         <NA>           <NA>
#>  9     S01       1      ImageR         C  -600       Target         ImageR
#> 10     S01       1      ImageR         C  -550   Distractor         ImageL
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
response_def <- create_response_def(
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
#> 1     S01         74    190        81      65     74     190   410
#> 2     S02         57    241        62      50     57     241   410
#> 3     S03         77    211        80      42     77     211   410
#> 4     S04        126    102        83      99    126     102   410
#> # ... with 3 more variables: Prop <dbl>, PropSE <dbl>, PropNA <dbl>
```

Or looks by participant x condition:

``` r
aggregate_looks(two_image_data, response_def, Subject + Condition ~ GazeByTarget)
#> # A tibble: 12 x 12
#>    Subject Condition Distractor Target Elsewhere Missing Others Primary
#>      <chr>     <chr>      <dbl>  <dbl>     <dbl>   <dbl>  <dbl>   <dbl>
#>  1     S01         A         16     63        30      14     16      63
#>  2     S01         B         17     37        17      11     17      37
#>  3     S01         C         41     90        34      40     41      90
#>  4     S02         A         16     76        18      13     16      76
#>  5     S02         B         10     49        11      12     10      49
#>  6     S02         C         31    116        33      25     31     116
#>  7     S03         A         28     59        26      10     28      59
#>  8     S03         B         15     48        10       9     15      48
#>  9     S03         C         34    104        44      23     34     104
#> 10     S04         A         40     27        24      32     40      27
#> 11     S04         B         23     20        24      15     23      20
#> 12     S04         C         63     55        35      52     63      55
#> # ... with 4 more variables: Looks <dbl>, Prop <dbl>, PropSE <dbl>,
#> #   PropNA <dbl>
```

We can also perform other kind of aggregations using different response definitions. For instance, we can compare left vs right images by writing a new response definition.

``` r
location_def <- create_response_def(
  primary = "ImageR",
  others = "ImageL",
  elsewhere = "tracked",
  missing = NA
)

aggregate_looks(two_image_data, location_def, Subject ~ GazeByLocation)
#> # A tibble: 4 x 11
#>   Subject ImageL ImageR Elsewhere Missing Others Primary Looks      Prop
#>     <chr>  <dbl>  <dbl>     <dbl>   <dbl>  <dbl>   <dbl> <dbl>     <dbl>
#> 1     S01    136    128        81      65    136     128   410 0.4848485
#> 2     S02    124    174        62      50    124     174   410 0.5838926
#> 3     S03    132    156        80      42    132     156   410 0.5416667
#> 4     S04    111    117        83      99    111     117   410 0.5131579
#> # ... with 2 more variables: PropSE <dbl>, PropNA <dbl>
```

Or we can handle data from a hypothetical four-image experiment.

``` r
four_image_data
#> # A tibble: 1,640 x 6
#>    Subject TrialNo TargetImage Condition  Time GazeByTarget
#>      <chr>   <int>       <chr>     <chr> <dbl>        <chr>
#>  1     S01       1  UpperRight         B -1000       Target
#>  2     S01       1  UpperRight         B  -950         <NA>
#>  3     S01       1  UpperRight         B  -900       Target
#>  4     S01       1  UpperRight         B  -850       Target
#>  5     S01       1  UpperRight         B  -800      tracked
#>  6     S01       1  UpperRight         B  -750   Competitor
#>  7     S01       1  UpperRight         B  -700         <NA>
#>  8     S01       1  UpperRight         B  -650         <NA>
#>  9     S01       1  UpperRight         B  -600       Target
#> 10     S01       1  UpperRight         B  -550        FoilB
#> # ... with 1,630 more rows

four_image_def <- create_response_def(
  primary = c("Target"),
  others = c("FoilA", "FoilB", "Competitor"),
  elsewhere = c("tracked"),
  missing = c(NA)
)

aggregate_looks(four_image_data, four_image_def, Subject ~ GazeByTarget)
#> # A tibble: 4 x 13
#>   Subject Competitor FoilA FoilB Target Elsewhere Missing Others Primary
#>     <chr>      <dbl> <dbl> <dbl>  <dbl>     <dbl>   <dbl>  <dbl>   <dbl>
#> 1     S01         85    21    35    171        29      69    141     171
#> 2     S02         85    20    37    166        26      76    142     166
#> 3     S03         81    23    26    183        24      73    130     183
#> 4     S04         68    18    44    171        29      80    130     171
#> # ... with 4 more variables: Looks <dbl>, Prop <dbl>, PropSE <dbl>,
#> #   PropNA <dbl>
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
