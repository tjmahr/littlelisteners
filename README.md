
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
#>  1     S01       1      ImageR         B -1000   Distractor         ImageL
#>  2     S01       1      ImageR         B  -950       Target         ImageR
#>  3     S01       1      ImageR         B  -900   Distractor         ImageL
#>  4     S01       1      ImageR         B  -850      tracked        tracked
#>  5     S01       1      ImageR         B  -800       Target         ImageR
#>  6     S01       1      ImageR         B  -750      tracked        tracked
#>  7     S01       1      ImageR         B  -700   Distractor         ImageL
#>  8     S01       1      ImageR         B  -650      tracked        tracked
#>  9     S01       1      ImageR         B  -600       Target         ImageR
#> 10     S01       1      ImageR         B  -550       Target         ImageR
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
#> # A tibble: 4 x 12
#>   .response_def Subject Distractor Target Elsewhere Missing Others Primary
#>           <chr>   <chr>      <dbl>  <dbl>     <dbl>   <dbl>  <dbl>   <dbl>
#> 1        Target     S01        106    177       119       8    106     177
#> 2        Target     S02         87    207        97      19     87     207
#> 3        Target     S03         98    138        98      76     98     138
#> 4        Target     S04        105    148        83      74    105     148
#> # ... with 4 more variables: Looks <dbl>, Prop <dbl>, PropSE <dbl>,
#> #   PropNA <dbl>
```

Or looks by participant x condition:

``` r
aggregate_looks(two_image_data, response_def, Subject + Condition ~ GazeByTarget)
#> # A tibble: 12 x 13
#>    .response_def Subject Condition Distractor Target Elsewhere Missing
#>            <chr>   <chr>     <chr>      <dbl>  <dbl>     <dbl>   <dbl>
#>  1        Target     S01         A         31     52        37       3
#>  2        Target     S01         B         54     93        55       3
#>  3        Target     S01         C         21     32        27       2
#>  4        Target     S02         A         21     67        30       5
#>  5        Target     S02         B         46    100        46      13
#>  6        Target     S02         C         20     40        21       1
#>  7        Target     S03         A         33     38        32      20
#>  8        Target     S03         B         50     75        45      35
#>  9        Target     S03         C         15     25        21      21
#> 10        Target     S04         A         36     38        26      23
#> 11        Target     S04         B         50     78        39      38
#> 12        Target     S04         C         19     32        18      13
#> # ... with 6 more variables: Others <dbl>, Primary <dbl>, Looks <dbl>,
#> #   Prop <dbl>, PropSE <dbl>, PropNA <dbl>
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
#> # A tibble: 4 x 12
#>   .response_def Subject ImageL ImageR Elsewhere Missing Others Primary
#>           <chr>   <chr>  <dbl>  <dbl>     <dbl>   <dbl>  <dbl>   <dbl>
#> 1        ImageR     S01    132    151       119       8    132     151
#> 2        ImageR     S02    119    175        97      19    119     175
#> 3        ImageR     S03    116    120        98      76    116     120
#> 4        ImageR     S04    126    127        83      74    126     127
#> # ... with 4 more variables: Looks <dbl>, Prop <dbl>, PropSE <dbl>,
#> #   PropNA <dbl>
```

Or we can handle data from a hypothetical four-image experiment.

``` r
four_image_data
#> # A tibble: 1,640 x 6
#>    Subject TrialNo TargetImage Condition  Time GazeByTarget
#>      <chr>   <int>       <chr>     <chr> <dbl>        <chr>
#>  1     S01       1   UpperLeft         B -1000         <NA>
#>  2     S01       1   UpperLeft         B  -950       Target
#>  3     S01       1   UpperLeft         B  -900       Target
#>  4     S01       1   UpperLeft         B  -850   Competitor
#>  5     S01       1   UpperLeft         B  -800   Competitor
#>  6     S01       1   UpperLeft         B  -750       Target
#>  7     S01       1   UpperLeft         B  -700         <NA>
#>  8     S01       1   UpperLeft         B  -650   Competitor
#>  9     S01       1   UpperLeft         B  -600   Competitor
#> 10     S01       1   UpperLeft         B  -550       Target
#> # ... with 1,630 more rows

four_image_def <- create_response_def(
  primary = c("Target"),
  others = c("FoilA", "FoilB", "Competitor"),
  elsewhere = c("tracked"),
  missing = c(NA)
)

aggregate_looks(four_image_data, four_image_def, Subject ~ GazeByTarget)
#> # A tibble: 4 x 14
#>   .response_def Subject Competitor FoilA FoilB Target Elsewhere Missing
#>           <chr>   <chr>      <dbl> <dbl> <dbl>  <dbl>     <dbl>   <dbl>
#> 1        Target     S01         95    24    37    163        23      68
#> 2        Target     S02         90    25    45    164        24      62
#> 3        Target     S03         85    20    28    191        29      57
#> 4        Target     S04         80    16    43    181        16      74
#> # ... with 6 more variables: Others <dbl>, Primary <dbl>, Looks <dbl>,
#> #   Prop <dbl>, PropSE <dbl>, PropNA <dbl>
```

Including multiple aggreations at once.

``` r
to_foila <- create_response_def(
  primary = c("FoilA"),
  others = c("Target", "FoilB", "Competitor"),
  elsewhere = c("tracked"),
  missing = c(NA),
  id = "To Foil A"
)

to_foilb <- create_response_def(
  primary = c("FoilB"),
  others = c("Target", "FoilA", "Competitor"),
  elsewhere = c("tracked"),
  missing = c(NA),
  id = "To Foil B"
)

defs <- list(to_foila, to_foilb)


aggregate_looks(four_image_data, defs, Subject ~ GazeByTarget)
#> # A tibble: 8 x 14
#>   .response_def Subject Competitor FoilA FoilB Target Elsewhere Missing
#>           <chr>   <chr>      <dbl> <dbl> <dbl>  <dbl>     <dbl>   <dbl>
#> 1     To Foil A     S01         95    24    37    163        23      68
#> 2     To Foil A     S02         90    25    45    164        24      62
#> 3     To Foil A     S03         85    20    28    191        29      57
#> 4     To Foil A     S04         80    16    43    181        16      74
#> 5     To Foil B     S01         95    24    37    163        23      68
#> 6     To Foil B     S02         90    25    45    164        24      62
#> 7     To Foil B     S03         85    20    28    191        29      57
#> 8     To Foil B     S04         80    16    43    181        16      74
#> # ... with 6 more variables: Others <dbl>, Primary <dbl>, Looks <dbl>,
#> #   Prop <dbl>, PropSE <dbl>, PropNA <dbl>
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
