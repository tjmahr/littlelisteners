
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
#> 1      S01       1      ImageL         A -1000   Distractor         ImageR
#> 2      S01       1      ImageL         A  -950      tracked        tracked
#> 3      S01       1      ImageL         A  -900   Distractor         ImageR
#> 4      S01       1      ImageL         A  -850   Distractor         ImageR
#> 5      S01       1      ImageL         A  -800   Distractor         ImageR
#> 6      S01       1      ImageL         A  -750      tracked        tracked
#> 7      S01       1      ImageL         A  -700       Target         ImageL
#> 8      S01       1      ImageL         A  -650      tracked        tracked
#> 9      S01       1      ImageL         A  -600      tracked        tracked
#> 10     S01       1      ImageL         A  -550   Distractor         ImageR
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
#> 1     S01        137     81       165      27    137      81   410
#> 2     S02        113    115       115      67    113     115   410
#> 3     S03         99    192       109      10     99     192   410
#> 4     S04        127    103       130      50    127     103   410
#> # ... with 3 more variables: Prop <dbl>, PropSE <dbl>, PropNA <dbl>
```

Or looks by participant x condition:

``` r
aggregate_looks(two_image_data, response_def, Subject + Condition ~ GazeByTarget)
#> # A tibble: 12 x 12
#>    Subject Condition Distractor Target Elsewhere Missing Others Primary
#>      <chr>     <chr>      <dbl>  <dbl>     <dbl>   <dbl>  <dbl>   <dbl>
#> 1      S01         A         41     17        53      12     41      17
#> 2      S01         B         60     46        52       6     60      46
#> 3      S01         C         36     18        60       9     36      18
#> 4      S02         A         34     36        35      18     34      36
#> 5      S02         B         47     48        45      24     47      48
#> 6      S02         C         32     31        35      25     32      31
#> 7      S03         A         25     67        29       2     25      67
#> 8      S03         B         42     77        42       3     42      77
#> 9      S03         C         32     48        38       5     32      48
#> 10     S04         A         38     35        35      15     38      35
#> 11     S04         B         54     41        51      18     54      41
#> 12     S04         C         35     27        44      17     35      27
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
#> 1     S01     96    122       165      27     96     122   410 0.5596330
#> 2     S02    121    107       115      67    121     107   410 0.4692982
#> 3     S03    162    129       109      10    162     129   410 0.4432990
#> 4     S04    112    118       130      50    112     118   410 0.5130435
#> # ... with 2 more variables: PropSE <dbl>, PropNA <dbl>
```

Or we can handle data from a hypothetical four-image experiment.

``` r
four_image_data
#> # A tibble: 1,640 x 6
#>    Subject TrialNo TargetImage Condition  Time GazeByTarget
#>      <chr>   <int>       <chr>     <chr> <dbl>        <chr>
#> 1      S01       1  UpperRight         B -1000       Target
#> 2      S01       1  UpperRight         B  -950      tracked
#> 3      S01       1  UpperRight         B  -900   Competitor
#> 4      S01       1  UpperRight         B  -850       Target
#> 5      S01       1  UpperRight         B  -800       Target
#> 6      S01       1  UpperRight         B  -750         <NA>
#> 7      S01       1  UpperRight         B  -700       Target
#> 8      S01       1  UpperRight         B  -650   Competitor
#> 9      S01       1  UpperRight         B  -600       Target
#> 10     S01       1  UpperRight         B  -550         <NA>
#> # ... with 1,630 more rows

four_image_def <- list(
  primary = c("Target"),
  others = c("FoilA", "FoilB", "Competitor"),
  elsewhere = c("tracked"),
  missing = c(NA)
)

aggregate_looks(four_image_data, four_image_def, Subject ~ GazeByTarget)
#> # A tibble: 4 x 13
#>   Subject Competitor FoilA FoilB Target Elsewhere Missing Others Primary
#>     <chr>      <dbl> <dbl> <dbl>  <dbl>     <dbl>   <dbl>  <dbl>   <dbl>
#> 1     S01         98    20    39    166        26      61    157     166
#> 2     S02         71    27    42    169        28      73    140     169
#> 3     S03         91    19    43    152        30      75    153     152
#> 4     S04         91    13    42    167        28      69    146     167
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
