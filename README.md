
<!-- README.md is generated from README.Rmd. Please edit that file -->
Little Listeners
================

Tools for working with data from word recognition eyetracking experiments.

Installation
------------

First install the devtools package, then install the package from this repository.

``` r
install.packages("devtools")
devtools::install_github("tjmahr/littlelisteners")
```

Overview
--------

``` r
library(dplyr)
library(littlelisteners)
```

### Eyetracking Data

Here's some made-up data for a hypothetical two-image eyetracking experiment.

``` r
two_image_data
#> # A tibble: 1,640 x 7
#>    Subject TrialNo TargetImage Condition  Time GazeByTarget GazeByLocation
#>      <chr>   <int>       <chr>     <chr> <dbl>        <chr>          <chr>
#>  1     S01       1      ImageL         A -1000         <NA>           <NA>
#>  2     S01       1      ImageL         A  -950   Distractor         ImageR
#>  3     S01       1      ImageL         A  -900      tracked        tracked
#>  4     S01       1      ImageL         A  -850       Target         ImageL
#>  5     S01       1      ImageL         A  -800       Target         ImageL
#>  6     S01       1      ImageL         A  -750   Distractor         ImageR
#>  7     S01       1      ImageL         A  -700      tracked        tracked
#>  8     S01       1      ImageL         A  -650       Target         ImageL
#>  9     S01       1      ImageL         A  -600       Target         ImageL
#> 10     S01       1      ImageL         A  -550   Distractor         ImageR
#> # ... with 1,630 more rows
```

Gazes are coded in the `GazeByTarget` column as looks to the `"Target"` or `"Distractor"` image with other looks coded as `"tracked"` (ambiguous/intermediate location) or `NA` (offscreen or missing).

### Response Definitions

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

### `aggregate_looks()`

These response definitions allow us to aggregate looking data in a generic way. The function `aggregate_looks` counts the number of looks to each of the four response categories using an aggregation formula. For example, we can count looks by participant.

``` r
aggregate_looks(two_image_data, response_def, Subject ~ GazeByTarget)
#> # A tibble: 4 x 12
#>   .response_def Subject Distractor Target Elsewhere Missing Others Primary
#>           <chr>   <chr>      <dbl>  <dbl>     <dbl>   <dbl>  <dbl>   <dbl>
#> 1        Target     S01         55    265        37      53     55     265
#> 2        Target     S02        123    140       134      13    123     140
#> 3        Target     S03        134     98       117      61    134      98
#> 4        Target     S04         65    265        58      22     65     265
#> # ... with 4 more variables: Looks <dbl>, Prop <dbl>, PropSE <dbl>,
#> #   PropNA <dbl>
```

Or looks by participant x condition:

``` r
aggregate_looks(two_image_data, response_def, Subject + Condition ~ GazeByTarget)
#> # A tibble: 12 x 13
#>    .response_def Subject Condition Distractor Target Elsewhere Missing
#>            <chr>   <chr>     <chr>      <dbl>  <dbl>     <dbl>   <dbl>
#>  1        Target     S01         A          9     49        14      10
#>  2        Target     S01         B         14     84        13      12
#>  3        Target     S01         C         32    132        10      31
#>  4        Target     S02         A         27     26        26       3
#>  5        Target     S02         B         40     42        38       3
#>  6        Target     S02         C         56     72        70       7
#>  7        Target     S03         A         29     20        22      11
#>  8        Target     S03         B         42     23        36      22
#>  9        Target     S03         C         63     55        59      28
#> 10        Target     S04         A         17     51         9       5
#> 11        Target     S04         B         14     84        18       7
#> 12        Target     S04         C         34    130        31      10
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
#> 1        ImageR     S01    181    139        37      53    181     139
#> 2        ImageR     S02    129    134       134      13    129     134
#> 3        ImageR     S03    109    123       117      61    109     123
#> 4        ImageR     S04    167    163        58      22    167     163
#> # ... with 4 more variables: Looks <dbl>, Prop <dbl>, PropSE <dbl>,
#> #   PropNA <dbl>
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
#>  5     S01       1  UpperRight         B  -800         <NA>
#>  6     S01       1  UpperRight         B  -750         <NA>
#>  7     S01       1  UpperRight         B  -700   Competitor
#>  8     S01       1  UpperRight         B  -650         <NA>
#>  9     S01       1  UpperRight         B  -600         <NA>
#> 10     S01       1  UpperRight         B  -550         <NA>
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
#> 1        Target     S01         84    32    34    164        20      76
#> 2        Target     S02         88    24    27    172        29      70
#> 3        Target     S03         85    28    34    161        27      75
#> 4        Target     S04         79    24    33    171        26      77
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
  label = "To Foil A"
)

to_foilb <- create_response_def(
  primary = c("FoilB"),
  others = c("Target", "FoilA", "Competitor"),
  elsewhere = c("tracked"),
  missing = c(NA),
  label = "To Foil B"
)

defs <- list(to_foila, to_foilb)


aggregate_looks(four_image_data, defs, Subject ~ GazeByTarget)
#> # A tibble: 8 x 14
#>   .response_def Subject Competitor FoilA FoilB Target Elsewhere Missing
#>           <chr>   <chr>      <dbl> <dbl> <dbl>  <dbl>     <dbl>   <dbl>
#> 1     To Foil A     S01         84    32    34    164        20      76
#> 2     To Foil A     S02         88    24    27    172        29      70
#> 3     To Foil A     S03         85    28    34    161        27      75
#> 4     To Foil A     S04         79    24    33    171        26      77
#> 5     To Foil B     S01         84    32    34    164        20      76
#> 6     To Foil B     S02         88    24    27    172        29      70
#> 7     To Foil B     S03         85    28    34    161        27      75
#> 8     To Foil B     S04         79    24    33    171        26      77
#> # ... with 6 more variables: Others <dbl>, Primary <dbl>, Looks <dbl>,
#> #   Prop <dbl>, PropSE <dbl>, PropNA <dbl>
```

### Other features

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
