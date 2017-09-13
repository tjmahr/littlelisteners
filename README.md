
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
#>  1     S01       1      ImageL         A -1000       Target         ImageL
#>  2     S01       1      ImageL         A  -950       Target         ImageL
#>  3     S01       1      ImageL         A  -900       Target         ImageL
#>  4     S01       1      ImageL         A  -850       Target         ImageL
#>  5     S01       1      ImageL         A  -800       Target         ImageL
#>  6     S01       1      ImageL         A  -750   Distractor         ImageR
#>  7     S01       1      ImageL         A  -700       Target         ImageL
#>  8     S01       1      ImageL         A  -650   Distractor         ImageR
#>  9     S01       1      ImageL         A  -600       Target         ImageL
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
#> 1        Target     S01         66    277        51      16     66     277
#> 2        Target     S02         78    193        71      68     78     193
#> 3        Target     S03        114    173       104      19    114     173
#> 4        Target     S04        105    144       118      43    105     144
#> # ... with 4 more variables: Looks <dbl>, Prop <dbl>, PropSE <dbl>,
#> #   PropNA <dbl>
```

Or looks by participant x condition:

``` r
aggregate_looks(two_image_data, response_def, Subject + Condition ~ GazeByTarget)
#> # A tibble: 12 x 13
#>    .response_def Subject Condition Distractor Target Elsewhere Missing
#>            <chr>   <chr>     <chr>      <dbl>  <dbl>     <dbl>   <dbl>
#>  1        Target     S01         A         10     56        13       3
#>  2        Target     S01         B         41    168        31       6
#>  3        Target     S01         C         15     53         7       7
#>  4        Target     S02         A         16     36        16      14
#>  5        Target     S02         B         49    114        42      41
#>  6        Target     S02         C         13     43        13      13
#>  7        Target     S03         A         25     33        21       3
#>  8        Target     S03         B         61    106        64      15
#>  9        Target     S03         C         28     34        19       1
#> 10        Target     S04         A         16     28        28      10
#> 11        Target     S04         B         72     84        68      22
#> 12        Target     S04         C         17     32        22      11
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
#> 1        ImageR     S01    189    154        51      16    189     154
#> 2        ImageR     S02    142    129        71      68    142     129
#> 3        ImageR     S03    150    137       104      19    150     137
#> 4        ImageR     S04    127    122       118      43    127     122
#> # ... with 4 more variables: Looks <dbl>, Prop <dbl>, PropSE <dbl>,
#> #   PropNA <dbl>
```

Or we can handle data from a hypothetical four-image experiment.

``` r
four_image_data
#> # A tibble: 1,640 x 6
#>    Subject TrialNo TargetImage Condition  Time GazeByTarget
#>      <chr>   <int>       <chr>     <chr> <dbl>        <chr>
#>  1     S01       1   LowerLeft         A -1000       Target
#>  2     S01       1   LowerLeft         A  -950       Target
#>  3     S01       1   LowerLeft         A  -900   Competitor
#>  4     S01       1   LowerLeft         A  -850       Target
#>  5     S01       1   LowerLeft         A  -800         <NA>
#>  6     S01       1   LowerLeft         A  -750       Target
#>  7     S01       1   LowerLeft         A  -700       Target
#>  8     S01       1   LowerLeft         A  -650       Target
#>  9     S01       1   LowerLeft         A  -600        FoilB
#> 10     S01       1   LowerLeft         A  -550         <NA>
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
#> 1        Target     S01         88    17    39    171        26      69
#> 2        Target     S02         82    24    35    170        25      74
#> 3        Target     S03         75    25    46    170        19      75
#> 4        Target     S04         76    25    30    168        24      87
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
#> 1     To Foil A     S01         88    17    39    171        26      69
#> 2     To Foil A     S02         82    24    35    170        25      74
#> 3     To Foil A     S03         75    25    46    170        19      75
#> 4     To Foil A     S04         76    25    30    168        24      87
#> 5     To Foil B     S01         88    17    39    171        26      69
#> 6     To Foil B     S02         82    24    35    170        25      74
#> 7     To Foil B     S03         75    25    46    170        19      75
#> 8     To Foil B     S04         76    25    30    168        24      87
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
