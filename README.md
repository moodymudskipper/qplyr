
<!-- README.md is generated from README.Rmd. Please edit that file -->

# alligator

Experimental!

Delayed evaluation with tidyverse verbs, a bit like {dbplyr} but for
local tables.

## Installation

Install with:

``` r
remotes::install_github("moodymudskipper/alligator")
```

## Example

``` r
library(tidyverse)
library(alligator)

al <-
  alligator(cars) |>
  mutate(a = speed * dist, b = speed /dist) |>
  filter(speed == 17)

# args are stored as quosures and we display their env in comments
al
#> cars |> # global
#>   mutate(
#>     a = speed * dist, # global
#>     b = speed / dist # global
#>   ) |>
#>   filter(
#>     speed == 17 # global
#>   )

# collect to eval
collect(al)
#>   speed dist   a       b
#> 1    17   32 544 0.53125
#> 2    17   40 680 0.42500
#> 3    17   50 850 0.34000
```
