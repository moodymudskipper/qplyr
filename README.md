
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qplyr

Delayed evaluation with tidyverse verbs, mimicking {dbplyr}’s API.

The benefits are :

- We can keep expensive calculations for later, calling `collect()` when
  needed
- The object contains its execution plan that we can see more clearly
  with `show_query()`
- We can even save a non computed object as a RDS and load it to compute
  it in a new session

It contains no exported function, it just registers methods for the
“quosure” class.

## Installation

Install with:

``` r
remotes::install_github("moodymudskipper/qplyr")
```

## Example

We start from a quosure on a local or lazy data frame and transform it
with tidyverse functions as we would usually do

``` r
library(tidyverse)
library(qplyr)

f1 <- function(x, ...) {
  mutate(x, a = speed * dist, ...)
}

f2 <- function(x, max_speed, other_cond) {
  min_speed <- 16
  filter(x, speed >= min_speed, speed <= {{ max_speed }}, {{ other_cond }})
}

q1 <- quo(cars) |> # note: we could quo a lazy table too
  f1(b = speed /dist) |>
  f2(17, dist == 32)
```

q1 is a regular quosure and prints like 1

``` r
q1
#> <quosure>
#> expr: ^filter(.data = ^mutate(.data = ^cars, a = ^speed * dist, b = ^speed /
#>           dist), ^speed >= min_speed, ^speed <= (^17), ^dist == 32)
#> env:  0x10d3ba2b0
```

The show_query() method prints friendlier and shows environments :

- next to generics: the env where they were called
- next to args (squashed quosures), the quosure’s env label.

``` r
show_query(q1)
#> cars |> # global
#>   mutate( # 0x10d3c0240
#>     a = speed * dist, # 0x10d3c0240
#>     b = speed / dist # global
#>   ) |>
#>   filter( # 0x10d3ba2b0
#>     speed >= min_speed, # 0x10d3ba2b0
#>     speed <= 17, # 0x10d3ba2b0
#>     dist == 32 # global
#>   )
```

The collect method is really just `rlang::eval_tidy()`

``` r
collect(q1)
#>   speed dist   a       b
#> 1    16   32 512 0.50000
#> 2    17   32 544 0.53125
```
