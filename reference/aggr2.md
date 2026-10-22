# Double aggregation

Aggregates value twice providing mean of means, SD of SDs, etc.

## Usage

``` r
aggr2(x, by, fun, ...)
```

## Arguments

- x:

  value to aggregate

- by:

  vector to aggregate by (e.g., ID of participant)

- fun:

  function to apply

- ...:

  additional parameters passed to fun

## Value

The object returned by `fun` when applied to the group summaries. Its
class depends on `fun`; scalar numeric summary functions such as
[base::mean](https://rdrr.io/r/base/mean.html) return one numeric value.

## Examples

``` r

x <- rnorm(100)
id <- rep(1:10, each = 10)

aggregate(x ~ id, FUN = mean)
#>    id          x
#> 1   1  0.2703988
#> 2   2 -0.1530186
#> 3   3  0.1141589
#> 4   4  0.1625241
#> 5   5  0.3340555
#> 6   6  0.0363812
#> 7   7 -0.1121301
#> 8   8  0.5841781
#> 9   9 -0.4975113
#> 10 10  0.4937354
aggr2(x, id, mean)
#> [1] 0.1232772
```
