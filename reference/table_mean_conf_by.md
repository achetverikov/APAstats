# Get a list of means and confidence intervals by group

Get a list of means and confidence intervals by group

## Usage

``` r
table_mean_conf_by(x, by, digits = 2, binom = FALSE)
```

## Arguments

- x:

  numeric vector to compute the mean for

- by:

  grouping variable

- digits:

  number of digits in results (default: 2)

- binom:

  compute binomial CI instead of the usual ones

## Value

a list of means and CI with keys corresponding to the group labels

## Examples

``` r
# Group means for continuous data
x <- rnorm(100, mean = 50, sd = 10)
groups <- rep(c("A", "B", "C"), length.out = 100)
table_mean_conf_by(x, groups)
#> $A
#> [1] "49.96"          "[46.39, 52.90]"
#> 
#> $B
#> [1] "47.10"          "[43.93, 50.26]"
#> 
#> $C
#> [1] "45.93"          "[41.78, 49.69]"
#> 

# Group means for binary data 
y <- rbinom(100, 1, prob = 0.7)
table_mean_conf_by(y, groups, binom = TRUE)
#> $A
#> [1] "0.62"         "[0.45, 0.76]"
#> 
#> $B
#> [1] "0.76"         "[0.59, 0.87]"
#> 
#> $C
#> [1] "0.64"         "[0.47, 0.78]"
#> 
```
