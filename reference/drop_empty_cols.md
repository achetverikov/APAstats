# Drop empty columns from a data.frame

Drop empty (consisting of NA only) columns from a data.frame. Based on
https://stackoverflow.com/a/2644009/1344028

## Usage

``` r
drop_empty_cols(df)
```

## Arguments

- df:

  a data.frame

## Value

data.frame without empty columns

## Examples

``` r
df <- data.frame(x = rnorm(20), y = rep("A", 20), z = rep(NA, 20))
str(df)
#> 'data.frame':    20 obs. of  3 variables:
#>  $ x: num  0.3582 0.6289 0.1433 0.0509 -1.3251 ...
#>  $ y: chr  "A" "A" "A" "A" ...
#>  $ z: logi  NA NA NA NA NA NA ...
df <- drop_empty_cols(df)
str(df)
#> 'data.frame':    20 obs. of  2 variables:
#>  $ x: num  0.3582 0.6289 0.1433 0.0509 -1.3251 ...
#>  $ y: chr  "A" "A" "A" "A" ...
```
