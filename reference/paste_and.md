# Paste several strings, add 'and' before last

Paste several strings, add 'and' before last

## Usage

``` r
paste_and(x, sep = ", ", suffix = "")
```

## Arguments

- x:

  vector of strings

- sep:

  separator (is not used for only two groups)

- suffix:

  suffix to append to each value before the separator

## Value

a string iterating the values in x

## Examples

``` r
data(iris)
# get mean petal width and SD by group
res <- as.vector(by(iris$Sepal.Width, iris$Species, apa_mean_sd))
res
#> [1] "_M_ = 3.43 (_SD_ = 0.38)" "_M_ = 2.77 (_SD_ = 0.31)"
#> [3] "_M_ = 2.97 (_SD_ = 0.32)"
paste_and(res)
#> [1] "_M_ = 3.43 (_SD_ = 0.38), _M_ = 2.77 (_SD_ = 0.31), and _M_ = 2.97 (_SD_ = 0.32)"
paste_and(res, sep = ";")
#> [1] "_M_ = 3.43 (_SD_ = 0.38);_M_ = 2.77 (_SD_ = 0.31);and _M_ = 2.97 (_SD_ = 0.32)"

data(memory_noise)
# get bias and SD in the two experiment samples
res <- as.vector(by(
  memory_noise$bias_percent,
  memory_noise$experiment,
  apa_mean_sd
))
res
#> [1] "_M_ = 2.63 (_SD_ = 17.39)"  "_M_ = -5.55 (_SD_ = 15.98)"
# no comma with two groups
paste_and(res)
#> [1] "_M_ = 2.63 (_SD_ = 17.39) and _M_ = -5.55 (_SD_ = 15.98)"
paste_and(res, suffix = " percentage points")
#> [1] "_M_ = 2.63 (_SD_ = 17.39) percentage points and _M_ = -5.55 (_SD_ = 15.98) percentage points"
```
