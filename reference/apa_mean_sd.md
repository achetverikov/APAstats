# Format means and standard deviations in APA style

Format means and standard deviations in APA style

## Usage

``` r
apa_mean_sd(
  x = NULL,
  m = NULL,
  sd = NULL,
  digits = 2,
  dtype = "p",
  m_units = "",
  sd_units = "",
  ...
)
```

## Arguments

- x:

  A numeric vector

- m:

  Pre-computed mean (optional)

- sd:

  Pre-computed standard deviation (optional)

- digits:

  Number of digits for rounding

- dtype:

  Format type: "p" for parentheses or "c" for comma

- m_units:

  Units for mean (e.g., "°" for degrees)

- sd_units:

  Units for standard deviation

- ...:

  Additional arguments passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

A formatted string with mean and standard deviation in APA style

## Examples

``` r
x <- rnorm(1000, 50, 25)
apa_mean_sd(x)
#> [1] "_M_ = 50.38 (_SD_ = 24.20)"
apa_mean_sd(m = 50.2, sd = 9.8)  # With pre-computed values
#> [1] "_M_ = 50.20 (_SD_ = 9.80)"
apa_mean_sd(x, dtype = "c")  # Comma format instead of parentheses
#> [1] "_M_ = 50.38, _SD_ = 24.20"
```
