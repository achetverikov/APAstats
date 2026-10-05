# Formatted rounding

Formatted rounding

## Usage

``` r
f_round(x, digits = 2, strip.lead.zeros = FALSE)
```

## Arguments

- x:

  A number

- digits:

  Number of decimal digits to keep

- strip.lead.zeros:

  remove zero before decimal point (default is false)

## Value

Value A number rounded to the specified number of digits

## Examples

``` r
f_round(5.8242)
#> [1] "5.82"
f_round(5.8251)
#> [1] "5.83"
f_round(5.82999, digits = 3)
#> [1] "5.830"
f_round(5.82999, digits = 4)
#> [1] "5.8300"
```
