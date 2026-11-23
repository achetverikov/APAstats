# Format mean and confidence interval values into APA-style string

Format mean and confidence interval values into APA-style string

## Usage

``` r
apa_format_mean_conf(
  mean_val,
  lower_ci,
  upper_ci,
  addCI = FALSE,
  digits = 2,
  unit = NULL,
  ...
)
```

## Arguments

- mean_val:

  The mean value

- lower_ci:

  Lower confidence interval bound

- upper_ci:

  Upper confidence interval bound

- addCI:

  Add "95% CI =" prefix

- digits:

  Number of digits to use

- unit:

  Optional suffix appended once after the complete descriptive block.
  Include any desired space or output-specific markup in the suffix, for
  example, `" ms"` or `"~\"ms\""` for plotmath. This labels values
  without converting their scale.

- ...:

  Other arguments passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

A formatted string with mean and confidence intervals

## Examples

``` r
# Example usage
mean_val <- 5.67
lower_ci <- 4.56
upper_ci <- 6.78
# Format mean and CI in APA style
apa_format_mean_conf(mean_val, lower_ci, upper_ci)
#> [1] "_M_ = 5.67 [4.56, 6.78]"
# Format mean and CI with additional CI prefix
apa_format_mean_conf(mean_val, lower_ci, upper_ci, addCI = TRUE)
#> [1] "_M_ = 5.67, 95% _CI_ = [4.56, 6.78]"
# Format mean and CI with custom number of digits
apa_format_mean_conf(mean_val, lower_ci, upper_ci, digits = 3)
#> [1] "_M_ = 5.670 [4.560, 6.780]"
apa_format_mean_conf(350, 320, 380, unit = " ms")
#> [1] "_M_ = 350.00 [320.00, 380.00] ms"
```
