# Describe brms model results

Describe brms model results

## Usage

``` r
# S3 method for class 'brmsfit'
apa(
  obj,
  term,
  trans = NULL,
  digits = 2,
  eff.size = FALSE,
  eff.size.type = "r",
  nsamples = 100,
  ci.type = "HPDI",
  ...
)
```

## Arguments

- obj:

  model object from
  [brms::brm](https://paulbuerkner.com/brms/reference/brm.html)

- term:

  model term to describe

- trans:

  an optional function to transform the results to another scale
  (default: NULL)

- digits:

  number of digits in the output

- eff.size:

  string describing how to compute an effect size (currently, either
  'fe_to_all', 'part_fe', or 'part_fe_re')

- eff.size.type:

  type of the effect size ('r' or 'r2')

- nsamples:

  number of samples to use for the effect size computations

- ci.type:

  type of intervals to use (currently, all that is not HPDI is treated
  as ETI using
  [`bayestestR::eti`](https://easystats.github.io/bayestestR/reference/eti.html))

- ...:

  other parameters passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

string describing the result

## Examples

``` r
if (FALSE) { # \dontrun{
if (requireNamespace("brms", quietly = TRUE)) {
  # Generate sample data
  x <- rnorm(500, sd = 6)
  y <- 4 * x + rnorm(500)
  
  # Fit a Bayesian regression model
  fit <- brms::brm(y ~ x, data = data.frame(x, y), chains = 1, iter = 500)
  apa(fit, "x")
  
  # Convert x to another scale for output
  apa(fit, "x", trans = function(val) val + 5)
}
} # }
```
