# Describe [brms](https://paulbuerkner.com/brms/reference/brms-package.html) model results

Describe
[brms](https://paulbuerkner.com/brms/reference/brms-package.html) model
results

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

A character result reporting the posterior mean coefficient, its 95%
interval, and optionally an effect-size estimate.

## Examples

``` r
# \donttest{
if (requireNamespace("brms", quietly = TRUE) &&
    requireNamespace("bayestestR", quietly = TRUE)) {
  # Generate sample data
  set.seed(42)
  x <- seq(-1, 1, length.out = 30)
  y <- 1 + 2 * x + rnorm(30, sd = 0.5)
  
  # Fit a Bayesian regression model
  fit <- brms::brm(y ~ x, data = data.frame(x, y), backend = "rstan",
                   chains = 1, cores = 1, iter = 500, seed = 42, refresh = 0)
  print(apa(fit, "x"))
  
  # Convert x to another scale for output
  print(apa(fit, "x", trans = function(val) val + 5))
}
#> Compiling Stan program...
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
# }
```
