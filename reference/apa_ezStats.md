# Describe ezStats results

Returns formatted string with mean and SD from ezStats object

## Usage

``` r
apa_ezStats(obj, term = 1, ...)
```

## Arguments

- obj:

  data.frame returned by
  [ez::ezStats](https://rdrr.io/pkg/ez/man/ezStats.html)

- term:

  model term to describe (a string with the term name or its sequential
  number)

- ...:

  other parameters passed to
  [apa_mean_sd](https://achetverikov.github.io/APAstats/reference/apa_mean_sd.md)

  If a string is used for a term and there is more than one factor,
  "X:Y:Z" format is assumed (value in first column : value in the
  second, and so on). If no term is supplied, the first row is used. So
  if you need to select a term based on two or more variables, you can
  also just filter ezStats result beforehand (or use a row number as a
  term).

## Value

string with formatted results

## Examples

``` r
if (requireNamespace("ez", quietly = TRUE) && requireNamespace("afex", quietly = TRUE)) {
  # Using the Stroop dataset from afex package
  data(stroop, package = "afex")
  # Get descriptive statistics

  ezstats_res <- ez::ezStats(
    data = stroop[!is.na(stroop$rt),],
    dv = rt,
    wid = pno,
    within = .(congruency)
  )
  
  # Format results in APA style
  apa_ezStats(ezstats_res, 1)
  apa_ezStats(ezstats_res, "incongruent")
  apa_ezStats(ezstats_res, 1, digits = 3)
}
#> Warning: Collapsing data to cell means. *IF* the requested effects are a subset of the full design, you must use the "within_full" argument, else results may be inaccurate.
#> [1] "_M_ = 0.614 (_SD_ = 0.091)"
```
