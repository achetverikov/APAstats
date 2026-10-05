# Describe contrasts created by emmeans

Describe contrasts created by emmeans

## Usage

``` r
# S3 method for class 'summary_emm'
apa(obj, term, dtype = "B", df = FALSE, ...)

# S3 method for class 'emmeans'
apa(obj, term, dtype = "B", df = FALSE, ...)
```

## Arguments

- obj:

  summary object from
  [emmeans::contrast](https://rvlenth.github.io/emmeans/reference/contrast.html)

- term:

  contrast number(s)

- dtype:

  description type, "t", "B", or any other letter

- df:

  include DF in t-test description (default: False)

- ...:

  other parameters passed to
  [format_results](https://achetverikov.github.io/APAstats/reference/format_results.md)

## Value

string with formatted results

## Examples

``` r
if (requireNamespace("emmeans", quietly = TRUE)) {
  # Create a model
  warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  
  # Create estimated marginal means
  warp.emm <- emmeans::emmeans(warp.lm, ~ tension | wool)
  
  # Create contrasts
  sum_contr <- summary(emmeans::contrast(warp.emm, "trt.vs.ctrl"))
  
  # Format results in APA style
  apa(sum_contr, 1)
  apa(sum_contr, 3)
  apa(sum_contr, 3, dtype = "t")
  apa(sum_contr, 3, dtype = "c")
  apa(sum_contr, 3, dtype = "c", df = TRUE)
}
#> [1] "_B_ = 0.56 (5.16), _t_(48) = 0.11, _p_ = .986"
```
