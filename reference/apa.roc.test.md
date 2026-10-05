# Describe differences between ROC curves

Describe differences between ROC curves

## Usage

``` r
# S3 method for class 'roc.test'
apa(obj, ...)

# S3 method for class 'roc.diff'
apa(obj, ...)
```

## Arguments

- obj:

  a difference between the ROC curves from
  [pROC::roc.test](https://rdrr.io/pkg/pROC/man/roc.test.html)

- ...:

  Additional arguments (currently unused)

## Value

result

## Examples

``` r
if (requireNamespace("pROC", quietly = TRUE)) {
  # Create sample data
  set.seed(42)
  n <- 100
  group <- factor(sample(c(0, 1), n, replace = TRUE))
  test1 <- rnorm(n, mean = 1 * (as.numeric(group) - 1))
  test2 <- rnorm(n, mean = 0.7 * (as.numeric(group) - 1))
  
  # Create ROC curves
  roc1 <- pROC::roc(group, test1)
  roc2 <- pROC::roc(group, test2)
  
  # Test difference between ROC curves
  roc_diff <- pROC::roc.test(roc1, roc2)
  
  # Format results in APA style
  apa(roc_diff)
}
#> Setting levels: control = 0, case = 1
#> Setting direction: controls < cases
#> Setting levels: control = 0, case = 1
#> Setting direction: controls < cases
#> character(0)
```
