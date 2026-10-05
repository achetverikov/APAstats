# Format results from list objects in APA style

This function detects the type of statistical results stored in a list
object and redirects to the appropriate APA formatting method. Currently
supports [ez::ezANOVA](https://rdrr.io/pkg/ez/man/ezANOVA.html) results
([apa.ezANOVA](https://achetverikov.github.io/APAstats/reference/apa.ezanova.md)).

## Usage

``` r
# S3 method for class 'list'
apa(obj, ...)
```

## Arguments

- obj:

  A list object containing statistical results

- ...:

  Additional arguments passed to the appropriate method

## Value

A formatted string with statistical results in APA style
