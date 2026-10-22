# Format results from list objects in APA style

This function detects the type of statistical results stored in a list
object and redirects to the appropriate APA formatting method. Currently
supports [ezANOVA](https://rdrr.io/pkg/ez/man/ezANOVA.html) results
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

A character vector of formatted ANOVA results, or a `data.frame`
containing selected ANOVA rows and a `res` column when
`append_to_table = TRUE`. See
[apa.ezANOVA](https://achetverikov.github.io/APAstats/reference/apa.ezanova.md).
