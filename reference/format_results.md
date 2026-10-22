# Format results

Internal function used to convert latex-formatted results to pandoc
style.

## Usage

``` r
format_results(res_str, type = "pandoc")
```

## Arguments

- res_str:

  text

- type:

  'pandoc', 'latex', or 'plotmath' (the latter is very poorly
  implemented)

## Value

A character vector containing `res_str` in the requested notation: LaTeX
markup, Pandoc emphasis, or text for plotmath expressions.
