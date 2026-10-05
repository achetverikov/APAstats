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

`res_str` with latex 'emph' tags replaced with pandoc '\_'
