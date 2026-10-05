# Round *p*-value

If p-value is \<= 0.001, returns ".001" else returns p-value rounded to
the specified number of digits, optionally including relation sign ("\<"
or "=").

## Usage

``` r
round_p(
  values,
  include.rel = 1,
  digits = 3,
  strip.lead.zeros = TRUE,
  replace.very.small = 0.001
)
```

## Arguments

- values:

  a vector of p-values

- include.rel:

  include relation sign

- digits:

  a number of decimal digits

- strip.lead.zeros:

  remove zero before decimal point

- replace.very.small:

  replace values lower than this criteria (NULL to keep values as is)

## Value

Formatted p-value

## Examples

``` r
p_values <- c(0.025, 0.0001, 0.001, 0.568)
round_p(p_values)
#> [1] "= .025" "< .001" "< .001" "= .568"
round_p(p_values, digits = 2)
#> [1] "= .03"  "< .001" "< .001" "= .57" 
round_p(p_values, include.rel = FALSE)
#> [1] ".025"   "< .001" "< .001" ".568"  
round_p(p_values, include.rel = FALSE, strip.lead.zeros = FALSE)
#> [1] "0.025"   "< 0.001" "< 0.001" "0.568"  
round_p(p_values, include.rel = FALSE, strip.lead.zeros = FALSE, replace.very.small = 0.01)
#> [1] "0.025"  "< 0.01" "< 0.01" "0.568" 
```
