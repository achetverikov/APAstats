# Cut a numeric variable into groups (bins) with advanced options

Cut a numeric variable into groups (bins) with advanced options

## Usage

``` r
adv_cut(
  x,
  ncuts = NULL,
  eq_groups = FALSE,
  cuts = NULL,
  num_labels = FALSE,
  labels = NULL,
  include_oob = TRUE,
  labels_at_means = TRUE,
  label_pairs_format = "[%.2f, %.2f]",
  ...
)
```

## Arguments

- x:

  vector of numeric values to cut into groups

- ncuts:

  number of cuts (default: NULL)

- eq_groups:

  should the groups be equal (default: FALSE)

- cuts:

  where to put the cuts (default: NULL), not used if ncuts is used

- num_labels:

  should the labels be transformed into numbers (default: FALSE)

- labels:

  a vector of labels to use for the groups (default: NULL)

- include_oob:

  include values outside of the boundaries provided in `cuts` (default:
  TRUE)

- labels_at_means:

  should labels be created as means between cuts (T) or as pairs of cuts
  (F)

- label_pairs_format:

  formatting string to use when labels are generated from pairs of cuts
  (default: \[%.2f, %.2f\])

- ...:

  other parameters passed to
  [base::cut](https://rdrr.io/r/base/cut.html)

  If `ncuts` is used, then the variable is cut into N cuts either of
  equal group size (eq_groups = TRUE) or equally distant from each other
  (eq_groups = FALSE). If `labels` are not provided, they are generated
  as means between cuts if labels_at_means is T.

## Value

a vector of group labels the same length as the original value vector

## Examples

``` r
set.seed(1)
x <- sample(1:100, 20)
sort(x)
#>  [1]  1  7 14 21 34 37 39 43 51 54 59 68 73 74 79 82 83 85 87 97

adv_cut(x, ncuts = 5)
#>  [1] 68.2 29.8 10.6 29.8 87.4 49   10.6 87.4 68.2 49   87.4 29.8 49   68.2 10.6
#> [16] 68.2 87.4 29.8 87.4 87.4
#> Levels: 10.6 29.8 49 68.2 87.4
adv_cut(x, ncuts = 5, eq_groups = TRUE)
#>  [1] 62   42.5 17.5 17.5 90   42.5 17.5 78   62   42.5 90   17.5 62   78   17.5
#> [16] 62   78   42.5 78   90  
#> Levels: 17.5 42.5 62 78 90
adv_cut(x, ncuts = 5, eq_groups = TRUE, num_labels = TRUE)
#>  [1] 62.0 42.5 17.5 17.5 90.0 42.5 17.5 78.0 62.0 42.5 90.0 17.5 62.0 78.0 17.5
#> [16] 62.0 78.0 42.5 78.0 90.0
adv_cut(x, ncuts = 5, eq_groups = TRUE, labels_at_means = FALSE)
#>  [1] [51.00, 73.00] [34.00, 51.00] [1.00, 34.00]  [1.00, 34.00]  [83.00, 97.00]
#>  [6] [34.00, 51.00] [1.00, 34.00]  [73.00, 83.00] [51.00, 73.00] [34.00, 51.00]
#> [11] [83.00, 97.00] [1.00, 34.00]  [51.00, 73.00] [73.00, 83.00] [1.00, 34.00] 
#> [16] [51.00, 73.00] [73.00, 83.00] [34.00, 51.00] [73.00, 83.00] [83.00, 97.00]
#> 5 Levels: [1.00, 34.00] [34.00, 51.00] [51.00, 73.00] ... [83.00, 97.00]
adv_cut(x, cuts = seq(0, 100, by = 20))
#>  [1] 70 30 10 30 90 50 10 90 50 50 90 30 50 70 10 70 70 30 90 90
#> Levels: 10 30 50 70 90
adv_cut(x, cuts = seq(0, 100, by = 20), labels_at_means = FALSE)
#>  [1] [60.00, 80.00]  [20.00, 40.00]  [0.00, 20.00]   [20.00, 40.00] 
#>  [5] [80.00, 100.00] [40.00, 60.00]  [0.00, 20.00]   [80.00, 100.00]
#>  [9] [40.00, 60.00]  [40.00, 60.00]  [80.00, 100.00] [20.00, 40.00] 
#> [13] [40.00, 60.00]  [60.00, 80.00]  [0.00, 20.00]   [60.00, 80.00] 
#> [17] [60.00, 80.00]  [20.00, 40.00]  [80.00, 100.00] [80.00, 100.00]
#> 5 Levels: [0.00, 20.00] [20.00, 40.00] [40.00, 60.00] ... [80.00, 100.00]
adv_cut(x, cuts = seq(0, 100, by = 20), 
           labels_at_means = FALSE, label_pairs_format = "[%i, %i]")
#>  [1] [60, 80]  [20, 40]  [0, 20]   [20, 40]  [80, 100] [40, 60]  [0, 20]  
#>  [8] [80, 100] [40, 60]  [40, 60]  [80, 100] [20, 40]  [40, 60]  [60, 80] 
#> [15] [0, 20]   [60, 80]  [60, 80]  [20, 40]  [80, 100] [80, 100]
#> Levels: [0, 20] [20, 40] [40, 60] [60, 80] [80, 100]
```
