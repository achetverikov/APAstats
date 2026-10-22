# Tufte-like breaks for axes

Tufte-like breaks for axes

## Usage

``` r
base_breaks(x, scale = "x", addSegment = TRUE, ...)

base_breaks_x(x, addSegment = TRUE, ...)

base_breaks_y(x, addSegment = TRUE, ...)
```

## Arguments

- x:

  vector of numbers to create breaks from

- scale:

  scale (x or y) to create breaks for

- addSegment:

  should we add a line to the scale? (TRUE/FALSE)

- ...:

  other parameters passed to scale\_(x or y)\_continuous

## Value

A continuous position scale (class `ScaleContinuousPosition`) with
breaks spanning `x`. When `addSegment = TRUE`, a list containing an
axis-segment layer and the scale, suitable for adding to a
[ggplot2](https://CRAN.R-project.org/package=ggplot2) plot.

## Functions

- `base_breaks_x()`: Tufte-like breaks for X axis

- `base_breaks_y()`: Tufte-like breaks for Y axis

## Examples

``` r
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  theme_minimal()

p

p + base_breaks(mtcars$wt, scale = "x") +
  base_breaks(mtcars$mpg, scale = "y")

```
