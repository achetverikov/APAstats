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

  should we add a line to the scale? (T/F)

- ...:

  other parameters passed to scale\_(x or y)\_continuous

## Value

scale\_(x or y)\_continuous with pretty breaks and accompaniying
geom_segment if addSegment == TRUE

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
#> [1] 1.513 2.000 3.000 4.000 5.000 5.424
#> [1] 10.4 15.0 20.0 25.0 30.0 33.9
```
