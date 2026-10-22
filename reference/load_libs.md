# Quietly load libraries

Quietly load libraries

## Usage

``` r
load_libs(libs)
```

## Arguments

- libs:

  a vector of libraries names

## Value

Invisibly, a list of logical scalars, one per package in `libs`,
indicating whether loading succeeded. Called for the side effect of
loading and attaching the requested packages.

## Examples

``` r
load_libs(c("ggplot2", "apastats2", "Hmisc"))
```
