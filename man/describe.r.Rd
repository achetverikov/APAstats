% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{describe.r}
\alias{describe.r}
\title{Describe Pearson test results}
\usage{
describe.r(rc, ...)
}
\arguments{
\item{rc}{an object from \link[stats]{cor.test}}

\item{...}{other arguments passed to \link{format.results}}
}
\value{
A string with correlation coefficient, sample size, and p-value.
}
\description{
Describe Pearson test results
}
\examples{
x <- rnorm(40)
y <- x * 2 + rnorm(40)
rc <- cor.test(x, y)
describe.r(rc)
}
