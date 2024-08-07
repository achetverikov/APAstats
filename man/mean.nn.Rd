% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{mean.nn}
\alias{mean.nn}
\title{Mean with na.rm=T}
\usage{
\method{mean}{nn}(x, ...)
}
\arguments{
\item{x}{a vector of numbers}

\item{...}{other arguments passed to mean}
}
\value{
mean of x with NA removed
}
\description{
Mean with na.rm=T
}
\examples{
x <- c(NA, 10, 90)
mean(x)
mean.nn(x)

}
