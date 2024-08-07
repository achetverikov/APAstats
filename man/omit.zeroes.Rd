% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{omit.zeroes}
\alias{omit.zeroes}
\title{Omit leading zero from number}
\usage{
omit.zeroes(x, digits = 2)
}
\arguments{
\item{x}{A number}

\item{digits}{Number of decimal digits to keep}
}
\value{
A number without leading zero
}
\description{
Omit leading zero from number
}
\examples{
omit.zeroes(0.2312)
omit.zeroes(0.2312, digits = 3)
omit.zeroes("000.2312", digits = 1)
}
