% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{describe.binom.mean.conf}
\alias{describe.binom.mean.conf}
\title{Describe mean and confidence intervals for binomial variable}
\usage{
describe.binom.mean.conf(x, digits = 2)
}
\arguments{
\item{x}{a vector of values}

\item{digits}{number of digits in results (default: 2)}
}
\value{
a string with the mean and confidence interval in square brackets
}
\description{
Describe mean and confidence intervals for binomial variable
}
\examples{

describe.binom.mean.conf(faces$correct[1:100])
# note that it is slightly different from asymptotic CI
describe.mean.conf(faces$correct[1:100], bootCI = FALSE)
# although similar to the bootstrapped CI
describe.mean.conf(faces$correct[1:100], bootCI = TRUE)
}
