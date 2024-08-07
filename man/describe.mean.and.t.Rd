% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{describe.mean.and.t}
\alias{describe.mean.and.t}
\title{Describe two-sample t-test with means and effect sizes}
\usage{
describe.mean.and.t(
  x,
  by,
  which.mean = 1,
  digits = 2,
  paired = FALSE,
  eff.size = FALSE,
  abs = FALSE,
  aggregate_by = NULL,
  transform.means = NULL,
  ...
)
}
\arguments{
\item{x}{dependent variable}

\item{by}{independent variable}

\item{which.mean}{which mean to show (0 - none, 1 - first group (default), 2 - second group, 3 - both)}

\item{digits}{number of digits in results (default: 2)}

\item{paired}{should it be a paired test (default: FALSE)}

\item{eff.size}{should we include effect size (default: FALSE)}

\item{abs}{should we show the absolute value if the t-test (T) or keep its sign (FALSE, default)}

\item{aggregate_by}{do the aggregation by the thrird variable(s): either NULL (default), a single vector variable, or a list of variables to aggregate by.}

\item{transform.means}{a function to transform means and confidence intervals (default: NULL)}

\item{...}{other parameters passed to \link{format.results}}
}
\value{
result
}
\description{
Describe two-sample t-test with means and effect sizes
}
\examples{
data(faces)
rt <- faces$answerTime
gr <- faces$stim_gender
describe.mean.and.t(rt, gr)
describe.mean.and.t(rt, gr, which.mean = 3)
describe.mean.and.t(rt, gr, eff.size = TRUE)

sid <- faces$sid
describe.mean.and.t(rt, gr, which.mean = 3, aggregate_by = sid)
log_rt <- log(rt * 1000)
describe.mean.and.t(log_rt, gr, which.mean = 3, aggregate_by = sid)
describe.mean.and.t(log_rt, gr, which.mean = 3, aggregate_by = sid, transform.means = exp)
}
