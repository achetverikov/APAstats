% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{describe.anova}
\alias{describe.anova}
\title{Describe the results of model comparison with anova}
\usage{
describe.anova(anova_res, rown = 2, f.digits = 2, ...)
}
\arguments{
\item{anova_res}{anova results}

\item{rown}{row number (default: 2)}

\item{f.digits}{number of digits in the results (default: 2)}

\item{...}{other parameters passed to [apastats::format.results]}
}
\value{
Formatted string with F (or chi2), df, and p
}
\description{
Describe the results of model comparison with anova
}
