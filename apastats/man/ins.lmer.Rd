% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{ins.lmer}
\alias{ins.lmer}
\title{Describe lmer in-text}
\usage{
ins.lmer(fm, term = NULL, digits = 2, adj.digits = TRUE)
}
\arguments{
\item{fm}{LMER model from lme4}

\item{term}{model term to describe (a string with the term name or its sequential number)}

\item{digits}{number of digits for B and SD}

\item{adj.digits}{automatically adjusts digits so that B would not show up as "0.00"}
}
\value{
result
}
\description{
A shortcut for describe.glm(..., short=4)
}
