% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature-selection.R
\name{calc_p_value_granger_fit}
\alias{calc_p_value_granger_fit}
\title{calc_p_value_granger_fit}
\usage{
calc_p_value_granger_fit(tag_x, tag_y, max_lag = 7L)
}
\arguments{
\item{tag_x}{An univariate series of observations.}

\item{tag_y}{An univariate series of observations.}

\item{max_lag}{An integer specifying the order of delays to include in the
auxiliary regression. Defaults to 7.}
}
\value{
A numeric vector.
}
\description{
calc_p_value_granger_fit
}
