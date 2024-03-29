% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/machine-learning.R
\name{optimize_with_spaceballs_princess}
\alias{optimize_with_spaceballs_princess}
\title{optimize_with_spaceballs_princess}
\usage{
optimize_with_spaceballs_princess(
  .model,
  .vars,
  .lower,
  .upper,
  .eps = 0.3,
  .maxiter = 10L,
  .option = "minimize",
  .use_all_cores = FALSE
)
}
\arguments{
\item{.model}{A fitted model from one of the \code{nexaverser::train} functions.}

\item{.vars}{A character vector with the names of the input variables.}

\item{.lower}{A vector of lower bounds for the vaiables in the function.}

\item{.upper}{A vector of upper bounds for the vaiables in the function.}

\item{.eps}{A convergence control parameter: if the maximum st.dev. of the
parameters of the elite individuals divided by its average value is smaller
than this number, the method considers that it converged.}

\item{.maxiter}{The number of iterations to run for finding a solution.}

\item{.option}{A string, either "maximize" or "minimize" the function.}

\item{.use_all_cores}{A flag to indicate if the user wants to use all the
cores fromm the compute instance to run the fitness functions.}
}
\value{
A \code{tibble}.
}
\description{
optimize_with_spaceballs_princess
}
