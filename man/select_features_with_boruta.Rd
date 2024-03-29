% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature-selection.R
\name{select_features_with_boruta}
\alias{select_features_with_boruta}
\title{select_features_with_boruta}
\usage{
select_features_with_boruta(
  .tag_dat,
  .target,
  .balance = FALSE,
  .with_tentative = TRUE,
  .return_data = FALSE,
  .task = "regression"
)
}
\arguments{
\item{.tag_dat}{A data frame with \code{date} and numeric cols.}

\item{.target}{The name (string) of the target variable.}

\item{.balance}{Logical, whether or not to balance the dataset.}

\item{.with_tentative}{Logical, whether or not to include the 'tentative
features' in the selection.}

\item{.return_data}{Logical, whether or not to return the dataset in the
return object.}

\item{.task}{Either "regression" or "classification".}
}
\value{
A character vector.
}
\description{
select_features_with_boruta
}
