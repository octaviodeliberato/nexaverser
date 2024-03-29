% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tag-assessment.R
\name{assess_tag}
\alias{assess_tag}
\title{assess_tag}
\usage{
assess_tag(
  .tag_dat,
  .avg_by = "day",
  .pad = FALSE,
  .imp = FALSE,
  .clean = FALSE,
  .per = 1L,
  .std = FALSE,
  .chg_pts = TRUE,
  .smooth = FALSE,
  .anom = TRUE,
  .alpha = 0.1
)
}
\arguments{
\item{.tag_dat}{A time series with \code{date} and \code{value} cols.}

\item{.avg_by}{A string, like "day" or "2 hours", to average the time series.}

\item{.pad}{Logical, whether or not to pad the time series.}

\item{.imp}{Logical, whether or not to impute missing values using linear
interpolation.}

\item{.clean}{Logical, whether or not to identify and replace outliers and
missing values}

\item{.per}{A seasonal period to use during the transformation. If period = 1,
linear interpolation is performed. If period > 1, a robust STL decomposition
is first performed and a linear interpolation is applied to the seasonally
adjusted data.}

\item{.std}{Logical, whether or not to standardize to mean 0 and standard
deviation 1.}

\item{.chg_pts}{Logical, whether or not to perform change-point analysis.}

\item{.smooth}{Logical, whether or not to include a trendline smoother.}

\item{.anom}{Logical, whether or not to carry out anomaly detection.}

\item{.alpha}{Controls the width of the "normal" range regarding anomaly
detection. Lower values are more conservative while higher values are less
prone to incorrectly classifying "normal" observations.}
}
\value{
A list.
}
\description{
assess_tag
}
\author{
Octavio Deliberato Neto.
}
