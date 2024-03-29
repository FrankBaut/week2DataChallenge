% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{plotAverageDay}
\alias{plotAverageDay}
\title{Plot behaviour profiles over an average day}
\usage{
plotAverageDay(
  data,
  exposurePrefix,
  exposureSuffix,
  yAxisLabel = exposurePrefix,
  outPng = NULL
)
}
\arguments{
\item{data}{Dataset to use}

\item{exposurePrefix}{Name of behaviour variable to plot}

\item{exposureSuffix}{Suffix of behaviour variable columns in dataset}

\item{yAxisLabel}{Label for y-axis}

\item{outPng}{Optional filename to save plot}
}
\value{
Plot of behaviour variable over hours of day
}
\description{
Plot behaviour profiles over an average day
}
