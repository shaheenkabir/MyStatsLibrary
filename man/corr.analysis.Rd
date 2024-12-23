% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corr.analysis.R
\name{corr.analysis}
\alias{corr.analysis}
\title{Correlation Analysis}
\usage{
corr.analysis(vec1, vec2)
}
\arguments{
\item{vec1}{A numeric vector.}

\item{vec2}{A numeric vector.}
}
\value{
The result of either Pearson or Spearman correlation test.
}
\description{
This function performs correlation analysis based on the normality of the input vectors.
It generates histograms, Q-Q plots, and chooses Pearson or Spearman correlation test based on Shapiro-Wilk test results.
}
\examples{
corr.analysis(rnorm(100), rnorm(100))
}
