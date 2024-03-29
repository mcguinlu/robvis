% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rob_data.R
\docType{data}
\name{data_rob1}
\alias{data_rob1}
\title{Example ROB1 assessment dataset}
\format{
A data frame with the following 9 columns:
\describe{
  \item{Study}{Study identifier}
  \item{Random.sequence.generation}{Domain 1}
  \item{Allocation.concealment}{Domain 2}
  \item{Blinding.of.participants.and.personnel}{Domain 3}
  \item{Blinding.of.outcome.assessment}{Domain 4}
  \item{Incomplete.outcome.data}{Domain 5}
  \item{Selective.reporting}{Domain 6}
  \item{Other.sources.of.bias}{Domain 7}
  \item{Overall}{Overall risk of bias}
         }
}
\source{
{Created for this package}
}
\usage{
data_rob1
}
\description{
A data frame containing 9 example assessments performed using
  the RoB1 assessment tool. Note that this dataset is distinct from other
  example datasets included in this package, in that the column names are the
  full domain names, rather than domain shortcodes (e.g. D1, D2, etc.). This
  is because the "Generic" (formerly "RoB1") template offered by
  \code{robvis} allows users to create custom risk-of-bias plots, and uses
  the column names of the provided dataset to define the domain names for
  these custom plots.
}
\keyword{internal}
