% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rob_dummy.R
\name{rob_dummy}
\alias{rob_dummy}
\title{Create "realistic" dummy risk of bias assessment data}
\usage{
rob_dummy(n, tool = "ROB2", study = TRUE)
}
\arguments{
\item{n}{Number of assessments to create}

\item{tool}{Tool used for assessment (see rob_tools()). Default is "ROB2".}

\item{study}{Should the returned dataframe contain a "Study" column. Default
is true.}
}
\description{
This function returns N example risk of bias assessments for the
  tool specified, where N is set by the user. Assessments are "realistic" in
  that the judgment in the overall column is set to the highest judgement
  across the domains for a given study, reflecting the recommendations of the
  tool creators.
}
\seealso{
Other helper: 
\code{\link{rob_append_weights}()},
\code{\link{rob_save}()},
\code{\link{rob_tools}()}
}
\concept{helper}
