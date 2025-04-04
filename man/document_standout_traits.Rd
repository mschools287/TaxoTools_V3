% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/document_standout_traits.R
\name{document_standout_traits}
\alias{document_standout_traits}
\title{Identify Standout Traits Based on Range Comparisons}
\usage{
document_standout_traits(
  data,
  trait_columns,
  grouped_by = "Species",
  cutoff = 2/3
)
}
\arguments{
\item{data}{A data frame containing trait and grouping variable data.}

\item{trait_columns}{A character vector of trait columns to analyze.}

\item{grouped_by}{A character string specifying the grouping column (e.g., "Species", "Clade").}

\item{cutoff}{A numeric value between 0 and 1 (default: 2/3). Defines the threshold for standout traits.}
}
\value{
A data frame listing the traits that have standout ranges for each group, with the maximum and minimum values for each trait.
If no standout traits are identified at the specified cutoff a message is printed.
}
\description{
This function identifies traits with significantly larger or smaller ranges compared to other groups in the dataset.
It computes the minimum and maximum values for each trait and compares the range of each group to the ranges of all others.
}
\examples{
data(data_morphodata)
document_standout_traits(
  data = data_morphodata,
  grouped_by = "Species",
  trait_columns = c("trait1", "trait2"),
  cutoff = 2/3)

}
