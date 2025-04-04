% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_data_matches.R
\name{process_data_matches}
\alias{process_data_matches}
\title{Check for Matches between Phylogenies and/or Datasets}
\usage{
process_data_matches(
  phy1 = NULL,
  phy2 = NULL,
  data1 = NULL,
  data2 = NULL,
  data1_column = NULL,
  data2_column = NULL
)
}
\arguments{
\item{phy1}{A \code{phylo} object representing the first phylogeny.}

\item{phy2}{A \code{phylo} object representing the second phylogeny.}

\item{data1}{A data frame representing the first dataset.}

\item{data2}{A data frame representing the second dataset.}

\item{data1_column}{A string indicating the column name in \code{data1} to compare.}

\item{data2_column}{A string indicating the column name in \code{data2} to compare.}
}
\value{
The function prints out the results but returns nothing (\code{invisible}).
}
\description{
This function compares the individuals between phylogenies and/or datasets
to identify mismatches. It
checks if the specimens (tip labels in phylogenies or values in a specified
column in datasets) match between the provided inputs.
}
\details{
The function allows three types of comparisons:
\enumerate{
\item \strong{Two phylogenies}: It compares the tip labels of two phylogenies (\code{phy1} and \code{phy2}).
\item \strong{Two datasets}: It compares the values of a specific column (\code{data1_column} and \code{data2_column}) in two datasets (\code{data1} and \code{data2}).
\item \strong{One phylogeny and one dataset}: It compares the tip labels of a phylogeny (\code{phy1}) with the values in a column of a dataset (\code{data1_column} in \code{data1}).
}

The function prints out any individuals that do not match in each comparison type.
}
\examples{
# Example comparing a phylogeny and a dataset:
process_data_matches(
  phy1 = data_treeTime,
  data1 = data_morphodata,
  data1_column = "Species_updated")

}
