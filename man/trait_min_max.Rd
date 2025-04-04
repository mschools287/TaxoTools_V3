% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trait_min_max.R
\name{trait_min_max}
\alias{trait_min_max}
\title{Identify Groups with Minimum and Maximum Trait Values}
\usage{
trait_min_max(data, grouped_by, trait_column)
}
\arguments{
\item{data}{A data frame containing group data with columns for names and trait values.}

\item{grouped_by}{A character string specifying the name of the column containing group identifiers.}

\item{trait_column}{A character string specifying the name of the column containing the trait values to be analyzed.}
}
\value{
A list with two elements:
\itemize{
\item \code{Min_Group} A data frame containing the groups with the minimum trait value and the corresponding trait value.
\item \code{Max_Group} A data frame containing the groups with the maximum trait value and the corresponding trait value.
}
}
\description{
This function identifies the groups with the minimum and maximum values for a specified trait, returning the groups and their corresponding trait values. If multiple groups share the minimum or maximum value, all of them will be included in the result.
}
\details{
The function uses the \code{min} and \code{max} functions to find the groups with the smallest and largest trait values, respectively. In the event of ties (i.e., multiple groups with identical minimum or maximum trait values), all groups with the tied values are returned. The result is a list containing two data frames: one for the groups with the minimum value and another for those with the maximum value.
}
\examples{
data(data_morphodata)
trait_min_max(
  data = data_morphodata,
  grouped_by = "Species",
  trait_column = "trait1")

}
