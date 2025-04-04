% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_treeML.R
\docType{data}
\name{data_treeML}
\alias{data_treeML}
\title{Maximum Likelihood Phylogenetic Tree}
\format{
An object of class \code{phylo} (from the \code{ape} package).
\itemize{
\item \code{edge} A matrix describing the edges of the tree.
\item \code{edge.length} A numeric vector giving the lengths of the edges in the tree.
\item \code{Nnode} An integer giving the number of internal nodes in the tree.
\item \code{node.label} Bootstrap support values for the tree branches, stored as node labels.
\item \code{tip.label} A character vector of tip labels for the 19 individuals included in the tree.
}
}
\source{
The tree was generated using IQ-TREE, a Maximum Likelihood software for constructing phylogenetic trees.
}
\usage{
data_treeML
}
\description{
A maximum likelihood (ML) phylogenetic tree constructed using 19 individuals, including 18 ingroup samples
and one outgroup. The tree was generated with IQ-TREE, using 1,000 bootstrap replicates to assess branch support and rooted in MEGA and \code{phytools}.
}
\details{
The tree was inferred using IQ-TREE with 1,000 bootstrap replicates and and rooted in MEGA and \code{phytools}.
This tree serves as an example dataset for ML phylogenetic analyses and visualization.
}
\examples{
data(data_treeML)
plot(
  data_treeML,
  main = "Maximum Likelihood Tree")

}
\keyword{datasets}
