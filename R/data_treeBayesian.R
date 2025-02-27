#' Bayesian Phylogenetic Tree
#'
#' A Bayesian phylogenetic tree constructed using 19 individuals, including 18 ingroup samples
#' and one outgroup. The tree was generated with MrBayes and annotated with posterior probabilities
#' converted to whole numbers using TreeGraph2 before being exported as a Newick file.
#'
#' @format An object of class \code{phylo} (from the \code{ape} package).
#' \itemize{
#'   \item \code{edge} A matrix describing the edges of the tree.
#'   \item \code{edge.length} A numeric vector giving the lengths of the edges in the tree.
#'   \item \code{Nnode} An integer giving the number of internal nodes in the tree.
#'   \item \code{node.label} Posterior probabilities for the tree branches, converted to whole numbers during annotation.
#'   \item \code{tip.label} A character vector of tip labels for the 19 individuals included in the tree.
#' }
#'
#' @details
#' The tree was inferred using MrBayes, a Bayesian phylogenetic analysis tool. Posterior probabilities
#' for branch support were calculated during the analysis and converted to whole numbers using TreeGraph2.
#' The tree was rooted in MEGA and `phytools`.
#' This tree serves as an example dataset for Bayesian phylogenetic analyses and visualization.
#'
#' @examples
#' data(data_treeBayesian)
#' plot(
#'   data_treeBayesian,
#'   main = "Bayesian Tree")
#'
#' @source
#' The tree was generated using MrBayes, with branch annotations converted to whole numbers in TreeGraph2
#' and exported in Newick format. The tree was rooted using `reroot()` from the package `phytools`
"data_treeBayesian"
