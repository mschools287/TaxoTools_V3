#' Bayesian Node Support Table
#'
#' A table containing node support values for all nodes in the Bayesian phylogenetic tree (`data_treeBayesian`).
#' This table was generated using the `phylo_node_table()` function and is designed for use with the
#' `phylo_merge_nodes()` function.
#'
#' @format A data frame with the following columns:
#' \itemize{
#'   \item \code{Node} Integer. The identifier for each node in the tree. Nodes are numbered according to the structure of `data_treeBayesian`.
#'   \item \code{Taxa} Character. The taxa associated with each node.
#'   \item \code{Support_or_Time} Numeric. The posterior probability values (scaled as whole numbers) for each node in the Bayesian tree.
#' }
#'
#' @details
#' The `data_bayesian_node_table` dataset was generated with `phylo_node_table()` and provides an example of how to store and analyze node support values,
#' facilitating comparative analyses across multiple phylogenies for downstream processing.
#' It is particularly useful for integration with the `phylo_merge_nodes` function,
#' which combines node support information from different trees for comparison.
#'
#' @examples
#' data(data_bayesian_node_table)
#' head(data_bayesian_node_table)
#'
#' # Example: Merging node tables from multiple trees
#' data(data_ml_node_table)
#' data_merged_table <- phylo_merge_nodes(data_ml_node_table, data_bayesian_node_table)
#'
#' @seealso
#' \code{\link{phylo_node_table}}, \code{\link{phylo_merge_nodes}}, \code{\link{data_treeBayesian}}, \code{\link{data_ml_node_table}}
#'
#' @source
#' The posterior probabilities were generated in MrBayes, and converted to whole numbers in TreeGraph2.
"data_bayesian_node_table"
