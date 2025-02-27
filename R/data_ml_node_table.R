#' Maximum Likelihood Node Support Table
#'
#' A table containing node support values for all nodes in the Maximum Likelihood (ML) phylogenetic tree (`data_treeML`).
#' This table was generated using the `phylo_node_table()` function and is designed for use with the
#' `phylo_merge_nodes()` function.
#'
#' @return
#' A data frame with the following columns:
#' \itemize{
#'   \item \code{Node} Integer. The identifier for each node in the tree. Nodes are numbered according to the structure of `data_treeML`.
#'   \item \code{Taxa} Character. The taxa associated with each node.
#'   \item \code{Support_or_Time} Numeric. The bootstrap values for each node in the Maximum Likelihood (ML) tree.
#' }
#'
#' @details
#' The `data_ml_node_table` dataset was generated with `phylo_node_table()` and provides an example of how to store and analyze node support values.
#' It is particularly useful for integration with the `phylo_merge_nodes()` function, which combines
#' node support information from different trees for comparison.
#'
#' @examples
#' data(data_ml_node_table)
#' head(data_ml_node_table)
#'
#' # Example: Merging node tables from multiple trees
#' data(data_bayesian_node_table)
#' data_merged_table <- phylo_merge_nodes(
#'   data_ml_node_table,
#'   data_bayesian_node_table)
#'
#' @seealso
#' \code{\link{phylo_node_table}}, \code{\link{phylo_merge_nodes}}, \code{\link{data_treeML}}, \code{\link{data_bayesian_node_table}}
#'
#' @source
#' The bootstrap values were calculated in a Maximum Likelihood analysis conducted in IQ-TREE.
"data_ml_node_table"

