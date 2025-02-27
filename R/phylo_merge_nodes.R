#' Merge Node Information from Multiple Trees
#'
#' This function merges node information from two phylogenetic trees (one Maximum Likelihood and one Bayesian) by matching taxa.
#' It combines the support values from both trees and creates a cleaner table with node and taxa information, including support values for each node.
#' Node tables can be generated using the \code{phylo_node_table()} function.
#'
#' @param ml_node_table A data frame containing node information from a Maximum Likelihood (ML) phylogenetic tree.
#' The table must include a column labeled "Taxa" representing the taxa names, and columns for node-specific data, including the support values.
#' Thistable can be generated using phylo_node_table().
#' @param bayesian_node_table A data frame containing node information from a Bayesian phylogenetic tree, with the same structure as the ML table,
#' including a "Taxa" column for taxa names and support values. This table can be generated using phylo_node_table().
#'
#' @details
#' The function ensures that the "Taxa" column is treated as a character in both input tables. It merges the two tables by the "Taxa" column,
#' aligning nodes from both trees and adding suffixes to differentiate the columns from each tree. The support values from each tree are formatted as a
#' combined string, displaying both the Maximum Likelihood and Bayesian support values for each node. A final cleaned table is returned, containing the "Taxa",
#' "Node_ML", "Node_Bayesian", and "Support_Values" columns.
#'
#' @return
#' A data frame containing merged node information with columns:
#' \itemize{
#'   \item \code{Taxa}: The taxa names.
#'   \item \code{Node_ML}: The node information from the Maximum Likelihood tree.
#'   \item \code{Node_Bayesian}: The node information from the Bayesian tree.
#'   \item \code{Support_Values}: A string combining the support values from both trees, formatted as "ML_support/Bayesian_support".
#' }
#'
#' The function also prints the resulting table.
#'
#' @examples
#' # Using node tables made with generate_node_table().
#' # Output can be used with other functions (plot_tree_with_node_info()).
#' data(data_ml_node_table, data_bayesian_node_table)
#' data_merged_table <- phylo_merge_nodes(
#'   ml_node_table = data_ml_node_table,
#'   bayesian_node_table = data_bayesian_node_table)
#'
#' @export
phylo_merge_nodes <- function(ml_node_table, bayesian_node_table) {
  # Ensure the Taxa columns are treated as character for comparison
  ml_node_table$Taxa <- as.character(ml_node_table$Taxa)
  bayesian_node_table$Taxa <- as.character(bayesian_node_table$Taxa)

  # Sort the Taxa within each string to ensure order doesn't matter
  ml_node_table$Taxa <- sapply(ml_node_table$Taxa, function(x) paste(sort(strsplit(x, ",\\s*")[[1]]), collapse = ", "))
  bayesian_node_table$Taxa <- sapply(bayesian_node_table$Taxa, function(x) paste(sort(strsplit(x, ",\\s*")[[1]]), collapse = ", "))

  # Merge the tables by the Taxa column
  merged_table <- merge(ml_node_table, bayesian_node_table,
                        by = "Taxa",
                        all = TRUE,
                        suffixes = c("_ML", "_Bayesian"))

  # Format the merged column for support values
  merged_table$Support_Values <- paste0(
    ifelse(!is.na(merged_table$Support_or_Time_ML),
           round(merged_table$Support_or_Time_ML, 2), "NA"), "/",
    ifelse(!is.na(merged_table$Support_or_Time_Bayesian),
           round(merged_table$Support_or_Time_Bayesian, 2), "NA")
  )

  # Create a cleaner table with Node and Taxa information
  result_table <- data.frame(
    Taxa = merged_table$Taxa,
    Node_ML = merged_table$Node_ML,
    Node_Bayesian = merged_table$Node_Bayesian,
    Support_Values = merged_table$Support_Values,
    stringsAsFactors = FALSE
  )

  # Print and return the result table
  print(result_table)
  return(result_table)
}
