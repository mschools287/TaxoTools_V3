#' Clade Support Summary
#'
#' This function generates a summary of node support or divergence times for a given taxon in a phylogenetic tree.
#' It supports various phylogeny types, including Maximum Likelihood (ML), Bayesian, and Time Tree methods.
#'
#' @param tree A phylogenetic tree of class \code{phylo}.
#' @param taxon_name A character string representing the taxon (e.g., "Species", "Clade") for which the clade support summary is calculated.
#' @param method A character string specifying the method to use. One of "ML", "Bayesian", or "Time Tree". Default is "ML".
#'
#' @return A list containing summary statistics for the support values or divergence times at nodes containing the specified taxon.
#'
#' @examples
#' # Example: Generate support stats from multiple trees
#' data(data_treeML, data_treeBayesian, data_treeTime)
#' taxon_name <- "species_d"
#' phylo_clade_support(
#'   tree = data_treeML,
#'   taxon_name = taxon_name,
#'   method = "ML")
#' phylo_clade_support(
#'   tree = data_treeBayesian,
#'   taxon_name = taxon_name,
#'   method = "Bayesian")
#' phylo_clade_support(
#'   tree = data_treeTime,
#'   taxon_name = taxon_name,
#'   method = "Time Tree")
#'
#' @export
phylo_clade_support <- function(tree, taxon_name, method = "ML") {
  # Get the node table for the given tree and method
  node_table <- phylo_node_table(tree, method)

  # Filter rows where all taxa listed in the Taxa column contain the taxon_name
  filtered_nodes <- node_table[sapply(node_table$Taxa, function(taxa) all(grepl(taxon_name, unlist(strsplit(taxa, ", "))))), ]

  # If no matching taxa, return an error
  if (nrow(filtered_nodes) == 0) {
    stop("No matching taxa found in the node table.")
  }

  # Extract the support or time values
  support_values <- filtered_nodes$Support_or_Time

  # Check if we have valid support values
  if (all(is.na(support_values))) {
    stop("No valid support or time values found for the taxon.")
  }

  # Print the node values that are being used for the calculations
  cat("\n### Node Support Summary for", taxon_name, "###\n")
  cat("\nThe following node values were used for the calculations:\n")
  print(filtered_nodes[, c("Node", "Support_or_Time", "Taxa")])

  # Calculate summary statistics
  summary_stats <- list(
    mean = mean(support_values, na.rm = TRUE),
    median = median(support_values, na.rm = TRUE),
    sd = sd(support_values, na.rm = TRUE),
    min = min(support_values, na.rm = TRUE),
    max = max(support_values, na.rm = TRUE)
  )

  return(summary_stats)
}
