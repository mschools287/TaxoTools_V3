#' Generate a Table of Node Values from a Phylogenetic Tree
#'
#' This function creates a table of node values from a given phylogenetic tree,
#' including node IDs, descendant taxa, and either support values or divergence times,
#' depending on the specified method.
#'
#' @param tree A phylogenetic tree of class \code{phylo}.
#' @param method A string specifying the method for the node table. \code{"ML"},
#' \code{"Bayesian"}, and \code{"Time Tree"} are all accepted methods.
#'
#' @details
#' This function iterates over all internal nodes in the phylogenetic tree,
#' calculates the descendant taxa for each node, and retrieves the corresponding
#' support values or divergence times based on the specified method. If the \code{"Time Tree"} method is chosen, the function computes divergence
#' times using node ages derived from the tree's edge lengths.
#'
#' @return A data frame containing the following columns:
#'  \itemize{
#'   \item \code{Node}: The node ID.
#'   \item \code{Taxa}: A comma-separated list of descendant taxa.
#'   \item \code{Support_or_Time}: The node's support value or divergence time.
#' }
#'
#' @examples
#' # Example: Generate a node table for a ML tree
#' phylo_node_table(
#'   tree = data_treeML,
#'   method = "ML")
#'
#' @importFrom ape node.depth.edgelength
#' @export
phylo_node_table <- function(tree, method) {
  # Helper function to get all descendant tips for a given node
  get_descendants <- function(tree, node) {
    children <- tree$edge[tree$edge[, 1] == node, 2]
    taxa <- c()
    for (child in children) {
      if (child <= length(tree$tip.label)) {
        taxa <- c(taxa, tree$tip.label[child])
      } else {
        taxa <- c(taxa, get_descendants(tree, child))
      }
    }
    return(taxa)
  }

  # Validate tree object
  if (!(class(tree) %in% c("phylo", "multiPhylo"))) {
    stop("The 'tree' object should be of class 'phylo' or 'multiPhylo'.")
  }

  # Initialize an empty data frame for the node table
  node_table <- data.frame(Node = integer(0), Taxa = character(0), Support_or_Time = numeric(0), stringsAsFactors = FALSE)

  # Get the number of tips and internal nodes
  Ntip <- length(tree$tip.label)
  Nnode <- tree$Nnode

  # Loop through internal nodes
  for (node in (Ntip + 1):(Ntip + Nnode)) {
    # Get taxa descended from the current node
    taxa_in_node <- get_descendants(tree, node)

    # Initialize support or divergence time
    support_value <- NA
    divergence_time <- NA

    # Handle ML and Bayesian methods
    if (method %in% c("ML", "Bayesian") && !is.null(tree$node.label)) {
      support_value <- as.numeric(tree$node.label[node - Ntip])
    }

    # Handle Time Tree method
    if (method == "Time Tree" && !is.null(tree$edge.length)) {
      node_ages <- node.depth.edgelength(tree)
      max_age <- max(node_ages)  # Root age
      node_ages <- max_age - node_ages
      divergence_time <- node_ages[node]
    }

    # Add current node data to the table
    if (method == "Time Tree") {
      node_table <- rbind(node_table,
                          data.frame(Node = node,
                                     Taxa = paste(taxa_in_node, collapse = ", "),
                                     Support_or_Time = round(divergence_time, 2)))
    } else {
      node_table <- rbind(node_table,
                          data.frame(Node = node,
                                     Taxa = paste(taxa_in_node, collapse = ", "),
                                     Support_or_Time = round(support_value, 2)))
    }
  }

  # Print and return the node table
  print(node_table)
  return(node_table)
}
