#' Plot Phylogenetic Tree with Annotated Node Support Values
#'
#' This function visualizes a phylogenetic tree with node support values from both Maximum Likelihood (ML) and Bayesian analyses.
#' It annotates tree nodes with support values from a provided data table and includes a subtitle indicating that bootstraps are displayed as ML/Bayesian.
#' A merged node table can be generated using the function \code{phylo_merge_nodes()}.
#'
#' @param tree An object of class \code{phylo} representing the phylogenetic tree to be plotted. The tree should contain node information for annotation.
#' @param tree_type A character string indicating the type of tree being plotted. Must be either \code{"ML"} or \code{"Bayesian"}.
#' @param merged_table A data frame containing node annotations, such as support values. This can be generated using \code{merge_node_tables()}.
#'
#' @details
#' The function takes a phylogenetic tree (\code{tree}) and a table of node annotations (\code{merged_table}) generated from multiple analyses and annotates the tree nodes with
#' the corresponding support values. The tree is plotted using the \code{plot.phylo} function from the \code{ape} package, and node support values
#' are displayed using \code{nodelabels()}.
#'
#' A subtitle ("Bootstraps are displayed as ML/Bayesian") is added below the main title to clarify the annotation format.
#'
#' @return
#' The function generates a plot with annotated node support values.
#'
#' @examples
#' # Assuming merged_table is available
#' phylo_plot_tree_nodes(
#'   tree = data_treeML,
#'   tree_type = "ML",
#'   merged_table = data_merged_table)
#'
#' @importFrom ape plot.phylo
#' @importFrom ape nodelabels
#' @export
phylo_plot_tree_nodes <- function(tree, tree_type = "ML", merged_table) {
  # Load required package
  if (!requireNamespace("ape", quietly = TRUE)) {
    stop("The 'ape' package is required but not installed.")
  }

  # Validate the tree object
  if (!inherits(tree, "phylo")) {
    stop("The 'tree' object should be of class 'phylo'.")
  }

  # Validate the tree_type argument
  if (!tree_type %in% c("ML", "Bayesian")) {
    stop("The 'tree_type' argument must be either 'ML' or 'Bayesian'.")
  }

  # Define plot title based on tree type
  plot_title <- ifelse(tree_type == "ML",
                       "ML Tree Annotated with Node Info",
                       "Bayesian Tree Annotated with Node Info")

  subtitle <- "Bootstraps are displayed as ML/Bayesian"

  # Add node labels to the tree
  tree_with_labels <- tree
  tree_with_labels$node.label <- rep("", ape::Nnode(tree_with_labels)) # Initialize empty labels

  # Annotate tree nodes with support values
  for (i in seq_len(nrow(merged_table))) {
    node_ml <- merged_table$Node_ML[i]
    node_bayesian <- merged_table$Node_Bayesian[i]
    support_values <- merged_table$Support_Values[i]

    # Assign support values based on the tree type
    if (tree_type == "ML" && !is.na(node_ml)) {
      tree_with_labels$node.label[node_ml - ape::Ntip(tree_with_labels)] <- support_values
    } else if (tree_type == "Bayesian" && !is.na(node_bayesian)) {
      tree_with_labels$node.label[node_bayesian - ape::Ntip(tree_with_labels)] <- support_values
    }
  }

  # Set graphical parameters to remove margins on left, right, and bottom
  old_par <- par(mar = c(0, 0, 4, 0))  # Increase top margin to accommodate subtitle

  # Plot the tree without node labels from the plot function
  plot(tree_with_labels, show.node.label = FALSE,
       main = plot_title,
       cex = 0.8)  # Adjust the size of labels here

  # Add subtitle
  mtext(subtitle, side = 3, line = 0.5, cex = 0.8)  # Add subtitle just below the main title

  # Add node labels manually with nodelabels()
  ape::nodelabels(tree_with_labels$node.label,
                  frame = "none",
                  adj = c(0.5, -0.5),
                  cex = 0.8)  # Adjust label size and position as needed

  # Restore the original graphical parameters
  par(old_par)
}
