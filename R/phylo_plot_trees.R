#' Plot Phylogenies
#'
#' This function plots multiple phylogenetic trees, allowing customization options for plot type, support value display, branch color, and other visual elements.
#' When support values are "colored," values that are 95-100 are green, 85-94 are orange, and those below 85 are red.
#'
#' @param trees A list of phylogenetic trees of class \code{phylo} or \code{multiPhylo} to be plotted.
#' @param methods A character vector of methods corresponding to the trees (e.g., "ML", "Bayesian", "Time Tree").
#' @param show_bootstraps Logical, indicating whether to display bootstrap values for ML trees. Default is FALSE.
#' @param bootstrap_color Color scheme for displaying bootstrap values. Default is "colored". Options are "none", "black", or "colored".
#' @param show_posterior Logical, indicating whether to display posterior support values for Bayesian trees. Default is FALSE.
#' @param posterior_color Color scheme for displaying posterior values. Default is "colored". Options are "none", "black", or "colored".
#' @param time_scale Logical, indicating whether to display divergence times for "Time Tree" methods. Default is FALSE.
#' @param side_by_side Logical, indicating whether to display trees side by side in a single plot. Default is FALSE.
#' @param name_data A data frame with two columns: Old_Name and New_Name, used for renaming tree tips. Default is NULL.
#' @param phylogeny_type The type of tree to plot: "phylogram" (default), "cladogram", or "radial".
#' @param edge_color Color for tree edges. Default is "black".
#'
#' @return This function does not return any values. It produces a plot of the phylogenetic trees with customizable features.
#'
#' @details
#' The function can handle multiple trees and display them side by side.
#' The trees can be plotted as phylograms, cladograms, or radial trees.
#' Node values (e.g., bootstrap, posterior probabilities, or divergence times) can be displayed based on the selected method.
#'
#' @examples
#' data(data_treeML, data_treeBayesian, data_treeTime, data_names)
#' phylo_plot_trees(
#'   trees = list(data_treeML, data_treeBayesian, data_treeTime),
#'   methods = c("ML", "Bayesian", "Time Tree"),
#'   show_bootstraps = TRUE,
#'   bootstrap_color = "colored",
#'   show_posterior = TRUE,
#'   posterior_color = "black",
#'   time_scale = TRUE,
#'   side_by_side = TRUE,
#'   name_data = data_names)
#'
#' @importFrom ape rtree
#' @importFrom ape nodelabels
#' @importFrom ape plot
#' @export
phylo_plot_trees <- function(trees,
                             methods,
                             show_bootstraps = FALSE,
                             bootstrap_color = "colored",
                             show_posterior = FALSE,
                             posterior_color = "colored",
                             time_scale = FALSE,
                             side_by_side = FALSE,
                             name_data = NULL,
                             phylogeny_type = "phylogram",
                             edge_color = "black") {

  # Check the input trees
  if (!all(sapply(trees, function(tree) inherits(tree, c("phylo", "multiPhylo"))))) {
    stop("All elements in 'trees' should be of class 'phylo' or 'multiPhylo'.")
  }

  # Check if methods match the number of trees
  if (length(trees) != length(methods)) {
    stop("The number of 'trees' and 'methods' must match.")
  }

  # Compute node ages
  compute_node_ages <- function(tree) {
    if (is.null(tree$edge.length)) {
      stop("The tree does not have edge lengths for calculating node ages.")
    }
    node_ages <- node.depth.edgelength(tree)  # Get node depths
    max_age <- max(node_ages)  # Root age
    node_ages <- max_age - node_ages  # Convert to time from present
    return(node_ages)
  }

  # Function to replace names in the tree
  replace_tree_names <- function(tree, name_data) {
    if (!all(c("Old_Name", "New_Name") %in% colnames(name_data))) {
      stop("Data frame must contain 'Old_Name' and 'New_Name' columns.")
    }
    old_names <- name_data$Old_Name
    new_names <- name_data$New_Name
    tree$tip.label <- ifelse(tree$tip.label %in% old_names,
                             new_names[match(tree$tip.label, old_names)],
                             tree$tip.label)
    return(tree)
  }

  # Function to assign support value colors
  assign_support_value_colors <- function(tree, color_type) {
    if (color_type == "none") {
      return(rep(NA, length(tree$node.label)))
    } else if (color_type == "black") {
      return(rep("black", length(tree$node.label)))
    } else {
      support_colors <- rep("red", length(tree$node.label))
      if (!is.null(tree$node.label)) {
        numeric_labels <- suppressWarnings(as.numeric(tree$node.label))
        valid_indices <- which(!is.na(numeric_labels))
        support_colors[valid_indices] <- ifelse(numeric_labels[valid_indices] >= 95, "green",
                                                ifelse(numeric_labels[valid_indices] >= 85, "orange", "red"))
      }
      return(support_colors)
    }
  }

  # Set up side-by-side plotting if requested
  if (side_by_side) {
    par(mfrow = c(1, length(trees)), mar = c(4, 4, 2, 1))
  }

  # Loop through the trees
  for (i in seq_along(trees)) {
    tree <- trees[[i]]
    method <- methods[i]

    # Replace names if name_data is provided
    if (!is.null(name_data)) {
      tree <- replace_tree_names(tree, name_data)
    }

    # Handle different tree methods
    if (method == "ML") {
      support_colors <- assign_support_value_colors(tree, bootstrap_color)
      plot(tree, show.node.label = FALSE, edge.width = 1, main = "Maximum Likelihood Tree", edge.col = edge_color)
      if (show_bootstraps && !is.null(tree$node.label)) {
        nodelabels(tree$node.label, frame = "none", col = support_colors, adj = c(1.5, 0.5), cex = 0.7)
      }
    } else if (method == "Bayesian") {
      support_colors <- assign_support_value_colors(tree, posterior_color)
      plot(tree, show.node.label = FALSE, edge.width = 1, main = "Bayesian Tree", edge.col = edge_color)
      if (show_posterior && !is.null(tree$node.label)) {
        nodelabels(tree$node.label, frame = "none", col = support_colors, adj = c(1.5, 0.5), cex = 0.7)
      }
    } else if (method == "Time Tree") {
      if (time_scale) {
        node_ages <- compute_node_ages(tree)
        tree$node.label <- round(node_ages[(length(tree$tip.label) + 1):length(node_ages)], 2)
      }
      plot(tree, show.node.label = TRUE, edge.width = 1, main = "Time Tree", edge.col = edge_color)
    }
  }

  # Reset plotting parameters
  if (side_by_side) {
    par(mfrow = c(1, 1))
  }
}


