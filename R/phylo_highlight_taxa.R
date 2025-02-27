#' Highlight Specific Taxa or Clades in Phylogenetic Trees
#'
#' This function visualizes one or more phylogenetic trees with options to highlight specific taxa, clades, or nodes
#' based on a support threshold. Custom colors and shapes can be applied to tips or nodes.
#'
#' @param trees A list of phylogenetic trees of class \code{phylo}.
#' @param taxa_to_highlight A vector of taxa names (or patterns, such as regular expressions) to highlight in the trees. Default is \code{NULL}.
#' @param highlight_colors A vector of colors to use for highlighting the specified taxa. Default is \code{NULL}, which uses red.
#' @param node_support_threshold Numeric value specifying the minimum node support value for highlighting nodes. Default is \code{NULL}.
#' @param support_color Color used for highlighting nodes with high support values. Default is \code{"blue"}.
#' @param highlight_shape Shape of the highlight marker. Options are \code{"circle"} (default) or \code{"square"}.
#' @param shape_size Numeric value controlling the size of the highlight markers. Default is \code{2}.
#' @param tip_label_size Numeric value controlling the size of tip labels. Default is \code{0.8}.
#'
#' @return The function creates plots of the phylogenetic trees with highlighted taxa and/or nodes.
#' @importFrom ape plot.phylo
#' @importFrom ape tiplabels
#' @importFrom ape nodelabels
#'
#' @examples
#' data(data_treeML, data_treeBayesian)
#' phylo_highlight_taxa(
#'     trees = list(data_treeML, data_treeBayesian),
#'     taxa_to_highlight = c("species_e", "species_d", "_a_"),
#'     highlight_colors = c("green", "orange", "purple"),
#'     node_support_threshold = 95,
#'     support_color = "blue",
#'     highlight_shape = "square",
#'     shape_size = 1,
#'     tip_label_size = 1.0)
#' @export
phylo_highlight_taxa <- function(trees,
                                 taxa_to_highlight = NULL,
                                 highlight_colors = NULL,
                                 node_support_threshold = NULL,
                                 support_color = "blue",
                                 highlight_shape = "circle",
                                 shape_size = 1,
                                 tip_label_size = 0.8) {
  # Ensure that trees is a list of phylogenetic trees
  if (!all(sapply(trees, inherits, "phylo"))) {
    stop("Each tree in the 'trees' list must be of class 'phylo'.")
  }

  # Set up the plotting area for multiple trees side by side
  n_trees <- length(trees)
  par(mfrow = c(1, n_trees), mar = c(3, 3, 2, 1), oma = c(0, 0, 2, 0))  # Adjusted top margin and outer margin (oma[3] to 4)

  # Loop over each tree and plot
  for (i in seq_along(trees)) {
    tree <- trees[[i]]

    # Plot the base tree with tight spacing
    plot(tree, cex = tip_label_size, show.node.label = FALSE, no.margin = TRUE)

    # Add the tree name in the outer margin space
    mtext(paste("Tree", i), side = 3, line = 0, cex = 1.2, font = 2)  # Add title to the top

    # Highlight specific taxa
    if (!is.null(taxa_to_highlight)) {
      for (j in seq_along(taxa_to_highlight)) {
        taxon <- taxa_to_highlight[j]
        # Find all matching tips based on the specified taxon
        tip_indices <- grep(taxon, tree$tip.label, ignore.case = TRUE)

        # Use the assigned color for the current taxon
        taxon_color <- if (is.null(highlight_colors)) "red" else highlight_colors[j]

        # Highlight each matching tip
        tiplabels(tip = tip_indices,
                  pch = ifelse(highlight_shape == "circle", 21, 22),
                  col = taxon_color,
                  bg = taxon_color,
                  cex = shape_size)
      }
    }

    # Highlight nodes with high support values
    if (!is.null(node_support_threshold)) {
      if (!is.null(tree$node.label)) {
        high_support_nodes <- which(as.numeric(tree$node.label) >= node_support_threshold)
        nodelabels(node = high_support_nodes + length(tree$tip.label),
                   frame = "none",
                   pch = ifelse(highlight_shape == "circle", 21, 22),
                   col = support_color,
                   bg = support_color,
                   cex = shape_size)
      }
    }
  }
}

