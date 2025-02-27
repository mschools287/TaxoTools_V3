#' Plot Histogram of Node Support Values for ML and/or Bayesian Trees
#'
#' This function generates a histogram of node support values from Maximum Likelihood (ML) trees, Bayesian trees, or both.
#' It visually compares the distribution of support values between the two types of trees. The support values are divided into
#' bins for easy visualization. If both trees are provided, their bars are displayed side by side for comparison.
#' If only one tree type is provided, its bars span the full bin width.
#'
#' @param tree_ml An object of class "phylo" containing a Maximum Likelihood (ML) tree with node support values.
#'                Default is NULL.
#'
#' @param tree_bayesian An object of class "phylo" containing a Bayesian tree with node support values.
#'                      Default is NULL.
#'
#' @param bin_width Numeric value specifying the width of bins for the histogram. Default is 5.
#'
#' @param colors A vector of colors to use for the histogram bars. If two trees are used the length of this vector should be 2.
#'
#' @details
#' The function extracts node support values from the provided ML and/or Bayesian trees and stores them in a data frame.
#' The support values are then grouped into bins and visualized in a histogram. If both ML and Bayesian trees are provided,
#' their support value distributions are plotted side by side. If only one type of tree is provided, its bars fill the entire
#' bin width to maximize visibility.
#'
#' The histogram uses a bar plot, with support value ranges on the x-axis and the frequency of nodes within each range on the y-axis.
#' Colors differentiate ML and Bayesian support values if both types of trees are provided. A legend is included for clarity.
#'
#' @return
#' A bar plot displaying the distribution of node support values for the given tree(s).
#'
#' @examples
#' data(data_treeML, data_treeBayesian)
#' phylo_node_support_hist(
#'   tree_ml = data_treeML,
#'   tree_bayesian = data_treeBayesian,
#'   bin_width = 5,
#'   colors = c("darkorchid", "salmon"))
#'
#' @import graphics
#' @export
phylo_node_support_hist <- function(tree_ml = NULL, tree_bayesian = NULL, bin_width = 5, colors = c("skyblue", "green")) {

  # Determine tree type automatically based on input trees
  if (!is.null(tree_ml) && !is.null(tree_bayesian)) {
    tree_type <- "both"
  } else if (!is.null(tree_ml)) {
    tree_type <- "ML"
  } else if (!is.null(tree_bayesian)) {
    tree_type <- "Bayesian"
  } else {
    stop("No valid tree provided. Please supply either 'tree_ml', 'tree_bayesian', or both.")
  }

  # Initialize a data frame to store the support values
  support_data <- data.frame(SupportValue = numeric(0), TreeType = character(0), stringsAsFactors = FALSE)

  # Process ML tree if provided
  if (tree_type == "ML" || tree_type == "both") {
    support_ml <- as.numeric(tree_ml$node.label)
    support_ml <- na.omit(support_ml)  # Remove NA values
    support_data <- rbind(support_data, data.frame(SupportValue = support_ml, TreeType = "ML"))
  }

  # Process Bayesian tree if provided
  if (tree_type == "Bayesian" || tree_type == "both") {
    support_bayesian <- as.numeric(tree_bayesian$node.label)
    support_bayesian <- na.omit(support_bayesian)  # Remove NA values
    support_data <- rbind(support_data, data.frame(SupportValue = support_bayesian, TreeType = "Bayesian"))
  }

  # Create bins for support values (including 100 in the last bin)
  support_data$SupportBin <- cut(support_data$SupportValue, breaks = seq(0, 100, by = bin_width), right = TRUE)

  # Create counts for each bin for each tree type
  counts_ml <- table(support_data$SupportBin[support_data$TreeType == "ML"])
  counts_bayesian <- table(support_data$SupportBin[support_data$TreeType == "Bayesian"])

  # Determine if one or both tree types are present
  if (tree_type == "both") {
    # Combine counts into a matrix for side-by-side bars
    barplot_height <- rbind(counts_ml, counts_bayesian)
    beside_setting <- TRUE  # Bars should be next to each other
    bar_colors <- colors
    legend_labels <- c("ML", "Bayesian")
  } else {
    # Only one tree type present, so show bars in full bin width
    barplot_height <- if (tree_type == "ML") counts_ml else counts_bayesian
    beside_setting <- FALSE  # Bars should take full bin width
    bar_colors <- colors[1]
    legend_labels <- if (tree_type == "ML") "ML" else "Bayesian"
  }

  # Set up the bar plot with proper labels and colors
  barplot_obj <- barplot(barplot_height, beside = beside_setting, col = bar_colors,
                         names.arg = levels(support_data$SupportBin),
                         main = "Node Support Values Histogram",
                         xlab = "Support Value Range", ylab = "Frequency",
                         ylim = c(0, max(barplot_height) + 1),
                         border = "black",
                         legend = FALSE,
                         cex.names = 0.7,  # Adjust label size
                         las = 2)  # Rotate labels 90 degrees

  # Add grid behind the plot
  grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted", lwd = 0.5)  # Customize grid appearance

  # Add a legend to the side of the plot
  legend("topleft", legend = legend_labels, fill = bar_colors,
         bty = "n", inset = c(0.05, 0))  # Adjust position of the legend
}
