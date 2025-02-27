#' Plot Divergence Time Histogram for a Time Tree
#'
#' This function visualizes the distribution of divergence times for a given timetree.
#' It calculates the divergence times based on the edge lengths of the tree and then plots a histogram
#' to display the distribution of divergence times across the tree's nodes.
#'
#' @param time_tree An object of class "phylo" or "timeTree", which contains the edge lengths representing divergence times.
#'                  If the tree does not contain edge lengths, an error will be raised.
#' @param bin_width Numeric value specifying the width of bins for the histogram. Default is 5.
#' @param color Character string specifying the color of the histogram bars. Default is "lightblue".
#'
#' @details
#' The function first checks whether the provided tree contains the \code{edge.length} attribute. If the attribute is missing,
#' the function stops and returns an error. If the values are present, a histogram is generated to visualize the distribution
#' of these divergence times, with the x-axis representing divergence time (in millions of years) and the y-axis representing
#' the frequency of nodes within specific time ranges.
#'
#' @return
#' The function creates a histogram plot that shows the distribution of divergence times
#' across the tree's nodes.
#'
#' @examples
#' data(data_treeTime)
#' phylo_divergence_histogram(
#'   time_tree = data_treeTime,
#'   bin_width = 2,
#'   color = "darkseagreen")
#'
#' @export
phylo_divergence_histogram <- function(time_tree, bin_width = 5, color = "lightblue") {
  # Check if the tree is a time tree object
  if (is.null(time_tree$edge.length)) {
    stop("The provided tree does not contain edge lengths for divergence time calculation.")
  }

  # Compute node ages using edge lengths
  node_ages <- node.depth.edgelength(time_tree)
  max_age <- max(node_ages)  # Root age
  node_ages <- max_age - node_ages  # Convert to divergence times

  # Determine breaks based on bin_width, ensuring it spans the range of node_ages
  breaks <- seq(0, max(node_ages) + bin_width, by = bin_width)

  # Plot the histogram of divergence times
  hist(node_ages, main = "Histogram of Divergence Times",
       xlab = "Divergence Time (in million years)",
       ylab = "Frequency", col = color, border = "black",
       breaks = breaks)
}
