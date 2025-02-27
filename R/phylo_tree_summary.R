#' Summarize Tree Statistics
#'
#' This function calculates summary statistics for a phylogenetic tree, including the number of nodes, depth of the tree, and additional statistics based on the tree's method (Maximum Likelihood, Bayesian, or Time Tree). It can summarize bootstrap values, posterior probabilities, or divergence times.
#'
#' @param tree A phylogenetic tree of class 'phylo' or 'multiPhylo'.
#' @param method A character string indicating the method used to generate the tree. Options are "ML" (Maximum Likelihood), "Bayesian", or "Time Tree".
#'
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{num_nodes}: The number of internal nodes in the tree.
#'   \item \code{max_depth}: The maximum depth of the tree (longest path from root to tip).
#'   \item \code{bootstrap_summary}: A list of summary statistics (min, max, mean, mode, median, sd) for bootstrap values, if applicable.
#'   \item \code{posterior_summary}: A list of summary statistics (min, max, mean, mode, median, sd) for posterior probabilities, if applicable.
#'   \item \code{divergence_summary}: A list of summary statistics (min, max, mean, mode, median, sd) for divergence times, if applicable.
#' }
#'
#' @details
#' This function computes various summary statistics for a phylogenetic tree based on the specified method. For ML trees, it summarizes bootstrap values; for Bayesian trees, it summarizes posterior probabilities; for time trees, it summarizes divergence times. The statistics returned for each of these include the minimum, maximum, mean, mode, median, and standard deviation.
#'
#' @examples
#' data(data_treeML, data_treeBayesian, data_treeTime)
#' phylo_tree_summary(
#'   tree = data_treeML,
#'   method = "ML")
#'
#' @importFrom ape Nnode node.depth.edgelength
#' @export
phylo_tree_summary <- function(tree, method) {

  # Helper function to calculate the mode
  calculate_mode <- function(values) {
    unique_values <- unique(values)
    unique_values[which.max(tabulate(match(values, unique_values)))]
  }

  # Check if the tree object is valid
  if (!inherits(tree, c("phylo", "multiPhylo"))) {
    stop("The 'tree' object should be of class 'phylo' or 'multiPhylo'.")
  }

  # Automatically set flags based on the method
  if (method == "ML") {
    include_bootstrap <- TRUE
    include_posterior <- FALSE
    include_divergence <- FALSE
  } else if (method == "Bayesian") {
    include_bootstrap <- FALSE
    include_posterior <- TRUE
    include_divergence <- FALSE
  } else if (method == "Time Tree") {
    include_bootstrap <- FALSE
    include_posterior <- FALSE
    include_divergence <- TRUE
  }

  # Initialize result list
  summary_result <- list()

  # Number of nodes (internal nodes)
  num_nodes <- Nnode(tree)
  summary_result$num_nodes <- num_nodes

  # Depth of the tree (longest path from root to tip)
  max_depth <- max(node.depth.edgelength(tree))
  summary_result$max_depth <- max_depth

  ###### For ML trees: Summarize bootstrap values ######
  if (include_bootstrap) {
    if (!is.null(tree$node.label)) {
      bootstrap_values <- as.numeric(tree$node.label)
      bootstrap_values <- bootstrap_values[!is.na(bootstrap_values)]
      if (length(bootstrap_values) > 0) {
        summary_result$bootstrap_summary <- list(
          min = min(bootstrap_values),
          max = max(bootstrap_values),
          mean = mean(bootstrap_values),
          mode = calculate_mode(bootstrap_values),
          median = median(bootstrap_values),
          sd = sd(bootstrap_values)
        )
      } else {
        summary_result$bootstrap_summary <- "No valid bootstrap values available"
      }
    } else {
      summary_result$bootstrap_summary <- "No bootstrap values available"
    }
  }

  ###### For Bayesian trees: Summarize posterior probabilities ######
  if (include_posterior) {
    if (!is.null(tree$node.label)) {
      posterior_values <- as.numeric(tree$node.label)
      posterior_values <- posterior_values[!is.na(posterior_values)]
      if (length(posterior_values) > 0) {
        summary_result$posterior_summary <- list(
          min = min(posterior_values),
          max = max(posterior_values),
          mean = mean(posterior_values),
          mode = calculate_mode(posterior_values),
          median = median(posterior_values),
          sd = sd(posterior_values)
        )
      } else {
        summary_result$posterior_summary <- "No valid posterior probabilities available"
      }
    } else {
      summary_result$posterior_summary <- "No posterior probabilities available"
    }
  }

  ###### For Time Trees: Summarize divergence times ######
  if (include_divergence) {
    if (!is.null(tree$edge.length)) {
      # Calculate node ages (divergence times) from edge lengths
      node_ages <- node.depth.edgelength(tree)
      max_age <- max(node_ages)  # Root age
      node_ages <- max_age - node_ages  # Convert to divergence times

      summary_result$divergence_summary <- list(
        min = min(node_ages),
        max = max(node_ages),
        mean = mean(node_ages),
        mode = calculate_mode(node_ages),
        median = median(node_ages),
        sd = sd(node_ages)
      )
    } else {
      summary_result$divergence_summary <- "No edge lengths available to compute divergence times"
    }
  }

  # Return the summary
  return(summary_result)
}

