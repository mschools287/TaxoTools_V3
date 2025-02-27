#' Count Specimens in a Phylogeny
#'
#' This function takes a phylogeny as input and returns the total number of specimens
#' (tips) in the phylogeny, along with the count of specimens belonging to a user-specified
#' group. The group can be specified by a whole or partial character string.
#'
#' @param data A phylogeny in the form of a 'phylo' object, typically from the \code{ape} package.
#' @param group_name A character string representing the name to match. Default is NULL.
#'
#' @return A list containing:
#'   \item{total_specimens}{The total number of tips in the phylogeny.}
#'   \item{specified_count}{The count of tips that match the specified character string. This is NULL
#'         if no group_name is provided.}
#'
#' @examples
#' # Example usage with a phylogenetic tree (data_treeML should be a phylo object):
#' process_phylo_count(
#'   data = data_treeML,
#'   group_name = "clade_3")
#'
#' @export
process_phylo_count <- function(data, group_name = NULL) {
  # Ensure input is a phylo object
  if (!inherits(data, "phylo")) {
    stop("Input must be a 'phylo' object.")
  }

  # Total number of specimens in the phylogeny
  total_count <- length(data$tip.label)

  # If no group_name is provided, return only the total count
  if (is.null(group_name)) {
    return(list(
      total_specimens = total_count,
      specified_count = NULL
    ))
  }

  # Find specimens matching the specified species/clade
  matching_tips <- grep(group_name, data$tip.label, value = TRUE)

  # If group_name matches an internal node (clade), get all descendant tips
  if (group_name %in% data$node.label) {
    node_id <- which(data$node.label == group_name) + length(data$tip.label)
    descendant_tips <- extract.clade(data, node_id)$tip.label
    matching_tips <- unique(c(matching_tips, descendant_tips))
  }

  # Count specimens for the specified species/clade
  specified_count <- length(matching_tips)

  return(list(
    total_specimens = total_count,
    specified_count = specified_count
  ))
}

