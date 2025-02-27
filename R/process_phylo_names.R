#' Update Phylogeny Tree Tip Labels
#'
#' This function updates the tip labels in a phylogenetic tree based on a mapping table of old and new names.
#' It matches the names in the tree to those in the table and replaces the old names with the new ones.
#'
#' @param tree A phylogenetic tree of class `phylo`. The tree must have tip labels that match those in the `Old_Name` column of `name_table`.
#' @param name_table A data frame with two columns: `Old_Name` containing the current names in the tree, and `New_Name` containing the updated names.
#'
#' @return A `phylo` object with updated tip labels.
#'
#' @details The function assumes that the `name_table` has two columns: `Old_Name` and `New_Name`. It checks if the `tree`'s tip labels exist in the `Old_Name` column of `name_table` and updates the labels accordingly. If a name does not exist in the `name_table`, it remains unchanged.
#'
#' @examples
#' # Example usage:
#' process_phylo_names(data_treeML, data_names)
#'
#' @import phytools
#' @export
process_phylo_names <- function(tree, name_table) {
  # Ensure the name_table has the correct columns
  if (!all(c("Old_Name", "New_Name") %in% colnames(name_table))) {
    stop("The name_table must have 'Old_Name' and 'New_Name' columns.")
  }

  # Create a named vector for the mapping from Old_Name to New_Name
  name_mapping <- setNames(name_table$New_Name, name_table$Old_Name)

  # Update the tip labels in the phylogeny tree
  tree$tip.label <- ifelse(tree$tip.label %in% names(name_mapping),
                           name_mapping[tree$tip.label],
                           tree$tip.label)

  return(tree)
}
