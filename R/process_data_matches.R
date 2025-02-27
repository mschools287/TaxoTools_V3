#' Check for Matches between Phylogenies and/or Datasets
#'
#' This function compares the individuals between phylogenies and/or datasets
#' to identify mismatches. It
#' checks if the specimens (tip labels in phylogenies or values in a specified
#' column in datasets) match between the provided inputs.
#'
#' @param phy1 A `phylo` object representing the first phylogeny.
#' @param phy2 A `phylo` object representing the second phylogeny.
#' @param data1 A data frame representing the first dataset.
#' @param data2 A data frame representing the second dataset.
#' @param data1_column A string indicating the column name in `data1` to compare.
#' @param data2_column A string indicating the column name in `data2` to compare.
#'
#' @details
#' The function allows three types of comparisons:
#' 1. **Two phylogenies**: It compares the tip labels of two phylogenies (`phy1` and `phy2`).
#' 2. **Two datasets**: It compares the values of a specific column (`data1_column` and `data2_column`) in two datasets (`data1` and `data2`).
#' 3. **One phylogeny and one dataset**: It compares the tip labels of a phylogeny (`phy1`) with the values in a column of a dataset (`data1_column` in `data1`).
#'
#' The function prints out any individuals that do not match in each comparison type.
#'
#' @return The function prints out the results but returns nothing (`invisible`).
#'
#' @examples
#' # Example comparing a phylogeny and a dataset:
#' process_data_matches(
#'   phy1 = data_treeTime,
#'   data1 = data_morphodata,
#'   data1_column = "Species_updated")
#'
#' @export
process_data_matches <- function(phy1 = NULL, phy2 = NULL, data1 = NULL, data2 = NULL,
                                   data1_column = NULL, data2_column = NULL) {

  # Check if both phylogenies are provided
  if (!is.null(phy1) && !is.null(phy2)) {
    if (!inherits(phy1, "phylo") || !inherits(phy2, "phylo")) {
      stop("Both inputs must be phylogeny objects if provided.")
    }

    # Check for matching tip labels between two phylogenies
    no_match_phylo1 <- setdiff(phy1$tip.label, phy2$tip.label)
    no_match_phylo2 <- setdiff(phy2$tip.label, phy1$tip.label)

    # Print results with proper line breaks
    cat("Specimens that do not match from phylogeny:\n")
    if (length(no_match_phylo1) > 0) {
      cat(paste(no_match_phylo1, collapse = ", "), "\n")
    } else {
      cat("None\n")
    }

    cat("\nSpecimens that do not match from phylogeny (second tree):\n")
    if (length(no_match_phylo2) > 0) {
      cat(paste(no_match_phylo2, collapse = ", "), "\n")
    } else {
      cat("None\n")
    }

    return(invisible())
  }

  # Check if both datasets are provided
  if (!is.null(data1) && !is.null(data2)) {
    if (is.null(data1_column) || is.null(data2_column)) {
      stop("Please provide the column names to compare between the two datasets.")
    }

    # Check if the specified columns exist in both datasets
    if (!(data1_column %in% colnames(data1)) || !(data2_column %in% colnames(data2))) {
      stop(paste("Columns", data1_column, "and", data2_column, "must exist in the respective datasets."))
    }

    # Compare the specified columns in both datasets
    no_match_data1 <- setdiff(data1[[data1_column]], data2[[data2_column]])
    no_match_data2 <- setdiff(data2[[data2_column]], data1[[data1_column]])

    # Print results with proper line breaks
    cat("Specimens that do not match from dataset 1:\n")
    if (length(no_match_data1) > 0) {
      cat(paste(no_match_data1, collapse = ", "), "\n")
    } else {
      cat("None\n")
    }

    cat("\nSpecimens that do not match from dataset 2:\n")
    if (length(no_match_data2) > 0) {
      cat(paste(no_match_data2, collapse = ", "), "\n")
    } else {
      cat("None\n")
    }

    return(invisible())
  }

  # Check if one phylogeny and one dataset are provided
  if (!is.null(phy1) && !is.null(data1)) {
    if (!inherits(phy1, "phylo")) {
      stop("The first input must be a phylogeny object.")
    }

    if (is.null(data1_column)) {
      stop("Please provide the column name in the dataset to compare with the phylogeny.")
    }

    # Check if the specified column exists in the dataset
    if (!(data1_column %in% colnames(data1))) {
      stop(paste("Column", data1_column, "must exist in the dataset."))
    }

    # Compare the tip labels from the phylogeny to the specified column in the dataset
    no_match_phylo <- setdiff(phy1$tip.label, data1[[data1_column]])
    no_match_data <- setdiff(data1[[data1_column]], phy1$tip.label)

    # Print results with proper line breaks
    cat("Specimens that do not match from phylogeny:\n")
    if (length(no_match_phylo) > 0) {
      cat(paste(no_match_phylo, collapse = ", "), "\n")
    } else {
      cat("None\n")
    }

    cat("\nSpecimens that do not match from dataset:\n")
    if (length(no_match_data) > 0) {
      cat(paste(no_match_data, collapse = ", "), "\n")
    } else {
      cat("None\n")
    }

    return(invisible())
  }

  stop("Please provide either two phylogenies, two datasets, or one phylogeny and one dataset.")
}
