#' Taxon Name Update Table
#'
#' A dataset containing a mapping of old taxon names (those present in the trees) to new taxon names, designed for updating names in phylogenies for better visualization.
#' Tables such as this can be used to replace outdated or inconsistent names in phylogenetic analyses.
#'
#' @format A data frame with 2 columns and as many rows as there are taxa to rename:
#' \describe{
#'   \item{Old_Name}{Character. The current names of taxa in the phylogeny.}
#'   \item{New_Name}{Character. The updated names to replace the old names in the phylogeny.}
#' }
#'
#' @details
#' This dataset is useful for maintaining consistency in taxon names across phylogenetic analyses,
#' particularly when names are updated due to taxonomic revisions or corrections.
#'
#' @examples
#' data(data_names)
#' head(data_names)
#'
#' # Example: Viewing a phylogeny with updated names using data_names
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
#' @source
#' The names were generated to match the names of taxa in the example phylogenies.
"data_names"
