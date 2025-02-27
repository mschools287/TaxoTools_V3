#' Fetch Taxonomic Hierarchy from NCBI
#'
#' This function retrieves the taxonomic hierarchy for a given taxonomic name from the NCBI taxonomy database.
#' It returns a named vector where each element represents a taxonomic rank and its corresponding scientific name.
#'
#' @param name A character string specifying the name to query in the NCBI taxonomy database.
#'
#' @return A named vector where:
#' \itemize{
#'   \item \code{Names} The taxonomic ranks.
#'   \item \code{Values} The corresponding scientific names for each rank.
#' }
#'
#' @details The function queries the NCBI taxonomy database using \code{entrez_search} to retrieve group
#' information. It then fetches taxonomic details using \code{entrez_fetch}, parses the XML response, and
#' extracts the hierarchical taxonomic ranks and their corresponding names.
#'
#' @examples
#' document_get_taxonomy(name = "Homo sapiens")
#'
#' @import rentrez xml2
#' @export
document_get_taxonomy <- function(name) {
  # Query NCBI for species information
  search_result <- entrez_search(db = "taxonomy", term = name)

  if (search_result$count == 0) {
    stop("Species not found in NCBI taxonomy database.")
  }

  # Fetch taxonomic information for the first result with rettype = "xml"
  taxon_data <- entrez_fetch(db = "taxonomy", id = search_result$ids[1], rettype = "xml")

  # Parse taxonomic hierarchy from the result
  taxon_list <- read_xml(taxon_data)

  # Extract the taxonomic ranks and their corresponding names
  taxon_ranks <- xml_find_all(taxon_list, ".//Taxon/Rank")
  taxon_names <- xml_find_all(taxon_list, ".//Taxon/ScientificName")

  # Ensure that we have the same number of ranks and names
  rank_values <- xml_text(taxon_ranks)
  name_values <- xml_text(taxon_names)

  # Create a named vector based on ranks and names
  taxonomy_vector <- setNames(name_values, rank_values)

  # Return the named vector with the taxonomic hierarchy
  return(taxonomy_vector)
}
