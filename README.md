# TaxoTools: An R Package for Integrating Character, Geographic, and Phylogenetic Data for Taxonomic Descriptions and Analyses

## Overview
*TaxoTools* is an R package designed to streamline the process of analyzing and documenting taxonomic data. It integrates character, geographic, and phylogenetic data to assist researchers in taxonomic analyses and descriptions. The package includes functions for data cleaning, visualization, statistical analysis, and generating outputs suitable for publication.

## Installation
To install *TaxoTools*, use one of the following commands:

1. Install from CRAN (if available)
```R
install.packages("TaxoTools")
```

2. Or install the development version from GitHub
```R
install.packages("devtools")
devtools::install_github("mschools287/TaxoTools")
```

## Features
### Data Processing Functions (process_*)
- **Data cleaning**: Identify and address missing or inconsistent values in trait datasets.
- **Trait conversions**: Convert trait units for usability.
- **Trait standardization**: Normalize trait values for comparability.
- **Name formatting**: Change names in datasets and compare names between datasets

### Trait Analysis Functions (trait_*)
- **Trait comparisons**: Identify key differences across groups and highlight distinguishing traits.
- **Summary statistics**: Calculate and visualize trait correlations, ranges, and group-specific metrics.
- **Diagnostic character check**: Check which groups are not distinguished by a trait.

### Geographic Analysis Functions (geo_*)
- **Occurrence mapping**: Plot occurrences on a geographic map.
- **Range estimation**: Calculate geographic ranges and minimum convex hull areas.
- **Spatial overlap**: Assess range overlap between groups.
- **Map relatedness**: Visulize phylogenetic relatedness on a geographic map.

### Phylogenetic Analysis Functions (phylo_*)
- **Support value comparison**: Overlay ML and Bayesian node support values for direct comparison.
- **Divergence analysis**: Generate histograms of divergence times for taxa or clades.
- **Clade summaries**: Merge node information, highlight key taxa, and summarize tree statistics.
- ***Phylogeny visulization**: Visulize and compare phylogenies, highlighting key taxa and nodes for interpretation.

### Taxonomic Documentation Functions (document_*)
- **Automated descriptions**: Generate formatted descriptions and diagnoses based on trait data.
- **Appendix generation**: Compile appendix sections, material examined sections, and trait range tables.
- **Trait standout analysis**: Identify diagnostic characters based on comparative trait distributions.
- **Taxonomic hierarchy retrieval**: Fetch and incorporate taxonomic information from NCBI.

## Usage
### Function and workflow examples
These functions and workflows use example character data, geographic data, metadata, and phylogenetic trees that are included in the package.
```R
library(TaxoTools)
```
1. Example of a data processing function (process_*)
```R
# Load your data
data(data_morphodata)

#Remove individuals that are less than 85% of the largest trait1 value per species
process_juveniles(
  data_morphodata,
  trait = "trait1", 
  group_by = "Species", 
  cutoff = 0.85)
```
2. Example of a trait analysis function (trait_*)
```R
# Load your data
data(data_morphodata)

# Example: Compare traits function
trait_compare(
  data = data_morphodata,
  group_by_column = "Species",
  group_values = c("Species A", "Species B"),
  traits = c("trait1", "trait2"))
```
3. Example of a geographic analysis function (geo_*)
```R
# Load your data
data(data_morphodata)

# Example: Calculate convex hull function
geo_convex_hull(
  data = data_morphodata,
  grouped_by = "Clade",
  lat_column = "Latitude",
  long_column = "Longitude")
```
4. Example of a phylogenetic workflow (phylo_*)
```R
# Load your data
data(data_treeML)
data(data_treeBayesian)

# Example (Step 1): Generate node tables from ML and Bayesian trees
data_ml_node_table  <- phylo_node_table(
   tree = data_treeML,
   method = "ML")
data_bayesian_node_table  <- phylo_node_table(
  tree = data_treeBayesian,
  method = "Bayesian")

# Example (Step 2): Merge node tables from ML and Bayesian trees
data_merged_table <- phylo_merge_nodes(
  ml_node_table = data_ml_node_table,
  bayesian_node_table = data_bayesian_node_table)

#Example (Step 3): Plot a phylogeny with node info from both ML and Bayesian support values
phylo_plot_tree_nodes(
  tree = data_treeML,
  tree_type = "ML",
  merged_table = data_merged_table)
```
5. Example of a documentation function (document_*)
```R
# Load your data
data(data_morphodata)

# Example: Species descriptions function
document_description_section(
  data = data_morphodata,
  trait_columns = c("trait1", "trait2", "trait3", "trait4"),
  grouped_by = "Clade")
```

### Detailed Documentation
For detailed function usage, see the package vignette:
```R
vignette("TaxoTools")
```

## Contributing
Contributions are welcome! Please submit issues or pull requests via the [GitHub repository](https://github.com/mschools287/TaxoTools).

## Citation
If you use **TaxoTools** in your research, please cite the accompanying paper:

[Author Names]. (Year). TaxoTools: An R Package for Integrating Morphological and Phylogenetic Data in Taxonomy. *Systematic Biology*. DOI: [insert DOI here]

## License
**TaxoTools** is licensed under the MIT License. See the `LICENSE` file for details.

## Contact
For questions or support, please contact Molly Schools at mollymjs@gmail.com.