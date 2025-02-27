## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(TaxoTools)
library(ape)
library(phytools)

## -----------------------------------------------------------------------------
sauresia_Time <- read.tree("C:\\Users\\mschools\\Documents\\TaxoTools\\vignettes\\examples\\sauresia_48tx_timetree.nwk")
plot(sauresia_Time)

## -----------------------------------------------------------------------------
sauresia_ML <- read.tree("C:\\Users\\mschools\\Documents\\TaxoTools\\vignettes\\examples\\sauresia_48tx_IQtree_rooted.nwk")
sauresia_ML_rooted <- reroot(
    sauresia_ML, 
    48, 
    position = sauresia_ML$edge.length[which(sauresia_ML$edge[,2]==48)]/2)
is.rooted(sauresia_ML_rooted)
plot(sauresia_ML_rooted, show.node.label = T)

## -----------------------------------------------------------------------------
sauresia_B <- read.tree("C:\\Users\\mschools\\Documents\\TaxoTools\\vignettes\\examples\\sauresia_48tx_MrBayes_rooted.nwk")
sauresia_B_rooted <- reroot(
    sauresia_B, 
    48, 
    position = sauresia_B$edge.length[which(sauresia_B$edge[,2]==48)]/2)
is.rooted(sauresia_B_rooted)
plot(sauresia_B_rooted, show.node.label = T)

## -----------------------------------------------------------------------------
sauresia_morpho <- read.csv("C:\\Users\\mschools\\Documents\\TaxoTools\\vignettes\\examples\\sauresia_morpho.csv")

## -----------------------------------------------------------------------------
sauresia_morpho2 <- read.csv("C:\\Users\\mschools\\Documents\\TaxoTools\\vignettes\\examples\\sauresia_morpho2.csv")

## -----------------------------------------------------------------------------
new_names <- read.csv("C:\\Users\\mschools\\Documents\\TaxoTools\\vignettes\\examples\\new_tree_names.csv")

## -----------------------------------------------------------------------------
process_check_data(
  data = sauresia_morpho,
  lat_col = "Latitude..decimal.",
  long_col = "Longitude..decimal.",
  elevation_col = "Elevation",
  trait_columns = c(
    "SVL", "pattern", "head.markings", "nuchal.lines",
    "dots.in.lateral.band", "mental.to.vent.scale.count",
    "midbody.scalecount", "total.finger.lamella", "strigae.on.ten.scales",
    "total.toe.length", "scale.under.eye.to.lip.distance", "eye.length", "arm.length",
    "ear.width", "rostral.width", "rostral.height", "head.length", "mental.width",
    "post.mental.width", "cloaca.width", "prefrontal.width",
    "largest.supraoccular.width", "Finger.III.length", "ear.to.eye.distance",
    "head.width", "frontal.width", "frontal.lenght"),
  date_col = "Date.Collected")

## -----------------------------------------------------------------------------
sauresia_morpho3 <- process_column_names(
    sauresia_morpho2, 
    new_column_names = c(
        "Name in Tree", "New names", "Type", "Genus", "Species", "Museum", "Number",
        "Country", "Province", "Locality", "Elevation",
        "Date Collected", "Collectors", "Remarks", "Latitude", "Longitude",
        "SVL", "Pattern", "Head Markings", "Nuchal Lines", "Dots in Lateral Band",
        "Mental to Vent Scale Count", "Midbody ScaleCount", "Total Finger Lamella",
        "Strigae on Ten Scales", "Total Toe Length", "Scale Under Eye to Lip Distance",
        "Eye Length", "Arm Length", "Ear Width", "Rostral Width", "Rostral Height",
        "Head Length", "Mental Width", "Post Mental Width", "Cloaca Width",
        "Prefrontal Width", "Largest Supraoccular Width", "Finger III Length",
        "Ear to Eye Distance", "Head Width", "Frontal Width", "Frontal Length"))

## -----------------------------------------------------------------------------
process_juveniles(
  sauresia_morpho3,
  trait = "SVL",
  group_by = "Species",
  cutoff = 0.85)

## -----------------------------------------------------------------------------
process_check_missing(data = sauresia_morpho3)

## -----------------------------------------------------------------------------
process_convert_units(
  data = sauresia_morpho3,
  trait_column = "SVL",
  conversion_factor = 10)

## -----------------------------------------------------------------------------
process_standardize_traits(
  data = sauresia_morpho3,
  reference_trait = "SVL",
  traits_to_standardize = c(
    "SVL", 
    "Pattern", 
    "Head Markings",
    "Head Width", "Frontal Width"))

## -----------------------------------------------------------------------------
process_phylo_count(
  sauresia_ML_rooted,
  group_name = "sepsoides")

## -----------------------------------------------------------------------------
process_specimen_count(
  sauresia_morpho3,
  grouped_by = "Species")

## -----------------------------------------------------------------------------
sauresia_ML_rooted_named <- process_phylo_names(
  sauresia_ML_rooted,
  new_names)
sauresia_B_rooted_named <- process_phylo_names(
  sauresia_B_rooted,
  new_names)
sauresia_T_named <- process_phylo_names(
  sauresia_Time,
  new_names)

## -----------------------------------------------------------------------------
process_data_matches(
  phy1 = sauresia_ML_rooted,
  data1 = sauresia_morpho3,
  data1_column = "Name in Tree")

## -----------------------------------------------------------------------------
process_subset_group(
  sauresia_morpho3,
  grouped_by = "Species",
  prefix = "Subset_")

## -----------------------------------------------------------------------------
trait_filter(
  sauresia_morpho3,
  individuals_column = "Number",
  trait_column = "SVL",
  threshold = 60,
  above = TRUE)

## -----------------------------------------------------------------------------
trait_min_max(
  sauresia_morpho3,
  grouped_by = "Species",
  trait_column = "SVL")

## -----------------------------------------------------------------------------
trait_summary(
  data = sauresia_morpho3,
  grouped_by = "Species",
  trait_column = "SVL")

## -----------------------------------------------------------------------------
trait_compare(
  data = sauresia_morpho3,
  group_by_column = "Species",
  group_values = c("Sauresia cayemitae", "Sauresia pangnolae"),
  traits = c("SVL", "Total Toe Length"))

## -----------------------------------------------------------------------------
trait_not_separated(
  data = sauresia_morpho3,
  trait_columns = c("SVL", "Pattern", "Head Markings",
                    "Head Width", "Frontal Width"),
  grouped_by = "Species")

## -----------------------------------------------------------------------------
geo_range(
  sauresia_morpho3,
  grouped_by = "Species",
  lat_col = "Latitude",
  long_col = "Longitude")

## -----------------------------------------------------------------------------
geo_convex_hull(
  sauresia_morpho3,
  grouped_by = "Species",
  lat_col = "Latitude",
  long_col = "Longitude",
  area_unit = "km^2")

## -----------------------------------------------------------------------------
geo_plot_map(
  sauresia_morpho3,
  grouped_by = "Species",
  lat_col = "Latitude",
  long_col = "Longitude")

## -----------------------------------------------------------------------------
geo_range_overlaps(
  sauresia_morpho3,
  group_by = "Species",
  lat_col = "Latitude",
  long_col = "Longitude")

## -----------------------------------------------------------------------------
geo_relatedness(
  data = sauresia_morpho3,
  lat_column = "Latitude",
  long_column = "Longitude",
  phylogeny = sauresia_ML_rooted_named,
  reference_species = "Wetmorena surda 1",
  individuals_column = "New names")

## -----------------------------------------------------------------------------
phylo_tree_summary(
  tree = sauresia_ML_rooted_named,
  method = "ML")

## -----------------------------------------------------------------------------
phylo_clade_support(
  sauresia_ML_rooted_named,
  "sepsoides",
  method = "ML")

## -----------------------------------------------------------------------------
phylo_divergence_histogram(
  sauresia_T_named,
  bin_width = .1,
  color = "lightblue")

## -----------------------------------------------------------------------------
phylo_taxa_divergence_hist(
  tree = sauresia_T_named,
  taxa_list = c("sepsoides", "agramma", "cayemitae"),
  colors = c("darkseagreen", "deeppink", "pink"),
  bin_width = 2)

## -----------------------------------------------------------------------------
phylo_node_support_hist(
  tree_ml = sauresia_ML_rooted_named,
  tree_bayesian = sauresia_B_rooted_named,
  bin_width = 5,
  colors = c("skyblue", "green"))

## -----------------------------------------------------------------------------
phylo_heatmap(phylogeny = sauresia_ML_rooted_named)

## -----------------------------------------------------------------------------
phylo_highlight_taxa(
  trees = list(sauresia_ML_rooted_named, sauresia_B_rooted_named),
  taxa_to_highlight = c("sepsoides", "Grande Cayemite"),
  highlight_colors = c("green", "orange"),
  node_support_threshold = 95,
  support_color = "blue",
  highlight_shape = "circle",
  shape_size = 1,
  tip_label_size = 0.8)

## -----------------------------------------------------------------------------
phylo_plot_trees(
  trees = list(sauresia_ML_rooted_named, sauresia_B_rooted_named, sauresia_T_named),
  methods = c("ML", "Bayesian", "Time Tree"),
  show_bootstraps = T,
  bootstrap_color = "colored",
  show_posterior = T,
  posterior_color = "colored",
  time_scale = T,
  side_by_side = T,
  name_data = new_names,
  phylogeny_type = "phylogram",
  edge_color = "blue")

## -----------------------------------------------------------------------------
# Create and store ML node table
ML_node_table <- phylo_node_table(
  sauresia_ML_rooted_named,
  "ML")

# Create and store Bayesian node table 
bayesian_node_table <- phylo_node_table(
  sauresia_B_rooted_named,
  "Bayesian")

## -----------------------------------------------------------------------------
merged_table <- phylo_merge_nodes(
  ML_node_table,
  bayesian_node_table)

## -----------------------------------------------------------------------------
phylo_plot_tree_nodes(
  tree = sauresia_ML_rooted_named,
  tree_type = "ML",
  merged_table = merged_table)

## -----------------------------------------------------------------------------
document_get_taxonomy("Sauresia sepsoides")

## -----------------------------------------------------------------------------
document_material_examined(
  sauresia_morpho3,
  grouped_by = "Species",
  region_col = "Province",
  institution_col = "Museum",
  number_col = "Number",
  collector_col = "Collectors",
  locality_col = "Locality",
  date_collected_col = "Date Collected",
  type_col = "Type",
  sex_col = "Sex",
  lat_col = "Latitude",
  long_col = "Longitude",
  elevation_col = "Elevation")

## -----------------------------------------------------------------------------
document_description_section(
  sauresia_morpho3,
  trait_columns = c("SVL", "Pattern", "Head Markings", "Nuchal Lines", "Dots in Lateral Band",
                    "Mental to Vent Scale Count", "Midbody ScaleCount", "Total Finger Lamella",
                    "Strigae on Ten Scales", "Total Toe Length", "Scale Under Eye to Lip Distance",
                    "Eye Length", "Arm Length", "Ear Width", "Rostral Width", "Rostral Height",
                    "Head Length", "Mental Width", "Post Mental Width", "Cloaca Width",
                    "Prefrontal Width", "Largest Supraoccular Width", "Finger III Length",
                    "Ear to Eye Distance", "Head Width", "Frontal Width", "Frontal Length"),
  grouped_by = "Species")

## -----------------------------------------------------------------------------
document_standout_traits(
  sauresia_morpho3,
  trait_columns = c("SVL", "Pattern", "Head Markings", "Nuchal Lines", "Dots in Lateral Band",
                    "Mental to Vent Scale Count", "Midbody ScaleCount", "Total Finger Lamella",
                    "Strigae on Ten Scales", "Total Toe Length", "Scale Under Eye to Lip Distance",
                    "Eye Length", "Arm Length", "Ear Width", "Rostral Width", "Rostral Height",
                    "Head Length", "Mental Width", "Post Mental Width", "Cloaca Width",
                    "Prefrontal Width", "Largest Supraoccular Width", "Finger III Length",
                    "Ear to Eye Distance", "Head Width", "Frontal Width", "Frontal Length"),
  grouped_by = "Species",
  cutoff = 2/3)

## -----------------------------------------------------------------------------
document_diagnoses_section(sauresia_morpho3,
                           trait_columns = c("SVL", "Pattern", "Head Markings", "Nuchal Lines", "Dots in Lateral Band",
                                             "Mental to Vent Scale Count", "Midbody ScaleCount", "Total Finger Lamella",
                                             "Strigae on Ten Scales", "Total Toe Length", "Scale Under Eye to Lip Distance",
                                             "Eye Length", "Arm Length", "Ear Width", "Rostral Width", "Rostral Height",
                                             "Head Length", "Mental Width", "Post Mental Width", "Cloaca Width",
                                             "Prefrontal Width", "Largest Supraoccular Width", "Finger III Length",
                                             "Ear to Eye Distance", "Head Width", "Frontal Width", "Frontal Length"),
                           grouped_by = "Species")

## -----------------------------------------------------------------------------
document_trait_range_table(
  sauresia_morpho3,
  grouped_by = "Species",
  trait_columns = c("SVL", "Pattern", "Head Markings", "Nuchal Lines", "Dots in Lateral Band",
                    "Mental to Vent Scale Count", "Midbody ScaleCount", "Total Finger Lamella",
                    "Strigae on Ten Scales", "Total Toe Length", "Scale Under Eye to Lip Distance",
                    "Eye Length", "Arm Length", "Ear Width", "Rostral Width", "Rostral Height",
                    "Head Length", "Mental Width", "Post Mental Width", "Cloaca Width",
                    "Prefrontal Width", "Largest Supraoccular Width", "Finger III Length",
                    "Ear to Eye Distance", "Head Width", "Frontal Width", "Frontal Length"),
  include_counts = TRUE,
  include_averages = TRUE)

## -----------------------------------------------------------------------------
document_appendix_section(
  sauresia_morpho3,
  grouped_by = "Species",
  country_code_col = "Country",
  region_col = "Province",
  locality_col = "Locality",
  lat_col = "Latitude",
  long_col = "Longitude",
  elevation_col = "Elevation",
  institution_col = "Museum",
  number_col = "Number")

## -----------------------------------------------------------------------------
?process_column_names
?document_appendix_section

