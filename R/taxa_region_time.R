#' Summarise taxa per region per time bin
#'
#' A function to produce a `dataframe` of shape 'taxon x region x time bin' from
#'    occurrence data.
#' @param dat \code{dataframe}. The `dataframe` describing the occurrence data,
#'    including `Taxon`, `Region`, `Locality` and `SampledAge` columns. The
#'    supplied `dataframe` should not contain any `NA` values.
#' @param bins \code{numeric}. A numerical `vector` designating the boundaries
#'    of the time bins used in the analysis.
#'
#' @returns A `dataframe` of shape 'taxon x region x time bin' describing where
#'    occurrences are recorded.
#'
#' @importFrom graphics hist
#' @examples
#' # Import internal dataset
#' data(carnivora)
#' # Generate vector describing time bin boundaries
#' bins <- c(66, 23, 2.6, 0)
#' # Generate sampled ages using the "median" method
#' carnivora <- ages(dat = carnivora)
#' # Compute taxon per region and time frequencies
#' taxa_region_time(dat = carnivora, bins = bins)
#' @export
taxa_region_time <- function(dat = NULL, bins = NULL) {

  # Handling errors
  if (is.data.frame(dat) == FALSE) {
    stop("`dat` should be a dataframe.")
  }

  if (any(is.na(dat))) {
    stop(paste("NA values detected in dataframe."))
  }

  if (is.vector(bins) == FALSE) {
    stop("`bins` should be a vector.")
  }

  if (is.numeric(bins) == FALSE) {
    stop("`bins` should contain numeric values (the ages of bin boundaries).")
  }

  if ("Taxon" %in% colnames(dat) == FALSE ||
      "Region" %in% colnames(dat) == FALSE ||
      "Locality" %in% colnames(dat) == FALSE ||
      "SampledAge" %in% colnames(dat) == FALSE) {
    stop("`dat` does not contain columns `Taxon`, `Region`, `Locality` and
         `SampledAge`")
  }

  if (!is.numeric(dat$SampledAge)) {
    stop("`SampledAge` column is not of numeric class")
  }

  # Split data by region
  region_tables <- split(dat, f = dat$Region)

  # Create empty dataframe
  occs_table <- data.frame()
  bins <- sort(-abs(bins))

  for(i in 1:length(region_tables)){
    # Pull one region
    region_table <- region_tables[[i]]
    region_name <- region_table$Region[1]
    region_taxa <- unique(region_table$Taxon)
    # Summarise global diversity
    global_taxa_list <- unique(dat$Taxon)
    all_taxa <- length(global_taxa_list)

    # Create empty dataframe
    taxa_time_table <- data.frame(matrix(0, all_taxa, length(bins) - 1))

    for (j in 1:length(region_taxa)){
      # Identify occurrences unique to the region and taxon
      indices_occurrences <- which(region_table$Taxon == region_taxa[j])
      age_occs <- region_table[indices_occurrences,]$SampledAge
      # Bin the occurrences through time
      h <- hist(x = -as.numeric(age_occs), breaks = bins, plot = FALSE)
      t_i <- which(global_taxa_list == region_taxa[j])
      taxa_time_table[t_i,] <- h$counts
    }

    # Annotate table
    colnames(taxa_time_table) <- sprintf("t%d", seq(length(bins) - 1))
    taxa_time_table <- cbind(Type = "occs", Region = region_name, taxa_time_table)
    row.names(taxa_time_table) <- global_taxa_list
    occs_table <- rbind(occs_table, taxa_time_table)
  }

  return(data.frame(occs_table))
}
