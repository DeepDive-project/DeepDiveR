#' Calculate the number of taxa per time bin per area
#'
#' A function to produce a table of shape `taxon x region x time bin` where
#' occurrences are recorded.
#'
#' @param dat \code{dataframe}. The `dataframe` describing the occurrence data,
#' including `Taxon`, `Area`, `MinAge`, `MaxAge` and `Locality` columns. A
#' `SampledAge` column must also be generated, such as using `ages()`.
#' @param bins \code{dataframe}. A `dataframe` designating the boundaries of
#' the time bins used in the analysis. These should reflect the empirical data
#' and be identical to the bins used in any corresponding DeepDive simulations
#' and training.
#' @returns A `dataframe` of shape `taxon x region x time bin` describing where
#' occurrences are recorded.
#'
#' @import dplyr
#' @examples
#' # Import internal dataset
#' dat <- tetrapods
#' # Assign ages for tetrapods
#' dat <- ages(dat = tetrapods)
#' # Set Carboniferous to Triassic time bins
#' time_bins <- build_stages(start = 359, end = 200)
#' # Calculate number of taxa per time bin and region
#' example1 <- taxa_time_per_area(dat = tetrapods, bins = time_bins)
#' @export
taxa_time_per_area <- function(dat = NULL, bins = NULL){

  # Handling errors
  if (is.data.frame(dat) == FALSE) {
    stop("`dat` should be a dataframe.")
  }

  if (is.data.frame(bins) == FALSE) {
    stop("`bins` should be a dataframe.")
  }

  if ("Taxon" %in% colnames(dat) == FALSE ||
      "Area" %in% colnames(dat) == FALSE ||
      "MinAge" %in% colnames(dat) == FALSE ||
      "MaxAge" %in% colnames(dat) == FALSE ||
      "Locality" %in% colnames(dat) == FALSE ||
      "SampledAge" %in% colnames(dat) == FALSE) {
    stop("`dat` does not contain columns `Taxon`, `Area`, `MinAge`, `MaxAge`,
         `Locality`, and `SampledAge`")
  }

  if (!is.numeric(dat$MinAge) || !is.numeric(dat$MaxAge)) {
    stop("`MinAge` and/or `MaxAge` columns are not of numeric class")
  }

  if ("stage" %in% colnames(bins) == FALSE ||
      "start" %in% colnames(bins) == FALSE ||
      "end" %in% colnames(bins) == FALSE ||
      "midpoint" %in% colnames(bins) == FALSE) {
    stop("`bins` does not contain columns `stage`, `start`, `end`, and
         `midpoint`")
  }

  if (!is.numeric(bins$start) || !is.numeric(bins$end) ||
      !is.numeric(bins$midpoint)) {
    stop("`start` and/or `end` and/or `midpoint` columns are not of numeric
         class")
  }

  # Find unique areas
  regions <- unique(dat$Area)

  taxonomic_level <- "Taxon"

  occs_table <- data.frame()

  #bins <- sort(-abs(bins))

  for(i in 1:length(regions)){
    area_table <- filter(dat, Area == regions[i])
    area_name <- area_table$Area[1]
    area_taxa <- unique(area_table[[taxonomic_level]])
    global_taxa_list <- unique(dat[[taxonomic_level]])
    all_taxa <- length(global_taxa_list)
    taxa_time_table <- data.frame(matrix(0, all_taxa, length(bins)-1))
    for (j in 1:length(area_taxa)){
      indices_occurrences <- which(area_table[[taxonomic_level]] == area_taxa[j])
      age_occs <- area_table[indices_occurrences,]$SampledAge
      h <- hist(x = age_occs, breaks = bins$end, plot = F)
      t_i <- which(global_taxa_list == area_taxa[j])
      taxa_time_table[t_i,] <- h$counts
    }
    colnames(taxa_time_table) <- sprintf("t%d", seq(length(bins) - 1))
    taxa_time_table <- cbind(Type = "occs", Area = area_name, taxa_time_table)
    row.names(taxa_time_table) <- global_taxa_list
    occs_table <- rbind(occs_table, taxa_time_table)
  }

  return(data.frame(occs_table))
}
