#' Summarise occurrences per continent and time bin
#'
#' A function to summarise the number of occurrences per region, provided as a
#' table of regions x bins.
#'
#' @param dat \code{dataframe}. A `dataframe` containing the fossil occurrences,
#' including `Taxon`, `Area`, `MinAge`, `MaxAge` and `Locality` columns. A
#' `SampledAge` column must also be generated, such as using `ages()`.
#' @param bins \code{dataframe}. A `dataframe` designating the boundaries of
#' the time bins used in the analysis. These should reflect the empirical data
#' and be identical to the bins used in any corresponding DeepDive simulations
#' and training.
#' @returns A `dataframe` of shape `region x time bin` describing where
#' occurrences are recorded.
#'
#' @examples
#' #' # Import internal dataset
#' dat <- tetrapods
#' #' # Assign ages for tetrapods
#' dat <- ages(dat = tetrapods)
#' # Set Carboniferous to Triassic time bins
#' time_bins <- build_stages(start = 359, end = 200)
#' # Summarise distribution of occurrences
#' example1 <- generate_occurrence_dataset(dat = dat, bins = time_bins)
#' @export
generate_occurrence_dataset <- function(dat = NULL, bins = NULL){

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
         `Locality` and `SampledAge`")
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

  # Describe localities
  list_areas <- unique(dat$Area)

  #Create shell
  n_occurrences <- data.frame(matrix(0, length(list_areas), length(bins) - 1))

  for (i in seq_len(length(list_areas))){
    indices_areas <- which(dat$Area == list_areas[i])
    total_occs_for_area <- length(indices_areas)
    area_dat <- dat[indices_areas,]
    age_occs <- area_dat$SampledAge
    h <- hist(x = as.numeric(age_occs), breaks = bins$end, plot = F)
    n_occurrences[i,] <- h$counts
  }
  return(n_ocurrences)
}
