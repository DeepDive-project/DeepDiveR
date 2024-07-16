#' Summarise locality distribution
#'
#' A function to create a `dataframe` of the summed number of localities per
#' area and time bin, provided as a table of regions x bins.
#'
#' @param dat \code{dataframe}. A `dataframe` containing the fossil occurrences,
#' including `Taxon`, `Area`, `MinAge`, `MaxAge` and `Locality` columns.
#' @param bins \code{dataframe}. A `dataframe` designating the boundaries of
#' the time bins used in the analysis. These should reflect the empirical data
#' and be identical to the bins used in any corresponding DeepDive simulations
#' and training.
#' @returns A `dataframe` of shape `region x time bin` describing where
#' occurrences are recorded.
#'
#' @examples
#' # Import internal dataset
#' dat <- tetrapods
#' # Set Carboniferous to Triassic time bins
#' time_bins <- build_stages(start = 359, end = 200)
#' # Summarise distribution of occurrences
#' example1 <- generate_locality_dataset(dat = dat, bins = time_bins)
#' @export
generate_locality_dataset <- function(dat = NULL, bins = NULL){

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
      "Locality" %in% colnames(dat) == FALSE) {
    stop("`dat` does not contain columns `Taxon`, `Area`, `MinAge`, `MaxAge`,
         and `Locality`")
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
  dat$Locality <- paste(dat$Area,dat$Locality)
  list_areas <- sort(unique(dat$Area))

  # Create shell
  localities <- data.frame(matrix(0, length(list_areas), length(bins) - 1))
  #bins <- sort(-abs(bins))

  for (i in seq_len(length(list_areas))){
    indices_areas <- which(dat$Area == list_areas[i])
    locality_ids <- dat[indices_areas,]$Locality
    uni_loc_ids <- unique(locality_ids)
    no_loc_in_area <- c()
    for(j in uni_loc_ids){
      t <- dat$SampledAge[which(dat$Locality == j)]
      no_loc_in_area <- c(no_loc_in_area, unique(t))
    }
    h <- hist(x = as.numeric(no_loc_in_area), breaks = bins$end, plot=F)
    localities[i,] <- h$counts
  }
  colnames(localities) <- sprintf("t%d", seq(length(bins) - 1))
  locs <- cbind(Type="locs", Area = list_areas, localities)
  return(data.frame(locs))
}
