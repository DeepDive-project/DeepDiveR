#' Summarise locality occupation through time
#'
#' A function to create a `dataframe` of the number of localities per region and
#' time bin.
#' @param dat \code{dataframe}. A `dataframe` containing the fossil occurrences,
#'    including `Taxon`, `Region`, `Locality` and `SampledAge` columns. The
#'    supplied `dataframe` should not contain any `NA` values.
#' @param bins \code{numeric}. A numerical `vector` designating the boundaries
#'    of the time bins used in the analysis.
#'
#' @returns A `dataframe` of shape 'region x time bin' describing where
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
#' # Compute locality counts
#' localities_through_time(dat = carnivora, bins = bins)
#' @export
localities_through_time <- function(dat = NULL, bins = NULL){

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

  # Concatenate Region and Locality
  dat$Locality <- paste(dat$Region,dat$Locality)

  # List unique localities
  list_regions <- sort(unique(dat$Region))

  # Create empty dataframe
  localities <- data.frame(matrix(0, length(list_regions), length(bins) - 1))
  bins <- sort(-abs(bins))

  for (i in seq_len(length(list_regions))) {
    # Identify occurrences linked to one region
    indices_regions <- which(dat$Region == list_regions[i])
    locality_ids <- dat[indices_regions,]$Locality
    # Count the unique localities associated with that region
    uni_loc_ids <- unique(locality_ids)
    no_loc_in_region <- c()
    for(j in uni_loc_ids){
      t <- dat$SampledAge[which(dat$Locality == j)]
      no_loc_in_region <- c(no_loc_in_region, unique(t))
    }
    # Bin the localities by time
    h <- hist(x = -as.numeric(no_loc_in_region), breaks = bins, plot = FALSE)
    localities[i,] <- h$counts
  }
  # Annotate table
  colnames(localities) <- sprintf("t%d", seq(length(bins) - 1))
  locs <- cbind(Type = "locs", Region = list_regions, localities)

  return(data.frame(locs))
}
