#' Make discrete geographic regions appear or disappear
#'
#' A function to add area constraints to new module in the configuration file
#' by providing a maximum and minimum age uncertainty for geographic regions
#' being connected to or disconnected from the study system.
#' @param config \code{character}. The name of the configuration object, created
#'   using `create_config()`, that will be edited.
#' @param region_ages \code{dataframe}. If left NULL (default), all geographic
#'   regions are configured to be present throughout the simulations. Otherwise,
#'   a `dataframe` describing uncertainty around the appearance or disappearance
#'   of geographic areas should be specified. `MinAge` should state the younger
#'   limit, and `MaxAge` the older limit, for each geographic `Region`. The number
#'   of rows in the `dataframe` must match `n_regions` in the configuration file.
#'   The supplied `dataframe` should not contain any `NA` values.
#' @param presence \code{logical}. When TRUE (the default), regions become
#'   available to occupy based on the time frames specified. When FALSE, regions
#'   will be removed from the simulations based on the time frames specified.
#'
#' @returns Adds an attribute for each region (named "Region1", "Region2"...
#'   etc.) with minimum and maximum certainty limits of migration becoming
#'   possible.
#'
#' @details Note that this function does not need to be used if all geographic
#'   regions are considered to be accessible throughout the simulations; this is
#'   the default.
#'
#' @importFrom R6 is.R6
#' @examples
#' # Import internal dataset
#' data(carnivora)
#' # Generate vector describing time bin boundaries
#' bins <- c(66, 23, 2.6, 0)
#' # Create configuration object
#' config <- create_config(name = "carnivora",
#'                         data_file = "data/carnivora_deepdive_input.csv",
#'                         bins = bins,
#'                         n_regions = length(unique(carnivora$Region)))
#' # Generate dataframe describing region presence
#' region_ages <- rbind(c("Africa", 66, 66),
#'                    c("Asia", 66, 66),
#'                    c("Europe", 66, 66),
#'                    c("North America", 66, 66),
#'                    c("South America", 11.608, 7.3))
#' region_ages <- as.data.frame(region_ages)
#' # Label columns
#' colnames(region_ages) <- c("Region", "MaxAge", "MinAge")
#' # Connect region data to configuration file
#' region_matrix(config = config, region_ages = region_ages)
#' @export
regions_matrix <- function(config = NULL, region_ages = NULL,
                         presence = TRUE) {

  # Handling errors
  if (is.R6(config) == FALSE) {
    stop("`config` should be a configuration file.")
  }

  if (!is.null(region_ages) && !is.data.frame(region_ages)) {
    stop("`region_ages` should be NULL or a dataframe.")
  }

  if (!is.logical(presence)) {
    stop("`presence` should be TRUE or FALSE.")
  }

  # Retrieve n_regions from configuration file
  n_regions <- config$data$general$n_regions

  if (!is.null(region_ages) && (nrow(region_ages) != n_regions)) {
    stop("Number of rows in `region_ages` should equal `n_regions` in config.")
  }

  if (!is.logical(presence)) {
    stop("`presence` must be TRUE or FALSE.")
  }

  if (!is.null(region_ages)) {
    # Reorder regions alphabetically
    region_ages <- region_ages[order(region_ages$Region),]
  }

  # Remove any blank spaces from region names for parameter naming
  for(i in 1:length(region_ages[,1])){
    region_ages[,1][i] <- gsub(" ", "", region_ages[,1][i], fixed = TRUE)
  }


  # Retrieve time bins from configuration file
  bins <- as.numeric(unlist(strsplit(config$data$general$time_bins, " ")))

  if(is.null(region_ages) & presence == TRUE){
    # for each region, specify two ages between which connection to others will
    # be established
    for(i in 1:n_regions) {
      parameter <- paste0("region_start_", region_ages$Region[i])
      config$data$region_constraints[[parameter]] <- paste(rep.int(max(bins), 2),
                                                    collapse = " ")
    }
  }

  if(is.null(region_ages) & presence == FALSE){
    # for each region, specify two ages between which connection to others will
    # be removed
    for(i in 1:n_regions) {
      parameter <- paste0("region_end_", region_ages$Region[i])
      config$data$region_constraints[[parameter]] <- paste(rep.int(max(bins), 2),
                                                         collapse = " ")
    }
  }

  if(!is.null(region_ages) & presence == TRUE){
    for(i in 1:n_regions) {
      parameter <- paste0("region_start_", region_ages$Region[i])
      config$data$region_constraints[[parameter]] <- paste(c(region_ages$MaxAge[i],
                                                           region_ages$MinAge[i]),
                                                         collapse = " ")
    }
  }

  if(!is.null(region_ages) & presence == FALSE){
    for(i in 1:n_regions) {
      parameter <- paste0("region_end_", region_ages$Region[i])
      config$data$region_constraints[[parameter]] <- paste(c(region_ages$MaxAge[i],
                                                           region_ages$MinAge[i]),
                                                         collapse = " ")
    }
  }
}
