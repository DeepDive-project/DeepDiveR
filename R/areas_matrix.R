#' Make discrete geographic regions appear or disappear
#'
#' A function to modify the simulations module by providing a maximum and
#'   minimum age range for geographic regions being connected to the study
#'   system.
#' @param config \code{character}. The name of the configuration object, created
#'   using `create_config()`, that will be edited.
#' @param area_ages \code{dataframe}. If left NULL (default), all geographic
#'   regions are configured to be present throughout the simulations. Otherwise,
#'   a `dataframe` describing the `MinAge` and `MaxAge` for each geographic
#'   `Area`, with older ages in column one. The number of rows in the
#'   `dataframe` must match `n_areas` in the configuration file. The
#'    supplied `dataframe` should not contain any `NA` values.
#' @param presence \code{logical}. When TRUE (the default), regions become
#'   available to occupy within the time frames specified. When FALSE, regions
#'   will be removed from the simulations between the ages specified.
#'
#' @returns Adds an attribute for each area (named "area1", "area2"... etc) with
#'   minimum and maximum age of migration becoming possible.
#'
#' @importFrom R6 is.R6
#' @examples
#' # Import internal dataset
#' data(carnivora)
#' # Generate vector describing time bin boundaries
#' bins <- c(66, 23, 2.6, 0)
#' # Create configuration object
#' config <- create_config(file_prefix = "carnivora",
#'                         data_file = "data/carnivora_deepdive_input.csv",
#'                         bins = bins,
#'                         n_areas = length(unique(carnivora$Area)))
#' # Generate dataframe describing area presence
#' area_ages <- rbind(c("Africa", 66, 66),
#'                    c("Asia", 66, 66),
#'                    c("Europe", 66, 66),
#'                    c("North America", 66, 66),
#'                    c("South America", 11.608, 7.3))
#' area_ages <- as.data.frame(area_ages)
#' # Label columns
#' colnames(area_ages) <- c("Area", "MaxAge", "MinAge")
#' # Connect area data to configuration file
#' config <- areas_matrix(config = config, area_ages = area_ages)
#' @export
areas_matrix <- function(config = NULL, area_ages = NULL,
                         presence = TRUE) {

  # Handling errors
  if (is.R6(config) == FALSE) {
    stop("`config` should be a configuration file.")
  }

  if (!is.null(area_ages) && !is.data.frame(area_ages)) {
    stop("`area_ages` should be NULL or a dataframe.")
  }

  # Retrieve n_areas from configuration file
  n_areas <- config$data$general$n_areas

  if (!is.null(area_ages) && (nrow(area_ages) != n_areas)) {
    stop("Number of rows in `area_ages` should equal `n_areas` in config.")
  }

  if (!is.logical(presence)) {
    stop("`presence` must be TRUE or FALSE.")
  }

  if (!is.null(area_ages)) {
    # Reorder areas alphabetically
    area_ages <- area_ages[order(area_ages$Area),]
  }

  # Retrieve time bins from configuration file
  bins <- as.numeric(unlist(strsplit(config$data$general$time_bins, " ")))

  if(is.null(area_ages) & presence == TRUE){
    # for each region, specify two ages between which connection to others will
    # be established
    for(i in 1:n_areas) {
      parameter <- paste0("area_", "start", i)
      config$data$simulations[[parameter]] <- rep.int(max(bins), 2)
    }
  }

  if(is.null(area_ages) & presence == FALSE){
    # for each region, specify two ages between which connection to others will
    # be removed
    for(i in 1:n_areas) {
      parameter <- paste0("area_", "end", i)
      config$data$simulations[[parameter]] <- rep.int(min(bins), 2)
    }
  }

  if(!is.null(area_ages) & presence == TRUE){
    for(i in 1:n_areas) {
      parameter <- paste0("area_", "end", i)
      config$data$simulations[[parameter]] <- c(as.numeric(area_ages$MaxAge[i]),
                                                as.numeric(area_ages$MinAge[i]))
    }
  }

  if(!is.null(area_ages) & presence == FALSE){
    for(i in 1:n_areas) {
      config$data$simulations[[parameter]] <- c(as.numeric(area_ages$MaxAge[i]),
                                                as.numeric(area_ages$MinAge[i]))
    }
  }
}
