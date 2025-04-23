#' Edit values in the configuration file
#'
#' A function to modify parameters within a configuration object for DeepDive,
#'   created using `create_config()`. Default values are listed in the
#'   Parameters table, which can be view using `View(parameters)`, while the
#'   values currently held by each parameter can be checked using the notation
#'   `config$data$[module]$[parameter]`. Extensive checks ensure that it is
#'   possible for the specific parameter to hold the desired value.
#'
#' @param config \code{character}. The name of the configuration object, created
#'   using `create_config()`, that will be edited.
#' @param parameter \code{character}. The name of the parameter to edit in
#'   the configuration file.
#' @param value \code{}. The value you want to assign to the parameter. The type
#'   of value is dependent upon the parameter.

#' @returns A configuration object with the value of the desired parameter
#'   changed. Once finalised, configuration files should be saved using
#'   `config$write("file_name.ini")`.
#'
#' @details A full list of parameters, and details about the values they can
#'    take, can be viewed using `data(parameters)`. Parameters relating to
#'    geographic area inclusion in the simulations are better altered using
#'    `regions_matrix()`.
#'
#' @import ConfigParser
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
#' # Edit configuration object
#' edit_config(config = config, parameter = "present_diversity", value = 313)
#'
#' @export
edit_config <- function(config = NULL, parameter = NULL, value = NULL){

  # Load parameter table
  data(parameters)

  # Initial handling errors
  if (is.R6(config) == FALSE) {
    stop("`config` should be a configuration file.")
  }

  if (is.null(parameter)) {
    stop("A parameter must be provided, view table using data(parameters).")
  }

  if (parameter %in% parameters$parameter == FALSE) {
    stop("Parameter does not exist in configuration file.")
  }

  # Parameter-specific handling errors
  if (parameters$limits[which(parameters$parameter == parameter)] ==
      "character") {
    if (!is.character(value)) {
      stop("For this parameter, provided value must be a character string.")
    }
  }

  if (parameters$limits[which(parameters$parameter == parameter)] ==
      "logical") {
    if (!is.logical(value)) {
      stop("For this parameter, provided value must be logical (TRUE/FALSE).")
    }
  }

  if (parameters$limits[which(parameters$parameter == parameter)] ==
      "number range") {
    if (!is.numeric(value) || length(value != 2)) {
      stop("For this parameter, provided value must be a numerical vector of
            length 2, such as c(0,1).")
    }
  }

  if (parameters$limits[which(parameters$parameter == parameter)] ==
      "numerical string") {
    if (!is.numeric(value) || length(value <= 1)) {
      stop("For this parameter, provided value must be a numerical vector with
            length greater than 1, such as c(0,10).")
    }
  }

  if (parameters$limits[which(parameters$parameter == parameter)] ==
      "positive decimal") {
    if (!is.numeric(value) || value < 0 || value > 1) {
      stop("For this parameter, provided value must be between 0 and 1.")
    }
  }

  if (parameters$limits[which(parameters$parameter == parameter)] ==
      "positive decimal range") {
    if (!is.numeric(value) || length(value != 2) ||
        value[1] < 0 || value[1] > 1 || value[2] < 0 || value[2] > 1) {
      stop("For this parameter, provided value must be a numerical vector of
            length 2, with both values between 0 and 1, such as c(0.1, 0.9).")
    }
  }

  if (parameters$limits[which(parameters$parameter == parameter)] ==
      "positive integer") {
    if (!is.numeric(value) || value < 0 || round(value, 0) != value) {
      stop("For this parameter, provided value must be a positive whole number,
            such as 5.")
    }
  }

  if (parameters$limits[which(parameters$parameter == parameter)] ==
      "positive integer range") {
    if (!is.numeric(value) || length(value != 2) ||
        value[1] < 0 || value[2] < 0 ||
        round(value[1], 0) != value[1] || round(value[2], 0) != value[2]) {
      stop("For this parameter, provided value must be a numerical vector of
            length 2, with both values being positive whole numbers, such as
            c(1, 5).")
    }
  }

  if (parameters$limits[which(parameters$parameter == parameter)] ==
      "positive number") {
    if (!is.numeric(value) || value < 0) {
      stop("For this parameter, provided value must be a positive number,
            such as 1.2.")
    }
  }

  if (parameters$limits[which(parameters$parameter == parameter)] ==
      "positive number range") {
    if (!is.numeric(value) || length(value != 2) ||
        value[1] < 0 || value [2] < 0) {
      stop("For this parameter, provided value must be a numerical vector of
            length 2, with both values being a positive number, such as
            c(0.1, 1.5).")
    }
  }

  # Obtain module(s) containing parameters
  module <- parameters$module[which(parameters$parameter == parameter)]

  # If only present in one module, update there
  if (length(module) == 1) {
    config$data[[module]][[parameter]] <- paste(value)
  }

  # If present in multiple modules, update all
  if (length(module) > 1) {
    for (i in length(module)) {
      config$data[[module[i]]][[parameter]] <- paste(value)
    }
  }
}
