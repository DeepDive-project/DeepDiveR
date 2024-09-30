#' Edit values in the configuration file
#'
#' A function to modify parameters within a configuration object for DeepDive,
#'   created using `create_config()`.
#' @param config \code{character}. The name of the configuration object, created
#'   using `create_config()`, that will be edited.
#' @param module \code{character}. The name of the block of the configuration
#'   object to which the parameter belongs.
#' @param parameter \code{character}. The name of the parameter to edit in
#'   the configuration file.
#' @param value \code{}. The value you want to assign to the parameter. The type
#'   of value is dependent upon the parameter.

#' @returns A configuration object with the value of the desired parameter
#' changed.
#'
#' @details Add full list of default parameters.
#'
#' @import ConfigParser
#' @examples
#' edit_config(attribute_name = "extant_sp", value=c(10, 10000),
#'   module="simulations", config)
#' @export
edit_config <- function(config = NULL, module = NULL,
                        parameter = NULL, value = NULL){

  # Handling errors

  variable <- paste(value, collapse=" ")
  config$data[module][[1]][parameter] <- variable
  # pass lists, vectors etc and then turn into a string
}
