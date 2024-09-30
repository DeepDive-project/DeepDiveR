#' Edit values in config
#'
#' 'edit_config()' accesses attributes in the configuration file and adjusts the
#' assigned value(s) to that of a provided integer or vector.
#' @param attribute_name A string providing direction to the attribute you want to edit e.g. "total_sp"
#' @param value An integer or vector value you want to assign to the attribute.
#' @param module A string indicating the block of the config the attribute can be found within.
#' @param config A config which can be generated in create_config(), that you will edit here.
#' @returns Edits settings in the config .ini file.
#' @examples
#' edit_config(attribute_name = "extant_sp", value=c(10, 10000), module="simulations", config)
#' @export
edit_config <- function(attribute_name, value, module, config){
  variable <- paste(value, collapse=" ")
  config$data[module][[1]][attribute_name] <- variable
  # pass lists, vectors etc and then turn into a string
}
