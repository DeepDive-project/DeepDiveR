#' Make discrete geographic regions appear or disappear
#'
#' 'areas_matrix()' adds attributes in the simulations module which provide a
#' maximum and minimum age range for regions becoming connected.
#' @param area_ages A matrix of minimum and maximum ages for areas appearing, with older ages in column one. Must have a number of rows = number of discrete areas.
#' @param n_areas The number of unique discrete areas used in the analysis.
#' @param config A config which can be generated in create_config() that you will add attributes to here.
#' @param bins A vector of time bins.
#' @param label Default "start" makes regions become available through time, if changed to "end" regions will no longer be available between the ages specified.
#' @returns Adds an attribute for each area (named "area1", "area2"... etc) with min and max age of migration becoming possible.
#' @examples
#' areas_matrix(area_ages, n_areas = length(unique(your_data$Area)), config)
#' @export
areas_matrix <- function(area_ages = NULL, n_areas, config, bins, label = "start"){
  if(is.null(area_ages) & label=="start"){
    # for each region, specify two ages between which connection to others will be established.
    area_ages <- c()
    for(i in 1:n_areas){
      area_ages <- rbind(area_ages,
                         c(max(bins), max(bins)))
    }
    set_value(attribute_name = paste0("area_", label, i), value=c(area_ages[i, 1], area_ages[i, 2]), module="simulations", config)
  }
  if(is.null(area_ages) & label=="end"){
    # for each region, specify two ages between which connection to others will be removed.
    area_ages <- c()
    for(i in 1:n_areas){
      area_ages <- rbind(area_ages,
                         c(min(bins), min(bins)))
    }
    set_value(attribute_name = paste0("area_", label, i), value=c(area_ages[i, 1], area_ages[i, 2]), module="simulations", config)
  }
  if(!is.null(area_ages)){
    for(i in 1:n_areas){
      set_value(attribute_name = paste0("area_", label, i), value=c(area_ages[i, 1], area_ages[i, 2]), module="simulations", config)
    }
  }
}
