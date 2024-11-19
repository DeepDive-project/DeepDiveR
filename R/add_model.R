#' Set the architecture of models to be trained in deepdive
#'
#' A function to assign the number of layers and nodes in the long short-term
#' memory and dense layers of a deep neural network in DeepDive.
#'
#' @param config \code{character}. The name of the configuration object, created
#'   using create_config()`, that will be edited.
#' @param lstm_nodes \code{numeric}. A numerical 'vector' designating the number
#'   of nodes and layers to be used in the long short-term memory architecture.
#' @param dense_nodes \code{numeric}. A numerical 'vector' designating the 
#'   number of nodes and layers to be used in the dense node architecture.
#' @param model_name \code{string}. An identifier to destinguish between models. 
#'
#' @returns Adds attributes for a new model, specified with lstm_nodes and
#' dense_nodes.
#'
#' @details This function can be used to train multiple models in the DeepDive
#' framework. Without running the function, a default of two lstm layers 64, 32 
#' and two fully-connected dense node layers 64, 32 will be used.
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
#'                         n_areas = length(unique(carnivora$Area)))
#' # Add model architectures to the config file
#' add_model(config = config, lstm_nodes = c(128, 64), dense_nodes = c(64, 32), name = "1")
#' @export
add_model <- function(config = NULL, lstm_nodes = NULL, dense_nodes = NULL, 
                      model_name = "") {
  
  # Handling errors
  if (is.R6(config) == FALSE) {
    stop("`config` should be a configuration file.")
  }
  
  if (!is.numeric(dense_nodes)) {
    stop("`dense_nodes` must be numeric.")
  }
  
  if (!is.numeric(lstm_nodes)) {
    stop("`lstm_nodes` must be numeric.")
  }
  
  
  lstm_nodes <- sort(lstm_nodes, decreasing=TRUE)
  dense_nodes <- sort(dense_nodes, decreasing=TRUE)
  
  lstm_parameter_name <- paste0("lstm_model_", model_name)
  
  dense_parameter_name <- paste0("dense_model_", model_name)
  
  
  #### below is not working, was intended to check if a parameter name is
  #### already present in the model training module and if so append 1 to the
  #### end or something like this to prevent overwriting.
  #### Should it be mandatory to provide a model_name so it doesn't name a param
  #### e.g. lstm_model_
  if (!is.null(config$data$model_training[lstm_parameter_name])){
    print("`model_name` must be different from existing lstm entries to prevent overwriting, model name has had the value 1 appended.")
    lstm_parameter_name <- paste0(lstm_parameter_name, 1)
  }
  
  if (!is.null(config$data$model_training[dense_parameter_name])){
    print("`model_name` must be different from existing dense entries to prevent overwriting, model name has had the value 1 appended.")
    dense_parameter_name <- paste0(dense_parameter_name, 1)
  }
      
  config$data$model_training[[lstm_parameter_name]] <- lstm_nodes
  
  config$data$model_training[[dense_parameter_name]] <- dense_nodes
  
}
