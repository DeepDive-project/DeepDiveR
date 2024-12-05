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
#'                         n_regions = length(unique(carnivora$Region)))
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
  lstm_nodes <- paste(lstm_nodes, collapse = " ")
  dense_nodes <- sort(dense_nodes, decreasing=TRUE)
  dense_nodes <- paste(lstm_nodes, collapse = " ")

  lstm_parameter_name <- paste0("lstm_model_", model_name)

  dense_parameter_name <- paste0("dense_model_", model_name)

  # Check if the combination of LSTM and dense nodes already exists in the config.
  model_exists <- FALSE
  for (existing_model in names(config$data$model_training)){
    existing_lstms <- config$data$model_training[[existing_model]]
    existing_dense <- config$data$model_training[[gsub("lstm_model_", "dense_model_", existing_model)]]
    if (identical(existing_lstms, lstm_nodes) && identical(existing_dense, dense_nodes)){
      model_exists <- TRUE
    }
  }

  if (model_exists == TRUE) {
    stop("This combination of lstm_nodes and dense_nodes already exists in the config. Model has not been added to the configuration.")
  }


  # If a model name is already in use, append a number to prevent overwriting
  counter<- 1
  while(lstm_parameter_name %in% names(config$data$model_training)){
    lstm_parameter_name <- paste0(lstm_parameter_name, "_", counter)
    dense_parameter_name <- paste0(dense_parameter_name, "_", counter)
    counter <- counter + 1
    print("`model_name` already exists. A number has been appended to prevent overwriting.")
  }

  # add model parameters to the configuration file
  config$data$model_training[[lstm_parameter_name]] <- lstm_nodes
  config$data$model_training[[dense_parameter_name]] <- dense_nodes
  print("Model has been added to the configuration.")
}
