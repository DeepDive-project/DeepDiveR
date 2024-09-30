#' Create DeepDive config object and file
#'
#' 'create_config()' generates a config object with settings needed to launch
#' DeepDive scripts in python. Uses ConfigParser (Hoefling, 2017) formatting.
#' @param simulate A TRUE/FALSE statement that switches on and off generation of simulated datasets used in model training and testing.
#' @param model_training A TRUE/FALSE statement that switches on and off the training of new RNN models.
#' @param empirical_predictions A TRUE/FALSE statement that switches on and off empirical analyses.
#' @param autotune When TRUE a new config will be saved with adjusted parameters used for analyses that reflect the empirical data.
#' @param present_diversity Number of extant taxa which predictions will be conditioned on. When NA no conditioning occurs.
#' @param path_wd Working directory where files for analyses can be found.
#' @param bins A vector of bin boundary ages.
#' @param sim_name A string that identifies a set of simulations.
#' @param n_areas An integer number of discrete sampling regions e.g. continents, basins.
#' @param simulations_file File where simulations will be saved.
#' @param add_test A TRUE/FALSE statement, when true a test set with the same settings as the training set are generated.
#' @param models_file File where trained models can be saved and retrieved.
#' @param feature_file File name for training features.
#' @param label_file File name for training labels.
#' @param empirical_input_file File name where empirical data in the input format for use in deepdive will be saved.
#' @param output_file File where outputs will be saved.
#' @returns Creates configuration file with settings to launch DeepDive.
#' @examples
#' config <- create_config(wd = paste0(getwd()), bins = time_bins,
#' sim_name = "test", n_areas = length(unique(your_data$Area)),
#' simulations_file = paste0(path_dat, "simulations"), add_test = T,
#' present_diversity = 200)
#' @export
create_config <- function(simulate = TRUE, model_training = TRUE,
                          empirical_predictions = TRUE,

                          autotune=TRUE,
                          present_diversity = NA,

                          # settings needed for simulation module
                          path_wd = NULL,
                          bins = NULL,
                          sim_name = "simulations",
                          n_areas = NULL,
                          simulations_file = "simulations",
                          add_test = TRUE,

                          # for the training module
                          models_file = "trained_models",
                          feature_file = NULL,
                          label_file = NULL,

                          # empirial analysis module
                          empirical_input_file = NULL,

                          output_file = NULL){

  config <- ConfigParser::ConfigParser$new()
  sims <- c()

  # general directory
  general <- c()
  general$wd <- path_wd
  if(any(bins < 0)){
    bins <- -bins
  }
  bins <- sort(bins, decreasing=TRUE)
  general$time_bins <- sort(paste(bins, collapse=" "))
  if(is.null(general$time_bins)){
    print("Warning: time_bins is set to NULL, please adjust using argument in create_config or using the set function below to provide values")
  }
  general$n_areas <- n_areas  # number of discrete regions
  if(is.null(general$n_areas)){
    print("Warning: n_areas is set to NULL, please adjust using argument in create_config or using the set function below")
  }
  general$autotune <- autotune
  general$present_diversity <- present_diversity

  config$data$general <- general

  # simulations
  if(simulate == TRUE){
    folders <- simulations_file
    sims$sim_name <- sim_name
    sims$n_CPUS <- 1  # number of CPUs used for simulations
    sims$n_training_simulations <- 10000  # total number of training simulations
    sims$training_seed <- 123
    if(add_test == TRUE){
      sims$test_seed <- 432
      sims$n_test_simulations <- 100  # total number of test simulations
    }


    sims$s_species <- 1  # number of starting species
    sims$total_sp <- paste(100, 5000, collapse="")  # min/max size data set
    sims$root_r <- paste(0.8*(max(bins)-min(bins))+min(bins), max(bins), collapse="")  # range of ages for origin of clade
    sims$min_extinct_sp <- 0  # minimum number of extinct lineages allowed
    sims$extant_sp <- paste(0, 10000, collapse="")  # min/max number of living species
    sims$rangeL <- paste(0.02, 0.5, collapse="")  # range of birth rates
    sims$rangeM <- paste(0.02, 0.5, collapse="")  # range of death rates
    sims$log_uniform_rates <- FALSE
    sims$p_mass_extinction <- 0.01  # probability of mass extinction per my
    sims$fixed_mass_extinction <- NA  # known time of mass extinction
    sims$magnitude_mass_ext <- paste(0.5, 1, collapse="")
    sims$p_equilibrium <- 0.01  # probability of equilibrium per my
    sims$p_constant_bd <- 0.01  # probability of constant birth-death rates per my
    sims$p_mass_speciation <- 0.01  # probability of mass speciation per my
    sims$p_dd_model <- 0.05  # probability of diversity-dependent diversification
    sims$dd_K <- paste(100,1000, collapse = "")  # carrying capacity for dd clades
    sims$dd_maxL <- 1 # starting sp rate for dd
    sims$pr_extant_clade <- 0.7  # probability of simulating an extant clade
    sims$poiL <- 4  # expected number of birth rate shifts
    sims$poiM <- 4  # expected number of death rate shifts
    sims$scale <- 10  # scaling
    sims$vectorize <- TRUE  # True will vectorise the birth-death simulation.


    # fossil simulator settings
    sims$eta <- paste(1, 1.75, collapse="")  # area-sp stochasticity
    sims$p_gap <- paste(0.01, 0.95, collapse="")  # probability of 0 preservation in a time bin
    sims$dispersal_rate <- "None"
    sims$max_dist <- 1
    sims$disp_rate_mean <- paste(0, 1, collapse="")
    sims$disp_rate_variance <- 1
    sims$area_mean <- 20  # G(a,b) distributed preservation rates across areas
    sims$area_variance <- 5
    sims$size_concentration_parameter <- paste(0.1, 3, collapse="")  # single value or array of length n_areas
    sims$link_area_size_carrying_capacity <- paste(1, 10, collapse="")  # positive, larger numbers = stronger link between area size and carrying capacity
    sims$p_origination_a_slope_mean <- 2  # mean slope of probability of origination area mean
    sims$p_origination_a_slope_sd <- 0.5  # standard deviations of the slopes
    sims$sp_mean <- paste(0.1, 0.5, collapse="")  # G(a,b) distributed preservation rates across species
    sims$sp_variance <- 2
    sims$slope <- paste(-0.01, 0, collapse="")  # change in log-sampling rate through time (log-linear)
    sims$intercept <- paste(0.1, 0.5, collapse="")  # initial sampling rate
    sims$sd_through_time <- paste(0.001, 0.01, collapse="")  # st dev in log-sampling rate through time
    sims$sd_through_time_skyline <- 1
    sims$mean_n_epochs_skyline <- 4
    sims$fraction_skyline_sampling <- 0.5
    sims$mean_skyline_sampling <- paste(0.1, 10, collapse="")
    sims$maximum_localities_per_bin <- 200
    sims$species_per_locality_multiplier <- 1
    sims$singletons_frequency <- 0.1  # adjust this to reflect empirical data
    sims$sims_folder <- simulations_file

    config$data$simulations <- sims

    }


  if(model_training == TRUE){
    # Settings for training models
    folders <- models_file
    mt <- c()
    mt$sims_folder <- simulations_file
    mt$model_folder <- models_file
    mt$lstm_layers <- paste(64, 32, collapse="")
    mt$dense_layer <- paste(64, 32, collapse="")
    mt$dropout <- 0
    mt$max_epochs <- 1000
    mt$patience <- 10
    mt$batch_size <- 100
    mt$validation_split <- 0.2
    mt$f <- feature_file
    mt$l <- label_file
    config$data$model_training <- mt
  }


  # For empirical predictions
  if(empirical_predictions == TRUE){
    e <- c()
    e$empirical_input_file <- empirical_input_file
    e$model_folder <- models_file
    e$n_predictions <- 1  # number of predictions per input file
    e$replicates <- 100  # number of age randomisation replicates used in data_pipeline.R
    e$output_file <- output_file
    folders <- output_file
    config$data$empirical_predictions <- e
  }

  return(config)
}
