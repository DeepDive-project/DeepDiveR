#' Prepare DeepDive data input
#'
#' A function to use fossil occurrence data to make input files for use in
#' DeepDive.
#'
#' @param dat \code{dataframe}. The `dataframe` describing the occurrence data,
#'    including `Taxon`, `Region`, `MinAge`, `MaxAge` and `Locality` columns. The
#'    supplied `dataframe` should not contain any `NA` values.
#' @param bins \code{numeric}. A numerical `vector` designating the boundaries
#'    of the time bins used in the analysis.
#' @param r \code{integer}. The number of age assignment replicates. Default
#'    is 100.
#' @param age_m \code{character}. The age assignment method. Default is
#'    `random_by_loc`. See `ages()` for more information.
#' @param output_file \code{character}. Defaults to `FALSE`, in which case the
#'    DeepDive input table is saved as an object. When a character string is
#'    provided, the object is written to a `csv` file with this name.
#' @returns If `output_file = NULL` a `dataframe` containing the occurrence data
#'    summary is returned as an object. If `output_file` is given a character
#'    string, the summary is saved to a `csv` file with this name, ready to
#'    input into DeepDive.
#'
#' @importFrom utils write.csv
#' @examples
#' # Import internal dataset
#' data(carnivora)
#' # Generate vector describing time bin boundaries
#' bins <- c(66, 23, 2.6, 0)
#' # Create DeepDive input file with three reps
#' example1 <- prep_dd_input(dat = carnivora, bins = bins, r = 3)
#' # Create DeepDive input files using median age assignment
#' example2 <- prep_dd_input(dat = carnivora, bins = bins, r = 1, age_m =
#'   "median")
#' @export
prep_dd_input <- function(dat = NULL, bins = NULL, r = 100,
                          age_m = "random_by_loc", output_file = NULL){

  # Handling errors
  if (is.null(dat)) {
    stop("`dat` must be provided.")
  }

  if (is.data.frame(dat) == FALSE) {
    stop("`dat` should be a dataframe.")
  }

  if (any(is.na(dat))) {
    stop(paste("NA values detected in dataframe."))
  }

  if (length(unique(dat$Region)) < 2) {
    stop(paste("At least two unique Regions must be present in dataframe."))
  }

  if (is.vector(bins) == FALSE) {
    stop("`bins` should be a vector.")
  }

  if (is.numeric(bins) == FALSE) {
    stop("`bins` should contain numeric values (the ages of bin boundaries).")
  }

  if ("Taxon" %in% colnames(dat) == FALSE ||
      "Region" %in% colnames(dat) == FALSE ||
      "MinAge" %in% colnames(dat) == FALSE ||
      "MaxAge" %in% colnames(dat) == FALSE ||
      "Locality" %in% colnames(dat) == FALSE) {
    stop("`dat` does not contain columns `Taxon`, `Region`, `MinAge`, `MaxAge`,
         and `Locality`.")
  }

  if (!is.numeric(dat$MinAge) || !is.numeric(dat$MaxAge)) {
    stop("`MinAge` and/or `MaxAge` columns are not of numeric class.")
  }

  order_check <- dat$MaxAge - dat$MinAge
  if (any(order_check < 0)) {
    stop(paste("All `MinAge` values must be smaller than `MaxAge` values."))
  }

  if (is.numeric(r) == FALSE) {
    stop("`r` should be an integer.")
  }

  if (age_m != "median" && age_m != "random" && age_m != "random_by_loc") {
    stop(paste("`age_m` must be 'median', 'random' or 'random_by_loc'."))
  }

  if (age_m == "median" && r > 1) {
    warning(paste("When age assignment is 'median', no sampling occurs, so all
                  replicates will be identical."))
  }

  if (!is.null(output_file) && !is.character(output_file)) {
    stop("`output_file` must be a character string.")
  }

  if (!is.null(output_file) && !endsWith(output_file, ".csv")) {
    stop(paste("Output file name must end `.csv`."))
  }

  deepdive_input <- data.frame()

  for(rep in 1:r){

    # Get ages and append
    sampled_dat <- ages(dat, method = age_m)

    # Get species or genera level data
    occs <- taxa_region_time(dat = sampled_dat, bins = bins)
    cnames <- c(colnames(occs))

    # Get locality data
    locs <- localities_through_time(dat = sampled_dat, bins = bins)

    # Get time bin data
    tbins <- data.frame(cbind(c("bin_start", "bin_mid", "bin_dur"), NA,
                              rbind(bins[-length(bins)],
                              rowMeans(cbind(bins[-length(bins)], bins[-1])),
                              bins[-length(bins)]-bins[-1])))
    names(tbins) <- cnames

    dd_input <- rbind(tbins, locs, occs)
    dd_input <- cbind(Replicate = rep, dd_input)
    deepdive_input <- rbind(deepdive_input, dd_input)
  }

  # Get DeepDive input file
  if(!is.null(output_file)){
    write.csv(deepdive_input, output_file, row.names = FALSE)
  } else {
    return(deepdive_input)
  }
}
