#' Prepare DeepDive data input
#'
#' A function to use fossil occurrence data to make input files for use in
#' DeepDive.
#'
#' @param dat \code{dataframe}. The `dataframe` describing the occurrence data,
#'    including `Taxon`, `Area`, `MinAge`, `MaxAge` and `Locality` columns. The
#'    supplied `dataframe` should not contain any `NA` values.
#' @param bins \code{numeric}. A numerical `vector` designating the boundaries
#'    of the time bins used in the analysis.
#' @param r \code{integer}. The number of age assignment replicates. Defaults
#'    to 1.
#' @param age_m \code{character}. The age assignment method. Default is
#'    `median`. See `ages()` for more information.
#' @param output_file \code{character}. Defaults to `FALSE`, in which case the
#'    DeepDive input table is saved as an object. When a character string is
#'    provided, the object is written to a `csv` file with this name.
#' @returns If `output_file = NULL` a dataframe containing the occurrence data
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
#' # Create DeepDive input file
#' example1 <- prep_dd_input(dat = carnivora, bins = bins)
#' # Create DeepDive input files using random age assignment, with five reps
#' example2 <- prep_dd_input(dat = carnivora, bins = bins, r = 5, age_m =
#'   "random")
#' @export
prep_dd_input <- function(dat = NULL, bins = NULL, r = 1,
                          age_m = "median", output_file = NULL){

  # Handling errors
  if (is.data.frame(dat) == FALSE) {
    stop("`dat` should be a dataframe.")
  }

  if (any(is.na(dat))) {
    stop(paste("NA values detected in dataframe."))
  }

  if (is.vector(bins) == FALSE) {
    stop("`bins` should be a vector.")
  }

  if (is.numeric(bins) == FALSE) {
    stop("`bins` should contain numeric values (the ages of bin boundaries).")
  }

  if ("Taxon" %in% colnames(dat) == FALSE ||
      "Area" %in% colnames(dat) == FALSE ||
      "MinAge" %in% colnames(dat) == FALSE ||
      "MaxAge" %in% colnames(dat) == FALSE ||
      "Locality" %in% colnames(dat) == FALSE) {
    stop("`dat` does not contain columns `Taxon`, `Area`, `MinAge`, `MaxAge`,
         and `Locality`")
  }

  if (!is.numeric(dat$MinAge) || !is.numeric(dat$MaxAge)) {
    stop("`MinAge` and/or `MaxAge` columns are not of numeric class")
  }

  order_check <- dat$MaxAge - dat$MinAge
  if (any(order_check < 0)) {
    stop(paste("All `MinAge` values must be smaller than `MaxAge` values"))
  }

  if (is.numeric(r) == FALSE) {
    stop("`r` should be an integer.")
  }

  if (age_m != "median" && age_m != "random" && age_m != "random_by_loc") {
    stop(paste("`age_m` must be 'median', 'random' or 'random_by_loc'"))
  }

  if (age_m == "random_by_loc" && "Locality" %in% colnames(dat) == FALSE) {
    stop(paste("`Locality` must be provided to use 'random_by_loc' method"))
  }

  deepdive_input <- data.frame()

  for(rep in 1:r){

    # Get ages and append
    sampled_dat <- ages(dat, method = age_m)

    # Split data by area
    area_tables <- split(sampled_dat, f = sampled_dat$Area)

    # Get species or genera level data
    occs <- taxa_time_per_area(sampled_dat, area_tables, bins = bins)
    cnames <- c(colnames(occs))

    # Get locality data
    locs <- generate_locality_dataset(sampled_dat, bins = bins)

    # Get time bin data
    tbins <- data.frame(cbind(c("bin_start", "bin_mid", "bin_dur"), NA,
                              rbind(bins[-length(bins)],
                              rowMeans(cbind(bins[-length(bins)], bins[-1])),
                              bins[-length(bins)]-bins[-1])))
    names(tbins) <- cnames

    dd_input <- rbind(tbins, locs, occs)
    dd_input <- cbind(Replicate = r, dd_input)
    deepdive_input <- rbind(deepdive_input, dd_input)
  }

  # Get DeepDive input file
  if(!is.null(output_file)){
    write.csv(deepdive_input, output_file, row.names = FALSE)
  } else {
    return(deepdive_input)
  }
}
