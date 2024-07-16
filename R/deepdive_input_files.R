#' Make input files for DeepDive
#'
#' A function to use fossil occurrence data to make input files for use in
#' DeepDive.
#'
#' @param dat \code{dataframe}. The `dataframe` describing the occurrence data,
#'    including `Taxon`, `Area`, `MinAge`, `MaxAge` and `Locality` columns.
#' @param bins \code{dataframe}. A `dataframe` designating the boundaries of
#'    the time bins used in the analysis.
#' @param reps \code{integer}. The number of age assignment replicates. Defaults
#'    to 1.
#' @param age_m \code{character}. The age assignment method. Default is
#'    `median`.
#' @returns A `csv` file is produced for each age replicate. The `Type` column
#'    indicates whether data are locality counts or the binned occurrence data
#'    per region.
#'
#' @examples
#' # Import internal dataset
#' dat <- tetrapods
#' # Create DeepDive input file
#' example1 <- write_dd_files(dat = dat, bins = bins)
#' # Create DeepDive input files using random age assigment, with five reps
#' example2 <- write_dd_files(dat = dat, bins = bins, r = 5, age_m = "random")
#' @export
write_dd_files <- function(dat = NULL, bins = NULL, reps = 1, age_m = "median"){

  # Handling errors
  if (is.data.frame(dat) == FALSE) {
    stop("`dat` should be a dataframe.")
  }

  if (is.data.frame(bins) == FALSE) {
    stop("`bins` should be a dataframe.")
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

  if ("stage" %in% colnames(bins) == FALSE ||
      "start" %in% colnames(bins) == FALSE ||
      "end" %in% colnames(bins) == FALSE ||
      "midpoint" %in% colnames(bins) == FALSE) {
    stop("`bins` does not contain columns `stage`, `start`, `end`, and
         `midpoint`")
  }

  if (!is.numeric(bins$start) || !is.numeric(bins$end) ||
      !is.numeric(bins$midpoint)) {
    stop("`start` and/or `end` and/or `midpoint` columns are not of numeric
         class")
  }

  if (is.numeric(reps) == FALSE) {
    stop("`reps` should be an integer.")
  }

  if (method != "median" && method != "random" && method != "random_by_loc") {
    stop(paste("`method` must be 'median', 'random' or 'random_by_loc'"))
  }


  # Get ages and append
  sampled_dat <- ages(dat, method = age_m)

  # Split data by area
  area_tables <- split(sampled_dat, f = sampled_dat$Area)

  # Get species or genera level data
  occs <- taxa_time_per_area(sampled_dat, bins = bins)

  # Get locality data
  locs <- generate_locality_dataset(sampled_dat, bins = bins)

  # Collate and save
  deepdive_input <- rbind(locs, occs)
  write.csv(deepdive_input,
            paste0(name, "empirical_data/", reps, "deepdive_input.csv"),
            row.names = FALSE)

  #generate_occurrence_dataset(sampled_dat, area_tables, bins=bins, res, name)
  # Occurrences per locality through time for each area
  #occs_per_loc <- occs/(locs+1)
}


#' Generate DeepDive input in one file
#'
#' 'write_dd_files2()' makes a single input .csv file for use in DeepDive.
#' @param dat Data frame of occurrences with taxon ID, Area, MinAge, MaxAge and
#' Locality columns.
#' @param r The number of age assignment replicates
#' @param age_m The age assignment method
#' @param bins Time bins used in the analysis (should reflect the empirical data
#' and be identical to the bins used in any corresponding DeepDive simulations
#' and training)
#' @returns Here each age replicate is indicated in the 'R' column and can be
#' set using the r setting. Locality and occurrence data by time bin are
#' generated alongside the replicate number, the data type (either locs or occs)
#' and the region ID.
#' @examples
#' write_dd_files2(dat, r=replicate, bins=bins, age_m="median")
#' @export
write_dd_files2 <- function(dat, r=replicate, age_m="median", bins){
  deepdive_input <- data.frame()
  for(rep in 1:r){
    sampled_dat <- ages(dat, method=age_m) # Get ages and append
    area_tables <- split(sampled_dat, f = sampled_dat$Area)  # Split data by area

    # Get species or genera level data
    occs <- taxa_time_per_area(sampled_dat, area_tables, bins=bins)
    cnames <- c(colnames(occs))

    # Get locality data
    locs <- generate_locality_dataset(sampled_dat, bins=bins)

    # Get time bin data
    tbins <- data.frame(cbind(c("bin_mid", "bin_dur"), NA,
                              rbind(bins$midpoint, bins$start-bins$end)))
    names(tbins) <- cnames

    dd_input <- rbind(locs, occs)
    dd_input <- cbind(R=rep, dd_input)
    deepdive_input <- rbind(deepdive_input, dd_input)
  }
  write.csv(deepdive_input, paste0(name, "empirical_data/deepdive_input.csv"),
            row.names=FALSE)
}


#' Prepare DeepDive Input
#'
#' 'prep_dd_input()' makes a single input file for use in DeepDive in one line.
#' @param dat Data frame of occurrences with taxon ID, Area, MinAge, MaxAge and
#' Locality columns.
#' @param r The number of age assignment replicates
#' @param age_m The age assignment method
#' @returns A .csv file containing: start age, midpoint and duration of time
#' bins, counts of the number of localities per region for each bin, counts of
#' occurrences of each taxon per region per time bin, the data type (bins, locs,
#' occs) and replicate number (where age replicates are used, column R).
#' @examples
#' prep_dd_input(dat=your_data, bins=bins, r=100, age_m = "random_by_loc")
#' @export
prep_dd_input <- function(dat=dat, bins, r=replicate, age_m=age_method,
                          output_file=NULL){

  deepdive_input <- data.frame()
  for(rep in 1:r){

    sampled_dat <- ages(dat, method=age_m) # Get ages and append
    area_tables <- split(sampled_dat, f = sampled_dat$Area)  # Split data by area

    # Get species or genera level data
    occs <- taxa_time_per_area(sampled_dat, area_tables, bins=bins)
    cnames <- c(colnames(occs))

    # Get locality data
    locs <- generate_locality_dataset(sampled_dat, bins=bins)

    # Get time bin data
    tbins <- data.frame(cbind(c("bin_start", "bin_mid", "bin_dur"), NA,
                              rbind(bins[-length(bins)],
                                    rowMeans(cbind(bins[-length(bins)], bins[-1])),
                                    bins[-length(bins)]-bins[-1])))
    names(tbins) <- cnames

    dd_input <- rbind(tbins, locs, occs)
    dd_input <- cbind(Replicate=rep, dd_input)
    deepdive_input <- rbind(deepdive_input, dd_input)
  }

  # Get DeepDive input file
  if(!is.null(output_file)){
    write.csv(deepdive_input, output_file,
              row.names=FALSE)
  }
  else{
    return(deepdive_input)
  }
}
