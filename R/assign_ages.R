#' Assign point ages for each occurrence
#'
#' A function to assign a single point age for each fossil occurrence from
#' within its age range (between `MinAge` and `MaxAge`).
#'
#' @param dat \code{dataframe}. The `dataframe` containing the fossil
#'    occurrences.
#' @param method \code{character}. The choice of age assignment method, either
#'    `median`, `random` or `random_by_loc`.
#' @returns The input `dataframe` with an additional column providing a
#'    `SampledAge`.
#' @details The assigned age can be chosen using either:
#' - `median` (default), the median age
#' - `random`, a random age drawn from a uniform distribution for each
#'    occurrence
#' - `random_by_loc`, a random age drawn from a uniform distribution that is
#'    applied to all occurrences sampled from the same locality which also have
#'    the same `MinAge` and `MaxAge`. In this case, a column named `Locality`
#'    must be provided which gives the locality identifier for each occurrence.
#' The supplied `dataframe` of occurrences should not contain any `NA` values.
#'
#' @importFrom dplyr distinct left_join mutate rowwise
#' @importFrom stats median runif
#' @examples
#' # Import internal dataset
#' data(carnivora)
#' # Add column of median ages
#' example1 <- ages(dat = carnivora)
#' # Add column of sampled ages
#' example2 <- ages(dat = carnivora, method = "random_by_loc")
#' @export
ages <- function(dat = NULL, method = "median"){

  # Handling errors
  if (is.data.frame(dat) == FALSE) {
    stop("`dat` should be a dataframe.")
  }

  if ("MinAge" %in% colnames(dat) == FALSE ||
      "MaxAge" %in% colnames(dat) == FALSE) {
    stop("`MinAge` and/or `MaxAge` columns do not exist in `dat`")
  }

  if (!is.numeric(dat$MinAge) || !is.numeric(dat$MaxAge)) {
    stop("`MinAge` and/or `MaxAge` columns are not of numeric class")
  }

  if (any(is.na(dat$MinAge)) || any(is.na(dat$MaxAge))) {
    stop(paste("NA values detected in `MinAge` and/or `MaxAge`"))
  }

  order_check <- dat$MaxAge - dat$MinAge
  if (any(order_check < 0)) {
    stop(paste("All `MinAge` values must be smaller than `MaxAge` values"))
  }

  if (method != "median" && method != "random" && method != "random_by_loc") {
    stop(paste("`method` must be 'median', 'random' or 'random_by_loc'"))
  }

  if (method == "random_by_loc" && "Locality" %in% colnames(dat) == FALSE) {
    stop(paste("`Locality` must be provided to use 'random_by_loc' method"))
  }

  # Compute median ages
  if (method == "median") {
    dat <- mutate(rowwise(dat), SampledAge = median(c(MinAge, MaxAge)))
  }

  # Sample random ages
  if (method == "random") {
    SampledAge <- runif(length(dat$MinAge),
                        min = dat$MinAge, max = dat$MaxAge)
    dat <- cbind(dat, SampledAge)
  }

  # Sample ages for each location
  if (method == "random_by_loc") {
    locate_and_assign <- c()
    for (i in unique(dat$Locality)) {
      locate <- which(dat$Locality == i)
      loc <- dat[locate, ]
      loc_distinct_ages <- loc %>% distinct(MinAge, MaxAge, Locality)
      SampledAge <- c()
      for(j in 1:nrow(loc_distinct_ages)){
        Age <- runif(n = 1,
                     min = loc_distinct_ages$MinAge[j],
                     max = loc_distinct_ages$MaxAge[j])
        SampledAge <- append(SampledAge, Age)
      }
      loc_distinct_ages <- cbind(loc_distinct_ages, SampledAge)
      locate_and_assign <- rbind(locate_and_assign, loc_distinct_ages)
    }
    dat <- left_join(dat, locate_and_assign,
                            by = c("MinAge" = "MinAge", "MaxAge" = "MaxAge",
                                   "Locality" = "Locality"))
  }
  return(data.frame(dat))
}
