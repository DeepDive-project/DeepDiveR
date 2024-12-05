#' Carnivore data
#'
#' A subset of occurrence data for the order Carnivora from Faurby et al.
#' (2024).
#'
#' @format A data frame with 11,309 rows and 5 columns:
#' \describe{
#'   \item{Taxon}{The taxonomic name given to the occurrence.}
#'   \item{Region}{The discrete geographic region in which the taxon was found.}
#'   \item{MinAge}{The youngest age estimate of the fossil occurrence, in
#'      millions of years.}
#'   \item{MaxAge}{The oldest age estimate of the fossil occurrence, in millions
#'      of years.}
#'   \item{Locality}{A unique identifier for the locality at which the fossil
#'      was recorded.}
#' }
#' @source Faurby S., Silvestro D., Werdelin L., and Antonelli, A. 2024. Reliable
#' biogeography requires fossils: insights from a new species-level phylogeny of
#' extinct and living carnivores. Proceedings of the Royal Society B,
#'      291(2028), 20240473.
#'      <https://royalsocietypublishing.org/doi/full/10.1098/rspb.2024.0473t>
"carnivora"

#' Geological stage data
#'
#' A description of the ages given to all Phanerozoic geological stages.
#'
#' @format A data frame with 102 rows and 9 columns:
#' \describe{
#'   \item{sys}{The geological period containing the stage.}
#'   \item{series}{The geological series containing the stage.}
#'   \item{stage}{The name of the geological stage.}
#'   \item{start}{The age of the start of the geological stage, in millions of
#'      years.}
#'   \item{end}{The age of the end of the geological stage, in millions of
#'      years.}
#'   \item{midpoint}{The age of the midpoint of the geological stage, in
#'      millions of years.}
#'   \item{dur}{The duration of the geological stage, in millions of years.}
#'   \item{seriescol}{The hexadecimal colour usually given to the geological
#'      series.}
#'   \item{stagecol}{The hexadecimal colour usually given to the geological
#'      stage.}
#' }
#' @source #To be completed#
"geo_bins"

#' DeepDive configuration file parameters
#'
#' A table describing the parameters included in the configuration file.
#'
#' @format A data frame with 80 rows and 6 columns:
#' \describe{
#'   \item{module}{The module in the configuration file within which the
#'      parameter can be found.}
#'   \item{parameter}{The name of the parameter.}
#'   \item{description}{A brief description of the parameter.}
#'   \item{default}{The default value given to the parameter when the
#'      configuration file is created.}
#'   \item{autotune}{A logical value describing whether the parameter can be
#'      autotuned (TRUE) or not (FALSE).}
#'   \item{limits}{The type of value which can be attributed to the parameter.}
#' }
"parameters"
