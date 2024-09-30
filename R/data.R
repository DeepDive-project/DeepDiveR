#' Carnivore data
#'
#' A subset of occurrence data for the order Carnivora from Faurby et al.
#' (2019).
#'
#' @format A data frame with 11,309 rows and 5 columns:
#' \describe{
#'   \item{Taxon}{The taxonomic name given to the occurrence.}
#'   \item{Area}{The discrete geographic region in which the taxon was found.}
#'   \item{MinAge}{The youngest age estimate of the fossil occurrence, in
#'      millions of years.}
#'   \item{MaxAge}{The oldest age estimate of the fossil occurrence, in millions
#'      of years.}
#'   \item{Locality}{A unique identifier for the locality at which the fossil
#'      was recorded.}
#' }
#' @source Faurby S., Werdelin L., and Antonelli, A. 2019. Dispersal ability
#'      predicts evolutionary success among mammalian carnivores. bioRxiv,
#'      10.1101, 755207.
#'      <https://www.biorxiv.org/content/10.1101/755207v1.abstract>
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
