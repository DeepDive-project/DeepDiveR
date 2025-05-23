% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxa_region_time.R
\name{taxa_region_time}
\alias{taxa_region_time}
\title{Summarise taxa per region per time bin}
\usage{
taxa_region_time(dat = NULL, bins = NULL)
}
\arguments{
\item{dat}{\code{dataframe}. The \code{dataframe} describing the occurrence data,
including \code{Taxon}, \code{Region}, \code{Locality} and \code{SampledAge} columns. The
supplied \code{dataframe} should not contain any \code{NA} values.}

\item{bins}{\code{numeric}. A numerical \code{vector} designating the boundaries
of the time bins used in the analysis.}
}
\value{
A \code{dataframe} of shape 'taxon x region x time bin' describing where
occurrences are recorded.
}
\description{
A function to produce a \code{dataframe} of shape 'taxon x region x time bin' from
occurrence data.
}
\examples{
# Import internal dataset
data(carnivora)
# Generate vector describing time bin boundaries
bins <- c(66, 23, 2.6, 0)
# Generate sampled ages using the "median" method
carnivora <- ages(dat = carnivora)
# Compute taxon per region and time frequencies
taxa_region_time(dat = carnivora, bins = bins)
}
