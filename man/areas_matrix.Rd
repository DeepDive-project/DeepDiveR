% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_config.R
\name{areas_matrix}
\alias{areas_matrix}
\title{Make discrete geographic regions appear or disappear}
\usage{
areas_matrix(area_ages = NULL, n_areas, config, bins, label = "start")
}
\arguments{
\item{area_ages}{A matrix of minimum and maximum ages for areas appearing, with older ages in column one. Must have a number of rows = number of discrete areas.}

\item{n_areas}{The number of unique discrete areas used in the analysis.}

\item{config}{A config which can be generated in create_config() that you will add attributes to here.}
}
\value{
Adds an attribute for each area (named "area1", "area2"... etc) with min and max age of migration becoming possible.
}
\description{
'areas_matrix()' adds attributes in the simulations module which provide a
maximum and minimum age range for regions becoming connected.
}
\examples{
areas_matrix(area_ages, n_areas = length(unique(your_data$Area)), config)
}
