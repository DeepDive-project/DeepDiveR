% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepdive_input_files.R
\name{write_dd_files2}
\alias{write_dd_files2}
\title{Generate DeepDive input in one file}
\usage{
write_dd_files2(dat, r = replicate, age_m = "median", bins)
}
\arguments{
\item{dat}{Data frame of occurrences with taxon ID, Area, MinAge, MaxAge and
Locality columns.}

\item{r}{The number of age assignment replicates}

\item{age_m}{The age assignment method}

\item{bins}{Time bins used in the analysis (should reflect the empirical data
and be identical to the bins used in any corresponding DeepDive simulations
and training)}
}
\value{
Here each age replicate is indicated in the 'R' column and can be
set using the r setting. Locality and occurrence data by time bin are
generated alongside the replicate number, the data type (either locs or occs)
and the region ID.
}
\description{
'write_dd_files2()' makes a single input .csv file for use in DeepDive.
}
\examples{
write_dd_files2(dat, r=replicate, bins=bins, age_m="median")
}
