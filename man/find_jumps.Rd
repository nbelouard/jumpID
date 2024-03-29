% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_jumps.R
\name{find_jumps}
\alias{find_jumps}
\title{Find jump dispersal events}
\usage{
find_jumps(dataset, bio_year = c(2014:2021), gap_size = 15)
}
\arguments{
\item{dataset}{A dataset to be processed}

\item{bio_year}{Years to be considered (default: 2014 to 2021)}

\item{gap_size}{Distance between the invasion front and the positive point
necessary for it to be considered a jump, in kilometers (default: 15)}
}
\value{
Two tables: one table \code{Thresholds} containing the threshold per year and sector,
one table \code{Jumps} containing the list of jumps
}
\description{
Find jump dispersal events after having applied attribute_sectors
}
\examples{
jumps <- find_jumps(dataset)
}
