% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Print a map showing where accidents occured}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{An integer representing the state.}

\item{year}{An integer representing the year.}
}
\value{
This function returns a map of locations of accidents.
}
\description{
This is a function that prints a map of the locations of accidents in a
   particular state and year.
}
\examples{
\dontrun{
fars_map_state(1, 2013)
}

}
