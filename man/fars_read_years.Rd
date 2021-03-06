% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Read fars data files based on a list of years.}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{A list of integers representing the years to include.}
}
\value{
This function returns a list with tibbles for every year included.
}
\description{
This is a function that reads multiple fars data files based on a list of 
years.
}
\examples{
\dontrun{
fars_read_years(2013)
fars_read_years(2013:2015)
fars_read_years(c(2013, 2015))
}

}
