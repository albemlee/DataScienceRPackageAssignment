#' Read fars data file
#'
#' This is a function that returns a tibble based on a csv.bz2 file.
#' 
#' @import dplyr
#' @import readr
#'
#' @param filename A character string or integer representing the location of
#'    the file to be read.
#' 
#' @return This function returns a tibble that includes the data in filename.
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename)){
                stop("file '", filename, "' does not exist")
        }
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create file
#'
#' This is a function that creates a filename based on the year.
#'
#' @import dplyr
#'
#' @param year A character string representing the year.
#' 
#' @return This function returns the filename that corresponds to year.
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        file <- sprintf("accident_%d.csv.bz2", year)
        system.file("extdata", file, package="DataScienceRPackageAssignment")
}

#' Read fars data files based on a list of years.
#'
#' This is a function that reads multiple fars data files based on a list of 
#' years.
#'
#' @import dplyr
#'
#' @param years A list of integers representing the years to include.
#' 
#' @return This function returns a list with tibbles for every year included.
#'
#' @examples
#' fars_read_years(2013)
#' fars_read_years(2013:2015)
#' fars_read_years(c(2013, 2015))
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dat <- dat %>%
                          dplyr::mutate(year = year) %>%
                          dplyr::select(MONTH, year)
                        return(dat)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarize the data based on years
#'
#' This is a function that reads multiple fars data files based on a list of 
#' years and provides the sum of data points for each year and month
#'
#' @import dplyr
#' @import tidyr
#'
#' @param years A list of integers representing the years to include.
#' 
#' @return This function returns a tibble with the number of accidents grouped
#'    by year and month.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013)
#' fars_summarize_years(2013:2015)
#' fars_summarize_years(c(2013, 2015))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dat_list <- dat_list %>%
          dplyr::bind_rows() %>%
          dplyr::group_by(year, MONTH) %>%
          dplyr::summarize(n = n()) %>%
          tidyr::spread(year, n)
        return(dat_list)
}

#' Print a map showing where accidents occured
#'
#' This is a function that prints a map of the locations of accidents in a
#'    particular state and year.
#'
#' @import dplyr
#' @import maps
#'
#' @param state.num An integer representing the state.
#'
#' @param year An integer representing the year.
#' 
#' @return This function returns a map of locations of accidents.
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
