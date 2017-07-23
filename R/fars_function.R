#' A file read function
#'
#' \code{fars_read} function reads your data file downloaded from the
#' \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{Fatality Analysis Reporting System}.
#' If file you are trying to read doesn't exist on the fars database then you will get a message \code{filename does not exist}.
#' Any messages associated with downloading the file will be suppressed. Data will be read using the \code{read_csv} function from the \code{readr} package.
#' Dataframe will be stored in \code{tbl_df} format using the \code{dplyr} package
#'
#' @param filename Path to the file containing data.
#' @return A tibble with data.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @examples
#' \dontrun{
#' fars_complete_2013 <- fars_read("accident_2013-csv.bz2")
#' }
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create file name using year
#'
#' \code{make_filename} function uses year as an argument and create file name by embedding \code{year} given as an argument to create a file.
#' File name is created by pasting \code{accident} and the input year and saving it in \code{csv.bz2} format.
#'
#' @param argument \code{year} takes the year for which the data file is to be made.
#' @return A character vector containing the complete file name of a compressed
#'   FARS report for a given year provided to the function as an argument.
#' @examples
#' make_filename(2013)
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read accident information by Month & Year for multiple FARS report files into a list of tibbles
#'
#' \code{fars_read_years} function takes list of years as an argument, create file names, load data files,
#' mutate year and then select month and year.  If data for any of the years has been already downloaded then that data
#' is not imported.  Instead data from \code{tryCatch} is used
#'
#' @param argument \code{years} takes list of years as an argument.
#' @return A list of tibbles. Each tibble contains just two columns of the original
#' FARS data set (\code{MONTH} and \code{year}). Each row corresponds to a single accident.
#' @importFrom dplyr mutate select
#' @details
#' This function is not designed for direct user interaction.
#' It is a helper function for \code{fars_summarize_years}.
#' If any of the objects requested via input is not available as a year file
#'  or is not coercible to integer an "invalid year" error message returns.
#' @examples
#' \dontrun{
#' fars_read_years(years = c(2013, 2014))
#' }
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize FARS report accidents by month and year in a tibble
#'
#' \code{fars_summarize_years} function takes list of years as an argument and
#'   provide the total number of accidents by month and year.
#' @param \code{years} takes list of years as an argument.
#' @return a tibble showing summary of number of accidents by month
#'   for each input year.
#' @inheritParams fars_read_years
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @examples
#' \dontrun{
#' fars_summarize_years(years = c(2013, 2014))
#' }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Maps points on the state map where accidents occured
#'
#' \code{fars_map_state} function takes state number and year to map points on the state map where accidents occured.
#' @details
#' \code{fars_map_state} function assumes that there will be a single integer
#'  value for state.num and single integer value for year. The implementation
#'  do not specifically check for these constraints.
#'
#'  If state does not exist in queried year: a error message of "nothing to draw:
#'   all regions out of bounds" apprears.
#'
#'  If multiple states will be entered as a vector: a warning message will appear
#'   stating "only the first element will be used", on the other hand, the graph
#'   will be created properly including all states inputted.
#'
#'  If an empty vector of states is inputted: an error message of "argument is of
#'   length zero" wil be displayed
#'
#'  If a non-existing year is entered: an error message of "Error in
#'   fars_read(filename) :" will be displayed.
#'
#'  If multiple years will be entered as a vector: an error message of
#'   "EXPR must be a length 1 vector" will be displayed.
#'
#'  If an empty vector of years is inputted: an error message of "argument is
#'   of length zero" wil be displayed
#'
#' @param state.num An integer representing a state as the index (between
#'  1 and 56).
#' @param An integer representing the year.
#' @return This function returns a NULL object. But as a side effect a map
#'  where dots represent accidents also appears in a seperate window.
#' @importFrom graphics points
#' @importFrom maps map
#' @importFrom dplyr filter
#' @examples
#' \dontrun{
#' fars_map_state(state.num = 1, year = 2013)
#'}
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
