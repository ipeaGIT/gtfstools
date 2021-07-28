#' Read GTFS files
#'
#' Reads GTFS text files from either a local `.zip` file or an URL.
#'
#' @param path The path to a GTFS `.zip` file.
#' @param files A character vector containing the text files to be read from the
#'   GTFS (without the `.txt` extension). If `NULL` (the default) all existing
#'   files are read.
#' @param fields A named list containing the fields to be read from each text
#'   file, in the format `list(file = c("field1", "field2"))`. If `NULL` (the
#'   default), all fields from the files specified in `files` are read. If a
#'   file is specified in `files` but not in `fields`, all fields from that file
#'   will be read (i.e. you may specify in `fields` only files whose fields you
#'   want to subset).
#' @param skip A character vector containing the text files that should not be
#'   read from the GTFS, without the `.txt` extension. If `NULL` (the default),
#'   no files are skipped. Cannot be used if `files` is already set.
#' @param quiet Whether to hide log messages and progress bars (defaults to
#'   `TRUE`).
#'
#' @return A `data.table`-based GTFS object: a `list` of `data.table`s in which
#' each table represents a GTFS text file.
#'
#' @section Details:
#' The column types of each `data.table` in the final GTFS object conform as
#' closely as possible to the [Google's Static GTFS Reference](
#' https://developers.google.com/transit/gtfs/reference). Exceptions are
#' date-related columns (such as `calendar.txt`'s `start_date` and `end_date`,
#' for example), which are converted to `Date` objects, instead of being kept as
#' `integer`s, allowing for easier data manipulation. These columns are
#' converted back to `integer`s when writing the GTFS object to a `.zip` file
#' using \code{\link{write_gtfs}}.
#'
#' @family io functions
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#'
#' gtfs <- read_gtfs(data_path)
#' names(gtfs)
#'
#' gtfs <- read_gtfs(data_path, files = c("trips", "stop_times"))
#' names(gtfs)
#'
#' gtfs <- read_gtfs(data_path, skip = "trips")
#' names(gtfs)
#'
#' gtfs <- read_gtfs(data_path, fields = list(agency = "agency_id"))
#' names(gtfs)
#' names(gtfs$agency)
#'
#' @export
read_gtfs <- function(path,
                      files = NULL,
                      fields = NULL,
                      skip = NULL,
                      quiet = TRUE) {

  # inputs are more thoroughly check in gtfsio::import_gtfs()

  checkmate::assert_string(path)
  checkmate::assert_character(files, null.ok = TRUE)
  checkmate::assert_list(fields, null.ok = TRUE)
  checkmate::assert_character(skip, null.ok = TRUE)
  checkmate::assert_logical(quiet)

  # read gtfs file using {gtfsio} and convert relevant fields from standard type

  gtfs <- gtfsio::import_gtfs(
    path,
    files = files,
    fields = fields,
    skip = skip,
    quiet = quiet
  )
  gtfs <- gtfsio::new_gtfs(gtfs, subclass = "dt_gtfs")
  gtfs <- convert_from_standard(gtfs)

  return(gtfs)

}
