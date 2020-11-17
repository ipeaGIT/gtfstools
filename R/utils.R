#' Check for field existence in a GTFS text file
#'
#' Checks for the existence of a given field in a GTFS text file (represented by
#' a column in a \code{data.table}). Must be used in conjunction with
#' \code{\link[checkmate]{assert}}.
#'
#' @param gtfs A GTFS object as created by \code{\link{read_gtfs}}.
#' @param file File in which the field should exist.
#' @param field Field to check existence of.
#'
#' @return If the check is successful, returns \code{TRUE} invisibly. Else,
#'   returns a string with error message.
check_gtfs_field_exists <- function(gtfs, file, field) {

  checkmate::assert_class(gtfs, "gtfs")
  checkmate::assert_string(file)
  checkmate::assert_character(field)

  # check if file is an index of gtfs

  checkmate::assert(check_gtfs_file_exists(gtfs, file))

  # check if "field" is a column in "file"

  non_existent_field <- field[! field %chin% names(gtfs[[file]])]

  if (identical(non_existent_field, character(0))) {

    return(invisible(TRUE))

  } else {

    error_message <- ifelse(
      length(non_existent_field) == 1,
      paste0("File '", file, "' must contain '", non_existent_field, "' field"),
      paste0(
        "File '", file, "' must contain the following fields: ",
        paste0("'", non_existent_field, "'", collapse = ", ")
      )
    )

    return(error_message)

  }

}



#' Check for GTFS text file existence
#'
#' Checks for the existence of a given file (as represented by a
#' \code{data.table}) in a GTFS object. Must be used in conjunction with
#' \code{\link[checkmate]{assert}}.
#'
#' @param gtfs A GTFS object as created by \code{\link{read_gtfs}}.
#' @param file File to check existence of.
#'
#' @return If the check is successful, returns \code{TRUE} invisibly. Else,
#'   returns a string with error message.
check_gtfs_file_exists <- function(gtfs, file) {

  checkmate::assert_class(gtfs, "gtfs")
  checkmate::assert_character(file)

  non_existent_file <- file[! file %chin% names(gtfs)]

  if (identical(non_existent_file, character(0))) {

    return(invisible(TRUE))

  } else {

    error_message <- ifelse(
      length(non_existent_file) == 1,
      paste0("Must contain '", non_existent_file, "' file"),
      paste0(
        "Must contain the following files: ",
        paste0("'", non_existent_file, "'", collapse = ", ")
      )
    )

    return(error_message)

  }

}



#' Convert string to time
#'
#' Converts strings in the "HH:MM:SS" format to \code{hms}.
#'
#' @param string A string in "HH:MM:SS" format.
#'
#' @return An \code{hms} object.
string_to_hms <- function(string) {

  checkmate::assert_character(string)

  split_string <- strsplit(string, ":")

  seconds_from_midgnight <- vapply(
    split_string,
    function(i) sum(as.integer(i) * c(3600L, 60L, 1L)),
    integer(1)
  )

  # "" strings result in seconds_to_midnight = 0. find those and replace to NA

  index_na <- which(lengths(split_string) == 0)
  seconds_from_midgnight[index_na] <- NA_integer_

  return(hms::hms(seconds = seconds_from_midgnight))

}
