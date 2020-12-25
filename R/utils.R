# set classes and methods to read dates as formatted in GTFS
methods::setClass("gtfs_date")
methods::setAs("character", "gtfs_date", function(from) as.Date(from, format = "%Y%m%d"))

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

  checkmate::assert_class(gtfs, "dt_gtfs")
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

  checkmate::assert_class(gtfs, "dt_gtfs")
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



#' Convert time string to seconds after midnight
#'
#' Converts strings in the "HH:MM:SS" format to seconds after midnight.
#'
#' @param string A string in "HH:MM:SS" format.
#'
#' @return The seconds after midnight of a given time string as an integer.
string_to_seconds <- function(string) {

  checkmate::assert_character(string)

  split_string <- strsplit(string, ":", fixed = TRUE)

  seconds_from_midgnight <- vapply(
    split_string,
    function(i) sum(as.integer(i) * c(3600L, 60L, 1L)),
    integer(1)
  )

  # "" strings result in seconds_to_midnight = 0. find those and replace to NA

  index_na <- which(lengths(split_string) == 0)
  seconds_from_midgnight[index_na] <- NA_integer_

  return(seconds_from_midgnight)

}



#' Convert seconds after midnight to time string
#'
#' Converts seconds after midnight as integers to strings in the "HH:MM:SS"
#' format.
#'
#' @param seconds An integer.
#'
#' @return A time-representing string.
seconds_to_string <- function(seconds) {

  checkmate::assert_integer(seconds)

  time_string <- data.table::fifelse(
    is.na(seconds),
    "",
    paste(
      formatC(seconds %/% 3600, width = 2, format = "d", flag = 0),
      formatC((seconds %% 3600) %/% 60, width = 2, format = "d", flag = 0),
      formatC((seconds %% 3600) %% 60, width = 2, format = "d", flag = 0),
      sep = ":"
    )
  )

  return(time_string)

}



#' Make a GTFS copy without a given file
#'
#' Creates a copy of a GTFS object without a given file. Used for testing.
#'
#' @param gtfs The GTFS to be copied, as created by \code{\link{read_gtfs}}.
#' @param file File to be removed.
#'
#' @return A GTFS object without the given file.
copy_gtfs_without_file <- function(gtfs, file) {

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_string(file)

  # check if file exists

  checkmate::assert(check_gtfs_file_exists(gtfs, file))

  # remove file

  gtfs_copy <- gtfs
  gtfs_copy[[file]] <- NULL

  return(gtfs_copy)

}



#' Make a GTFS copy without a given field from a given file
#'
#' Creates a copy of a GTFS object without a given field from a given file. Used
#' for testing.
#'
#' @param gtfs The GTFS to be copied, as created by \code{\link{read_gtfs}}.
#' @param file File from which the field must be removed.
#' @param field Field to be removed.
#'
#' @return A GTFS object without the given field.
copy_gtfs_without_field <- function(gtfs, file, field) {

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_string(file)
  checkmate::assert_string(field)

  # check if field exists

  checkmate::assert(check_gtfs_field_exists(gtfs, file, field))

  # remove field

  gtfs_copy <- gtfs
  gtfs_copy[[file]] <- data.table::copy(gtfs[[file]])
  gtfs_copy[[file]][, (field) := NULL]

  return(gtfs_copy)

}
