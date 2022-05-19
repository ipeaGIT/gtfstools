#' Convert time string to seconds after midnight
#'
#' Converts strings in the "HH:MM:SS" format to seconds after midnight.
#'
#' @param string A string in "HH:MM:SS" format.
#'
#' @return The seconds after midnight of a given time string as an integer.
#'
#' @keywords internal
string_to_seconds <- function(string) {
  checkmate::assert_character(string)

  seconds_from_midgnight <- cpp_time_to_seconds(string)

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
#'
#' @keywords internal
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
#' @template gtfs
#' @param file File to be removed.
#'
#' @return A GTFS object without the given file.
#'
#' @keywords internal
copy_gtfs_without_file <- function(gtfs, file) {

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_string(file)

  # check if file exists

  gtfsio::assert_file_exists(gtfs, file)

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
#' @template gtfs
#' @param file File from which the field must be removed.
#' @param field Field to be removed.
#'
#' @return A GTFS object without the given field.
#'
#' @keywords internal
copy_gtfs_without_field <- function(gtfs, file, field) {

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_string(file)
  checkmate::assert_string(field)

  # check if field exists

  gtfsio::assert_field_exists(gtfs, file, field)

  # remove field

  gtfs_copy <- gtfs
  gtfs_copy[[file]] <- data.table::copy(gtfs[[file]])
  gtfs_copy[[file]][, (field) := NULL]

  return(gtfs_copy)

}



#' Make a GTFS copy with a field of a different class
#'
#' Creates a copy of a GTFS object while changing the class of a given field.
#' Used for testing.
#'
#' @template gtfs
#' @param file File whose field must have the class changed.
#' @param field Field to have the class changed.
#' @param class The desired class.
#'
#' @return A GTFS object with the field of desired class.
#'
#' @keywords internal
copy_gtfs_diff_field_class <- function(gtfs, file, field, class) {

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_string(file)
  checkmate::assert_string(field)
  checkmate::assert_string(class)

  # check if field exists

  gtfsio::assert_field_exists(gtfs, file, field)

  # select the coercion function to use

  if (class == "factor")
    fn <- as.factor
  if (class == "character")
    fn <- as.character

  # change field class

  gtfs_copy <- gtfs
  gtfs_copy[[file]] <- data.table::copy(gtfs[[file]])
  gtfs_copy[[file]][, get("field") := fn(eval(parse(text = field)))]

  return(gtfs_copy)

}
