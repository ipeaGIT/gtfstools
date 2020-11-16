#' Get trip duration
#'
#' Returns the duration of each specified \code{trip_id}.
#'
#' @param gtfs A GTFS object as created by \code{\link{read_gtfs}}.
#' @param trip_id A string vector including the \code{trip_id}s to have their
#'   durations calculated. If \code{NULL} (the default) the function calculates
#'   the duration of every \code{trip_id} in the GTFS.
#' @param unit A string representing the time unit in which the durations are
#'   desired. One of \code{"secs"}, \code{"mins"} (the default), \code{"hours"}
#'   or \code{"days"}.
#'
#' @return A \code{data.table} containing the duration of each specified trip.
#'
#' @section Details:
#' The duration of a trip is defined as the time difference between its last
#' arrival time and its first departure time, as specified in the
#' \code{stop_times} file.
#'
#' @export
get_trip_duration <- function(gtfs, trip_id = NULL, unit = "mins") {

  checkmate::assert_class(gtfs, "gtfs")
  checkmate::assert_character(trip_id, null.ok = TRUE)
  checkmate::assert(
    checkmate::check_string(unit),
    checkmate::check_names(unit, subset.of = c("secs", "mins", "hours", "days")),
    combine = "and"
  )

  # check if required fields and files exist

  checkmate::assert(
    check_gtfs_field_exists(
      gtfs,
      "stop_times",
      c("trip_id", "arrival_time", "departure_time")
    )
  )

  if (!is.null(trip_id)) {
    relevant_trips <- trip_id
  } else {
    relevant_trips <- unique(gtfs$stop_times$trip_id)
  }

  # calculate durations

  durations <- gtfs$stop_times[
    trip_id %chin% relevant_trips,
    .(duration = as.numeric(max(arrival_time), units = unit) - as.numeric(min(departure_time), units = unit)),
    keyby = trip_id
  ]

  return(durations)

}
