#' Get trip duration
#'
#' Returns the duration of each specified \code{trip_id}.
#'
#' @param gtfs A GTFS object as created by \code{\link{read_gtfs}}.
#' @param trip_id A string vector including the \code{trip_id}s to have their
#'   durations calculated. If \code{NULL} (the default) the function calculates
#'   the duration of every \code{trip_id} in the GTFS.
#' @param unit A string representing the time unit in which the durations are
#'   desired. One of \code{"s"} (seconds), \code{"min"} (minutes, the default),
#'   \code{"h"} (hours) or \code{"d"} (days).
#'
#' @return A \code{data.table} containing the duration of each specified trip.
#'
#' @section Details:
#' The duration of a trip is defined as the time difference between its last
#' arrival time and its first departure time, as specified in the
#' \code{stop_times} file.
#'
#' @examples
#' data_path <- system.file("extdata/poa_gtfs.zip", package = "gtfstools")
#'
#' gtfs <- read_gtfs(data_path)
#'
#' trip_duration <- get_trip_duration(gtfs)
#' head(trip_duration)
#'
#' trip_ids <- c("274-2@1#640", "262-2@1#1427")
#' trip_duration <- get_trip_duration(gtfs, trip_id = trip_ids)
#' trip_duration
#'
#' trip_duration <- get_trip_duration(gtfs, trip_id = trip_ids, unit = "h")
#' trip_duration
#'
#' @export
get_trip_duration <- function(gtfs, trip_id = NULL, unit = "min") {

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_character(trip_id, null.ok = TRUE)
  checkmate::assert(
    checkmate::check_string(unit),
    checkmate::check_names(unit, subset.of = c("s", "min", "h", "d")),
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

  # create auxiliary columns if needed

  durations <- gtfs$stop_times[trip_id %chin% relevant_trips]

  durations[
    trip_id %chin% relevant_trips,
    `:=`(
      arrival_time_secs = string_to_seconds(arrival_time),
      departure_time_secs = string_to_seconds(departure_time)
    )
  ]

  # calculate durations

  durations <- durations[
    trip_id %chin% relevant_trips,
    .(duration = max(arrival_time_secs, na.rm = TRUE) - min(departure_time_secs, na.rm = TRUE)),
    keyby = trip_id
  ]

  # convert duration to desired unit

  if (unit != "s") {
    durations[, duration := as.numeric(units::set_units(units::as_units(duration, "s"), unit, mode = "standard"))]
  }

  return(durations[])

}
