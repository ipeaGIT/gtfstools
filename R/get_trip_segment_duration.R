#' Get trip segments' duration
#'
#' Returns the duration of segments between stops of each specified
#' \code{trip_id}.
#'
#' @param gtfs A GTFS object as created by \code{\link{read_gtfs}}.
#' @param trip_id A string vector including the \code{trip_id}s to have their
#'   segments' durations calculated. If \code{NULL} (the default) the function
#'   calculates the segments' duration of every \code{trip_id} in the GTFS.
#' @param unit A string representing the time unit in which the durations are
#'   desired. One of \code{"s"} (seconds), \code{"min"} (minutes, the default),
#'   \code{"h"} (hours) or \code{"d"} (days).
#'
#' @return A \code{data.table} containing the segments' duration of each
#'   specified trip.
#'
#' @section Details:
#' A trip segment is defined as the path between two subsequent stops in the
#' same trip. The  duration of a segment is defined as the time difference
#' between its arrival time and its departure time, as specified in the
#' \code{stop_times} file.
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#'
#' gtfs <- read_gtfs(data_path)
#'
#' trip_segment_dur <- get_trip_segment_duration(gtfs)
#' head(trip_segment_dur)
#'
#' trip_ids <- c("CPTM L07-0", "2002-10-0")
#' trip_segment_dur <- get_trip_segment_duration(gtfs, trip_id = trip_ids)
#' trip_segment_dur
#'
#' trip_segment_dur <- get_trip_segment_duration(gtfs, trip_ids, unit = "s")
#' trip_segment_dur
#'
#' @export
get_trip_segment_duration <- function(gtfs, trip_id = NULL, unit = "min") {

  checkmate::assert_class(gtfs, "gtfs")
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

  if (! is.null(trip_id)) {
    relevant_trips <- trip_id
  } else {
    relevant_trips <- unique(gtfs$stop_times$trip_id)
  }

  # create auxiliary columns if needed

  durations <- gtfs$stop_times[trip_id %chin% relevant_trips]

  durations[
    ,
    `:=`(
      arrival_time_secs = string_to_seconds(arrival_time),
      departure_time_secs = string_to_seconds(departure_time)
    )
  ]

  # calculate durations

  durations <- durations[order(trip_id, stop_sequence)]
  durations[, last_stop_departure := data.table::shift(departure_time_secs, 1L, type = "lag")]
  durations <- durations[stop_sequence != 1]
  durations[
    ,
    `:=`(
      segment = stop_sequence - 1,
      duration = arrival_time_secs - last_stop_departure
    )
  ]

  # select desired columns and convert duration to desired unit

  durations <- durations[, .(trip_id, segment, duration)]

  if (unit != "s") {
    durations[, duration := as.numeric(units::set_units(units::as_units(duration, "s"), unit, mode = "standard"))]
  }

  return(durations[])

}
