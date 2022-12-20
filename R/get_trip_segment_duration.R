#' Get trip segments' duration
#'
#' Returns the duration of segments between stops of each specified `trip_id`.
#'
#' @template gtfs
#' @param trip_id A string vector including the `trip_id`s to have their
#'   segments' duration calculated. If `NULL` (the default) the function
#'   calculates the segments' duration of every `trip_id` in the GTFS.
#' @param unit A string representing the time unit in which the duration is
#'   desired. One of `"s"` (seconds), `"min"` (minutes, the default),
#'   `"h"` (hours) or `"d"` (days).
#'
#' @return A `data.table` containing the segments' duration of each specified
#'   trip.
#'
#' @section Details:
#' A trip segment is defined as the path between two subsequent stops in the
#' same trip. The  duration of a segment is defined as the time difference
#' between its arrival time and its departure time, as specified in the
#' `stop_times` file.
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#'
#' gtfs <- read_gtfs(data_path)
#'
#' trip_segment_dur <- get_trip_segment_duration(gtfs)
#' head(trip_segment_dur)
#'
#' # use the trip_id argument to control which trips are analyzed
#' trip_segment_dur <- get_trip_segment_duration(gtfs, trip_id = "CPTM L07-0")
#' trip_segment_dur
#'
#' # use the unit argument to control in which unit the durations are calculated
#' trip_segment_dur <- get_trip_segment_duration(gtfs, "CPTM L07-0", unit = "s")
#' trip_segment_dur
#'
#' @export
get_trip_segment_duration <- function(gtfs, trip_id = NULL, unit = "min") {
  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_character(trip_id, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert(
    checkmate::check_string(unit),
    checkmate::check_names(unit, subset.of = c("s", "min", "h", "d")),
    combine = "and"
  )

  gtfsio::assert_field_class(
    gtfs,
    "stop_times",
    c("trip_id", "arrival_time", "departure_time"),
    c("character", "character", "character")
  )

  # select trip_ids to get segment's duration of and raise warning if a given
  # trip_id doesn't exist in 'stop_times'

  if (!is.null(trip_id)) {
    relevant_trips <- trip_id

    invalid_trip_id <- trip_id[! trip_id %chin% unique(gtfs$stop_times$trip_id)]

    if (!identical(invalid_trip_id, character(0))) {
      warning(
        "'stop_times' doesn't contain the following trip_id(s): ",
        paste0("'", invalid_trip_id, "'", collapse = ", ")
      )
    }

    durations <- gtfs$stop_times[trip_id %chin% relevant_trips]
  } else {
    durations <- gtfs$stop_times
  }

  # create auxiliary columns if needed

  if (!gtfsio::check_field_exists(gtfs, "stop_times", "departure_time_secs")) {
    durations[, departure_time_secs := string_to_seconds(departure_time)]
    created_departure_secs <- TRUE
  }

  if (!gtfsio::check_field_exists(gtfs, "stop_times", "arrival_time_secs")) {
    durations[, arrival_time_secs := string_to_seconds(arrival_time)]
    created_arrival_secs <- TRUE
  }

  # calculate durations

  durations[
    ,
    last_stop_departure := data.table::shift(
      departure_time_secs,
      1L,
      type = "lag"
    )
  ]
  durations <- durations[!durations[, .I[1], by = trip_id]$V1]
  durations[, duration := arrival_time_secs - last_stop_departure]
  durations[, segment := seq.int(1, .N, length.out = .N), by = trip_id]

  # select desired columns and convert duration to desired unit

  desired_cols <- c("trip_id", "segment", "duration")
  durations <- durations[, setdiff(names(durations), desired_cols) := NULL]
  data.table::setcolorder(durations, desired_cols)

  if (unit != "s") {
    durations[
      ,
      duration := as.numeric(
        units::set_units(
          units::as_units(duration, "s"),
          unit,
          mode = "standard"
        )
      )
    ]
  }

  # the function may have created some colums to gtfs$stop_times if a copy
  # wasn't made when creating the durations table, so we need to clean them up

  if (
    gtfsio::check_field_exists(gtfs, "stop_times", "departure_time_secs") &
    exists("created_departure_secs")
  ) {
    gtfs$stop_times[, departure_time_secs := NULL]
  }

  if (
    gtfsio::check_field_exists(gtfs, "stop_times", "arrival_time_secs") &
    exists("created_arrival_secs")
  ) {
    gtfs$stop_times[, arrival_time_secs := NULL]
  }

  if (gtfsio::check_field_exists(gtfs, "stop_times", "last_stop_departure")) {
    gtfs$stop_times[, last_stop_departure := NULL]
  }

  return(durations[])
}
