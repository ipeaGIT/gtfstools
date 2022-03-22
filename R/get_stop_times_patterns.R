get_stop_times_patterns <- function(gtfs, trip_id = NULL, type = "spatial") {
  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_character(trip_id, null.ok = TRUE)
  checkmate::assert(
    checkmate::check_string(type),
    checkmate::check_names(type, subset.of = c("spatial", "spatiotemporal")),
    combine = "and"
  )

  must_exist <- c("trip_id", "stop_id", "stop_sequence")
  if (type == "spatiotemporal") {
    must_exist <- c("departure_time", "arrival_time")
  }
  gtfsio::assert_field_exists(gtfs, "stop_times", fields = must_exist)

  # select trips to check patterns of and raise warning if any of them doesn't
  # exist in stop_times

  if (!is.null(trip_id)) {
    relevant_trips <- trip_id

    invalid_trips <- trip_id[! trip_id %chin% gtfs$stop_times$trip_id]

    if (!identical(invalid_trips, character(0))) {
      warning(
        paste0(
          "'stop_times' doesn't contain the following trip_id(s): "),
        paste0("'", invalid_trips, "'", collapse = ", ")
      )
    }

    patterns <- gtfs$stop_times[trip_id %chin% relevant_trips]
  } else {
    patterns <- gtfs$stop_times
  }

  if (type == "spatial") {
    patterns <- patterns[
      ,
      .(data = list(.SD)),
      keyby = trip_id,
      .SDcols = setdiff(must_exist, "trip_id")
    ]
  } else {
    if (
      !gtfsio::check_field_exists(gtfs, "stop_times", "departure_time_secs")
    ) {
      patterns[, departure_time_secs := string_to_seconds(departure_time)]
      created_departure_secs <- TRUE
    }
    if (
      !gtfsio::check_field_exists(gtfs, "stop_times", "arrival_time_secs")
    ) {
      patterns[, arrival_time_secs := string_to_seconds(arrival_time)]
      created_arrival_secs <- TRUE
    }

    patterns[
      ,
      `:=`(
        template_departure = departure_time_secs - min(departure_time_secs),
        template_arrival = arrival_time_secs - min(departure_time_secs)
      ),
      by = trip_id
    ]

    patterns <- patterns[
      ,
      .(data = list(.SD)),
      keyby = trip_id,
      .SDcols = c(
        "stop_id",
        "stop_sequence",
        "template_departure",
        "template_arrival"
      )
    ]

    if (gtfsio::check_field_exists(gtfs, "stop_times", "template_arrival")) {
      gtfs$stop_times[, c("template_departure", "template_arrival") := NULL]
    }

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
  }

  unique_groups <- unique(patterns$data)
  patterns[, pattern_id := match(data, unique_groups)]
  patterns[, data := NULL]

  return(patterns[])
}
