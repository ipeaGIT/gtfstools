#' Convert frequencies to stop times
#'
#' Creates `stop_times` entries based on the frequencies specified in the
#' `frequencies` table.
#'
#' @param gtfs A GTFS object.
#' @param trip_id A character vector including the `trip_id`s to have their
#' frequencies converted to `stop_times` entries. If `NULL` (the default), the
#' function converts all trips listed in the `frequencies` table.
#'
#' @return A GTFS object with updated `frequencies`, `stop_times` and `trips`
#' tables.
#'
#' @section Details:
#' A single trip described in a `frequencies` table may yield multiple trips
#' after converting the GTFS. Let's say, for example, that the `frequencies`
#' table describes a trip called `"example_trip"`, that starts at 08:00 and
#' stops at 09:00, with a 30 minutes headway.
#'
#' In practice, that means that one trip will depart at 08:00, another at 08:30
#' and yet another at 09:00. `frequencies_to_stop_times()` appends a `"_<n>"`
#' suffix to the newly created trips to differentiate each one of them (e.g. in
#' this case, the new trips, described in the `trips` and `stop_times` tables,
#' would be called `"example_trip_1"`, `"example_trip_2"` and
#' `"example_trip_3"`).
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#' trip <- "CPTM L07-0"
#'
#' # converts all trips listed in the frequencies table
#' converted_gtfs <- frequencies_to_stop_times(gtfs)
#'
#' # converts only the specified trip_id
#' converted_gtfs <- frequencies_to_stop_times(gtfs, trip)
#'
#' # how the specified trip_id was described in the frequencies table
#' head(gtfs$frequencies[trip_id == trip])
#'
#' # the first row of each equivalent stop_times entry in the converted gtfs
#' equivalent_stop_times <- converted_gtfs$stop_times[grepl(trip, trip_id)]
#' equivalent_stop_times[equivalent_stop_times[, .I[1], by = trip_id]$V1]
#'
#' @export
frequencies_to_stop_times <- function(gtfs, trip_id = NULL) {
  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_character(trip_id, null.ok = TRUE)
  gtfsio::assert_field_class(
    gtfs,
    "frequencies",
    c("trip_id", "start_time", "end_time", "headway_secs"),
    c("character", "character", "character", "integer")
  )
  gtfsio::assert_field_class(
    gtfs,
    "stop_times",
    c("trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence"),
    c("character", "character", "character", "character", "integer")
  )

  if (!is.null(trip_id)) {
    relevant_trips <- trip_id
  } else {
    relevant_trips <- unique(gtfs$frequencies$trip_id)
  }

  # raise warning if a given trip_id doesn't exist in 'frequencies'

  if (!is.null(trip_id)) {
    invalid_trip_id <- trip_id[
      ! trip_id %chin% unique(gtfs$frequencies$trip_id)
    ]

    if (!identical(invalid_trip_id, character(0))) {
      warning(
        "'frequencies' doesn't contain the following trip_id(s): ",
        paste0("'", invalid_trip_id, "'", collapse = ", ")
      )
    }
  }

  # if they do not exist already, create auxiliary columns that hold the start
  # and end time (in the case of frequencies) and the arrival and departure time
  # (in the case of stop_times) of each trip in seconds

  if (!gtfsio::check_field_exists(gtfs, "frequencies", "start_time_secs")) {
    gtfs$frequencies[
      trip_id %chin% relevant_trips,
      start_time_secs := string_to_seconds(start_time)
    ]
    created_start_secs <- TRUE
  }

  if (!gtfsio::check_field_exists(gtfs, "frequencies", "end_time_secs")) {
    gtfs$frequencies[
      trip_id %chin% relevant_trips,
      end_time_secs := string_to_seconds(end_time)
    ]
    created_end_secs <- TRUE
  }

  if (!gtfsio::check_field_exists(gtfs, "stop_times", "departure_time_secs")) {
    gtfs$stop_times[
      trip_id %chin% relevant_trips,
      departure_time_secs := string_to_seconds(departure_time)
    ]
    created_departure_secs <- TRUE
  }

  if (!gtfsio::check_field_exists(gtfs, "stop_times", "arrival_time_secs")) {
    gtfs$stop_times[
      trip_id %chin% relevant_trips,
      arrival_time_secs := string_to_seconds(arrival_time)
    ]
    created_arrival_secs <- TRUE
  }

  # first step: figure out, based on the 'frequencies' table, what are the
  # departure times of each trip to be added to the 'stop_times' table

  departure_times <- lapply(
    relevant_trips,
    function(trip) {
      trip_frequencies <- gtfs$frequencies[trip_id == trip]
      trip_frequencies[
        ,
        departure_times := mapply(
          seq,
          from = start_time_secs,
          to = end_time_secs,
          by = headway_secs,
          SIMPLIFY = FALSE
        )
      ]

      departure_secs <- unlist(trip_frequencies$departure_times)

      # there may be some duplicated departure times if the same value is listed
      # in the upper and lower limit of two differente 'frequencies' entries
      # (e.g. if the table specifies one frequency from 5am to 6am and another
      # from 6am to 7am, the 6am departure may appear in the departures
      # generated by both entries).
      # so we take the unique 'departure_secs' values.

      departure_secs <- unique(departure_secs)

      # each new trip added to the 'stop_times' will be name after the original
      # trip, with a _<n> suffix. so the trip "original_trip" will generate the
      # trips "original_trip_1", "original_trip_2", ..., "original_trip_<n>"

      new_trips_names <- paste0(trip, "_", seq.int(1, length(departure_secs)))
      departure_secs <- structure(departure_secs, names = new_trips_names)
    }
  )

  # second step: identify the stop_times template of each relevant trip.
  # since we have the departure time of each one of the trips to be added, we
  # subtract the template's first departure time value from all template's
  # departure and arrival times.

  templates <- lapply(
    relevant_trips,
    function(trip) {
      template <- gtfs$stop_times[trip_id == trip]

      first_departure <- min(template$departure_time_secs, na.rm = TRUE)
      template[
        ,
        `:=`(
          departure_time_secs = departure_time_secs - first_departure,
          arrival_time_secs = arrival_time_secs - first_departure
        )
      ]
    }
  )

  # third step: build a "new" stop_times table by adding the departure time of
  # each new trip to the departure and arrival time of its correspondent
  # template

  stop_times_to_add <- mapply(
    departure_times,
    templates,
    SIMPLIFY = FALSE,
    FUN = function(departures, template) {
      n_stops <- nrow(template)
      n_departures <- length(departures)

      seconds_to_add <- rep(departures, each = n_stops)
      template_dep <- rep(template$departure_time_secs, times = n_departures)
      template_arr <- rep(template$arrival_time_secs, times = n_departures)

      new_departures <- template_dep + seconds_to_add
      new_arrivals <- template_arr + seconds_to_add

      data.table::data.table(
        trip_id = rep(names(departures), each = n_stops),
        stop_id = rep(template$stop_id, times = n_departures),
        stop_sequence = rep(template$stop_sequence, times = n_departures),
        departure_time_secs = new_departures,
        arrival_time_secs = new_arrivals
      )
    }
  )
  stop_times_to_add <- data.table::rbindlist(stop_times_to_add)
  stop_times_to_add[
    ,
    `:=`(
      departure_time = seconds_to_string(departure_time_secs),
      arrival_time = seconds_to_string(arrival_time_secs)
    )
  ]
  data.table::setcolorder(stop_times_to_add, neworder = names(gtfs$stop_times))

  # fourth step: filter the original stop_times table and bind the new one to it

  filtered_stop_times <- gtfs$stop_times[! trip_id %chin% relevant_trips]
  gtfs$stop_times <- rbind(filtered_stop_times, stop_times_to_add)

  # fifth step: adjust the trips table to include the new trips

  trips_to_add <- mapply(
    relevant_trips,
    departure_times,
    SIMPLIFY = FALSE,
    FUN = function(trip, departures) {
      n_departures <- length(departures)
      new_trips <- gtfs$trips[trip_id == trip]
      new_trips <- new_trips[rep(1, n_departures)]
      new_trips[, trip_id := names(departures)]
    }
  )
  trips_to_add <- data.table::rbindlist(trips_to_add)

  filtered_trips <- gtfs$trips[! trip_id %chin% relevant_trips]
  gtfs$trips <- rbind(filtered_trips, trips_to_add)

  # sixth step: adjust the frequencies table

  filtered_frequencies <- gtfs$frequencies[! trip_id %chin% relevant_trips]

  if (nrow(filtered_frequencies) > 0) {
    gtfs$frequencies <- filtered_frequencies
  } else {
    gtfs$frequencies <- NULL
  }

  # conditionally remove the auxiliary columns with the times in seconds

  frequencies_exist <- gtfsio::check_file_exists(gtfs, "frequencies")

  if (exists("created_start_secs") && frequencies_exist) {
    gtfs$frequencies[, start_time_secs := NULL]
  }

  if (exists("created_end_secs") && frequencies_exist) {
    gtfs$frequencies[, end_time_secs := NULL]
  }

  if (exists("created_departure_secs")) {
    gtfs$stop_times[, departure_time_secs := NULL]
  }

  if (exists("created_arrival_secs")) {
    gtfs$stop_times[, arrival_time_secs := NULL]
  }

  return(gtfs)
}
