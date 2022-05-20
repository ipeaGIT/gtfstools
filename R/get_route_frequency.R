#' Get route frequency
#'
#' Returns the headway (in seconds) and the number of departures of each
#' specified `route_id` within a given time period.
#'
#' @template gtfs
#' @param route_id A character vector including the `route_id`s to have their
#'   frequencies calculated. If `NULL` (the default), the function calculates
#'   the speed of every `route_id` in the GTFS.
#' @param from A string. The starting point of the time of day, in the
#' "HH:MM:SS" format.
#' @param to A string. The ending point of the time of day, in the "HH:MM:SS"
#' format.

#' @return A `data.table` containing the headway (in seconds) and number of
#' departures of each specified `route_id` within a given time period.
#'
#' @family frequency functions
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#'
#' gtfs <- read_gtfs(data_path)
#'
#' # all routes
#' route_frequency <- get_route_frequency(gtfs,
#'                                        from = '08:00:00',
#'                                        to = '20:00:00')
#' head(route_frequency)
#'
#' # selected routes
#' route_ids <- c("CPTM L07", "2002-10")
#' route_frequency <- get_route_frequency(gtfs,
#'                                        route_id = route_ids,
#'                                        from = '08:00:00',
#'                                        to = '20:00:00')
#' print(route_frequency)
#'
#' @export
get_route_frequency <- function(gtfs,
                                route_id = NULL,
                                from,
                                to) {
  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_character(route_id, null.ok = TRUE)
  checkmate::assert_string(from, pattern = "^\\d{2}:\\d{2}:\\d{2}$")
  checkmate::assert_string(to, pattern = "^\\d{2}:\\d{2}:\\d{2}$")

  from_secs <- string_to_seconds(from)
  to_secs <- string_to_seconds(to)
  if (from_secs > to_secs) stop("'from' must be lower than 'to'.")

  # convert GTFS time to seconds
  gtfs <- gtfstools::convert_time_to_seconds(gtfs)

# if frequencies.txt exist
  if (!is.null(gtfs$frequencies) | nrow(gtfs$frequencies)==0) {

    # check all fields required exist
    gtfsio::assert_field_class(
      gtfs,
      "frequencies",
      c("trip_id", "start_time", "end_time", "headway_secs"),
      c("character", "character", "character", "integer")
    )

    # bring route_id to frequencies table
    freq <- data.table::copy(gtfs$frequencies)
    freq[gtfs$trips, on='trip_id', route_id := i.route_id]

    # filter route_id info
    if (!is.null(route_id)) {
      routeid_input <- route_id
      freq <- freq[ route_id %chin% routeid_input,]
    }

    # filter time of the day
    freq_trip <- freq[ start_time_secs >= from_secs & end_time_secs <= to_secs, ]

    # calculate number of departures by trip_id
    freq_trip[, total_departures := (end_time_secs-start_time_secs)/headway_secs,
              by = .(route_id, trip_id)]

    # aggregate by route
    freq_route <- freq_trip[, .(headway_secs = mean(headway_secs) / .N,
                                total_departures = sum(total_departures)
                                ),
                            by = .(route_id, start_time, end_time)]

    return(freq_route[])

  } else {
# if frequencies.txt file does NOT exist

    # bring route_id to stop_times table
    stop_times <- data.table::copy(gtfs$stop_times)
    stop_times[gtfs$trips, on='trip_id', route_id := i.route_id]

    # filter route_id info
    if (!is.null(route_id)) {
      routeid_input <- route_id
      stop_times <- stop_times[ route_id %chin% routeid_input,]
    }

    # filter time of the day
    stop_times_trip <- stop_times[ departure_time_secs  >= from_secs & arrival_time_secs  <= to_secs, ]

    # aggregate by route
    stop_times_route <- stop_times_trip[, .(start_time = from,
                                            end_time = to,
                                            headway_secs = (to_secs- from_secs) / sum(stop_sequence[which(stop_sequence ==1)]),
                                            total_departures = sum(stop_sequence[which(stop_sequence ==1)])
                                            ),
                                        by = route_id]

    return(stop_times_route[])
    }
}
