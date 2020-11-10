#' Return trip duration
#'
#' Returns the duration of each specified \code{trip_id}.
#'
#' @param gtfs A GTFS file as read by \code{tidytransit::read_gtfs()}.
#' @param trip_id A string vector including the \code{trip_id}s to have their
#'   durations calculated. If \code{NULL} (the default) the function calculates
#'   the duration of every \code{trip_id} in the GTFS.
#'
#' @return A table containing the duration (in minutes) of each specified trip.
#'
#' @export
trip_duration <- function(gtfs, trip_id = NULL) {

  # filter gtfs stop_times and frequencies to reduce computation time

  if (! is.null(trip_id)) {
    relevant_trips <- trip_id
  } else {
    relevant_trips <- unique(gtfs$trips$trip_id)
  }

  gtfs$stop_times <- dplyr::filter(gtfs$stop_times, trip_id %in% relevant_trips)

  if (exists("frequencies", gtfs)) {
    gtfs$frequencies <- dplyr::filter(gtfs$frequencies, trip_id %in% relevant_trips)
  }

  # calculate duration in minutes

  gtfs <- tidytransit::set_hms_times(gtfs)

  durations <- gtfs$stop_times %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(
      duration = as.numeric(max(arrival_time_hms) - min(departure_time_hms)) / 60,
      .groups = "drop"
    )

  return(durations)

}
