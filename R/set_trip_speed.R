#' Set trip average speed
#'
#' Sets the average speed of each specified \code{trip_id} by changing the
#' \code{arrival_time} and \code{departure_time} columns in \code{stop_times}.
#'
#' @param gtfs A GTFS object as created by \code{\link{read_gtfs}}.
#' @param trip_id A string vector including the \code{trip_id}s to have their
#'   average speed set.
#' @param speed A numeric representing the speed to be set. Its length must
#'   either equal 1, in which case the value is recycled for all
#'   \code{trip_id}s, or equal \code{trip_id}'s length.
#' @param unit A string representing the unit in which the speed is given. One
#'   of \code{"km/h"} (the default) or \code{"m/s"}.
#' @param by_reference Whether to update \code{stop_times}' \code{data.table} by
#'   reference. Defaults to \code{FALSE}.
#'
#' @return If \code{by_reference} is set to \code{FALSE}, returns a GTFS object
#'   with the time columns of its \code{stop_times} adjusted. Else, returns a
#'   GTFS object invisibly (note that in this case the original GTFS object is
#'   altered).
#'
#' @section Details:
#' The average speed is calculated as the difference between the arrival time at
#' the last stop minus the departure time at the first top, over the trip's
#' length (as calculated via \code{\link{get_trip_geometry}}, based on the
#' \code{shapes} file). The arrival and departure times at all other stops (i.e.
#' not the first neither the last) are set as \code{""}, which is written as
#' \code{NA} with \code{\link{write_gtfs}}. Some transport routing software,
#' such as \href{http://www.opentripplanner.org/}{OpenTripPlanner}, support
#' specifying stop times like so. In such cases, they estimate arrival/departure
#' times at the others stops based on the average speed as well. We plan to add
#' that feature to this function in the future.
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#'
#' gtfs <- read_gtfs(data_path)
#'
#' # the examples below require the 'lwgeom' package to be installed
#' if (requireNamespace("lwgeom", quietly = TRUE)) {
#'
#'   gtfs_new_speed <- set_trip_speed(gtfs, trip_id = "CPTM L07-0", 50)
#'   gtfs_new_speed$stop_times[trip_id == "CPTM L07-0"]
#'
#'   # original gtfs remains unchanged
#'   gtfs$stop_times[trip_id == "CPTM L07-0"]
#'
#'   # now do it by reference
#'   set_trip_speed(gtfs, trip_id = "CPTM L07-0", 50, by_reference = TRUE)
#'   gtfs$stop_times[trip_id == "CPTM L07-0"]
#'
#' }
#'
#' @export
set_trip_speed <- function(gtfs,
                           trip_id,
                           speed,
                           unit = "km/h",
                           by_reference = FALSE) {

  env <- environment()

  # checking if {lwgeom} is installed. {lwgeom} is a {sf} dependency required to
  # run sf::st_length()

  if (!requireNamespace("lwgeom", quietly = TRUE))
    stop(
      "The 'lwgeom' package is required to run this function. ",
      "Please install it first."
    )

  # input checking

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_character(trip_id)
  checkmate::assert(
    checkmate::check_numeric(speed, len = 1),
    checkmate::check_numeric(speed, len = length(trip_id)),
    combine = "or"
  )
  checkmate::assert(
    checkmate::check_string(unit),
    checkmate::check_names(unit, subset.of = c("km/h", "m/s")),
    combine = "and"
  )
  checkmate::assert_logical(by_reference)

  # check if required fields and files exist

  gtfsio::assert_field_class(
    gtfs,
    "stop_times",
    c("trip_id", "arrival_time", "departure_time", "stop_sequence"),
    c("character", "character", "character", "integer")
  )

  # calculate the length of each given trip_id

  trip_length <- get_trip_geometry(gtfs, trip_id, file = "shapes")
  trip_length_ids <- trip_length$trip_id
  trip_length <- sf::st_length(trip_length)
  trip_length <- as.numeric(units::set_units(trip_length, "km"))
  names(trip_length) <- trip_length_ids

  # set speed adequate unit (use km/h for calculations)

  if (length(speed) == 1) speed <- rep(speed, length(trip_id))

  units(speed) <- unit
  speed <- as.numeric(units::set_units(speed, "km/h"))
  names(speed) <- trip_id

  # calculate each trip duration (in hours) based on its length and given
  # desired speed. calculate only of those 'trip_id's that exist in 'stop_times'

  trip_duration <- trip_length[trip_length_ids] / speed[trip_length_ids]

  # if by_reference is set to FALSE, make a copy of stop_times

  if (by_reference) {
    stop_times <- gtfs$stop_times
  } else {
    stop_times <- data.table::copy(gtfs$stop_times)
  }

  # find index of:
  # - all stops relative to given trip_ids
  # - first and last stops of each given trip_id
  # - the difference of above objects (thus the intermediate entries)

  desired_trips_index <- stop_times[, .I[trip_id %chin% trip_length_ids]]

  min_stops_index <- stop_times[
    trip_id %chin% trip_length_ids,
    .I[1],
    by = trip_id
  ]
  min_stops_index <- min_stops_index$V1

  # issue #37 - max() raises a warning if it receives a integer(0), which
  # happens when none of the specified 'trip_id's exist in the gtfs object
  if (identical(trip_length_ids, character(0))) {
    max_stops_index <- 0
  } else {
    max_stops_index <- stop_times[
      trip_id %chin% trip_length_ids,
      .I[max(stop_sequence)],
      by = trip_id
    ]
    max_stops_index <- max_stops_index$V1
  }

  na_time_stops_index <- setdiff(
    desired_trips_index,
    c(min_stops_index, max_stops_index)
  )

  # make sure that first stop arrival_time equals departure_time and save value

  stop_times[min_stops_index, arrival_time := departure_time]

  first_departure <- stop_times[min_stops_index, departure_time]
  first_departure <- string_to_seconds(first_departure)

  # substitute last stop arrival and departure_time by first_departure plus
  # duration

  trip_duration <- as.integer(trip_duration * 3600)

  last_arrival  <- first_departure + trip_duration
  last_arrival  <- seconds_to_string(last_arrival)

  stop_times[
    max_stops_index,
    `:=`(arrival_time = last_arrival, departure_time = last_arrival)
  ]

  # substitute given 'trip_id's intermediate stops arrival and departure time
  # by ""

  stop_times[na_time_stops_index, `:=`(arrival_time = "", departure_time = "")]

  # if by_reference is TRUE, return gtfs invisibly

  if (by_reference) return(invisible(gtfs))

  # else assign stop_times to gtfs$stop_times and return gtfs

  gtfs$stop_times <- stop_times

  return(gtfs)

}
