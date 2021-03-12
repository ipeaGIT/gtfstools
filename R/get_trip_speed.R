#' Get trip speed
#'
#' Returns the speed of each specified \code{trip_id}, based on the geometry
#' created from either on the \code{shapes} or the \code{stop_times} file (or
#' both).
#'
#' @param gtfs A GTFS object as created by \code{\link{read_gtfs}}.
#' @param trip_id A string vector including the \code{trip_id}s to have their
#'   speeds calculated. If \code{NULL} (the default), the function calculates
#'   the speed of every \code{trip_id} in the GTFS.
#' @param file The file from which geometries should be generated, either
#'   \code{shapes} and \code{stop_times} (geometries are used to calculate the
#'   length of a trip). Defaults to \code{shapes}.
#' @param unit A string representing the unit in which the speeds are desired.
#'   Either \code{"km/h"} (the default) or \code{"m/s"}.
#'
#' @return A \code{data.table} containing the duration of each specified trip
#'   and the file from which geometries were generated.
#'
#' @section Details:
#' Please check \code{\link{get_trip_geometry}} documentation to understand how
#' geometry generation differs depending on the chosen file.
#'
#' @seealso \code{\link{get_trip_geometry}}
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#'
#' gtfs <- read_gtfs(data_path)
#'
#' # the examples below require the 'lwgeom' package to be installed
#' if (requireNamespace("lwgeom", quietly = TRUE)) {
#'
#'   trip_speed <- get_trip_speed(gtfs)
#'   head(trip_speed)
#'
#'   trip_ids <- c("CPTM L07-0", "2002-10-0")
#'   trip_speed <- get_trip_speed(gtfs, trip_ids)
#'   trip_speed
#'
#'   trip_speed <- get_trip_speed(
#'     gtfs,
#'     trip_ids,
#'     file = c("shapes", "stop_times")
#'   )
#'   trip_speed
#'
#'   trip_speed <- get_trip_speed(gtfs, trip_ids, unit = "m/s")
#'   trip_speed
#'
#' }
#'
#' @export
get_trip_speed <- function(gtfs,
                           trip_id = NULL,
                           file = "shapes",
                           unit = "km/h") {

  # checking if {lwgeom} is installed. {lwgeom} is a {sf} dependency required to
  # run sf::st_length()

  if (!requireNamespace("lwgeom", quietly = TRUE))
    stop(
      "The 'lwgeom' package is required to run this function. ",
      "Please install it first."
    )

  # input checking

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_character(trip_id, null.ok = TRUE)
  checkmate::assert_names(file, subset.of = c("shapes", "stop_times"))
  checkmate::assert(
    checkmate::check_string(unit),
    checkmate::check_names(unit, subset.of = c("km/h", "m/s")),
    combine = "and"
  )

  # check if fields and files required to estimate trip duration exist (a bit of
  # an overlap, since these are also checked in a later get_trip_duration call,
  # but it prevents cases where the files required to generate geometries are
  # present but those required to estimate trip duration are not, which would
  # cause errors to be thrown very late)

  checkmate::assert(
    check_gtfs_field_exists(
      gtfs,
      "stop_times",
      c("trip_id", "arrival_time", "departure_time")
    )
  )

  # generate desired geometries - checking for required files/fields is done
  # pretty early into get_trip_geometry code

  trips_geometries <- get_trip_geometry(gtfs, trip_id, file)

  # calculate the length of each geometry

  trips_length <- sf::st_length(trips_geometries)
  if (unit == "km/h") trips_length <- units::set_units(trips_length, "km")
  trips_length <- as.numeric(trips_length)

  # create data.table holding the length of each trip

  trips_speed <- data.table::data.table(
    trip_id     = trips_geometries$trip_id,
    origin_file = trips_geometries$origin_file,
    length      = trips_length
  )

  # calculate trips' duration

  existing_trips <- unique(trips_speed$trip_id)

  duration_unit <- data.table::fifelse(unit == "km/h", "h", "s")

  trips_duration <- get_trip_duration(gtfs, existing_trips, duration_unit)

  # join trips_speed and trips_duration

  trips_speed[trips_duration, on = "trip_id", duration := i.duration]

  # calculate speed as length/duration

  trips_speed[, speed := length / duration]

  # remove length and duration columns

  trips_speed[, `:=`(length = NULL, duration = NULL)]

  return(trips_speed[])

}
