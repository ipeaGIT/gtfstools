#' Filter GTFS object by `route_type` (transport mode)
#'
#' Filters a GTFS object by `route_type`s, keeping (or dropping) the relevant
#' entries in each file.
#'
#' @param gtfs A GTFS object.
#' @param route_type An integer vector. The `route_type`s used to filter the
#'   data.
#' @param keep A logical. Whether the entries related to the specified
#'   `route_type`s should be kept or dropped (defaults to `TRUE`, which keeps
#'   the entries).
#'
#' @return The GTFS object passed to the `gtfs` parameter, after the filtering
#' process.
#'
#' @details # Route types
#' Valid options are:
#' - 0 - Tram, Streetcar, Light rail. Any light rail or street level system
#' within a metropolitan area.
#' - 1 - Subway, Metro. Any underground rail system within a metropolitan area.
#' - 2 - Rail. Used for intercity or long-distance travel.
#' - 3 - Bus. Used for short- and long-distance bus routes.
#' - 4 - Ferry. Used for short- and long-distance boat service.
#' - 5 - Cable tram. Used for street-level rail cars where the cable runs
#' beneath the vehicle, e.g., cable car in San Francisco.
#' - 6 - Aerial lift, suspended cable car (e.g., gondola lift, aerial tramway).
#' Cable transport where cabins, cars, gondolas or open chairs are suspended by
#' means of one or more cables.
#' - 7 - Funicular. Any rail system designed for steep inclines.
#' - 11 - Trolleybus. Electric buses that draw power from overhead wires using
#' poles.
#' - 12 - Monorail. Railway in which the track consists of a single rail or a
#' beam.
#'
#' @family filtering functions
#'
#' @examples
#' # read gtfs
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' object.size(gtfs)
#'
#' # keeps entries related to passed route_types
#' smaller_gtfs <- filter_by_route_type(gtfs, route_type = 1)
#' object.size(smaller_gtfs)
#'
#' # drops entries related to passed route_types
#' smaller_gtfs <- filter_by_route_type(gtfs, route_type = 1, keep = FALSE)
#' object.size(smaller_gtfs)
#'
#' @export
filter_by_route_type <- function(gtfs, route_type, keep = TRUE) {

  # input checking

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_integerish(route_type)
  checkmate::assert_logical(keep, len = 1)

  possible_types <- c(0, 1, 2, 3, 4, 5, 6, 7, 11, 12)
  checkmate::assert_names(
    as.character(route_type),
    subset.of = as.character(possible_types),
    .var.name = "route_type"
  )

  # storing the current environment to filter using the values of 'trip_id'

  env <- environment()

  # select the 'route_id's that correspond to the given 'route_type's to filter
  # those using filter_by_route_id()

  if (gtfsio::check_field_exists(gtfs, "routes", c("route_id", "route_type"))) {

    gtfsio::assert_field_class(
      gtfs,
      "routes",
      c("route_id", "route_type"),
      c("character", "integer")
    )

    route_id <- gtfs$routes[
      route_type %in% get("route_type", envir = env)
    ]$route_id

    gtfs <- filter_by_route_id(gtfs, route_id, keep)

  }

  return(gtfs)

}


