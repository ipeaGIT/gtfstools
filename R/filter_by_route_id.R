#' Filter GTFS object by `route_id`
#'
#' Filters a GTFS object by `route_id`s, keeping (or dropping) the relevant
#' entries in each file.
#'
#' @param gtfs A GTFS object.
#' @param route_id A character vector. The `route_id`s used to filter the data.
#' @param keep A logical. Whether the entries related to the specified
#'   `route_id`s should be kept or dropped (defaults to `TRUE`, which keeps the
#'   entries).
#'
#' @return The GTFS object passed to the `gtfs` parameter, after the filtering
#' process.
#'
#' @family filtering functions
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#' route_ids <- c("6450-51", "CPTM L11")
#'
#' object.size(gtfs)
#'
#' # keeps entries related to passed route_ids
#' smaller_gtfs <- filter_by_route_id(gtfs, route_ids)
#' object.size(smaller_gtfs)
#'
#' # drops entries related to passed route_ids
#' smaller_gtfs <- filter_by_route_id(gtfs, route_ids, keep = FALSE)
#' object.size(smaller_gtfs)
#'
#' @export
filter_by_route_id <- function(gtfs, route_id, keep = TRUE) {

  # input checking

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_character(route_id)
  checkmate::assert_logical(keep, len = 1)

  # selecting the filter operator used to filter 'trip_id's based on 'keep' and
  # storing the current environment to filter using the values of 'trip_id'

  `%ffilter%` <- `%chin%`
  if (!keep) `%ffilter%` <- Negate(`%chin%`)

  env <- environment()

  relevant_agencies <- vector("character", 0)

  # 'routes', 'trips' and 'fare_rules'' can be filtered using 'route_id' itself,
  # so `%ffilter%` is used. the other files depend on relational associations
  # with 'route_id' that come from these 3 files.

  # routes (route_id)

  if (gtfsio::check_fields_exist(gtfs, "routes", "route_id")) {

    gtfsio::assert_fields_types(gtfs, "routes", "route_id", "character")
    gtfs$routes <- gtfs$routes[route_id %ffilter% get("route_id", envir = env)]

    # 'routes' allows us to filter by 'agency_id'
    # note that we can filter agency via routes -> agency_id or via
    # fare_rules -> fare_attributes -> agency_id. so we populate
    # 'relevant_agencies' with the relevant agencies coming from these different
    # options.
    #
    # also, 'agency_id' is conditionally required, which means that it may not
    # be listed if 'agency' has only one row.

    relevant_agencies_routes <- unique(gtfs$routes$agency_id)

    # 'agency' (agency_id)

    if (gtfsio::check_fields_exist(gtfs, "agency", "agency_id")) {

      gtfsio::assert_fields_types(gtfs, "agency", "agency_id", "character")

      if (is.null(relevant_agencies_routes) && nrow(gtfs$agency) == 1)
        relevant_agencies_routes <- gtfs$agency$agency_id

      relevant_agencies <- c(relevant_agencies, relevant_agencies_routes)

    }

  }

  # trips (route_id)

  if (gtfsio::check_fields_exist(gtfs, "trips", "route_id")) {

    gtfsio::assert_fields_types(gtfs, "trips", "route_id", "character")
    gtfs$trips <- gtfs$trips[route_id %ffilter% get("route_id", envir = env)]

    # 'trips' allows us to filter by 'trip_id', 'service_id' and 'shape_id'

    relevant_trips <- unique(gtfs$trips$trip_id)
    relevant_services <- unique(gtfs$trips$service_id)
    relevant_shapes <- unique(gtfs$trips$shape_id)

    # 'shapes' (shape_id)

    if (gtfsio::check_fields_exist(gtfs, "shapes", "shape_id")) {

      gtfsio::assert_fields_types(gtfs, "shapes", "shape_id", "character")
      gtfs$shapes <- gtfs$shapes[shape_id %chin% relevant_shapes]

    }

    # 'calendar' and 'calendar_dates' (service_id)

    if (gtfsio::check_fields_exist(gtfs, "calendar", "service_id")) {

      gtfsio::assert_fields_types(gtfs, "calendar", "service_id", "character")
      gtfs$calendar <- gtfs$calendar[service_id %chin% relevant_services]

    }

    if (gtfsio::check_fields_exist(gtfs, "calendar_dates", "service_id")) {

      gtfsio::assert_fields_types(
        gtfs,
        "calendar_dates",
        "service_id",
        "character"
      )
      gtfs$calendar_dates <- gtfs$calendar_dates[
        service_id %chin% relevant_services
      ]

    }

    # 'stop_times' and 'frequencies' (trip_id)

    if (gtfsio::check_fields_exist(gtfs, "frequencies", "trip_id")) {

      gtfsio::assert_fields_types(gtfs, "frequencies", "trip_id", "character")
      gtfs$frequencies <- gtfs$frequencies[trip_id %chin% relevant_trips]

    }

    if (gtfsio::check_fields_exist(gtfs, "stop_times", "trip_id")) {

      gtfsio::assert_fields_types(gtfs, "stop_times", "trip_id", "character")
      gtfs$stop_times <- gtfs$stop_times[trip_id %chin% relevant_trips]

      # 'stop_times' allows us to filter by 'stop_id'. it's important to keep,
      # however, not only the stops that appear on stop_times, but also their
      # parent stops, that may not be listed on such file
      # (get_parent_station may raise a warning if a stop is present in
      # 'stop_times' but not in 'stops', which will be suppressed for now)

      relevant_stops <- unique(gtfs$stop_times$stop_id)

      if (gtfsio::check_fields_exist(gtfs, "stops", "parent_station")) {

        suppressWarnings(
          stops_with_parents <- get_parent_station(gtfs, relevant_stops)
        )
        relevant_stops <- stops_with_parents$stop_id

      }

      # 'stops' (stop_id)

      if (gtfsio::check_fields_exist(gtfs, "stops", "stop_id")) {

        gtfsio::assert_fields_types(gtfs, "stops", "stop_id", "character")
        gtfs$stops <- gtfs$stops[stop_id %chin% relevant_stops]

        # 'stops' allows us to filter by 'zone_id' and 'level_id'

        relevant_levels <- unique(gtfs$stops$level_id)

        # 'levels' (level_id)

        if (gtfsio::check_fields_exist(gtfs, "levels", "level_id")) {

          gtfsio::assert_fields_types(gtfs, "levels", "level_id", "character")
          gtfs$levels <- gtfs$levels[level_id %chin% relevant_levels]

        }

        # 'fare_rules' (zone_id)
        # filtering by 'zone_id' would lead us to keep entries in 'fare_rules'
        # that relate to these zones, even if they didn't have anything to do
        # with the given 'route_id's. this could lead to non-intuitive results
        # (e.g. when not wanting to keep a route_id, but which eventually ends
        # up being kept because it is a zone that was deemed relevant because
        # other routes pass through it), so this step will be skipped

      }

    }

  }

  # fare_rules (route_id)

  if (gtfsio::check_fields_exist(gtfs, "fare_rules", "route_id")) {

    gtfsio::assert_fields_types(gtfs, "fare_rules", "route_id", "character")
    gtfs$fare_rules <- gtfs$fare_rules[
      route_id %ffilter% get("route_id", envir = env)
    ]

    # 'fare_rules' allows us to filter by 'fare_id'

    relevant_fares <- unique(gtfs$fare_rules$fare_id)

    # 'fare_attributes' (fare_id)

    if (gtfsio::check_fields_exist(gtfs, "fare_attributes", "fare_id")) {

      gtfsio::assert_fields_types(
        gtfs,
        "fare_attributes",
        "fare_id",
        "character"
      )
      gtfs$fare_attributes <- gtfs$fare_attributes[
        fare_id %chin% relevant_fares
      ]

    }

    # 'fare_attributes' allows us to filter by 'agency_id'
    # 'agency_id' is conditionally required, which means that it may not be
    # listed if 'agency' has only one row.

    relevant_agencies_fa <- unique(gtfs$fare_attributes$agency_id)

    # 'agency' (agency_id)

    if (gtfsio::check_fields_exist(gtfs, "agency", "agency_id")) {

      gtfsio::assert_fields_types(gtfs, "agency", "agency_id", "character")

      if (is.null(relevant_agencies_fa) && nrow(gtfs$agency) == 1)
        relevant_agencies_fa <- gtfs$agency$agency_id

      relevant_agencies <- c(relevant_agencies, relevant_agencies_fa)

    }

  }

  # filtering 'agency' based on relevant_agencies

  if (length(relevant_agencies) >= 1) {

    relevant_agencies <- unique(relevant_agencies)
    gtfs$agency <- gtfs$agency[agency_id %chin% relevant_agencies]

  }

  return(gtfs)

}
