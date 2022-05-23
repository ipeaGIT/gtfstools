#' Filter GTFS object by `shape_id`
#'
#' Filters a GTFS object by `shape_id`s, keeping (or dropping) the relevant
#' entries in each file.
#'
#' @template gtfs
#' @param shape_id A character vector. The `shape_id`s used to filter the data.
#' @param keep A logical. Whether the entries related to the specified
#'   `shape_id`s should be kept or dropped (defaults to `TRUE`, which keeps the
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
#' shape_ids <- c("17846", "68962")
#'
#' object.size(gtfs)
#'
#' # keeps entries related to passed shape_ids
#' smaller_gtfs <- filter_by_shape_id(gtfs, shape_ids)
#' object.size(smaller_gtfs)
#'
#' # drops entries related to passed shape_ids
#' smaller_gtfs <- filter_by_shape_id(gtfs, shape_ids, keep = FALSE)
#' object.size(smaller_gtfs)
#'
#' @export
filter_by_shape_id <- function(gtfs, shape_id, keep = TRUE) {
  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_character(shape_id, any.missing = FALSE)
  checkmate::assert_logical(keep, len = 1, any.missing = FALSE)

  # selecting the filter operator used to filter 'shape_id's based on 'keep' and
  # storing the current environment to filter using the values of 'shape_id'

  `%ffilter%` <- `%chin%`
  if (!keep) `%ffilter%` <- Negate(`%chin%`)

  env <- environment()

  # 'shapes' and 'trips' can be filtered using 'shape_id' itself, so `%ffilter%`
  # is used. the other files depend on relational associations with 'shape_id'
  # that come from these 2 files.

  # 'shapes' (shape_id)

  if (gtfsio::check_field_exists(gtfs, "shapes", "shape_id")) {

    gtfsio::assert_field_class(gtfs, "shapes", "shape_id", "character")
    gtfs$shapes <- gtfs$shapes[shape_id %ffilter% get("shape_id", envir = env)]

  }

  # 'trips' (shape_id)

  if (gtfsio::check_field_exists(gtfs, "trips", "shape_id")) {

    gtfsio::assert_field_class(gtfs, "trips", "shape_id", "character")
    gtfs$trips <- gtfs$trips[shape_id %ffilter% get("shape_id", envir = env)]

    # 'trips' allows us to filter by 'route_id', 'service_id' and 'trip_id'

    relevant_routes <- unique(gtfs$trips$route_id)
    relevant_services <- unique(gtfs$trips$service_id)
    relevant_trips <- unique(gtfs$trips$trip_id)

    # 'calendar' and 'calendar_dates' (service_id)

    if (gtfsio::check_field_exists(gtfs, "calendar", "service_id")) {

      gtfsio::assert_field_class(gtfs, "calendar", "service_id", "character")
      gtfs$calendar <- gtfs$calendar[service_id %chin% relevant_services]

    }

    if (gtfsio::check_field_exists(gtfs, "calendar_dates", "service_id")) {

      gtfsio::assert_field_class(
        gtfs,
        "calendar_dates",
        "service_id",
        "character"
      )
      gtfs$calendar_dates <- gtfs$calendar_dates[
        service_id %chin% relevant_services
      ]

    }

    # 'routes' and 'fare_rules' (route_id)
    # note that we can filter agency both via routes -> agency_id and via
    # fare_rules -> fare_id -> fare_attributes -> agency_id. so we create a
    # 'relevant_agencies' vector and, populate with agency_ids coming from both
    # possibilities and use all of them to filter agency later

    relevant_agencies <- vector("character", length = 0L)

    if (gtfsio::check_field_exists(gtfs, "routes", "route_id")) {

      gtfsio::assert_field_class(gtfs, "routes", "route_id", "character")
      gtfs$routes <- gtfs$routes[route_id %chin% relevant_routes]

      # 'routes' allows us to filter by 'agency_id'.
      # 'agency_id' is conditionally required, which means that it may not be
      # listed if 'agency' has only one row.

      relevant_agencies_routes <- unique(gtfs$routes$agency_id)

      if (is.null(relevant_agencies_routes))
        relevant_agencies_routes <- unique(gtfs$agency$agency_id)

      relevant_agencies <- c(relevant_agencies, relevant_agencies_routes)

    }

    if (gtfsio::check_field_exists(gtfs, "fare_rules", "route_id")) {

      gtfsio::assert_field_class(gtfs, "fare_rules", "route_id", "character")
      gtfs$fare_rules <- gtfs$fare_rules[route_id %chin% relevant_routes]

      # 'fare_rules' allows us to filter by 'fare_id'

      relevant_fares <- unique(gtfs$fare_rules$fare_id)

      # 'fare_attributes' (fare_id)

      if (gtfsio::check_field_exists(gtfs, "fare_attributes", "fare_id")) {

        gtfsio::assert_field_class(
          gtfs,
          "fare_attributes",
          "fare_id",
          "character"
        )
        gtfs$fare_attributes <- gtfs$fare_attributes[
          fare_id %chin% relevant_fares
        ]

        # 'fare_attributes' allows us to filter by 'agency_id'. again,
        # 'agency_id' is conditionally required, which means that it may not be
        # listed if 'agency' has only one row.

        relevant_agencies_fare_att <- unique(gtfs$fare_attributes$agency_id)

        if (is.null(relevant_agencies_fare_att))
          relevant_agencies_fare_att <- unique(gtfs$agency$agency_id)

        relevant_agencies <- c(relevant_agencies, relevant_agencies_fare_att)

      }

    }

    # 'agency' (agency_id, that comes both from routes and fare_attributes)

    if (gtfsio::check_field_exists(gtfs, "agency", "agency_id") &&
        exists("relevant_agencies")) {

      # keeping only unique agency_ids from relevant_agencies, since they may
      # come from two different sources
      relevant_agencies <- unique(relevant_agencies)

      gtfsio::assert_field_class(gtfs, "agency", "agency_id", "character")
      gtfs$agency <- gtfs$agency[agency_id %chin% relevant_agencies]

    }

    # 'frequencies' and 'stop_times' (trip_id)

    if (gtfsio::check_field_exists(gtfs, "frequencies", "trip_id")) {

      gtfsio::assert_field_class(gtfs, "frequencies", "trip_id", "character")
      gtfs$frequencies <- gtfs$frequencies[trip_id %chin% relevant_trips]

    }

    if (gtfsio::check_field_exists(gtfs, "stop_times", "trip_id")) {

      gtfsio::assert_field_class(gtfs, "stop_times", "trip_id", "character")
      gtfs$stop_times <- gtfs$stop_times[trip_id %chin% relevant_trips]

      # 'stop_times' allows us to filter by 'stop_id'. it's important to keep,
      # however, not only the stops that appear on stop_times, but also their
      # parent stops, that may not be listed on such file

      relevant_stops <- unique(gtfs$stop_times$stop_id)

      if (gtfsio::check_field_exists(gtfs, "stops", "parent_station")) {

        # this may result in a warning if stop_times has stop_ids not listed in
        # stops
        suppressWarnings(
          stops_with_parents <- get_parent_station(gtfs, relevant_stops)
        )
        relevant_stops <- stops_with_parents$stop_id

      }

      # 'stops', 'transfers' and 'pathways' (stop_id)

      from_to_stop_id <- c("from_stop_id", "to_stop_id")

      if (gtfsio::check_field_exists(gtfs, "transfers", from_to_stop_id)) {

        gtfsio::assert_field_class(
          gtfs,
          "transfers",
          from_to_stop_id,
          rep("character", 2)
        )
        gtfs$transfers <- gtfs$transfers[
          from_stop_id %chin% relevant_stops & to_stop_id %chin% relevant_stops
        ]

      }

      if (gtfsio::check_field_exists(gtfs, "pathways", from_to_stop_id)) {

        gtfsio::assert_field_class(
          gtfs,
          "pathways",
          from_to_stop_id,
          rep("character", 2)
        )
        gtfs$pathways <- gtfs$pathways[
          from_stop_id %chin% relevant_stops & to_stop_id %chin% relevant_stops
        ]

      }

      if (gtfsio::check_field_exists(gtfs, "stops", "stop_id")) {

        gtfsio::assert_field_class(gtfs, "stops", "stop_id", "character")
        gtfs$stops <- gtfs$stops[stop_id %chin% relevant_stops]

        # 'stops' allows us to filter by 'level_id'

        relevant_levels <- unique(gtfs$stops$level_id)

        # 'levels' (level_id)

        if (gtfsio::check_field_exists(gtfs, "levels", "level_id")) {

          gtfsio::assert_field_class(gtfs, "levels", "level_id", "character")
          gtfs$levels <- gtfs$levels[level_id %chin% relevant_levels]

        }

      }

    }

  }

  return(gtfs)

}
