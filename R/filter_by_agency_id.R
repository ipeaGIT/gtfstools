#' Filter GTFS object by `agency_id`
#'
#' Filters a GTFS object by `agency_id`s, keeping (or dropping) the relevant
#' entries in each file.
#'
#' @template gtfs
#' @param agency_id A character vector. The `agency_id`s used to filter the
#' data.
#' @param keep A logical. Whether the entries related to the specified
#' `agency_id`s should be kept or dropped (defaults to `TRUE`, which keeps the
#' entries).
#'
#' @return The GTFS object passed to the `gtfs` parameter, after the filtering
#' process.
#'
#' @family filtering functions
#'
#' @examples
#' data_path <- system.file("extdata/ber_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#' agency_id <- "92"
#'
#' object.size(gtfs)
#'
#' # keeps entries related to passed agency_id
#' smaller_gtfs <- filter_by_agency_id(gtfs, agency_id)
#' object.size(smaller_gtfs)
#'
#' # drops entries related to passed agency_id
#' smaller_gtfs <- filter_by_agency_id(gtfs, agency_id, keep = FALSE)
#' object.size(smaller_gtfs)
#'
#' @export
filter_by_agency_id <- function(gtfs, agency_id, keep = TRUE) {
  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_character(agency_id, any.missing = FALSE)
  checkmate::assert_logical(keep, len = 1, any.missing = FALSE)

  # selecting the filter operator used to filter 'agency_id's based on 'keep'.
  # storing the current environment to filter using the values of 'agency_id'

  `%ffilter%` <- `%chin%`
  if (!keep) `%ffilter%` <- Negate(`%chin%`)

  env <- environment()

  # 'agency', 'routes', 'fare_attributes' and 'attributions' can be filtered
  # using 'agency_id' itself, so `%ffilter%` is used. the other files depend on
  # relational associations with 'agency_id' that come from these 4 tables.

  # 'agency', 'attributes' and 'fare_attributes' (agency_id)

  if (gtfsio::check_field_exists(gtfs, "agency", "agency_id")) {
    gtfsio::assert_field_class(gtfs, "agency", "agency_id", "character")
    gtfs$agency <- gtfs$agency[
      agency_id %ffilter% get("agency_id", envir = env)
    ]
  }

  if (gtfsio::check_field_exists(gtfs, "attributions", "agency_id")) {
    gtfsio::assert_field_class(gtfs, "attributions", "agency_id", "character")
    gtfs$attributions <- gtfs$attributions[
      agency_id %ffilter% get("agency_id", envir = env)
    ]
  }

  if (gtfsio::check_field_exists(gtfs, "fare_attributes", "agency_id")) {
    # 'fare_attributes' could be used to filter 'fare_rules' via fare_id. but
    # that introduces some complexity, because agency_id is an optional field
    # in both 'fare_attributes' and 'routes' if 'agency' contains only one
    # agency, and 'routes' can also be used to filter 'fare_rules' (via
    # route_id). i'm opting to filter 'fare_rules' via the 'routes' path only,
    # afterall a fare only makes sense if it's associated to a route, but i can
    # change this behaviour later if needed
    gtfsio::assert_field_class(
      gtfs,
      "fare_attributes",
      "agency_id",
      "character"
    )
    gtfs$fare_attributes <- gtfs$fare_attributes[
      agency_id %ffilter% get("agency_id", envir = env)
    ]
  }

  # routes (agency_id)

  if (gtfsio::check_field_exists(gtfs, "routes", "agency_id")) {
    gtfsio::assert_field_class(gtfs, "routes", "agency_id", "character")
    gtfs$routes <- gtfs$routes[
      agency_id %ffilter% get("agency_id", envir = env)
    ]

    # 'routes' allows us to filter by 'route_id'
    relevant_routes <- unique(gtfs$routes$route_id)

    # 'fare_rules' and 'trips' (route_id)

    if (gtfsio::check_field_exists(gtfs, "fare_rules", "route_id")) {
      gtfsio::assert_field_class(gtfs, "fare_rules", "route_id", "character")
      gtfs$fare_rules <- gtfs$fare_rules[route_id %chin% relevant_routes]
    }

    if (gtfsio::check_field_exists(gtfs, "trips", "route_id")) {
      gtfsio::assert_field_class(gtfs, "trips", "route_id", "character")
      gtfs$trips <- gtfs$trips[route_id %chin% relevant_routes]

      # 'trips' allows us to filter by 'trip_id', 'service_id' and 'shape_id'
      relevant_services <- unique(gtfs$trips$service_id)
      relevant_shapes <- unique(gtfs$trips$shape_id)
      relevant_trips <- unique(gtfs$trips$trip_id)

      # 'shapes' (shape_id)

      if (gtfsio::check_field_exists(gtfs, "shapes", "shape_id")) {
        gtfsio::assert_field_class(gtfs, "shapes", "shape_id", "character")
        gtfs$shapes <- gtfs$shapes[shape_id %chin% relevant_shapes]
      }

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

      # 'stop_times' and 'frequencies' (trip_id)

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
        # (get_parent_station() may raise a warning if a stop is present in
        # 'stop_times' but not in 'stops', which will be suppressed for now)

        relevant_stops <- unique(gtfs$stop_times$stop_id)

        if (gtfsio::check_field_exists(gtfs, "stops", "parent_station")) {
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
            from_stop_id %chin% relevant_stops &
              to_stop_id %chin% relevant_stops
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
            from_stop_id %chin% relevant_stops &
              to_stop_id %chin% relevant_stops
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
  }

  return(gtfs)
}
