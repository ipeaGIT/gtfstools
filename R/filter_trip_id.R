#' Filter GTFS object by `trip_id`
#'
#' Filters a GTFS object by `trip_id`s, keeping (or dropping) the relevant
#' entries in each file.
#'
#' @param gtfs A GTFS object.
#' @param trip_id A character vector. The `trip_id`s used to filter the data.
#' @param keep A logical. Whether the entries related to the specified
#'   `trip_id`s should be kept or dropped (defaults to `TRUE`, which keeps the
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
#' trip_ids <- c("CPTM L07-0", "2002-10-0")
#'
#' object.size(gtfs)
#'
#' # keeps entries related to passed trip_ids
#' smaller_gtfs <- filter_trip_id(gtfs, trip_ids)
#' object.size(smaller_gtfs)
#'
#' # drops entries related to passed trip_ids
#' smaller_gtfs <- filter_trip_id(gtfs, trip_ids, keep = FALSE)
#' object.size(smaller_gtfs)
#'
#' @export
filter_trip_id <- function(gtfs, trip_id, keep = TRUE) {

  # input checking

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_character(trip_id)
  checkmate::assert_logical(keep, len = 1)

  # selecting the filter operator used to filter 'trip_id's based on 'keep' and
  # storing the current environment to filter using the values of 'trip_id'

  `%ffilter%` <- `%chin%`
  if (!keep) `%ffilter%` <- Negate(`%chin%`)

  env <- environment()

  # 'trips', 'stop_times' and 'frequencies' can be filtered using 'trip_id'
  # itself, so `%ffilter%` is used. the other files depend on relational
  # associations with 'trip_id' that come from these 3 files.

  # 'trips' (trip_id)

  if (gtfsio::check_fields_exist(gtfs, "trips", "trip_id")) {

    gtfsio::assert_fields_types(gtfs, "trips", "trip_id", "character")
    gtfs$trips <- gtfs$trips[trip_id %ffilter% get("trip_id", envir = env)]

    # 'trips' allows us to filter by 'route_id', 'service_id' and 'shape_id'

    relevant_routes <- unique(gtfs$trips$route_id)
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

    # 'routes' and 'fare_rules' (route_id)
    # note that following the 'routes' route we can filter agency via routes ->
    # agency_id, but following 'fare_rules' route we can filter agency via
    # fare_rules -> fare_id -> fare_attributes -> agency_id
    # so we create a 'relevant_agencies' vector that holds the relevant
    # agency_ids from both options and use all of them to filter agency later

    relevant_agencies <- vector("character", length = 0L)

    if (gtfsio::check_fields_exist(gtfs, "routes", "route_id")) {

      gtfsio::assert_fields_types(gtfs, "routes", "route_id", "character")
      gtfs$routes <- gtfs$routes[route_id %chin% relevant_routes]

      # 'routes' allows us to filter by 'agency_id'. but 'agency_id' is
      # conditionally required, which means that if it doesn't exist in 'routes'
      # it might be because there's only one agency.
      # so we assume that if 'relevant_agencies_routes' happens to be NULL we
      # should keep the agency file untouched

      relevant_agencies_routes <- unique(gtfs$routes$agency_id)

      if (is.null(relevant_agencies_routes))
        relevant_agencies_routes <- unique(gtfs$agency$agency_id)

      relevant_agencies <- c(relevant_agencies, relevant_agencies_routes)

    }

    if (gtfsio::check_fields_exist(gtfs, "fare_rules", "route_id")) {

      gtfsio::assert_fields_types(gtfs, "fare_rules", "route_id", "character")
      gtfs$fare_rules <- gtfs$fare_rules[route_id %chin% relevant_routes]

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

        # 'fare_attributes' allows us to filter by 'agency_id'. again,
        # 'agency_id' is conditionally required, so we assume that we should
        # keep all agency_ids if 'relevant_agencies_fare_att' happens to be NULL

        relevant_agencies_fare_att <- unique(gtfs$fare_attributes$agency_id)

        if (is.null(relevant_agencies_fare_att))
          relevant_agencies_fare_att <- unique(gtfs$agency$agency_id)

        relevant_agencies <- c(relevant_agencies, relevant_agencies_fare_att)

      }

    }

    # 'agency' (agency_id, that comes both from routes and fare_attributes)

    if (gtfsio::check_fields_exist(gtfs, "agency", "agency_id") &
        length(relevant_agencies) > 0) {

      # keeping only unique agency_ids from relevant_agencies, since it may come
      # from two differente sources
      relevant_agencies <- unique(relevant_agencies)

      gtfsio::assert_fields_types(gtfs, "agency", "agency_id", "character")
      gtfs$agency <- gtfs$agency[agency_id %chin% relevant_agencies]

    }

  }

  # 'stop_times' (trip_id)

  if (gtfsio::check_fields_exist(gtfs, "stop_times", "trip_id")) {

    gtfsio::assert_fields_types(gtfs, "stop_times", "trip_id", "character")
    gtfs$stop_times <- gtfs$stop_times[
      trip_id %ffilter% get("trip_id", envir = env)
    ]

    # 'stop_times' allows us to filter by 'stop_id'. it's important to keep,
    # however, not only the stops that appear on stop_times, but also their
    # parent stops, that may not be listed on such file

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

    }

  }

  # 'frequencies' (trip_id)

  if (gtfsio::check_fields_exist(gtfs, "frequencies", "trip_id")) {

    gtfsio::assert_fields_types(gtfs, "frequencies", "trip_id", "character")
    gtfs$frequencies <- gtfs$frequencies[
      trip_id %ffilter% get("trip_id", envir = env)
    ]

  }

  return(gtfs)

}
