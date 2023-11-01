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

  `%ffilter%` <- `%chin%`
  if (!keep) `%ffilter%` <- Negate(`%chin%`)

  # 'agency', 'routes', 'fare_attributes' and 'attributions' can be filtered
  # using 'agency_id' itself, so `%ffilter%` is used. the other files depend on
  # relational associations with 'agency_id' that come from these 4 tables, so
  # we use %chin%

  # 'agency', 'attributions', 'fare_attributes' and 'routes' (agency_id)
  #
  # 'fare_attributes' could be used to filter 'fare_rules' via fare_id. but
  # that introduces some complexity, because agency_id is an optional field
  # in both 'fare_attributes' and 'routes' if 'agency' contains only one
  # agency, and 'routes' can also be used to filter 'fare_rules' (via
  # route_id). i'm opting to filter 'fare_rules' via the 'routes' path only,
  # afterall a fare only makes sense if it's associated to a route, but i can
  # change this behaviour later if needed

  gtfs <- filter_agency_from_agency_id(gtfs, agency_id, `%ffilter%`)
  gtfs <- filter_attributions_from_agency_id(gtfs, agency_id, `%ffilter%`)
  gtfs <- filter_fare_attr_from_agency_id(gtfs, agency_id, `%ffilter%`)
  gtfs <- filter_routes_from_agency_id(gtfs, agency_id, `%ffilter%`)

  # 'routes' allows us to filter by 'route_id'

  relevant_routes <- unique(gtfs$routes$route_id)

  # 'fare_rules' and 'trips' (route_id)

  gtfs <- filter_fare_rules_from_route_id(gtfs, relevant_routes, `%chin%`)
  gtfs <- filter_trips_from_route_id(gtfs, relevant_routes, `%chin%`)

  # 'trips' allows us to filter by 'trip_id', 'service_id' and 'shape_id'

  relevant_services <- unique(gtfs$trips$service_id)
  relevant_shapes <- unique(gtfs$trips$shape_id)
  relevant_trips <- unique(gtfs$trips$trip_id)

  # 'shapes' (shape_id)

  gtfs <- filter_shapes_from_shape_id(gtfs, relevant_shapes, `%chin%`)

  # 'calendar' and 'calendar_dates' (service_id)

  gtfs <- filter_calendar_from_service_id(gtfs, relevant_services, `%chin%`)
  gtfs <- filter_calend_dates_from_service_id(gtfs, relevant_services, `%chin%`)

  # 'stop_times' and 'frequencies' (trip_id)

  gtfs <- filter_frequencies_from_trip_id(gtfs, relevant_trips, `%chin%`)
  gtfs <- filter_stop_times_from_trip_id(gtfs, relevant_trips, `%chin%`)

  # 'stop_times' allows us to filter by 'stop_id'
  # it's important to keep not only the stops that appear on stop_times, but
  # also their parent stops
  # TODO: also include their children

  relevant_stops <- get_stops_and_parents(gtfs)

  # 'stops', 'transfers' and 'pathways' (stop_id)

  gtfs <- filter_transfers_from_stop_id(gtfs, relevant_stops, `%chin%`)
  gtfs <- filter_pathways_from_stop_id(gtfs, relevant_stops, `%chin%`)
  gtfs <- filter_stops_from_stop_id(gtfs, relevant_stops, `%chin%`)

  # 'stops' allows us to filter by 'level_id'

  relevant_levels <- unique(gtfs$stops$level_id)

  # 'levels' (level_id)

  gtfs <- filter_levels_from_level_id(gtfs, relevant_levels, `%chin%`)

  return(gtfs)
}
