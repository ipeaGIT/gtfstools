#' Filter a GTFS object using a `simple features` object
#'
#' Filters a GTFS object using the geometry of an `sf` object, keeping (or
#' dropping) entries related to shapes and trips selected through a spatial
#' operation.
#'
#' @template gtfs
#' @param geom An `sf` object. Describes the geometry used to filter the data.
#' @param spatial_operation A spatial operation function from the set of
#'   options listed in [geos_binary_pred][sf::geos_binary_pred] (check the
#'   [DE-I9M](https://en.wikipedia.org/wiki/DE-9IM) Wikipedia entry for the
#'   definition of each function). Defaults to `sf::st_intersects`, which tests
#'   if the shapes and trips have ANY intersection with the object specified in
#'   `geom`. Please note that `geom` is passed as the `x` argument of these
#'   functions.
#' @param keep A logical. Whether the entries related to the shapes and trips
#'   that cross through the given geometry should be kept or dropped (defaults
#'   to `TRUE`, which keeps the entries).
#'
#' @return The GTFS object passed to the `gtfs` parameter, after the filtering
#' process.
#'
#' @family filtering functions
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' shape_id <- "68962"
#' shape_sf <- convert_shapes_to_sf(gtfs, shape_id)
#' bbox <- sf::st_bbox(shape_sf)
#' object.size(gtfs)
#'
#' # keeps entries that intersect with the specified polygon
#' smaller_gtfs <- filter_by_sf(gtfs, bbox)
#' object.size(smaller_gtfs)
#'
#' # drops entries that intersect with the specified polygon
#' smaller_gtfs <- filter_by_sf(gtfs, bbox, keep = FALSE)
#' object.size(smaller_gtfs)
#'
#' # uses a different function to filter the gtfs
#' smaller_gtfs <- filter_by_sf(gtfs, bbox, spatial_operation = sf::st_contains)
#' object.size(smaller_gtfs)
#'
#' @export
filter_by_sf <- function(gtfs,
                         geom,
                         spatial_operation = sf::st_intersects,
                         keep = TRUE) {

  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_logical(keep, len = 1)
  checkmate::assert(
    checkmate::check_class(geom, "sf"),
    checkmate::check_class(geom, "sfc"),
    checkmate::check_class(geom, "bbox")
  )
  assert_spatial_operation(spatial_operation)

  # convert 'geom' to polygon if a bounding box was given

  if (inherits(geom, "bbox")) geom <- sf::st_buffer(sf::st_as_sfc(geom), 0)

  # raise an error if 'geom' crs is not 4326, and merge features if geom more
  # than one feature

  if (sf::st_crs(geom) != sf::st_crs(4326))
    stop("'geom' CRS must be WGS 84 (EPSG 4326).")

  if (
    (inherits(geom, "sf") && nrow(geom) > 1) ||
      (inherits(geom, "sfc") && length(geom) > 1)
  ) {
    geom <- sf::st_union(geom)
  }

  # actual filtering

  gtfs_list <- vector("list", 2L)

  if (gtfsio::check_file_exists(gtfs, "shapes")) {

    shapes_sf <- convert_shapes_to_sf(gtfs)
    did_succeed_operation <- spatial_operation(geom, shapes_sf, sparse = FALSE)

    shapes_sf <- shapes_sf[did_succeed_operation, ]

    shapes_gtfs <- filter_by_shape_id(gtfs, shapes_sf$shape_id, keep)
    gtfs_list[[1]] <- shapes_gtfs

  }

  if (gtfsio::check_file_exists(gtfs, "stop_times")) {

    trips_sf <- get_trip_geometry(gtfs, file = "stop_times")
    did_succeed_operation <- spatial_operation(geom, trips_sf, sparse = FALSE)

    trips_sf <- trips_sf[did_succeed_operation, ]

    trips_gtfs <- filter_by_trip_id(gtfs, trips_sf$trip_id, keep)
    gtfs_list[[2]] <- trips_gtfs

  }

  # remove NULL elements from 'gtfs_list' and raise error if it's empty (happens
  # when neither 'shapes' nor 'stop_times' tables are present in gtfs)

  gtfs_list <- Filter(Negate(is.null), gtfs_list)

  if (length(gtfs_list) == 0)
    stop(
      "Could not conduct spatial operations with the provided GTFS object. ",
      "It must contain either a 'shapes' or a 'stop_times' table."
    )

  # merge the gtfs objects

  result_gtfs <- merge_gtfs(gtfs_list)
  result_gtfs <- remove_duplicates(result_gtfs)

  return(result_gtfs)

}

assert_spatial_operation <- function(spatial_operation) {
  available_operations <- list(
    intersects = sf::st_intersects,
    disjoint = sf::st_disjoint,
    touches = sf::st_touches,
    crosses = sf::st_crosses,
    within = sf::st_within,
    contains = sf::st_contains,
    contains_properly = sf::st_contains_properly,
    overlaps = sf::st_overlaps,
    equals = sf::st_equals,
    covers = sf::st_covers,
    covered_by = sf::st_covered_by,
    equals_exact = sf::st_equals_exact,
    is_within_distance = sf::st_is_within_distance
  )

  checkmate::assert_class(spatial_operation, "function")

  operation_matches <- vapply(
    available_operations,
    FUN.VALUE = logical(1),
    FUN = function(op) identical(op, spatial_operation)
  )

  if (!any(operation_matches)) {
    function_listing <- paste0(
      "'sf::st_", names(available_operations), "'",
      collapse = ", "
    )

    stop(
      "Assertion on 'spatial_operation' failed: ",
      "Must be a geometric binary predicate listed in '?sf::geos_binary_pred' ",
      "- i.e. one of ", function_listing, "."
    )
  }

  return(invisible(TRUE))
}
