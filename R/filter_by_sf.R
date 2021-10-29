#' Filter a GTFS object using a `simple features` object
#'
#' Filters a GTFS object using the geometry of an `sf` object, keeping (or
#' dropping) entries related to shapes and trips selected through an spatial
#' operation.
#'
#' @param gtfs A GTFS object.
#' @param geom An `sf` object. Describes the geometry used to filter the data.
#' @param spatial_operation An spatial operation function from the set of
#'   options listed in [geos_binary_pred][sf::geos_binary_pred]. Defaults to
#'   `sf::st_intersects`, which tests if the shapes and trips have ANY
#'   intersection with the object specified in `geom`.
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
#' polygon <- sf::st_polygon(
#'   list(rbind(
#'     c(bbox$xmin, bbox$ymin),
#'     c(bbox$xmin, bbox$ymax),
#'     c(bbox$xmax, bbox$ymax),
#'     c(bbox$xmax, bbox$ymin),
#'     c(bbox$xmin, bbox$ymin)
#'   ))
#' )
#' polygon <- sf::st_sf(geom = sf::st_sfc(polygon), crs = 4326)
#'
#' # keeps entries that intersect with the specified polygon
#' smaller_gtfs <- filter_by_sf(gtfs, polygon)
#' object.size(smaller_gtfs)
#'
#' # drops entries that intersect with the specified polygon
#' smaller_gtfs <- filter_by_sf(gtfs, polygon, keep = FALSE)
#' object.size(smaller_gtfs)
#'
#' @export
filter_by_sf <- function(gtfs,
                         geom,
                         spatial_operation = sf::st_intersects,
                         keep = TRUE) {

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_class(geom, "sf")
  checkmate::assert_logical(keep, len = 1)

  gtfs_list <- vector("list", 2L)

  if (gtfsio::check_files_exist(gtfs, "shapes")) {

    shapes_sf <- convert_shapes_to_sf(gtfs)
    did_succeed_operation <- spatial_operation(shapes_sf, geom, sparse = FALSE)

    shapes_sf <- shapes_sf[did_succeed_operation, ]

    shapes_gtfs <- filter_by_shape_id(gtfs, shapes_sf$shape_id, keep)
    gtfs_list[[1]] <- shapes_gtfs

  }

  if (gtfsio::check_files_exist(gtfs, "stop_times")) {

    trips_sf <- get_trip_geometry(gtfs, file = "stop_times")
    did_succeed_operation <- spatial_operation(trips_sf, geom, sparse = FALSE)

    trips_sf <- trips_sf[did_succeed_operation, ]

    trips_gtfs <- filter_by_trip_id(gtfs, trips_sf$trip_id, keep)
    gtfs_list[[2]] <- trips_gtfs

  }

  # if neither 'shapes' or 'stop_times' exist the list will be empty, and an
  # error is raise

  if (length(gtfs_list) == 0)
    stop(
      "Could not conduct spatial operations with the provided GTFS object. ",
      "It must contain either a 'shapes' or a 'stop_times' table."
    )

  # remove NULL elements from 'gtfs_list' and merge the gtfs objects

  gtfs_list <- Filter(Negate(is.null), gtfs_list)

  result_gtfs <- merge_gtfs(gtfs_list)
  result_gtfs <- remove_duplicates(result_gtfs)

  return(result_gtfs)

}
