#' Get trip geometry
#'
#' Returns the geometry of each specified \code{trip_id}, based either on the
#' \code{shapes} or the \code{stop_times} file (or both).
#'
#' @param gtfs A GTFS object as created by \code{\link{read_gtfs}}.
#' @param trip_id A string vector including the \code{trip_id}s to have their
#'   geometries generated. If \code{NULL} (the default), the function generates
#'   geometries for every \code{trip_id} in the GTFS.
#' @param file The file from which geometries should be generated. By default
#'   uses both \code{shapes} and \code{stop_times}.
#' @param crs The CRS of the resulting object. Defaults to 4326 (WGS 84).
#'
#' @return A \code{LINESTRING sf}.
#'
#' @section Details:
#' The geometry generation works differently for the two files. In the case of
#' \code{shapes}, the shape as described in the text file is converted to an
#' \code{sf} object. For \code{stop_times} the geometry is the result of linking
#' subsequent stops along a straight line (stops' coordinates are retrieved from
#' the \code{stops} file). Thus, the resolution of the geometry when generated
#' with \code{shapes} tends to be much higher than when created with
#' \code{stop_times}.
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#'
#' gtfs <- read_gtfs(data_path)
#'
#' trip_geometry <- get_trip_geometry(gtfs)
#' head(trip_geometry)
#'
#' trip_ids <- c("CPTM L07-0", "2002-10-0")
#' trip_geometry <- get_trip_geometry(gtfs, trip_id = trip_ids)
#' trip_geometry
#' plot(trip_geometry["origin_file"])
#'
#' @export
get_trip_geometry <- function(gtfs,
                              trip_id = NULL,
                              file = c("shapes", "stop_times"),
                              crs = 4326) {

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_character(trip_id, null.ok = TRUE)
  checkmate::assert_names(file, subset.of = c("shapes", "stop_times"))
  checkmate::assert_numeric(crs)

  # create linestrings from shapes

  if ("shapes" %in% file) {

    # check if required fields and files exist

    checkmate::assert(
      check_gtfs_field_exists(gtfs, "trips", c("trip_id", "shape_id")),
      check_gtfs_field_exists(
        gtfs,
        "shapes",
        c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence")
      ),
      combine = "and"
    )

    if (!is.null(trip_id)) {

      env   <- environment()
      trips <- gtfs$trips[trip_id %chin% get("trip_id", envir = env)]

    } else {

      trips <- gtfs$trips

    }

    relevant_shapes <- unique(trips$shape_id)

    # generate geometry

    shapes_sf <- gtfs$shapes[shape_id %chin% relevant_shapes]
    shapes_sf <- shapes_sf[order(shape_id, shape_pt_sequence)]
    shapes_sf <- sfheaders::sf_linestring(
      shapes_sf,
      x = "shape_pt_lon",
      y = "shape_pt_lat",
      linestring_id = "shape_id"
    )
    shapes_sf <- sf::st_set_crs(shapes_sf, crs)
    shapes_sf <- data.table::setDT(shapes_sf)[trips, on = "shape_id"]
    shapes_sf[, origin_file := "shapes"]
    shapes_sf <- shapes_sf[, .(trip_id, origin_file, geometry)]

  }

  # create linestrings from stop_times

  if ("stop_times" %in% file) {

    # check if required fields and files exist

    checkmate::assert(
      check_gtfs_field_exists(gtfs, "trips", "trip_id"),
      check_gtfs_field_exists(
        gtfs, "stop_times", c("trip_id", "stop_id", "stop_sequence")
      ),
      check_gtfs_field_exists(
        gtfs, "stops", c("stop_id", "stop_lat", "stop_lon")
      ),
      combine = "and"
    )

    if (!is.null(trip_id)) {
      relevant_trips <- trip_id
    } else {
      relevant_trips <- unique(gtfs$trips$trip_id)
    }

    # generate geometry

    stop_times_sf <- gtfs$stop_times[trip_id %chin% relevant_trips]
    stop_times_sf <- stop_times_sf[order(trip_id, stop_sequence)]
    stop_times_sf[gtfs$stops, on = "stop_id", `:=`(stop_lat = i.stop_lat, stop_lon = i.stop_lon)]
    stop_times_sf <- sfheaders::sf_linestring(
      stop_times_sf,
      x = "stop_lon",
      y = "stop_lat",
      linestring_id = "trip_id"
    )
    stop_times_sf <- data.table::setDT(sf::st_set_crs(stop_times_sf, crs))
    stop_times_sf[, origin_file := "stop_times"]

  }

  # tidy final object

  if (length(file) == 2) {
    final_sf <- rbind(shapes_sf, stop_times_sf)
  } else {
    final_sf <- get(paste0(file, "_sf"))
  }

  final_sf <- final_sf[, .(trip_id, origin_file, geometry)]
  final_sf <- sf::st_as_sf(final_sf)

  if (crs != 4326) final_sf <- sf::st_transform(final_sf, crs)

  return(final_sf)

}
