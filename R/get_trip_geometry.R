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
  checkmate::assert(
    checkmate::check_numeric(crs),
    checkmate::check_class(crs, "crs"),
    combine = "or"
  )

  # check if required fields and files exist

  if ("shapes" %in% file) {

    checkmate::assert(
      check_gtfs_field_exists(gtfs, "trips", c("trip_id", "shape_id")),
      check_gtfs_field_exists(
        gtfs,
        "shapes",
        c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence")
      ),
      combine = "and"
    )

  }

  if ("stop_times" %in% file) {

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

  }

  # select trip_ids to get geometry of

  if (!is.null(trip_id)) {
    relevant_trips <- trip_id
  } else {
    relevant_trips <- unique(gtfs$trips$trip_id)
  }

  # raise warning if a given 'trip_id' doesn't exist in 'trips'

  if (!is.null(trip_id)) {

    invalid_trip_id <- trip_id[! trip_id %chin% unique(gtfs$trips$trip_id)]

    if (!identical(invalid_trip_id, character(0))) {

      warning(
        paste0(
          "'trips' doesn't contain the following trip_id(s): "),
        paste0("'", invalid_trip_id, "'", collapse = ", ")
      )

    }

  }

  # create linestrings from shapes

  if ("shapes" %in% file) {

    # select shape_ids to get geometry of

    trips <- gtfs$trips[trip_id %chin% relevant_trips & shape_id != ""]

    relevant_shapes <- unique(trips$shape_id)

    # generate geometry; the condition for nrow == 0 prevents an sfheaders error

    shapes_sf <- gtfs$shapes[shape_id %chin% relevant_shapes]
    shapes_sf <- shapes_sf[order(shape_id, shape_pt_sequence)]

    if (nrow(shapes_sf) == 0) {

      empty_linestring <- sf::st_sfc()
      class(empty_linestring)[1] <- "sfc_LINESTRING"

      shapes_sf <- sf::st_sf(
        shape_id = character(),
        geometry = empty_linestring,
        stringsAsFactors = FALSE
      )

    } else {

      shapes_sf <- sfheaders::sf_linestring(
        shapes_sf,
        x = "shape_pt_lon",
        y = "shape_pt_lat",
        linestring_id = "shape_id"
      )

    }

    shapes_sf <- sf::st_set_crs(shapes_sf, 4326)
    shapes_sf <- data.table::setDT(shapes_sf)[trips, on = "shape_id"]
    shapes_sf[, origin_file := "shapes"]
    shapes_sf <- shapes_sf[, .(trip_id, origin_file, geometry)]

  }

  # create linestrings from stop_times

  if ("stop_times" %in% file) {

    # generate geometry; the condition for nrow == 0 prevents an sfheaders error

    stop_times_sf <- gtfs$stop_times[trip_id %chin% relevant_trips]
    stop_times_sf <- stop_times_sf[order(trip_id, stop_sequence)]
    stop_times_sf[
      gtfs$stops,
      on = "stop_id",
      `:=`(stop_lat = i.stop_lat, stop_lon = i.stop_lon)
    ]

    if (nrow(stop_times_sf) == 0) {

      empty_linestring <- sf::st_sfc()
      class(empty_linestring)[1] <- "sfc_LINESTRING"

      stop_times_sf <- sf::st_sf(
        trip_id = character(),
        geometry = empty_linestring,
        stringsAsFactors = FALSE
      )

    } else {

      stop_times_sf <- sfheaders::sf_linestring(
        stop_times_sf,
        x = "stop_lon",
        y = "stop_lat",
        linestring_id = "trip_id"
      )

    }

    stop_times_sf <- sf::st_set_crs(stop_times_sf, 4326)
    data.table::setDT(stop_times_sf)
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

  # transform crs from 4326 to the one passed to 'crs'

  if (crs != 4326 && crs != sf::st_crs(4326))
    final_sf <- sf::st_transform(final_sf, crs)

  return(final_sf)

}
