#' Return trip geometry
#'
#' Returns the geometry of each specified \code{trip_id}, based either on the
#' \code{shapes} or the \code{stop_times} file (or both).
#'
#' @param gtfs A GTFS file as read by \code{tidytransit::read_gtfs()}.
#' @param trip_id A string vector including the \code{trip_id}s to have their
#'   geometries generated. If \code{NULL} (the default) the function generates
#'   geometries for every \code{trip_id} in the GTFS.
#' @param file The file from which geometries should be generated. By default
#'   uses both \code{shapes} and \code{stop_times}.
#' @param crs The CRS of the resulting object. Defaults to 4326 (WGS 84).
#'
#' @return A \code{LINESTRING sf}.
#'
#' @export

trip_geometry <- function(gtfs,
                          trip_id = NULL,
                          file = c("shapes", "stop_times"),
                          crs = 4326) {

  # create linestrings from shapes

  if ("shapes" %in% file) {

    if (! is.null(trip_id)) {

      env   <- environment()
      trips <- dplyr::filter(gtfs$trips, trip_id %in% get("trip_id", envir = env))

    } else {

      trips <- gtfs$trips

    }

    relevant_shapes <- unique(trips$shape_id)

    shapes_sf <- gtfs$shapes %>%
      dplyr::filter(shape_id %in% relevant_shapes) %>%
      dplyr::arrange(shape_id, shape_pt_sequence) %>%
      sfheaders::sf_linestring(
        x = "shape_pt_lon",
        y = "shape_pt_lat",
        linestring_id = "shape_id"
      ) %>%
      sf::st_set_crs(4326) %>%
      dplyr::right_join(trips, by = "shape_id") %>%
      dplyr::select(trip_id, geometry) %>%
      dplyr::mutate(origin_file = "shapes")

  }

  # create linestrings from stop_times

  if ("stop_times" %in% file) {

    if (! is.null(trip_id)) {

      relevant_trips <- trip_id

    } else {

      relevant_trips <- unique(gtfs$trips$trip_id)

    }

    stop_times_sf <- gtfs$stop_times %>%
      dplyr::filter(trip_id %in% relevant_trips) %>%
      dplyr::arrange(trip_id, stop_sequence) %>%
      dplyr::left_join(gtfs$stops, by = "stop_id") %>%
      sfheaders::sf_linestring(
        x = "stop_lon",
        y = "stop_lat",
        linestring_id = "trip_id"
      ) %>%
      sf::st_set_crs(4326) %>%
      dplyr::mutate(origin_file = "stop_times")
  }

  # tidy final object

  if (length(file) == 2) {

    final_sf <- dplyr::bind_rows(shapes_sf, stop_times_sf)

  } else {

    final_sf <- get(paste0(file, "_sf"))

  }

  final_sf <- dplyr::select(final_sf, trip_id, origin_file, geometry)

  if (crs != 4326) final_sf <- sf::st_transform(final_sf, crs)

  return(final_sf)

}
