#' Return the length of segments between stops
#'
#' Returns the length of segments between stops of each specified \code{trip_id},
#' based either on the \code{shapes} (not yet supported) or the
#' \code{stop_times}.
#'
#' @param gtfs A GTFS file as read by \code{tidytransit::read_gtfs()}.
#' @param trip_id A string vector including the \code{trip_id}s to have their
#'   segments' length calculated. If \code{NULL} (the default) the function
#'   calculates the segments' length for every \code{trip_id} in the GTFS.
#' @param file The file from which geometries should be generated. By now
#'   supports only \code{stop_times}.
#' @param drop_geometry Whether the function should drop segment's geometry
#'   (defaults to \code{TRUE}).
#' @param crs The CRS of the resulting object. Defaults to 4326 (WGS 84).
#'
#' @return A table containing the segments of each specified trip, their length
#'   (in meters) and their geometry, if desired.
#'
#' @export
stop_to_stop_length <- function(gtfs,
                                trip_id = NULL,
                                file = "stop_times",
                                drop_geometry = TRUE,
                                crs = 4326) {

  # calculate the geometry of segment between stops

  segments <- get_trip_geometry(gtfs, trip_id = trip_id, file, crs) %>%
    nngeo::st_segments(progress = FALSE) %>%
    dplyr::group_by(trip_id) %>%
    dplyr::mutate(segment = 1:dplyr::n()) %>%
    dplyr::ungroup()

  # calculate the length and bind to result

  segment_length <- as.numeric(sf::st_length(segments))

  segments <- dplyr::mutate(segments, length = segment_length)

  # either drop the geometry of tidy the result

  if (drop_geometry) {

    segments <- sf::st_drop_geometry(segments)

  } else {

    segments <- segments %>%
      dplyr::rename(geometry = result) %>%
      dplyr::relocate(geometry, .after = dplyr::last_col())

  }

  return(segments)

}
