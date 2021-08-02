#' Convert `stops` table to simple feature object
#'
#' Converts the `stops` table to a `POINT sf` object.
#'
#' @param gtfs A GTFS object.
#' @param stop_id A character vector including the `stop_id`s to be converted.
#'   If `NULL` (the default), all stops are converted.
#' @param crs The CRS of the resulting object, either as an EPSG code or as an
#'   `crs` object. Defaults to 4326 (WGS 84).
#'
#' @return A `POINT sf` object.
#'
#' @examples
#' # read gtfs
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' stops_sf <- convert_stops_to_sf(gtfs)
#' head(stops_sf)
#'
#' stops_sf <- convert_stops_to_sf(gtfs, stop_id = "18848")
#' stops_sf
#'
#' @export
convert_stops_to_sf <- function(gtfs, stop_id = NULL, crs = 4326) {

  # input checking

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_character(stop_id, null.ok = TRUE)
  checkmate::assert(
    checkmate::check_numeric(crs),
    checkmate::check_class(crs, "crs"),
    combine = "or"
  )

  gtfsio::assert_fields_types(
    gtfs,
    "stops",
    c("stop_id", "stop_lon", "stop_lat"),
    c("character", "numeric", "numeric")
  )

  # select relevant stop_ids

  if (!is.null(stop_id))
    relevant_stops <- stop_id
  else
    relevant_stops <- unique(gtfs$stops$stop_id)

  # raise warning/error if given 'stop_id's don't exist in 'stops'

  if (!is.null(stop_id)) {

    invalid_stop_id <- stop_id[! stop_id %chin% unique(gtfs$stops$stop_id)]

    if (identical(invalid_stop_id, stop_id))
      stop("'stops' doesn't contain any of the ids passed to 'stop_id'.")

    if (!identical(invalid_stop_id, character(0)))
      warning(
        paste0(
          "'stops' doesn't contain the following stop_id(s): "),
        paste0("'", invalid_stop_id, "'", collapse = ", ")
      )

  }

  # filter 'stops' table and create sf from it

  stops_sf <- gtfs$stops[stop_id %chin% relevant_stops]
  stops_sf <- sfheaders::sf_point(
    stops_sf,
    x = "stop_lon",
    y = "stop_lat",
    keep = TRUE
  )
  stops_sf <- sf::st_set_crs(stops_sf, 4326)

  # transform crs from 4326 to the one passed to 'crs'

  if (crs != 4326 && crs != sf::st_crs(4326))
    stops_sf <- sf::st_transform(stops_sf, crs)

  return(stops_sf)

}
