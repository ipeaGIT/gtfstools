#' @title Convert GTFS stops to simple feature object
#'
#' @description
#' Returns the `stops.txt` data from a GTFS feed as a \code{POINT sf} object.
#'
#' @param gtfs A GTFS object as created by \code{\link{read_gtfs}}.
#' @param crs The coordinate reference system (CRS) represented as an EPSG code.
#'            Defaults to 4326 (latlong WGS84).
#'
#' @return A \code{POINT sf} object.
#'
#' @examples
#' # read gtfs
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' stops_sf <- convert_stops_to_sf(gtfs)
#' head(stops_sf)
#'
#'
#' @export
convert_stops_to_sf <- function(gtfs, crs = 4326){

  ## input checking
  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_data_table(gtfs$stops, .var.name= 'gtfs$stops')
  checkmate::assert(
    checkmate::check_numeric(crs),
    checkmate::check_class(crs, "crs"),
    combine = "or"
  )

  ## fun
  temp_stops_sf <- sfheaders::sf_point(gtfs$stops, x = "stop_lon", y = "stop_lat", keep = TRUE)
  sf::st_crs(temp_stops_sf) <- crs

  return(temp_stops_sf)
}
