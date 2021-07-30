#' @title Convert GTFS shapes to simple feature object
#'
#' @description
#' Returns the `shapes.txt` data from a GTFS feed as a \code{LINESTRING sf} object.
#'
#' @param gtfs A GTFS object as created by \code{\link{read_gtfs}}.
#' @param crs The coordinate reference system (CRS) represented as an EPSG code.
#'            Defaults to 4326 (latlong WGS84).
#'
#' @return A \code{LINESTRING sf} object.
#'
#' @examples
#' # read gtfs
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' stops_sf <- convert_shapes_to_sf(gtfs)
#' head(stops_sf)
#'
#' @export
convert_shapes_to_sf <- function(gtfs, crs = 4326){

  ## input checking
  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_data_table(gtfs$shapes, .var.name= 'gtfs$shapes')
  checkmate::assert_data_table(gtfs$shapes, .var.name= 'gtfs$shapes$shape_pt_sequence')
  checkmate::assert(
    checkmate::check_numeric(crs),
    checkmate::check_class(crs, "crs"),
    combine = "or"
  )

  ## FUN

  # sort data
  temp_shapes <- data.table::setDT(gtfs$shapes)[order(shape_id, shape_pt_sequence)]

  # convert to sf
  temp_shapes <- sfheaders::sf_linestring(temp_shapes, x = "shape_pt_lon" , y = "shape_pt_lat", linestring_id = "shape_id")

  # add projection
  sf::st_crs(temp_shapes) <- crs

  # calculate distances
  data.table::setDT(temp_shapes)[, length := sf::st_length(geometry)]
  data.table::setDT(temp_shapes)[, length := units::set_units(length, "km") ]

  # back to sf
  temp_shapes <- sf::st_sf(temp_shapes)
  return(temp_shapes)
}
