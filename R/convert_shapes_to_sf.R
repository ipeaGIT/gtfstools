#' Convert `shapes` table to simple feature object
#'
#' @description
#' Converts the `shapes` table to a `LINESTRING sf` object.
#'
#' @param gtfs A GTFS object.
#' @param shape_id A character vector including the `shape_id`s to be converted.
#'   If `NULL` (the default), all shapes are converted.
#' @param crs The CRS of the resulting object, either as an EPSG code or as an
#'   `crs` object. Defaults to 4326 (WGS 84).
#'
#' @return A `LINESTRING sf` object.
#'
#' @examples
#' # read gtfs
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' shapes_sf <- convert_shapes_to_sf(gtfs)
#' head(shapes_sf)
#'
#' shapes_sf <- convert_shapes_to_sf(gtfs, shape_id = "17846")
#' shapes_sf
#'
#' @export
convert_shapes_to_sf <- function(gtfs, shape_id = NULL, crs = 4326) {

  # input checking

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_character(shape_id, null.ok = TRUE)
  checkmate::assert(
    checkmate::check_numeric(crs),
    checkmate::check_class(crs, "crs"),
    combine = "or"
  )

  gtfsio::assert_fields_types(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence"),
    c("character", "numeric", "numeric", "integer")
  )

  # select relevant shape_ids

  if (!is.null(shape_id))
    relevant_shapes <- shape_id
  else
    relevant_shapes <- unique(gtfs$shapes$shape_id)

  # raise warning/error if given 'shape_id's don't exist in 'shapes'

  if (!is.null(shape_id)) {

    invalid_shape_id <- shape_id[! shape_id %chin% unique(gtfs$shapes$shape_id)]

    if (identical(invalid_shape_id, shape_id))
      stop("'shapes' doesn't contain any of the ids passed to 'shape_id'.")

    if (!identical(invalid_shape_id, character(0)))
      warning(
        paste0(
          "'shapes' doesn't contain the following shape_id(s): "),
        paste0("'", invalid_shape_id, "'", collapse = ", ")
      )

  }

  # filter 'shapes' table, sort it by 'shape_pt_sequence' and 'shape_id' and
  # create sf from it

  shapes_sf <- gtfs$shapes[shape_id %chin% relevant_shapes]
  shapes_sf <- shapes_sf[order(shape_id, shape_pt_sequence)]
  shapes_sf <- sfheaders::sf_linestring(
    shapes_sf,
    x = "shape_pt_lon",
    y = "shape_pt_lat",
    linestring_id = "shape_id"
  )
  shapes_sf <- sf::st_set_crs(shapes_sf, 4326)

  # transform crs from 4326 to the one passed to 'crs'

  if (crs != 4326 && crs != sf::st_crs(4326))
    shapes_sf <- sf::st_transform(shapes_sf, crs)

  return(shapes_sf)

}
