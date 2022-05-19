#' Convert `shapes` table to simple feature object
#'
#' Converts the `shapes` table to a `LINESTRING sf` object.
#'
#' @template gtfs
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
  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_character(shape_id, null.ok = TRUE)
  checkmate::assert(
    checkmate::check_numeric(crs),
    checkmate::check_class(crs, "crs"),
    combine = "or"
  )

  gtfsio::assert_field_class(
    gtfs,
    "shapes",
    c("shape_id", "shape_pt_lat", "shape_pt_lon"),
    c("character", "numeric", "numeric")
  )

  # select relevant shape_ids and  raise warning/error if given shape_ids don't
  # exist in shapes

  if (!is.null(shape_id)) {
    relevant_shapes <- shape_id

    invalid_shape_id <- shape_id[! shape_id %chin% unique(gtfs$shapes$shape_id)]

    if (!identical(invalid_shape_id, character(0))) {
      warning(
        paste0(
          "'shapes' doesn't contain the following shape_id(s): "),
        paste0("'", invalid_shape_id, "'", collapse = ", ")
      )
    }

    shapes <- gtfs$shapes[shape_id %chin% relevant_shapes]
  } else {
    shapes <- gtfs$shapes
  }

  # create an empty LINESTRING sf if shapes is empty

  if (nrow(shapes) == 0) {
    empty_linestring <- sf::st_sfc()
    class(empty_linestring)[1] <- "sfc_LINESTRING"

    shapes_sf <- sf::st_sf(
      shape_id = character(),
      geometry = empty_linestring,
      stringsAsFactors = FALSE
    )
  } else {
    shapes_sf <- sfheaders::sf_linestring(
      shapes,
      x = "shape_pt_lon",
      y = "shape_pt_lat",
      linestring_id = "shape_id"
    )
  }

  shapes_sf <- sf::st_set_crs(shapes_sf, 4326)

  # transform crs from 4326 to the one passed to 'crs'

  if (crs != 4326 && crs != sf::st_crs(4326)) {
    shapes_sf <- sf::st_transform(shapes_sf, crs)
  }

  return(shapes_sf)

}
