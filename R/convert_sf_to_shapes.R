convert_sf_to_shapes <- function(sf_shapes, calculate_distance = FALSE) {
  # TODO: check inputs
  # TODO: check what happens if sf_shapes has nrow = 0

  shapes_points <- sfheaders::sf_cast(sf_shapes, "POINT")

  if (calculate_distance) {
    shapes_points <- calculate_shape_dist_traveled(shapes_points)
  }

  shapes <- sfheaders::sf_to_df(shapes_points, fill = TRUE)
  data.table::setDT(shapes)

  shapes[, c("sfg_id", "point_id") := NULL]
  data.table::setnames(
    shapes,
    old = c("x", "y"),
    new = c("shape_pt_lon", "shape_pt_lat")
  )

  shapes[, shape_pt_sequence := seq_len(.N), by = shape_id]

  return(shapes[])
}

calculate_shape_dist_traveled <- function(shapes_points) {
    # TODO: check crs, if empty throw error

    empty_point <- sf::st_as_sfc(
      "POINT(EMPTY)",
      crs = sf::st_crs(shapes_points)
    )

    lagged_geometry <- append(empty_point, shapes_points$geometry)
    lagged_geometry <- lagged_geometry[-length(lagged_geometry)]

    distance_to_prev_point <- sf::st_distance(
      shapes_points$geometry,
      lagged_geometry,
      by_element = TRUE
    )

    data.table::setDT(shapes_points)
    shapes_points[, dist_to_prev_point := distance_to_prev_point]
    shapes_points[
      shapes_points[, .I[1], by = shape_id]$V1,
      dist_to_prev_point := 0
    ]
    shapes_points[
      ,
      shape_dist_traveled := cumsum(dist_to_prev_point),
      by = shape_id
    ]
    shapes_points[, shape_dist_traveled := as.numeric(shape_dist_traveled)]
    shapes_points[, dist_to_prev_point := NULL]

    shapes_points <- sf::st_sf(shapes_points)

}
