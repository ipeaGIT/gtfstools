spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
spo_gtfs <- read_gtfs(spo_path)
spo_shape <- "68962"
bbox <- sf::st_bbox(convert_shapes_to_sf(spo_gtfs, spo_shape))
polygon <- sf::st_as_sf(sf::st_buffer(sf::st_as_sfc(bbox), 0))


# tests -------------------------------------------------------------------


test_that("raises error due to incorrect input types", {
  expect_error(filter_by_sf(unclass(spo_gtfs), bbox))
  expect_error(filter_by_sf(spo_gtfs, unclass(bbox)))
  expect_error(filter_by_sf(spo_gtfs, sf::st_transform(polygon, 4674)))
  expect_error(
    filter_by_sf(spo_gtfs, bbox, spatial_operation = "sf::st_intersect")
  )
  expect_error(filter_by_sf(spo_gtfs, bbox, keep = "TRUE"))
})

test_that("results in a dt_gtfs object", {
  # a dt_gtfs object is a list with "dt_gtfs" and "gtfs" classes
  dt_gtfs_class <- c("dt_gtfs", "gtfs", "list")
  smaller_gtfs <- filter_by_sf(spo_gtfs, bbox)
  expect_s3_class(smaller_gtfs, dt_gtfs_class)
  expect_type(smaller_gtfs, "list")

  # all objects inside a dt_gtfs are data.tables
  invisible(lapply(smaller_gtfs, expect_s3_class, "data.table"))
})

test_that("doesn't change given gtfs", {
  # (except for some tables' indices)

  original_gtfs <- read_gtfs(spo_path)
  gtfs <- read_gtfs(spo_path)
  expect_identical(original_gtfs, gtfs)

  smaller_gtfs <- filter_by_sf(gtfs, bbox)
  expect_equal(original_gtfs, gtfs, check.attributes = FALSE)
})

test_that("supports sf, sfc and bbox objects", {
  result1 <- filter_by_sf(spo_gtfs, polygon)
  result2 <- filter_by_sf(spo_gtfs, sf::st_geometry(polygon))
  result3 <- filter_by_sf(spo_gtfs, bbox)

  expect_identical(result1, result2)
  expect_identical(result1, result3)
})

test_that("'keep' and 'spatial_operation' arguments work correctly", {
  # st_intersects

  shapes <- convert_shapes_to_sf(spo_gtfs)
  shapes_intersected <- sf::st_intersects(polygon, shapes, sparse = FALSE)
  shapes_intersected <- shapes[shapes_intersected, ]$shape_id

  trips <- get_trip_geometry(spo_gtfs, file = "stop_times")
  trips_intersected <- sf::st_intersects(polygon, trips, sparse = FALSE)
  trips_intersected <- trips[trips_intersected, ]$trip_id

  smaller_keeping <- filter_by_sf(spo_gtfs, bbox)
  expect_true(all(smaller_keeping$trips$trip_id %chin% trips_intersected))
  expect_true(all(smaller_keeping$shapes$shape_id %chin% shapes_intersected))

  smaller_not_keeping <- filter_by_sf(spo_gtfs, bbox, keep = FALSE)
  expect_true(!any(smaller_not_keeping$trips$trip_id %chin% trips_intersected))
  expect_true(
    !any(smaller_not_keeping$shapes$shape_id %chin% shapes_intersected)
  )

  # st_contains

  shapes_contained <- sf::st_contains(polygon, shapes, sparse = FALSE)
  shapes_contained <- shapes[shapes_contained, ]$shape_id

  trips_contained <- sf::st_contains(polygon, trips, sparse = FALSE)
  trips_contained <- trips[trips_contained, ]$trip_id

  smaller_keeping <- filter_by_sf(
    spo_gtfs,
    bbox,
    spatial_operation = sf::st_contains
  )
  expect_true(all(smaller_keeping$trips$trip_id %chin% trips_contained))
  expect_true(all(smaller_keeping$shapes$shape_id %chin% shapes_contained))

  smaller_not_keeping <- filter_by_sf(
    spo_gtfs,
    bbox,
    spatial_operation = sf::st_contains,
    keep = FALSE
  )
  expect_true(!any(smaller_not_keeping$trips$trip_id %chin% trips_contained))
  expect_true(
    !any(smaller_not_keeping$shapes$shape_id %chin% shapes_contained)
  )
})

test_that("works with sf describing two features", {
  another_shape <- "17846"
  another_bbox <- sf::st_bbox(convert_shapes_to_sf(spo_gtfs, another_shape))
  another_polygon <- sf::st_as_sf(sf::st_as_sfc(another_bbox))
  bigger_polygon <- rbind(polygon, another_polygon)

  smaller_gtfs <- filter_by_sf(
    spo_gtfs,
    bigger_polygon,
    spatial_operation = sf::st_contains
  )

  # shape 17847 is also contained inside 17846's bbox
  expect_true(
    all(smaller_gtfs$shapes$shape_id %chin% c("68962", "17846", "17847"))
  )
})

test_that("error if gtfs doesn't contain neither shapes nor stop_times table", {
  spo_gtfs$shapes <- NULL
  spo_gtfs$stop_times <- NULL
  expect_error(filter_by_sf(spo_gtfs, bbox))
})
