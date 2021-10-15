context("Get convert_shapes_to_sf")


# setup -------------------------------------------------------------------

data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)


# tests -------------------------------------------------------------------

### expected behavior
test_that("convert_shapes_to_sf - expected behavior", {

  shape_sf <- convert_shapes_to_sf(gtfs)

  # check class
  expect_s3_class(convert_shapes_to_sf(gtfs), 'sf')

  # check projection
  expect_equal(sf::st_crs(shape_sf)$input, "EPSG:4326")

  # check dimensions
  expect_equal(nrow(shape_sf), 36)
  expect_equal(ncol(shape_sf), 2)

})

### expected errors and messages

test_that("convert_shapes_to_sf - errors and warnings", {

  # Wrong crs
  testthat::expect_error(convert_shapes_to_sf(gtfs = gtfs, crs = 'aaaaa'))

  # Wrong gtfs input
  testthat::expect_error(convert_shapes_to_sf(gtfs = 'aaaaa'))

  # No stops data
  gtfs$shapes <- NULL
  testthat::expect_error(convert_shapes_to_sf(gtfs = gtfs))

})
