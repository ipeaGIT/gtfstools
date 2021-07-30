context("Get convert_stops_to_sf")


# setup -------------------------------------------------------------------

data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)


# tests -------------------------------------------------------------------

### expected behavior
test_that("convert_stops_to_sf - expected behavior", {

  stops1 <- convert_stops_to_sf(gtfs)

  # check class
  expect_s3_class(convert_stops_to_sf(gtfs), 'sf')

  # check projection
  expect_equal(sf::st_crs(stops1)$input, "EPSG:4326")

  # check dimensions
  expect_equal(nrow(stops1), 654)
  expect_equal(ncol(stops1), 4)

})

### expected errors and messages

test_that("convert_stops_to_sf - errors and warnings", {

  # Wrong crs
  testthat::expect_error(convert_stops_to_sf(gtfs = gtfs, crs = 'aaaaa'))

  # Wrong gtfs input
  testthat::expect_error(convert_stops_to_sf(gtfs = 'aaaaa'))

  # No stops data
  gtfs$stops <- NULL
  testthat::expect_error(convert_stops_to_sf(gtfs = gtfs))

})
