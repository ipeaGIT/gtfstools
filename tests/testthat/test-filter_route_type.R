context("Get filter_route_type")


# setup -------------------------------------------------------------------

data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)


# tests -------------------------------------------------------------------

### expected behavior
test_that("filter_route_type - expected behavior", {

  # check dimensions
  subset1 <- filter_route_type(gtfs, route_types=c(2, 3), keep=TRUE)
  expect_equal(nrow(subset1$routes), 13)
  expect_equal(nrow(subset1$trips), 24)
  expect_equal(nrow(subset1$shapes), 9277)

  # check dimensions
  subset2 <- filter_route_type(gtfs, route_types=c(2, 3), keep=FALSE)
  expect_equal(nrow(subset2$routes), 6)
  expect_equal(nrow(subset2$trips), 12)
  expect_equal(nrow(subset2$shapes), 3018)

  })

### expected errors and messages

test_that("filter_route_type - errors and warnings", {

  # Wrong keep input
  testthat::expect_error(filter_route_type(gtfs = gtfs, route_types= "6450-51", keep= 99999))

  # Wrong gtfs input
  testthat::expect_error(filter_route_type(gtfs = 'abcdefg'))

  # Wrong route_types input
  testthat::expect_error(filter_route_type(gtfs = gtfs, route_types= 99999))

  # No routes files
  gtfs$routes <- NULL
  testthat::expect_error(filter_route_type(gtfs = gtfs, route_types= 3))


})
