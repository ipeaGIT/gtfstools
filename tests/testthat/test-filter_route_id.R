context("Filter by route_id")


# setup -------------------------------------------------------------------

data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)


# tests -------------------------------------------------------------------

### expected behavior
test_that("filter_route_id - expected behavior", {

  # check dimensions
  subset1 <- filter_route_id(
    gtfs,
    route_id = c("6450-51", "CPTM L11"),
    keep=TRUE
  )
  expect_equal(nrow(subset1$routes), 2)
  expect_equal(nrow(subset1$trips), 3)
  expect_equal(nrow(subset1$shapes), 1654)

  # check dimensions
  subset2 <- filter_route_id(gtfs, route_id=c("6450-51", "CPTM L11"), keep=FALSE)
  expect_equal(nrow(subset2$routes), 17)
  expect_equal(nrow(subset2$trips), 33)
  expect_equal(nrow(subset2$shapes), 10641)

  })

### expected errors and messages

test_that("filter_route_id - errors and warnings", {

  # Wrong keep input
  testthat::expect_error(filter_route_id(gtfs = gtfs, route_id= "6450-51", keep= 99999))

  # Wrong gtfs input
  testthat::expect_error(filter_route_id(gtfs = 'abcdefg'))

  # Wrong route_ids input
  testthat::expect_error(filter_route_id(gtfs = gtfs, route_id= 99999))

})
